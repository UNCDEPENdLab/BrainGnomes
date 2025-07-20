#include "BrainGnomes.h"

// to support RNifti object returns via Rcpp, we need RNiftiAPI.h
// this should only be included once for the entire package because it includes function implementations
// see: https://cran.r-project.org/web/packages/RNifti/readme/README.html
#include "RNiftiAPI.h"

// helper function for writing out nifti
void writeToFile(const RNifti::NiftiImage& image, const std::string& outfile, int datatype) {
  if (!outfile.empty()) {
    // write interpolated image to file
    image.toFile(outfile, datatype);
  }
}

//' Interpolate fMRI Time Series with Cubic Splines in a NIfTI File
//'
//' This function performs voxelwise natural cubic spline interpolation over the time
//' dimension of a 4D NIfTI image. Timepoints to interpolate are specified, and interpolation
//' is applied independently to each voxel's time series.
//'
//' The function reads the NIfTI image from disk, performs interpolation in memory,
//' and optionally writes the result back to a new NIfTI file.
//'
//' @name natural_spline_4d
//' @param infile Character string. Path to the input 4D NIfTI file (e.g., BOLD fMRI data).
//' @param t_interpolate Integer vector (1-based). Specifies the timepoints (TRs) to interpolate.
//'        Timepoints outside the valid range [1, T] are ignored with a warning.
//' @param edge_nn Logical. If \code{TRUE}, extrapolated values at the edges of the time series
//'        are filled in using nearest-neighbor extrapolation instead of cubic splines.
//' @param outfile Character string (optional). If provided, the interpolated image will
//'        be written to this path. If omitted, the result is returned but not saved.
//' @param internal Logical. If FALSE (the default), an array of class "niftiImage", containing
//'        the image pixel or voxel values, will be returned. If TRUE, the return value will be
//'        an object of class "internalImage", which contains only minimal metadata about the image.
//'        Either way, the return value has an attribute which points to a C data structure
//'        containing the full image. Cf. `RNifti::readNifti`
//'
//' @return A \code{niftiImage} object with the same dimensions and metadata as the input,
//'         with interpolated values inserted at the specified timepoints.
//'
//' @details The interpolation is voxelwise and assumes column-major order. If a voxel time series
//' has fewer than three valid (non-interpolated) timepoints, or is constant across time, it is skipped.
//' Linear extrapolation is used for timepoints outside the valid range if \code{edge_nn = FALSE}, matching
//' R's `splinefun` approach with natural splines. If \code{edge_nn = TRUE}, nearest-neighbor extrapolation
//' is used for interpolation timepoints at the beginning or end of the timeseries, potentially reducing
//' extreme values in extrapolation.
//'
//' This implementation uses RNifti’s C++ API (\code{NiftiImage}, \code{NiftiImageData}) for efficient
//' memory access and file handling, allowing the function to operate directly on NIfTI files.
//'
//' @examples
//' \dontrun{
//' out_img <- natural_spline_4d(
//'   infile = "bold.nii.gz",
//'   t_interpolate = 91:95,
//'   outfile = "bold_interpolated.nii.gz",
//'   edge_nn = TRUE
//' )
//' }
//' @import RNifti
//' @export

// [[Rcpp::export]]
Rcpp::RObject natural_spline_4d(std::string infile, const std::vector<int>& t_interpolate , bool edge_nn = false, std::string outfile = "", bool internal = false) {
  // N.B. RNifti provides a .toArrayOrPointer method for the NiftiImage class that returns an Rcpp::Robject class, which
  // can then be handled directly by conventional Rcpp (see Rnifti main.cpp readNifti function). This proves
  // far easier than attempting a direct RcppExport SEXP approach, which requires manual call registration
  
  NiftiImage image(infile); // read nifti 44
  int datatype = image->datatype; // preserve initial datatype in output file
  
  /* integer data types can have significant problems because they rely on scl_inter (intercept) and
  * scl_slope (slope) to convert integers to floating point values. As a result, if the integers are near
  * the storage boundary (e.g., -32768 for INT16) and interpolation produces values out of the range, invalid
  * values will be returned (unless we rescale the integers, slope, and intercept). Thus, convert to FLOAT32
  * Note that image.changeDataType should work, but it throws and error if we with to use the slope in the file,
  * like image.changeDatatype(DT_FLOAT32, true); because it complains "Resetting the slope and intercept for an 
  * image with them already set is not supported" (NiftiImage_impl.h, line 744). But we cannot rely on unscaled
  * values because they will be just random integers (not affected by intercept and slope). My workaround is to
  * essentially do what .changeDataType does here to convert to FLOAT32 */
  if (datatype == DT_INT8 || datatype == DT_INT16 || datatype == DT_INT32 || datatype == DT_INT64) {
    //image.changeDatatype(DT_FLOAT32, true); // doesn't work as expected
    NiftiImageData float_data(image.data(), DT_FLOAT32); // this will get the scaled data as expected
    image.replaceData(float_data); // force float32 data back into NiftiImage object
    float_data.disown(); // explicitly free pointer to this temporary object
  }
  
  NiftiImageData data(image); // get pointer to image data
  
  //long nvox = data.length();
  std::vector<long long int> dims = image.dim();
  if (dims.size() < 4 || dims[0] <= 0 || dims[1] <= 0 || dims[2] <= 0 || dims[3] <= 0) {
    stop("Input image must be 4D");
  }
  
  int n_x = dims[0], n_y = dims[1], n_z = dims[2], n_t = dims[3];
  
  // Helper function to index 4D array stored as vector in column-major order
  auto flat_index = [&](int x, int y, int z, int t) {
    return x + n_x * (y + n_y * (z + n_z * t));
  };

  std::ostringstream msg;
  
  int n_pts = t_interpolate.size(); // number of timepoints to interpolate
  if (n_pts < 1) {
    Rcpp::warning("No timepoints to interpolate. Just returning unchanged image.");
    writeToFile(image, outfile, datatype);
    return image.toArrayOrPointer(internal, "NIfTI image");
  }
  
  // Validate timepoints -- note that we assume R-friendly 1-based indexing!
  std::vector<double> xout;
  for (int ti : t_interpolate) {
    if (ti < 1 || ti > n_t) {
      msg << "Invalid t_interpolate value: " << ti << ". t_interpolate must be between 1 and " << n_t << ".";
      stop(msg.str()); 
    } else {
      xout.push_back(ti - 1.0); // populate xout as a 0-indexed double vector compatible with natural_spline_interp
    }
  }
  
  std::vector<bool> mask(n_t, true);
  for (int ti : xout) mask[ti] = false;
  
  std::vector<double> x(n_t); // build a 0-based index vector for x (time/volume)
  for (int ti = 0; ti < n_t; ++ti) x[ti] = ti;
  
  std::vector<double> x_valid; // build a vector of x values at which to interpolate y
  for (int ti = 0; ti < n_t; ++ti) {
    if (mask[ti]) x_valid.push_back(ti);
  }
  
  if (x_valid.size() < 3) {
    stop("Fewer than 3 valid timepoints for interpolation.");
  }
  
  // calculate the first and last valid (non-interpolated) timepoints; used for edge interpolation
  int first_valid = -1, last_valid = -1;
  if (edge_nn) {
    for (int t = 0; t < n_t; ++t) {
      if (mask[t]) {
        if (first_valid == -1) first_valid = t;
        last_valid = t;
      }
    }
  }
  
  std::vector<double> y(n_t); // working time series for voxel[x,y,z]
  std::vector<double> y_valid(n_pts); // working time series for interpolated y values
  std::vector<double> y_interp(n_pts);
  
  // int n_const_ts = 0;
  
  for (int xi = 0; xi < n_x; ++xi) {
    for (int yi = 0; yi < n_y; ++yi) {
      for (int zi = 0; zi < n_z; ++zi) {
        
        // extract time series for voxel[x,y,z]
        for (int ti = 0; ti < n_t; ++ti) {
          y[ti] = data[flat_index(xi, yi, zi, ti)];
        }
        
        y_valid.clear(); // reset vector for non-interpolated time series to avoid push_back growth
        for (int k = 0; k < n_t; ++k) {
          if (mask[k]) y_valid.push_back(y[k]);
        }
        
        // skip constant time series for speed
        double first_val = y_valid[0];
        bool is_constant = true;
        for (double val : y_valid) {
          if (val != first_val) {
            is_constant = false;
            break;
          }
        }
        
        if (is_constant) {
          // n_const_ts += 1;
          continue;
        }
        
        // perform interpolation if time series is not constant
        y_interp = natural_spline_interp(x_valid, y_valid, xout);
        
        // handle nearest-neighbor edge interpolation, if requested
        if (edge_nn && first_valid >= 0 && last_valid >= 0) {
          for (size_t i = 0; i < n_pts; ++i) {
            int t = xout[i];
            if (t < first_valid) y_interp[i] = y[first_valid];
            else if (t > last_valid) y_interp[i] = y[last_valid];
          }
        }
        
        // Write back any vector that has been interpolated
        for (size_t ti = 0; ti < n_pts; ++ti) {
          int t = xout[ti];
          data[flat_index(xi, yi, zi, t)] = static_cast<float>(y_interp[ti]); //y_interp[ti]; //
          //data[flat_index(xi, yi, zi, t)] = (y_interp[ti] - intercept) / slope;
        }
      }
    }
  }

  // for debugging int data types with intercept and slope
  // Rcpp::Rcout << "slope: " << data.slope << ", intercept: " << data.intercept << std::endl;
  // Rcpp::Rcout << "n_const_ts: " << n_const_ts << std::endl;
  
  // write interpolated image to file, if requested
  writeToFile(image, outfile, datatype);
  
  return image.toArrayOrPointer(internal, "NIfTI image");
}

