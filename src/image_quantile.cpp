#define RNIFTI_NIFTILIB_VERSION 2
#include <Rcpp.h>
#include <algorithm>
#include <vector>
#include <cmath>
#include <stdexcept>
#include <RNifti.h>
// #include "RNiftiAPI.h"  // uncomment to use with sourceCpp
typedef int64_t dim_t;

using namespace Rcpp;

//' Compute Quantiles from a 3D or 4D NIfTI Image
//'
//' Computes one or more quantiles from a 3D or 4D NIfTI image. Optionally applies a 3D brain mask
//' and/or excludes zero-valued voxels. For 4D images, the function pools over all timepoints.
//'
//' @name image_quantile
//' @param in_file Path to the input 3D or 4D NIfTI image (.nii or .nii.gz).
//' @param brain_mask Optional path to a 3D NIfTI image used as a brain mask. Voxels with values > 0.001 are retained.
//'                  The mask must have the same spatial dimensions as the input image. If \code{R_NilValue}, no mask is used.
//' @param quantiles A numeric vector of probabilities in `[0, 1]` specifying which quantiles to compute (e.g., 0.5 for the median).
//' @param exclude_zero If \code{true}, zero-valued voxels in the image will be excluded from the quantile calculation.
//'
//' @return A named numeric vector of quantiles. Names are formatted as percentage strings (e.g., "50.00%").
//'
//' @details
//' - For 4D images, the mask (if used) is applied identically to all volumes.
//' - Quantile calculation uses partial sorting for performance (via \code{std::nth_element}).
//' - Throws an error if no voxels are valid after masking or zero exclusion.
//'
//' @examples
//' \dontrun{
//' # Compute the median
//' image_quantile("bold.nii.gz", 0.5)
//'
//' # With masking and zero exclusion
//' image_quantile("bold.nii.gz", "mask.nii.gz", c(0.25, 0.5, 0.75), exclude_zero=TRUE)
//' }
//'
//' @export

// [[Rcpp::export]]
NumericVector image_quantile(std::string in_file,
                             Nullable<std::string> brain_mask = R_NilValue,
                             NumericVector quantiles = NumericVector::create(0.5),
                             bool exclude_zero = false) {
  
  // validate quantile specification between 0 and 1
  for (R_xlen_t i = 0; i < quantiles.size(); ++i) {
    if (quantiles[i] > 1.0 || quantiles[i] < 0.0) stop("All quantiles must be between 0 and 1.");
  }
  
  // read image
  RNifti::NiftiImage image(in_file);
  
  // grab voxel data into a float vector
  std::vector<float> img_data(image.data().begin(), image.data().end());
  
  // Get image dimensions
  std::vector<dim_t> dims = image.dim();
  size_t ndim = dims.size();
  if (ndim < 3 || ndim > 4) stop("Only 3D and 4D images are supported.");
  
  size_t nx = dims[0], ny = dims[1], nz = dims[2];
  size_t nt = (ndim == 4) ? dims[3] : 1;
  size_t vol_size = nx * ny * nz; // spatial dimensions
  
  // vector of whether to include voxel in calculation
  std::vector<bool> include(img_data.size(), true);
  
  bool use_mask = brain_mask.isNotNull();
  std::vector<float> mask_data; // empty mask_data definition
  
  // handle image mask to constrain quantile calculations
  if (use_mask) {
    RNifti::NiftiImage mask_img(as<std::string>(brain_mask));
    std::vector<dim_t> mask_dims = mask_img.dim();
    if (mask_dims.size() != 3 ||
        mask_dims[0] != nx || mask_dims[1] != ny || mask_dims[2] != nz) {
      stop("Mask must be 3D and match the spatial dimensions of the image.");
    }
    
    // populate mask data
    // this does not work because the NiftiImageData iterator does not support +=
    // mask_data.assign(mask_img.data().begin(), mask_img.data().end());
    
    // resize the vector and do a standard data copy
    mask_data.resize(mask_img.nVoxels());
    std::copy(mask_img.data().begin(), mask_img.data().end(), mask_data.begin());
    mask_img.dropData(); // drop mask data from memory (no longer needed)
  }
  
  // repeatedly re-use 3D mask, if relevant
  for (size_t t = 0; t < nt; ++t) {
    for (size_t i = 0; i < vol_size; ++i) {
      size_t idx = t * vol_size + i;
      bool include_voxel = true;
      if (use_mask) include_voxel = include_voxel && (mask_data[i] > 0.001);
      if (exclude_zero && include_voxel) include_voxel = include_voxel && (img_data[idx] != 0.0f);
      include[idx] = include_voxel;
      }
  }
  
  int nvals = std::count(include.begin(), include.end(), true);
  std::vector<float> values; // vector of values for quantile calculation
  values.reserve(nvals);
  
  // Rcout << "Voxels included is: " << nvals << std::endl;
  
  // populate values vector
  for (size_t i = 0; i < img_data.size(); ++i) {
    if (include[i]) values.push_back(img_data[i]);
  }
  
  if (values.empty()) stop("No valid voxels found for quantile calculation.");
  
  // this uses std::nth_element to only reorder values up to the point of interest (e.g., the 10th percentile), whereas sort reorders the full array (slower)
  NumericVector result(quantiles.size());
  CharacterVector result_names(quantiles.size());
  
  for (R_xlen_t i = 0; i < quantiles.size(); ++i) {
    double p = quantiles[i];
    
    //double q = p / 100.0;
    double pos = p * (values.size() - 1);
    // identify the two nearest ranks in a sorted vector of values (if we need to interpolate between tem)
    size_t lo = static_cast<size_t>(std::floor(pos));
    size_t hi = static_cast<size_t>(std::ceil(pos));
    double frac = pos - lo; // will be zero if pos resolves to an integer (quantile falls precisely on a value)
    
    // Rearranges elements so that values[lo] is the correct lo-th smallest value (but leaves the rest unordered).
    std::nth_element(values.begin(), values.begin() + lo, values.end());
    float val_lo = values[lo];
    float val_hi;
    
    if (hi == lo) { // If no interpolation is needed, use val_lo for both ends.
      val_hi = val_lo;
    } else { // Otherwise, partially sort again to get val_hi, the upper neighbor for interpolation.
      std::nth_element(values.begin(), values.begin() + hi, values.end());
      val_hi = values[hi];
    }
    
    // Linear interpolation between the two elements in values that are closest to the target quantile
    result[i] = val_lo * (1.0 - frac) + val_hi * frac;
    
    // Format name as "50%" etc.
    std::ostringstream ss;
    ss << std::fixed << std::setprecision(2) << p*100.0 << "%";
    result_names[i] = ss.str();
  }
  
  result.attr("names") = result_names;
  return result;
}

