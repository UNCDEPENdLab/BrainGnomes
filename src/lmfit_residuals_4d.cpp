#include "BrainGnomes.h"

//' Apply Confound Regression to 4D fMRI Data Using Voxelwise Linear Models
//'
//' This function performs voxelwise confound regression by fitting a linear model to a subset of timepoints
//' (e.g., uncensored volumes) for each voxel in a 4D NIfTI image. The fitted model is then applied to all
//' timepoints to obtain predicted values, and the residuals are returned as a cleaned 4D image.
//'
//' This approach mirrors the strategy used in the XCP-D pipeline, where nuisance regressors are fit only
//' to valid (non-censored) timepoints to prevent bias, but the resulting model is applied to the full
//' dataset including censored timepoints (e.g., for continuity or interpolation).
//'
//' Constant columns in the design matrix are automatically removed. If an intercept column (all ones) is present,
//' it is preserved; otherwise, an intercept will be added if \code{add_intercept = TRUE}.
//'
//' @name lmfit_residuals_4d
//'
//' @param infile Path to a 4D NIfTI image file to denoise (e.g., functional data).
//' @param X A numeric matrix where rows correspond to timepoints and columns to nuisance regressors.
//'          Typically includes motion parameters, physiological noise, etc.
//' @param include_rows Logical vector indicating which timepoints to include in model fitting (e.g., uncensored).
//'          Must be the same length as the number of timepoints in the image and the number of rows in \code{X}.
//' @param add_intercept Logical; if \code{TRUE}, adds an intercept column to the design matrix unless one is already present.
//' @param outfile Optional path to write the output residuals image. If empty, no file is written.
//' @param internal Logical; if \code{TRUE}, returns an internal RNifti pointer. Otherwise returns an R array.
//' @param preserve_mean Logical; if \code{TRUE}, constant time series will be left unchanged (not demeaned or recentered).
//' @param set_mean Optional numeric value; if specified, all residual time series will be shifted to have this mean
//'        (default is 0). Cannot be used in combination with \code{preserve_mean = TRUE}.
//'
//' @return A residualized 4D NIfTI image, either as an in-memory array or RNifti object (if \code{internal = TRUE}).
//' @export
//'
//' @examples
//' \dontrun{
//'   X <- cbind(1, motion_params, compcor)
//'   residual_img <- lmfit_residuals_4d(
//'     infile = "func.nii.gz",
//'     X = X,
//'     include_rows = !censor_vector,
//'     add_intercept = TRUE,
//'     outfile = "residual.nii.gz",
//'     set_mean = 1000
//'   )
//' }
//' 
//' @references
//' Ciric, R. et al. (2018). Mitigating head motion artifact in functional connectivity MRI. *Nature Protocols*.
//' https://xcp-d.readthedocs.io/en/latest/
//' https://dannyjameswilliams.co.uk/portfolios/sc2/rcpp/

// [[Rcpp::depends(RcppArmadillo, RNifti)]]
// [[Rcpp::export]]
Rcpp::RObject lmfit_residuals_4d(std::string infile, const arma::mat& X, const LogicalVector& include_rows, 
  bool add_intercept = true, std::string outfile = "", bool internal = false,
  bool preserve_mean = false, double set_mean = 0.0) {
  
  bool use_set = std::abs(set_mean) > 1e-8;
  
  if (use_set && preserve_mean) {
    Rcpp::warning("Cannot use preserve_mean = TRUE and have a non-zero value for set_mean. The set_mean will be ignored.");
  }
  
  RNifti::NiftiImage image(infile); // read nifti
  int datatype = image->datatype;
  
  // Convert to float if using integer types (due to slope/intercept issues)
  if (datatype == DT_INT8 || datatype == DT_INT16 || datatype == DT_INT32 || datatype == DT_INT64) {
    RNifti::NiftiImageData float_data(image.data(), DT_FLOAT32);
    image.replaceData(float_data);
    float_data.disown();
  }
  
  RNifti::NiftiImageData data(image);
  
  std::vector<dim_t> dims = image.dim();
  if (dims.size() < 4 || dims[3] <= 1) stop("Image must be 4D with more than 1 timepoint.");
  
  int n_x = dims[0], n_y = dims[1], n_z = dims[2];
  arma::uword n_t = dims[3]; // use uword for consistency with arma matrix size types
  
  if (X.n_rows != n_t || include_rows.size() != n_t) {
    stop("X and include_rows must match the number of timepoints in the image.");
  }
  
  // Add intercept if requested
  arma::mat X_use = X;
  std::vector<arma::uword> keep_cols;
  bool has_intercept = false;
  
  // check for constant columns and existing intercept
  for (arma::uword j = 0; j < X.n_cols; ++j) {
    double min_val = X.col(j).min();
    double max_val = X.col(j).max();
    double range = std::abs(max_val - min_val);
    
    if (std::abs(max_val - 1.0) < 1e-8 && std::abs(min_val - 1.0) < 1e-8 && !has_intercept) {
      has_intercept = true;
      keep_cols.push_back(j); // keep the first intercept
    } else if (range < 1e-8) {
      Rcpp::Rcout << "Dropping constant column " << j + 1 << " from design matrix." << std::endl;
    } else {
      keep_cols.push_back(j);
    }
  }
  
  X_use = X_use.cols(arma::uvec(keep_cols));
  
  // Add intercept if needed
  if (!has_intercept && add_intercept) {
    X_use.insert_cols(0, arma::vec(n_t, arma::fill::ones));
  }
  
  // Build vector of selected row indices for regression fitting
  std::vector<arma::uword> indices;
  for (arma::uword ti = 0; ti < n_t; ++ti) {
    if (include_rows[ti]) indices.push_back(ti);
  }
  
  arma::uword n_t_sub = indices.size();
  
  // fail on rank deficient regression (p > n)
  if (n_t_sub < X_use.n_cols) {
    stop("Not enough uncensored timepoints to estimate model: %d available, but need at least %d", n_t_sub, X_use.n_cols);
  }
  
  arma::uvec idx = arma::uvec(indices);
  arma::mat X_sub = X_use.rows(idx);
  
  // lookup function for datapoint inside 4D data matrix
  auto flat_index = [&](int x, int y, int z, int t) {
    return x + n_x * (y + n_y * (z + n_z * t));
  };
  
  arma::vec y(n_t);
  arma::vec y_sub(n_t_sub); // subset of y for included rows
  arma::vec beta, fitted(n_t), residuals(n_t);

  for (int xi = 0; xi < n_x; ++xi) {
    for (int yi = 0; yi < n_y; ++yi) {
      for (int zi = 0; zi < n_z; ++zi) {
        
        for (arma::uword ti = 0; ti < n_t; ++ti) {
          y[ti] = data[flat_index(xi, yi, zi, ti)]; // this timeseries
        }
        
        y_sub = y.elem(idx); // valid points in y
        
        // Based on basic timing tests, we get a small improvement in speed if we skip constant time series
        double range = y_sub.max() - y_sub.min();
        if (std::abs(range) < 1e-8) {
          residuals.fill(0.0);
        } else {
          beta = arma::solve(X_sub, y_sub);  // OLS coefficients
          fitted = X_use * beta; // compute fits on full time series
          residuals = y - fitted; // compute residuals on full time series  
        }
        
        if (preserve_mean) {
          residuals += arma::mean(y_sub); // add back the mean of the valid timepoints
        } else if (use_set) {
          residuals += set_mean; // add intended mean, if requested 
        }
        
        for (arma::uword ti = 0; ti < n_t; ++ti) {
          data[flat_index(xi, yi, zi, ti)] = static_cast<float>(residuals[ti]);
        }
      }
    }
  }
  
  // Write output if requested
  if (!outfile.empty()) {
    image.toFile(outfile, datatype);
  }
  
  return image.toArrayOrPointer(internal, "NIfTI image");
}