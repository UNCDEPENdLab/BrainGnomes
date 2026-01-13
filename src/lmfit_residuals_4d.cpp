#include "BrainGnomes.h"
#include <algorithm>
#include <cmath>
#include <limits>
#include <numeric>
#include <sstream>
#include <string>

// for sourceCpp compilation
// #define RNIFTI_NIFTILIB_VERSION 2
// #include <RcppArmadillo.h>
// #include <array>
// #include <vector>
// #include <queue>
// #include <cmath>
// #include <RNifti.h>
// #include "RNiftiAPI.h"

// using namespace Rcpp;
// using namespace RNifti;

// typedef int64_t dim_t;

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
//' it is preserved. If \code{add_intercept = TRUE}, an intercept column will be added (if not present).
//'
//' @name lmfit_residuals_4d
//'
//' @param infile Path to a 4D NIfTI image file to denoise (e.g., functional data).
//' @param X A numeric matrix where rows correspond to timepoints and columns to nuisance regressors.
//'          Typically includes motion parameters, physiological noise, etc.
//' @param include_rows Optional logical vector identifying the timepoints used when estimating the model
//'          (e.g., uncensored volumes). If supplied it must have length \code{nrow(X)}; when \code{NULL}, all timepoints are used.
//' @param add_intercept Logical; if \code{TRUE}, adds an intercept column to the design matrix unless one is already present.
//' @param outfile Optional path to write the output residuals image. If empty, no file is written.
//' @param internal Logical; if \code{TRUE}, returns an internal RNifti pointer. Otherwise returns an R array.
//' @param preserve_mean Logical; if \code{TRUE}, constant time series will be left unchanged (not demeaned or recentered).
//' @param set_mean Optional numeric value; if specified, all residual time series will be shifted to have this mean
//'        (default is 0). Cannot be used in combination with \code{preserve_mean = TRUE}.
//' @param regress_cols Optional integer vector (1-based) indicating which columns of \code{X} should be regressed out.
//'        When omitted, all non-constant columns are removed unless \code{exclusive = TRUE}.
//' @param exclusive Logical; if \code{TRUE}, only the columns listed in \code{regress_cols} (and an intercept, if present)
//'        are used to estimate the model. This allows for partial regression that preserves other effects.
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
Rcpp::RObject lmfit_residuals_4d(
    std::string infile, const arma::mat& X, 
    const Rcpp::Nullable<Rcpp::LogicalVector>& include_rows = R_NilValue,
    bool add_intercept = false, std::string outfile = "", bool internal = false,
    bool preserve_mean = false, double set_mean = 0.0,
    const Rcpp::Nullable<Rcpp::IntegerVector>& regress_cols = R_NilValue,
    bool exclusive = false) {
  
  bool use_pivoted_qr = false; // toggle used in tests to force the robust solver path
  bool use_set = std::abs(set_mean) > 1e-8;
  if (use_set && preserve_mean) {
    Rcpp::warning("Cannot use preserve_mean = TRUE and have a non-zero value for set_mean. The set_mean will be ignored.");
  }
  
  // Materialize nullable regress_cols so downstream logic can assume a concrete vector.
  Rcpp::IntegerVector regress_cols_vec;
  if (regress_cols.isNotNull()) regress_cols_vec = regress_cols.get();
  else regress_cols_vec = Rcpp::IntegerVector(0);
  if (exclusive && regress_cols_vec.size() == 0) {
    stop("exclusive = TRUE requires regress_cols to specify at least one column.");
  }
  
  // Normalize include_rows: missing input becomes an empty logical vector we treat as "use all rows".
  Rcpp::LogicalVector _include_rows;
  if (include_rows.isNotNull()) _include_rows = include_rows.get();
  else _include_rows = Rcpp::LogicalVector();
  
  RNifti::NiftiImage image(infile); // read nifti
  int datatype = image->datatype;
  
  // Convert to float if using integer types (due to slope/intercept issues)
  if (datatype == DT_INT8 || datatype == DT_INT16 || datatype == DT_INT32 || datatype == DT_INT64 ||
      datatype == DT_UINT8 || datatype == DT_UINT16 || datatype == DT_UINT32 || datatype == DT_UINT64) {
    RNifti::NiftiImageData float_data(image.data(), DT_FLOAT32);
    image.replaceData(float_data);
    float_data.disown();
  }
  
  RNifti::NiftiImageData data(image);
  
  std::vector<dim_t> dims = image.dim();
  if (dims.size() < 4 || dims[3] <= 1) stop("Image must be 4D with more than 1 timepoint.");
  
  int n_x = dims[0], n_y = dims[1], n_z = dims[2];
  arma::uword n_t = dims[3]; // use uword for consistency with arma matrix size types

  if (X.n_rows != n_t) {
    stop("X must have the same number of rows as image timepoints.");
  }
  if (include_rows.isNotNull() && _include_rows.size() != n_t) {
    stop("include_rows must be length %d (number of timepoints) or empty.", n_t);
  }

  // Build vector of indices for the subset of timepoints that participate in the fit.
  std::vector<arma::uword> indices;
  indices.reserve(n_t);
  if (include_rows.isNull()) {
    for (arma::uword ti = 0; ti < n_t; ++ti) {
      indices.push_back(ti);
    }
  } else {
    for (arma::uword ti = 0; ti < n_t; ++ti) {
      if (_include_rows[ti] == TRUE) {
        indices.push_back(ti);
      }
    }
  }

  arma::uword n_t_sub = indices.size();
  if (n_t_sub == 0) {
    stop("No timepoints selected for fitting (include_rows has no TRUE values).");
  }

  arma::uvec idx(indices.size());
  for (std::size_t i = 0; i < indices.size(); ++i) {
    idx[i] = indices[i];
  }
  arma::mat X_valid = X.rows(idx);

  // Detect constant columns (except the first intercept); drop them to keep the design full rank.
  std::vector<arma::uword> keep_cols;
  keep_cols.reserve(X.n_cols);
  std::vector<arma::uword> dropped_constant_cols;
  bool has_intercept = false;
  arma::sword intercept_original = -1;

  for (arma::uword j = 0; j < X.n_cols; ++j) {
    arma::vec col = X_valid.col(j);
    double min_val = col.min();
    double max_val = col.max();
    double range = std::abs(max_val - min_val);
    bool is_intercept = std::abs(max_val - 1.0) < 1e-8 && std::abs(min_val - 1.0) < 1e-8;

    if (is_intercept && !has_intercept) {
      has_intercept = true;
      intercept_original = static_cast<arma::sword>(j);
      keep_cols.push_back(j); // preserve the first intercept column encountered
    } else if (range < 1e-8) {
      dropped_constant_cols.push_back(j + 1); // record 1-based column index for reporting
    } else {
      keep_cols.push_back(j);
    }
  }

  if (!dropped_constant_cols.empty()) {
    std::ostringstream msg;
    for (std::size_t i = 0; i < dropped_constant_cols.size(); ++i) {
      if (i > 0) msg << ", ";
      msg << dropped_constant_cols[i];
    }
    std::string dropped_str = msg.str();
    Rcpp::warning("Dropping constant column(s) from design matrix: %s", dropped_str.c_str());
  }

  arma::mat X_use;
  if (keep_cols.empty()) {
    X_use = arma::mat(n_t, 0);
  } else {
    arma::uvec keep_cols_uvec(keep_cols.size());
    for (std::size_t i = 0; i < keep_cols.size(); ++i) {
      keep_cols_uvec[i] = keep_cols[i];
    }
    X_use = X.cols(keep_cols_uvec);
  }

  std::vector<int> original_to_kept(X.n_cols, -1);
  for (std::size_t new_index = 0; new_index < keep_cols.size(); ++new_index) {
    original_to_kept[keep_cols[new_index]] = static_cast<int>(new_index);
  }

  arma::sword intercept_idx = -1;
  // Track the intercept column (if we kept one) so exclusive fits can retain it without rescanning.
  if (intercept_original >= 0) {
    int mapped_idx = original_to_kept[static_cast<std::size_t>(intercept_original)];
    if (mapped_idx >= 0) {
      intercept_idx = static_cast<arma::sword>(mapped_idx);
    }
  }

  // Decide which columns will be regressed out. When exclusive = FALSE we default to regressing everything.
  std::vector<bool> regress_mask(keep_cols.size(), false);
  if (regress_cols_vec.size() != 0) {
    for (int j = 0; j < regress_cols_vec.size(); ++j) {
      int original_col = regress_cols_vec[j] - 1; // shift to 0-based indexing
      if (original_col < 0 || original_col >= static_cast<int>(original_to_kept.size())) {
        continue;
      }
      int mapped = original_to_kept[original_col];
      if (mapped >= 0) regress_mask[static_cast<std::size_t>(mapped)] = true;
    }
  } else if (!exclusive) {
    std::fill(regress_mask.begin(), regress_mask.end(), true);
  }

  std::vector<arma::uword> regress_indices;
  regress_indices.reserve(keep_cols.size());
  for (std::size_t j = 0; j < regress_mask.size(); ++j) {
    if (regress_mask[j]) {
      regress_indices.push_back(static_cast<arma::uword>(j));
    }
  }

  // Add an explicit intercept if the caller asked for one and the surviving design lacks it.
  if (!has_intercept && add_intercept) {
    X_use.insert_cols(0, arma::vec(n_t, arma::fill::ones));
    // ensure the added intercept is always regressed out
    for (auto &col_index : regress_indices) {
      col_index += 1; // shift existing selections
    }
    regress_indices.insert(regress_indices.begin(), static_cast<arma::uword>(0));
    has_intercept = true;
    intercept_idx = 0;
  }
  
  // Validate regressor indices after any modifications. Remove entries that no longer exist.
  regress_indices.erase(
      std::remove_if(
          regress_indices.begin(),
          regress_indices.end(),
          [&](arma::uword col) { return col >= X_use.n_cols; }),
      regress_indices.end());

  if (regress_indices.empty() && X_use.n_cols > 0 && !exclusive && regress_cols_vec.size() == 0) {
    regress_indices.resize(X_use.n_cols);
    std::iota(regress_indices.begin(), regress_indices.end(), static_cast<arma::uword>(0));
  }
  
  if (exclusive && regress_indices.empty()) {
    stop("exclusive = TRUE requires at least one usable regressor column after preprocessing.");
  }

  // Map the validated regressors into Armadillo objects. For exclusive fits, we also shrink the
  // matrix seen by the solver so nuisance columns cannot bleed into the pivoted solution.
  arma::uvec regress_cols_uvec(regress_indices.size());
  for (std::size_t i = 0; i < regress_indices.size(); ++i) {
    regress_cols_uvec[i] = regress_indices[i];
  }

  std::vector<arma::uword> fit_cols_vec;
  if (exclusive) {
    std::vector<bool> fit_mask(X_use.n_cols, false);
    for (arma::uword col : regress_indices) fit_mask[col] = true;
    if (intercept_idx >= 0) fit_mask[static_cast<std::size_t>(intercept_idx)] = true;
    for (arma::uword j = 0; j < X_use.n_cols; ++j) {
      if (fit_mask[j]) fit_cols_vec.push_back(j);
    }
  } else {
    fit_cols_vec.reserve(X_use.n_cols);
    for (arma::uword j = 0; j < X_use.n_cols; ++j) {
      fit_cols_vec.push_back(j);
    }
  }

  arma::uvec fit_cols_uvec(fit_cols_vec.size());
  std::vector<arma::sword> fit_lookup(X_use.n_cols, -1);
  for (std::size_t i = 0; i < fit_cols_vec.size(); ++i) {
    arma::uword col = fit_cols_vec[i];
    fit_cols_uvec[i] = col;
    fit_lookup[col] = static_cast<arma::sword>(i);
  }

  arma::mat X_fit = X_use.cols(fit_cols_uvec);
  arma::mat X_regress = X_use.cols(regress_cols_uvec);
  
  arma::uvec predict_indices(regress_cols_uvec.n_elem);
  for (arma::uword k = 0; k < regress_cols_uvec.n_elem; ++k) {
    arma::uword target_col = regress_cols_uvec[k];
    arma::sword mapped = fit_lookup[target_col];
    if (mapped < 0) {
      stop("Internal error: regression column not present in fitting matrix.");
    }
    predict_indices[k] = static_cast<arma::uword>(mapped);
  }

  // Basic sanity check: do the uncensored timepoints outnumber the predictors we intend to fit?
  if (n_t_sub < X_fit.n_cols) {
    stop("Not enough uncensored timepoints to estimate model: %d available, but need at least %d", n_t_sub, X_fit.n_cols);
  }

  arma::mat X_sub = X_fit.rows(idx);
  arma::mat Q_proj;
  arma::mat R_factor;
  arma::uvec piv;
  arma::mat Q_tmp, R_tmp;
  arma::uword n_predictors = X_sub.n_cols;
  bool used_pivot = false;

  if (n_predictors == 0) stop("Design matrix has no columns after preprocessing.");
  
  if (use_pivoted_qr || !arma::qr_econ(Q_tmp, R_tmp, X_sub)) {
    // Rcout << "Using pivoted QR" << std::endl;
    arma::mat Q_full, R_full;
    if (!arma::qr(Q_full, R_full, piv, X_sub, "vector")) {
      stop("QR decomposition failed (both qr_econ and pivoted qr).");
    }
    // Rcout << "Q_full size: " << Q_full.n_rows << " x " << Q_full.n_cols << ", R_full size: " <<
    //   R_full.n_rows << " x " << R_full.n_cols << " piv length: " << piv.size() << std::endl;

    used_pivot = true;
    Q_proj = Q_full.cols(0, n_predictors - 1);
    R_factor = arma::trimatu(R_full.submat(0, 0, n_predictors - 1, n_predictors - 1));
  } else {
    Q_proj = Q_tmp;
    R_factor = arma::trimatu(R_tmp);
  }

  // assess conditioning of R to decide whether the fast solver is safe
  arma::vec R_diag = arma::abs(R_factor.diag());
  double max_diag = 0.0;
  double min_diag = 0.0;
  if (!R_diag.is_empty()) {
    max_diag = R_diag.max();
    min_diag = R_diag.min();
  }

  bool diag_has_nonfinite = false;
  for (arma::uword i = 0; i < R_diag.n_elem; ++i) {
    if (!std::isfinite(R_diag[i])) {
      diag_has_nonfinite = true;
      break;
    }
  }

  const double eps = std::numeric_limits<double>::epsilon(); // machine precision for floating point
  const double rank_tol = eps * static_cast<double>(n_t_sub);
  bool rank_deficient = diag_has_nonfinite || R_diag.is_empty() || max_diag == 0.0 || (min_diag <= rank_tol * max_diag);

  bool ill_conditioned = false;
  if (!rank_deficient && max_diag > 0.0) {
    const double cond_ratio = min_diag / max_diag;
    const double cond_tol = std::sqrt(eps);
    if (cond_ratio < cond_tol) ill_conditioned = true;
  }

  if (rank_deficient) {
    Rcpp::warning("Design matrix appears rank deficient; using robust solver.");
  } else if (ill_conditioned) {
    Rcpp::warning("Design matrix is poorly conditioned; using robust solver.");
  }

  const bool allow_fast_solver = !(used_pivot || rank_deficient || ill_conditioned);

  // precompute transpose used for voxelwise coefficient calculation
  arma::mat Q_proj_t = Q_proj.t();

  auto flat_index = [&](int x, int y, int z, int t) {
    return x + n_x * (y + n_y * (z + n_z * t));
  };

  // preallocate voxel loop ingredients
  arma::vec y(n_t), y_sub(n_t_sub), Qt_y;
  arma::vec beta_fit(n_predictors), beta_perm(n_predictors), beta_sub(predict_indices.n_elem);
  arma::vec fitted(n_t), residuals(n_t);

  // for debugging regressor selection  
  // Rcout << "X_fit size: " << X_fit.n_rows << " x " << X_fit.n_cols << ", X_regress size: " <<
  //   X_regress.n_rows << " x " << X_regress.n_cols << std::endl <<
  //     "fit_cols length: " << fit_cols_vec.size() << std::endl <<
  //       "regress_cols length: " << regress_indices.size();
  // 
  // Rcout << "fit_cols_vec: " << Rcpp::IntegerVector(fit_cols_vec.begin(), fit_cols_vec.end()) << std::endl;
  // Rcout << "regress_indices" << Rcpp::IntegerVector(regress_indices.begin(), regress_indices.end()) << std::endl;

  for (int xi = 0; xi < n_x; ++xi) {
    for (int yi = 0; yi < n_y; ++yi) {
      for (int zi = 0; zi < n_z; ++zi) {

        for (arma::uword ti = 0; ti < n_t; ++ti) {
          y[ti] = data[flat_index(xi, yi, zi, ti)]; // this timeseries
        }
        y_sub = y.elem(idx); // valid points in y

        // Based on basic timing tests, we get a small improvement in speed if we skip constant time series
        double range = y_sub.max() - y_sub.min();
        if (std::abs(range) < 1e-6) {
          residuals.zeros();
        } else {
          Qt_y = Q_proj_t * y_sub;
          if (allow_fast_solver) {
            beta_perm = arma::solve(R_factor, Qt_y, arma::solve_opts::fast);
          } else {
            beta_perm = arma::solve(R_factor, Qt_y);
          }
          if (used_pivot) { // need to put coefficients back in original locations prior to pivoted permutation
            beta_fit.zeros();
            for (arma::uword k = 0; k < n_predictors; ++k) {
              beta_fit(piv[k]) = beta_perm[k];
            }
          } else {
            beta_fit = beta_perm;
          }
          beta_sub = beta_fit.elem(predict_indices);
          fitted = X_regress * beta_sub;
          residuals = y - fitted;
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
