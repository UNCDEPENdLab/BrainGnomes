#include "BrainGnomes.h"
#include <vector>
#include <algorithm>
#include <stdexcept>
#include <cmath>

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

/**
 * @brief Computes the initial conditions for an IIR filter to achieve a steady-state response.
 *
 * This function calculates the initial state vector `zi` for a direct-form II transposed
 * implementation of an IIR filter, corresponding to the steady-state of a step input.
 * The result can be used to initialize the filter so that it begins in a state matching
 * its long-term behavior, avoiding transients at the start of the output.
 *
 * The method mirrors the behavior of `scipy.signal.lfilter_zi`, using the companion matrix
 * representation of the filter's state transition dynamics.
 *
 * @param b Numerator coefficients of the IIR filter (feedforward terms).
 * @param a Denominator coefficients of the IIR filter (feedback terms). Must have a non-zero leading coefficient.
 *
 * @return A std::vector<double> representing the initial state of the filter (length = max(length(a), length(b)) - 1).
 *
 * @throws std::invalid_argument if `a` or `b` is empty, or if all `a` coefficients are zero.
 *
 * @note The function assumes the filter coefficients define a causal, stable system. Coefficients
 *       are normalized internally so that a[0] == 1. If needed, trailing zeros are padded.
 *
 * @see lfilter, filtfilt
 */
// [[Rcpp::depends(RcppArmadillo)]]
std::vector<double> lfilter_zi_arma(const arma::vec& b, const arma::vec& a) {
  if (a.n_elem < 1 || b.n_elem < 1)
    stop("Filter coefficients 'a' and 'b' must have at least one element.");
  
  // Remove leading zeros from 'a'
  arma::vec a_trim = a;
  while (a_trim.n_elem > 1 && std::abs(a_trim(0)) < 1e-12)
    a_trim = a_trim.subvec(1, a_trim.n_elem - 1);
  
  if (a_trim.n_elem == 0)
    stop("At least one 'a' coefficient must be non-zero.");
  
  // Normalize if a[0] ≠ 1
  arma::vec b_norm = b;
  arma::vec a_norm = a_trim;
  if (std::abs(a_trim(0) - 1.0) > 1e-12) {
    b_norm = b_norm / a_trim(0);
    a_norm = a_norm / a_trim(0);
  }
  
  size_t n = std::max(b_norm.n_elem, a_norm.n_elem);
  if (a_norm.n_elem < n)
    a_norm.resize(n, true);
  if (b_norm.n_elem < n)
    b_norm.resize(n, true);
  
  // Companion matrix (transposed) for direct-form II
  mat A = zeros<mat>(n - 1, n - 1);
  for (size_t i = 1; i < n; ++i)
    A(0, i - 1) = -a_norm(i);
  for (size_t i = 1; i < n - 1; ++i)
    A(i, i - 1) = 1.0;
  
  mat IminusAT = eye<mat>(n - 1, n - 1) - A.t();
  
  vec B = b_norm.subvec(1, n - 1) - a_norm.subvec(1, n - 1) * b_norm(0);
  vec zi = solve(IminusAT, B);
  
  return std::vector<double>(zi.begin(), zi.end());
}


/**
 * @brief Applies an IIR filter to a signal using the Direct Form II Transposed structure.
 *
 * This function filters a 1D signal `x` using the IIR filter defined by numerator (`b`) and
 * denominator (`a`) coefficients. It supports optional initial conditions `zi` that define
 * the filter's starting state, making it suitable for seamless filtering across signal chunks
 * or zero-phase filtering (e.g., via `filtfilt`).
 *
 * The implementation uses the Direct Form II Transposed structure for efficient filtering.
 *
 * @param b The numerator coefficients of the filter (feedforward part).
 * @param a The denominator coefficients of the filter (feedback part). `a[0]` must be 1.0.
 * @param x The input signal to be filtered.
 * @param zi Optional initial conditions for the filter's delay state. If provided, must have length `max(b.size(), a.size()) - 1`.
 *
 * @return A vector of the same length as `x`, containing the filtered output.
 *
 * @throws std::invalid_argument if `a[0]` is not equal to 1.0 within a small tolerance,
 *         or if the length of `zi` does not match `max(b.size(), a.size()) - 1` when used.
 *
 * @note This implementation assumes `a[0] == 1.0`. Coefficients should be normalized before calling this function.
 *       If `zi` is not supplied, the filter state is initialized to zero.
 *
 * @see lfilter_zi_arma, filtfilt
 */
std::vector<double> lfilter(const std::vector<double>& b,
                            const std::vector<double>& a,
                            const std::vector<double>& x,
                            const std::vector<double>& zi = {}) {
  const size_t nb = b.size();
  const size_t na = a.size();
  const size_t nfilt = std::max(nb, na);
  const size_t n = x.size();
  
  if (a.empty() || std::abs(a[0] - 1.0) > 1e-10) {
    throw std::invalid_argument("First coefficient of 'a' must be 1.0.");
  }
  
  std::vector<double> y(n, 0.0);
  std::vector<double> state(nfilt - 1, 0.0);
  
  // Initialize with zi if provided
  if (!zi.empty()) {
    if (zi.size() != nfilt - 1) {
      throw std::invalid_argument("Length of zi must be max(length(b), length(a)) - 1.");
    }
    state = zi;
  }
  
  for (size_t i = 0; i < n; ++i) {
    double input = x[i];
    double acc = b[0] * input + state[0];
    y[i] = acc;
    
    for (size_t j = 1; j < nfilt; ++j) {
      double bj = (j < nb) ? b[j] : 0.0;
      double aj = (j < na) ? a[j] : 0.0;
      double next_state = (j < nfilt - 1) ? state[j] : 0.0;
      state[j - 1] = next_state + bj * input - aj * acc;
    }
  }
  
  return y;
}

// Pure C++ adaptation of scipy filtfilt -- see below for documentation
std::vector<double> filtfilt(const std::vector<double>& x, const std::vector<double>& b, const std::vector<double>& a,
                             int padlen = -1, const std::string& padtype = "constant", bool use_zi = true) {
  
  int n = x.size(); // length of time series
  if (n < 3) throw std::invalid_argument("Input signal too short for filtering.");
  
  // as in scipy, default to 3x the length of the filter coefficients
  int default_padlen = std::max({static_cast<int>(b.size()), static_cast<int>(a.size()), 1}) * 3;
  if (padlen < 0) padlen = default_padlen;
  
  if (padlen > n - 1) throw std::invalid_argument("padlen must be <= length(x) - 1.");
  
  // Pad signal to reduce edge artifacts
  std::vector<double> x_padded(2 * padlen + n);
  for (int i = 0; i < padlen; ++i) {
    if (padtype == "constant") { // pad left with first value and right with last value
      x_padded[i] = x[0];
      x_padded[padlen + n + i] = x[n - 1];
    } else if (padtype == "odd") {
      x_padded[i] = 2 * x[0] - x[padlen - i];
      x_padded[padlen + n + i] = 2 * x[n - 1] - x[n - 2 - i];
    } else if (padtype == "even") {
      x_padded[i] = x[padlen - i];
      x_padded[padlen + n + i] = x[n - 2 - i];
    } else if (padtype == "zero") { // pad with zeros
      x_padded[i] = 0.0;
      x_padded[padlen + n + i] = 0.0;
    } else {
      throw std::invalid_argument("Unsupported padtype.");
    }
  }
  
  std::copy(x.begin(), x.end(), x_padded.begin() + padlen);
  
  std::vector<double> zi, zi_fwd, zi_bwd;
  if (use_zi) {
    zi = lfilter_zi_arma(b, a); // 
    zi_fwd.resize(zi.size());
    // multiply values in zi against first element in x_padded
    for (size_t i = 0; i < zi.size(); ++i) zi_fwd[i] = zi[i] * x_padded[0];
  }
  
  // Forward filter
  std::vector<double> y_fwd = lfilter(b, a, x_padded, zi_fwd);
  
  // Reverse direction
  std::reverse(y_fwd.begin(), y_fwd.end());
  
  if (use_zi) {
    zi_bwd.resize(zi.size());
    // multiply values in zi against the now-first element in the reversed vector
    for (size_t i = 0; i < zi.size(); ++i) zi_bwd[i] = zi[i] * y_fwd[0];
  }
  
  // Backward filter
  std::vector<double> y_bwd = lfilter(b, a, y_fwd, zi_bwd);
  
  // Reverse again and trim padding
  std::reverse(y_bwd.begin(), y_bwd.end());
  std::vector<double> y_trim(y_bwd.begin() + padlen, y_bwd.begin() + padlen + n);
  
  return y_trim;
}


//' Zero-Phase IIR Filtering via Forward and Reverse Filtering
//'
//' Applies an IIR filter to a 1D numeric signal using forward and backward passes
//' to eliminate phase distortions, similar to `scipy.signal.filtfilt`.
//' This function implements a pure C++ version using the Direct Form II Transposed structure,
//' including optional initial condition handling via steady-state initialization.
//'
//' @name filtfilt_cpp
//' @param x A numeric vector representing the input time series.
//' @param b A numeric vector of numerator (feedforward) filter coefficients.
//' @param a A numeric vector of denominator (feedback) filter coefficients. Must have `a[0] == 1.0`.
//' @param padlen Number of samples to extend on each edge for padding. If `-1` (default), uses `3 * max(length(a), length(b))`.
//' @param padtype Type of padding at the signal boundaries. One of `"constant"` (default), `"odd"`, `"even"`, or `"zero"`.
//' @param use_zi Logical. If `TRUE` (default), use steady-state initial conditions to minimize transients.
//'
//' @return A numeric vector of the same length as `x`, containing the filtered signal.
//'
//' @details
//' The function applies the IIR filter in the forward direction, reverses the result,
//' and applies the filter again, then reverses the final output. This approach
//' removes phase delay. Padding is used to minimize edge artifacts, and the filter
//' state is optionally initialized with values derived from the steady-state
//' step response of the filter (`lfilter_zi_arma()`).
//'
//' @references
//' - Scipy Signal Documentation: \url{https://docs.scipy.org/doc/scipy/reference/generated/scipy.signal.filtfilt.html}
//' - Gustafsson, F. (1996). Determining the initial states in forward-backward filtering. IEEE Transactions on Signal Processing.
//'
//' @seealso \code{\link{lfilter}}, \code{\link{lfilter_zi_arma}}
//'
//' @export

// [[Rcpp::export]]
NumericVector filtfilt_cpp(NumericVector x, NumericVector b, NumericVector a,
                               int padlen = -1, std::string padtype = "constant", bool use_zi = true) {
  std::vector<double> x_vec(x.begin(), x.end());
  std::vector<double> b_vec(b.begin(), b.end());
  std::vector<double> a_vec(a.begin(), a.end());
  
  std::vector<double> result = filtfilt(x_vec, b_vec, a_vec, padlen, padtype, use_zi);
  return NumericVector(result.begin(), result.end());
}

//' Apply Butterworth Filter to 4D NIfTI Image
//'
//' This function applies a temporal Butterworth filter to each voxel time series
//' in a 4D NIfTI image using forward-backward filtering.
//'
//' @name butterworth_filter_cpp
//' @param infile Character string. Path to the input 4D NIfTI file.
//' @param b Numeric vector. Numerator filter coefficients.
//' @param a Numeric vector. Denominator filter coefficients.
//' @param outfile Character string. Optional path to save the filtered image.
//' @param internal Logical. Whether to return an internal RNifti image object (default = false).
//' @param padtype String. Padding type: "even", "odd", "constant", or "zero" (default = "even").
//' @param use_zi Logical. Whether to use steady-state initial conditions (default = true).
//'
//' @return A 4D filtered NIfTI image as a niftiImage or internalImage object.
//'
//' @keywords internal
// [[Rcpp::export]]
Rcpp::RObject butterworth_filter_cpp(std::string infile,
                                     const std::vector<double>& b,
                                     const std::vector<double>& a,
                                     std::string outfile = "",
                                     bool internal = false,
                                     std::string padtype = "even",
                                     int padlen = -1,
                                     bool use_zi = true) {
  
  NiftiImage image(infile); // read input
  int datatype = image->datatype;
  
  if (datatype == DT_INT8 || datatype == DT_INT16 || datatype == DT_INT32 || datatype == DT_INT64) {
    NiftiImageData float_data(image.data(), DT_FLOAT32);
    image.replaceData(float_data);
    float_data.disown();
  }
  
  NiftiImageData data(image);
  std::vector<long long int> dims = image.dim();
  if (dims.size() < 4 || dims[0] <= 0 || dims[1] <= 0 || dims[2] <= 0 || dims[3] <= 0) {
    stop("Input image must be 4D");
  }
  
  int n_x = dims[0], n_y = dims[1], n_z = dims[2], n_t = dims[3];
  
  auto flat_index = [&](int x, int y, int z, int t) {
    return x + n_x * (y + n_y * (z + n_z * t));
  };
  
  std::vector<double> y(n_t);
  std::vector<double> y_filt(n_t);
  
  for (int xi = 0; xi < n_x; ++xi) {
    for (int yi = 0; yi < n_y; ++yi) {
      for (int zi = 0; zi < n_z; ++zi) {
        for (int ti = 0; ti < n_t; ++ti) {
          y[ti] = data[flat_index(xi, yi, zi, ti)];
        }
        
        // skip filtering for constant time series
        double first_val = y[0];
        bool is_constant = true;
        for (double val : y) {
          if (val != first_val) {
            is_constant = false;
            break;
          }
        }
        
        if (is_constant) continue;
        
        // apply frequency filter to this voxel
        y_filt = filtfilt(y, b, a, padlen, padtype, use_zi);
        
        for (int ti = 0; ti < n_t; ++ti) {
          data[flat_index(xi, yi, zi, ti)] = static_cast<float>(y_filt[ti]);
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
