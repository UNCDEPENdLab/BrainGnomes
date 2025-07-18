#include "BrainGnomes.h"
#include <vector>
#include <stdexcept>
#include <algorithm>

// natural spline interpolation using std::vector for speed
std::vector<double> natural_spline_interp(const std::vector<double>& x, const std::vector<double>& y, const std::vector<double>& xout) {
  int n = x.size();
  if (n < 3) stop("Need at least three points for cubic spline interpolation");
  if (y.size() != n) stop("x and y must have the same length");
  
  // Step 1: Compute interval widths and check monotonicity
  std::vector<double> h(n - 1), alpha(n - 1);
  for (int i = 0; i < n - 1; ++i) {
    h[i] = x[i + 1] - x[i];
    if (h[i] <= 0) throw std::invalid_argument("x must be strictly increasing");
  }
  
  // Step 2: Compute alpha for spline system
  for (int i = 1; i < n - 1; ++i) {
    alpha[i] = (3.0 / h[i]) * (y[i + 1] - y[i]) - (3.0 / h[i - 1]) * (y[i] - y[i - 1]);
  }
  
  // Step 3: Solve tridiagonal system for spline coefficients
  std::vector<double> l(n), mu(n), z(n), c(n), b(n - 1), d(n - 1);
  l[0] = 1.0; mu[0] = z[0] = 0.0;
  
  for (int i = 1; i < n - 1; ++i) {
    l[i] = 2.0 * (x[i + 1] - x[i - 1]) - h[i - 1] * mu[i - 1];
    mu[i] = h[i] / l[i];
    z[i] = (alpha[i] - h[i - 1] * z[i - 1]) / l[i];
  }
  
  l[n - 1] = 1.0; z[n - 1] = c[n - 1] = 0.0;
  
  for (int j = n - 2; j >= 0; --j) {
    c[j] = z[j] - mu[j] * c[j + 1];
    b[j] = (y[j + 1] - y[j]) / h[j] - h[j] * (c[j + 1] + 2.0 * c[j]) / 3.0;
    d[j] = (c[j + 1] - c[j]) / (3.0 * h[j]);
  }
  
  std::vector<double> result(xout.size());
  
  for (size_t i = 0; i < xout.size(); ++i) {
    double xi = xout[i];
    int j = 0;
    
    if (xi < x[0]) {
      // Linear extrapolation using leftmost slope
      result[i] = y[0] + b[0] * (xi - x[0]);
      continue;
    } else if (xi > x[n - 1]) {
      // Derivative of spline at x[n - 1] using last interval [x[n-2], x[n-1]].
      // Trickier than left:
      //   1) Use the slope at x[n - 1], i.e., the derivative of the cubic spline at x[n - 1].
      //   2) Compute that derivative using the spline coefficients for interval [x[n-2], x[n-1]].
      double dx_last = x[n - 1] - x[n - 2];
      double slope = b[n - 2] + 2.0 * c[n - 2] * dx_last + 3.0 * d[n - 2] * dx_last * dx_last;
      result[i] = y[n - 1] + slope * (xi - x[n - 1]);
      continue;
    } else if (xi == x[n - 1]) {
      // Directly use last point if on the right boundary -- trivial speedup?
      result[i] = y[n - 1];
      continue;
    } else {
      // binary search to find interval
      int lo = 0, hi = n - 2;
      while (lo <= hi) {
        int mid = (lo + hi) / 2;
        if (x[mid] <= xi && xi <= x[mid + 1]) {
          j = mid;
          break;
        } else if (xi < x[mid]) {
          hi = mid - 1;
        } else {
          lo = mid + 1;
        }
      }
    }
    
    // Evaluate the cubic polynomial at xi
    double dx = xi - x[j];
    result[i] = y[j] + b[j] * dx + c[j] * dx * dx + d[j] * dx * dx * dx;
  }
  
  return result;
}



//' Cubic spline interpolation with natural spline and linear extrapolation
//' 
//' @name natural_spline_interp
//' @description Performs natural cubic spline interpolation for given input values.
//' This function takes known data points `(x, y)` and evaluates the cubic spline
//' interpolation at specified output points `xout`. It uses a natural spline
//' formulation with zero second derivatives at the endpoints.
//'
//' @param x A numeric vector of strictly increasing x-values (time or position).
//' @param y A numeric vector of y-values at each x (same length as x).
//' @param xout A numeric vector of points at which to interpolate.
//'
//' @return A numeric vector of interpolated y-values at each point in `xout`.
//'
//' @details The function performs cubic spline interpolation using a tridiagonal
//' system to solve for the spline coefficients. If `xout` values fall outside
//' the `x` range, the function uses linear extrapolation, mirror R's `splinefun`
//' with `method = "natural"`
//'
//' @examples
//'   x <- c(0, 1, 2, 3, 4)
//'   y <- c(0, 1, 0, 1, 0)
//'   xout <- seq(0, 4, by = 0.1)
//'   natural_spline_interp(x, y, xout)
//' @export

// [[Rcpp::export]]
NumericVector natural_spline_interp(NumericVector x, NumericVector y, NumericVector xout) {
  // Check for finite values in input
  if (!Rcpp::all(Rcpp::is_finite(x)).is_true() || !Rcpp::all(Rcpp::is_finite(y)).is_true()) {
    stop("x and y must contain only finite values");
  }
  
  // This is a wrapper function around the std function above (overload)
  std::vector<double> x_std(x.begin(), x.end());
  std::vector<double> y_std(y.begin(), y.end());
  std::vector<double> xout_std(xout.begin(), xout.end());
  
  std::vector<double> result_std = natural_spline_interp(x_std, y_std, xout_std);
  return Rcpp::wrap(result_std);
}
