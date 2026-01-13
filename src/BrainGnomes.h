#ifndef _braingnomes_BRAINGNOMES_h
#define _braingnomes_BRAINGNOMES_h

#define ARMA_WARN_LEVEL 1 // disable warnings about approximate solutions for rank-deficient regressions
#define ARMA_NO_DEBUG

#define RNIFTI_NIFTILIB_VERSION 2
//#include "Rcpp.h"
#include <RcppArmadillo.h>
#include "RNifti.h"

//#include "RNiftiAPI.h" // cannot be included in overall .h
using namespace Rcpp;
using namespace RNifti;
//using namespace arma; // for unknown reasons, this blows up the compilation

// needed for NiftiImage constructor from a subset of volumes
typedef int64_t dim_t;

//function definitions

NumericVector natural_spline_interp(NumericVector x, NumericVector y, NumericVector xout);
std::vector<double> natural_spline_interp(const std::vector<double>& x, const std::vector<double>& y, const std::vector<double>& xout);

// loop over a 4D Nifti and apply natural spline interpolation at the requested points
Rcpp::RObject natural_spline_4d(std::string infile, const std::vector<int>& t_interpolate, bool edge_nn, std::string outfile, bool internal);

// helper function to remove volumes from a nifti input
void remove_nifti_volumes(std::string infile, const std::vector<int>& remove_tpts, std::string outfile);

Rcpp::RObject lmfit_residuals_4d(std::string infile, const arma::mat &X,
                                 const Rcpp::Nullable<Rcpp::LogicalVector>& include_rows,
                                 bool add_intercept, std::string outfile, bool internal,
                                 bool preserve_mean, double set_mean,
                                 const Rcpp::Nullable<Rcpp::IntegerVector>& regress_cols,
                                 bool exclusive);
Rcpp::RObject butterworth_filter_4d(std::string infile, const std::vector<double>& b, const std::vector<double>& a,
                                     std::string outfile, bool internal, std::string padtype, int padlen, bool use_zi);
  
#endif
