#ifndef _braingnomes_BRAINGNOMES_h
#define _braingnomes_BRAINGNOMES_h

#define RNIFTI_NIFTILIB_VERSION 2
#include "Rcpp.h"
#include "RNifti.h"
//#include "RNiftiAPI.h" // cannot be included in overall .h
using namespace Rcpp;
using namespace RNifti;

//function definitions

NumericVector natural_spline_interp(NumericVector x, NumericVector y, NumericVector xout);
std::vector<double> natural_spline_interp(const std::vector<double>& x, const std::vector<double>& y, const std::vector<double>& xout);

// loop over a 4D Nifti and apply natural spline interpolation at the requested points
Rcpp::RObject natural_spline_4d(std::string infile, const std::vector<int>& t_interpolate, bool edge_nn, std::string outfile, bool internal);
#endif
