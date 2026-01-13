// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(Rcpp)]]

#include <Rcpp.h>
#include <Rmath.h>
#include <algorithm>
#include <vector>
#include <array>
#include <cmath>
using namespace Rcpp;

inline bool isFinite(double x){ return R_FINITE(x); }
inline int idx3(int i,int j,int k,int nx,int ny){ return i + nx*j + nx*ny*k; }

// Compute empirical ACF (correlation) for a single 3D volume and a set of offsets
// vol: numeric array with dim=c(nx,ny,nz) (column-major R layout)
// mask: logical vector of length nx*ny*nz
// dims: integer vector (3) [nx,ny,nz]
// offs: integer matrix with nrow offsets and 3 columns (di,dj,dk)
// Returns vector of length nrow(offs)
// [[Rcpp::export]]
NumericVector acf_for_volume_cpp(NumericVector vol,
                                 LogicalVector mask,
                                 IntegerVector dims,
                                 IntegerMatrix offs){
  const int nx = dims[0], ny = dims[1], nz = dims[2];
  const int nvox = nx*ny*nz;
  if(vol.size() != nvox || mask.size() != nvox)
    stop("acf_for_volume_cpp: size mismatch");

  // mean/var over masked finite voxels
  double sum=0.0, sumsq=0.0; int mcount=0;
  for(int idx=0; idx<nvox; ++idx){
    if(mask[idx]){
      double v = vol[idx];
      if(isFinite(v)){ sum += v; sumsq += v*v; ++mcount; }
    }
  }
  NumericVector out(offs.nrow());
  if(mcount < 9) return out; // zeros
  double var = (sumsq - (sum*sum)/mcount) / (mcount - 1.0);
  if(!(var > 0)) return out;

  // loop offsets
  for(int r=0; r<offs.nrow(); ++r){
    const int di = offs(r,0), dj = offs(r,1), dk = offs(r,2);
    int i0 = (di>=0) ? 0 : -di;
    int j0 = (dj>=0) ? 0 : -dj;
    int k0 = (dk>=0) ? 0 : -dk;
    int i1 = (di>=0) ? nx-1-di : nx-1;
    int j1 = (dj>=0) ? ny-1-dj : ny-1;
    int k1 = (dk>=0) ? nz-1-dk : nz-1;
    if(i1 < i0 || j1 < j0 || k1 < k0){ out[r]=0.0; continue; }
    double sxy=0.0; int np=0;
    for(int kk=k0; kk<=k1; ++kk){
      for(int jj=j0; jj<=j1; ++jj){
        for(int ii=i0; ii<=i1; ++ii){
          int idx = idx3(ii, jj, kk, nx, ny);
          int idx2 = idx3(ii+di, jj+dj, kk+dk, nx, ny);
          if(mask[idx] && mask[idx2]){
            double a = vol[idx];
            double b = vol[idx2];
            if(isFinite(a) && isFinite(b)){
              sxy += (a - sum/mcount) * (b - sum/mcount);
              ++np;
            }
          }
        }
      }
    }
    out[r] = (np > 5) ? ( sxy / ( var * (np - 1.0) ) ) : 0.0;
  }
  return out;
}

// Compute mean ACF across time for a 4D array (dims = c(nx,ny,nz,nt))
// arr: numeric array (4D)
// mask: logical 3D mask (length nx*ny*nz)
// offs: nOff x 3 offsets
// demed: if true, subtract voxelwise median across time before ACF computation
// [[Rcpp::export]]
NumericVector acf_mean_4d_cpp(NumericVector arr,
                              LogicalVector mask,
                              IntegerVector dims,
                              IntegerMatrix offs,
                              bool demed=false){
  const int nx = dims[0], ny = dims[1], nz = dims[2], nt = dims[3];
  const int nvox = nx*ny*nz;
  if(arr.size() != nvox*nt || mask.size() != nvox)
    stop("acf_mean_4d_cpp: size mismatch");

  // Possibly compute voxelwise median across time and subtract
  std::vector<double> work(nt);
  std::vector<double> medv; medv.resize(demed ? nvox : 0);
  if(demed){
    for(int idx=0; idx<nvox; ++idx){
      if(mask[idx]){
        int m=0;
        for(int t=0; t<nt; ++t){ work[m++] = arr[idx + nvox*t]; }
        // nth_element for median
        int mid = m/2;
        std::nth_element(work.begin(), work.begin()+mid, work.begin()+m);
        double med = work[mid];
        if((m % 2)==0){ // even, average two middles
          std::nth_element(work.begin(), work.begin()+mid-1, work.begin()+m);
          med = 0.5*(med + work[mid-1]);
        }
        medv[idx] = med;
        for(int t=0; t<nt; ++t){ arr[idx + nvox*t] -= med; }
      }
    }
  }

  NumericVector out(offs.nrow());
  std::fill(out.begin(), out.end(), 0.0);

  // Accumulate ACF over time points
  for(int t=0; t<nt; ++t){
    NumericVector vol = arr[ Range(nvox*t, nvox*(t+1)-1) ];
    // compute mean/var for this time point over mask
    double sum=0.0, sumsq=0.0; int mcount=0;
    for(int idx=0; idx<nvox; ++idx){ if(mask[idx]){ double v=vol[idx]; if(isFinite(v)){ sum+=v; sumsq+=v*v; ++mcount; } } }
    if(mcount < 9) continue;
    double var = (sumsq - (sum*sum)/mcount) / (mcount - 1.0);
    if(!(var > 0)) continue;

    for(int r=0; r<offs.nrow(); ++r){
      const int di = offs(r,0), dj = offs(r,1), dk = offs(r,2);
      int i0 = (di>=0) ? 0 : -di;
      int j0 = (dj>=0) ? 0 : -dj;
      int k0 = (dk>=0) ? 0 : -dk;
      int i1 = (di>=0) ? nx-1-di : nx-1;
      int j1 = (dj>=0) ? ny-1-dj : ny-1;
      int k1 = (dk>=0) ? nz-1-dk : nz-1;
      if(i1 < i0 || j1 < j0 || k1 < k0) continue;
      double sxy=0.0; int np=0;
      double meanv = sum/mcount;
      for(int kk=k0; kk<=k1; ++kk){
        for(int jj=j0; jj<=j1; ++jj){
          for(int ii=i0; ii<=i1; ++ii){
            int idx = idx3(ii, jj, kk, nx, ny);
            int idx2 = idx3(ii+di, jj+dj, kk+dk, nx, ny);
            if(mask[idx] && mask[idx2]){
              double a = vol[idx];
              double b = vol[idx2];
              if(isFinite(a) && isFinite(b)){
                sxy += (a - meanv) * (b - meanv);
                ++np;
              }
            }
          }
        }
      }
      if(np > 5) out[r] += sxy / ( var * (np - 1.0) );
    }
  }

  // average across time points that contributed
  for(int r=0; r<offs.nrow(); ++r) out[r] /= nt;
  return out;
}

inline double clamp_val(double x, double lo, double hi) {
  if (x < lo) return lo;
  if (x > hi) return hi;
  return x;
}

inline double acf_model_val(double r, double a, double b, double c) {
  return a * std::exp(-0.5 * (r*r)/(b*b)) + (1.0 - a) * std::exp(-r/c);
}

double acf_cost(const std::vector<double> &r,
                const std::vector<double> &acf,
                const std::array<double,3> &par) {
  double aa = par[0], bb = par[1], cc = par[2];
  if (!(aa > 0.0 && aa < 1.0 && bb > 0.0 && cc > 0.0)) return 1e30;
  double sse = 0.0;
  for (size_t i = 0; i < r.size(); ++i) {
    double pred = acf_model_val(r[i], aa, bb, cc);
    double diff = pred - acf[i];
    sse += diff * diff;
  }
  if (!std::isfinite(sse)) return 1e30;
  return sse;
}

// Simple bounded Nelder-Mead optimizer for 3 parameters
std::array<double,3> nm_optimize(const std::vector<double> &r,
                                 const std::vector<double> &acf,
                                 const std::array<double,3> &start,
                                 const std::array<double,3> &lower,
                                 const std::array<double,3> &upper,
                                 int max_iter = 400) {
  const double alpha = 1.0, gamma = 2.0, rho = 0.5, sigma = 0.5;
  std::vector<std::array<double,3>> simplex(4);
  std::vector<double> values(4);
  simplex[0] = start;
  for (int i = 0; i < 3; ++i) {
    std::array<double,3> pt = start;
    double step = 0.05 * std::max(1e-3, upper[i] - lower[i]);
    pt[i] = clamp_val(pt[i] + step, lower[i], upper[i]);
    simplex[i+1] = pt;
  }
  for (int i = 0; i < 4; ++i) values[i] = acf_cost(r, acf, simplex[i]);

  auto sort_simplex = [&]() {
    std::vector<std::pair<double,int>> order(4);
    for (int i = 0; i < 4; ++i) order[i] = std::make_pair(values[i], i);
    std::sort(order.begin(), order.end(),
              [](const std::pair<double,int> &a, const std::pair<double,int> &b){
                return a.first < b.first;
              });
    std::vector<std::array<double,3>> simplex_old = simplex;
    std::vector<double> values_old = values;
    for (int i = 0; i < 4; ++i) {
      simplex[i] = simplex_old[order[i].second];
      values[i] = values_old[order[i].second];
    }
  };

  for (int iter = 0; iter < max_iter; ++iter) {
    sort_simplex();
    double max_spread = 0.0;
    for (int i = 0; i < 4; ++i) {
      for (int k = 0; k < 3; ++k) {
        max_spread = std::max(max_spread, std::fabs(simplex[i][k] - simplex[0][k]));
      }
    }
    double val_spread = std::fabs(values[3] - values[0]);
    if (max_spread < 1e-6 && val_spread < 1e-6) break;

    std::array<double,3> centroid = {0.0, 0.0, 0.0};
    for (int i = 0; i < 3; ++i) {
      for (int k = 0; k < 3; ++k) centroid[k] += simplex[i][k];
    }
    for (int k = 0; k < 3; ++k) centroid[k] /= 3.0;

    std::array<double,3> reflection;
    for (int k = 0; k < 3; ++k) {
      reflection[k] = centroid[k] + alpha * (centroid[k] - simplex[3][k]);
      reflection[k] = clamp_val(reflection[k], lower[k], upper[k]);
    }
    double fr = acf_cost(r, acf, reflection);

    if (fr < values[0]) {
      std::array<double,3> expansion;
      for (int k = 0; k < 3; ++k) {
        expansion[k] = centroid[k] + gamma * (reflection[k] - centroid[k]);
        expansion[k] = clamp_val(expansion[k], lower[k], upper[k]);
      }
      double fe = acf_cost(r, acf, expansion);
      if (fe < fr) {
        simplex[3] = expansion;
        values[3] = fe;
      } else {
        simplex[3] = reflection;
        values[3] = fr;
      }
    } else if (fr < values[2]) {
      simplex[3] = reflection;
      values[3] = fr;
    } else {
      std::array<double,3> contraction;
      if (fr < values[3]) {
        for (int k = 0; k < 3; ++k) {
          contraction[k] = centroid[k] + rho * (reflection[k] - centroid[k]);
          contraction[k] = clamp_val(contraction[k], lower[k], upper[k]);
        }
      } else {
        for (int k = 0; k < 3; ++k) {
          contraction[k] = centroid[k] - rho * (centroid[k] - simplex[3][k]);
          contraction[k] = clamp_val(contraction[k], lower[k], upper[k]);
        }
      }
      double fc = acf_cost(r, acf, contraction);
      if (fc < values[3]) {
        simplex[3] = contraction;
        values[3] = fc;
      } else {
        for (int i = 1; i < 4; ++i) {
          for (int k = 0; k < 3; ++k) {
            simplex[i][k] = simplex[0][k] + sigma * (simplex[i][k] - simplex[0][k]);
            simplex[i][k] = clamp_val(simplex[i][k], lower[k], upper[k]);
          }
          values[i] = acf_cost(r, acf, simplex[i]);
        }
      }
    }
  }
  sort_simplex();
  return simplex[0];
}

// [[Rcpp::export]]
List fit_acf_model_cpp(NumericVector r_in, NumericVector acf_in) {
  if (r_in.size() != acf_in.size() || r_in.size() == 0)
    return List::create(_["ok"] = false);
  std::vector<std::pair<double,double>> pairs(r_in.size());
  for (int i = 0; i < r_in.size(); ++i) {
    pairs[i].first = r_in[i];
    pairs[i].second = acf_in[i];
  }
  std::sort(pairs.begin(), pairs.end(),
            [](const std::pair<double,double> &a,
               const std::pair<double,double> &b){ return a.first < b.first; });
  std::vector<double> keep_r, keep_a;
  size_t i = 0, n = pairs.size();
  while (i < n) {
    size_t j = i + 1;
    double thresh = pairs[i].first * 1.01;
    while (j < n && pairs[j].first <= thresh) ++j;
    double sum = 0.0;
    for (size_t k = i; k < j; ++k) sum += pairs[k].second;
    keep_r.push_back(pairs[i].first);
    keep_a.push_back(sum / double(j - i));
    i = j;
  }
  for (size_t k = 0; k < keep_a.size(); ++k) {
    keep_a[k] = std::min(1.0, std::max(keep_a[k], 0.0));
    if (k > 0 && keep_a[k] > keep_a[k-1]) keep_a[k] = keep_a[k-1];
  }
  size_t cross_idx = keep_a.size();
  for (size_t k = 0; k < keep_a.size(); ++k) {
    if (keep_a[k] <= 0.5) { cross_idx = k; break; }
  }
  if (cross_idx == keep_a.size())
    return List::create(_["ok"] = false, _["r"] = keep_r, _["acf_emp"] = keep_a);
  double base_val = std::max(keep_a[cross_idx], 1e-6);
  double logr = std::log(base_val);
  double r_cross = keep_r[cross_idx];
  double b0 = std::sqrt(std::max(1e-6, -0.5 / logr)) * r_cross;
  double c0 = -r_cross / logr;
  std::array<double,3> par0 = {0.5, b0, c0};
  std::array<double,3> lower = {0.006, 0.05 * b0, 0.05 * c0};
  std::array<double,3> upper = {0.994, 5.55 * b0, 5.55 * c0};
  for (int k = 0; k < 3; ++k) {
    if (!(upper[k] > lower[k])) upper[k] = lower[k] * 1.5 + 1e-3;
    par0[k] = clamp_val(par0[k], lower[k], upper[k]);
  }
  std::array<double,3> best = nm_optimize(keep_r, keep_a, par0, lower, upper);
  double best_cost = acf_cost(keep_r, keep_a, best);
  if (!std::isfinite(best_cost) || best_cost >= 1e20)
    return List::create(_["ok"] = false, _["r"] = keep_r, _["acf_emp"] = keep_a);
  std::vector<double> model(keep_r.size());
  for (size_t k = 0; k < keep_r.size(); ++k) {
    model[k] = acf_model_val(keep_r[k], best[0], best[1], best[2]);
  }
  double half_r = NA_REAL;
  for (size_t k = 1; k < keep_r.size(); ++k) {
    if (model[k] <= 0.5 && model[k-1] > 0.5) {
      double x1 = model[k-1], x2 = model[k];
      double y1 = keep_r[k-1], y2 = keep_r[k];
      double t = (0.5 - x1) / (x2 - x1 + 1e-12);
      t = clamp_val(t, 0.0, 1.0);
      half_r = y1 + t * (y2 - y1);
      break;
    }
  }
  if (!std::isfinite(half_r))
    return List::create(_["ok"] = false, _["r"] = keep_r, _["acf_emp"] = keep_a);
  return List::create(
    _["ok"] = true,
    _["a"] = best[0],
    _["b"] = best[1],
    _["c"] = best[2],
    _["d"] = 2.0 * half_r,
    _["r"] = keep_r,
    _["acf_emp"] = keep_a,
    _["acf_model"] = model
  );
}

// [[Rcpp::export]]
List acf_estimate_cpp(NumericVector arr4d,
                      LogicalVector mask,
                      IntegerVector dims,
                      IntegerMatrix offs,
                      NumericVector radii) {
  if (dims.size() != 4) stop("dims must have length 4");
  const int nx = dims[0], ny = dims[1], nz = dims[2], nt = dims[3];
  const int nvox = nx*ny*nz;
  if (arr4d.size() != nvox*nt || mask.size() != nvox)
    stop("acf_estimate_cpp: dimension mismatch");
  const int nOff = offs.nrow();
  if (nOff == 0 || radii.size() != nOff)
    return List::create(_["ok"] = false);
  std::vector<double> acf_sum(nOff, 0.0);
  int count = 0;
  const double *arr_ptr = arr4d.begin();
  for (int t = 0; t < nt; ++t) {
    const double *vol = arr_ptr + nvox * t;
    double sum = 0.0, sumsq = 0.0;
    int mcount = 0;
    for (int idx = 0; idx < nvox; ++idx) {
      if (mask[idx]) {
        double v = vol[idx];
        if (isFinite(v)) {
          sum += v;
          sumsq += v * v;
          ++mcount;
        }
      }
    }
    if (mcount < 9) continue;
    double meanv = sum / mcount;
    double var = (sumsq - (sum*sum)/mcount) / (mcount - 1.0);
    if (!(var > 0)) continue;
    std::vector<double> tmp(nOff, 0.0);
    bool any = false;
    for (int r = 0; r < nOff; ++r) {
      int di = offs(r,0), dj = offs(r,1), dk = offs(r,2);
      int i0 = (di>=0) ? 0 : -di;
      int j0 = (dj>=0) ? 0 : -dj;
      int k0 = (dk>=0) ? 0 : -dk;
      int i1 = (di>=0) ? nx-1-di : nx-1;
      int j1 = (dj>=0) ? ny-1-dj : ny-1;
      int k1 = (dk>=0) ? nz-1-dk : nz-1;
      if (i1 < i0 || j1 < j0 || k1 < k0) { tmp[r] = 0.0; continue; }
      double sxy = 0.0; int np = 0;
      for (int kk = k0; kk <= k1; ++kk) {
        for (int jj = j0; jj <= j1; ++jj) {
          for (int ii = i0; ii <= i1; ++ii) {
            int idx = idx3(ii, jj, kk, nx, ny);
            int idx2 = idx3(ii+di, jj+dj, kk+dk, nx, ny);
            if (mask[idx] && mask[idx2]) {
              double a = vol[idx];
              double b = vol[idx2];
              if (isFinite(a) && isFinite(b)) {
                sxy += (a - meanv) * (b - meanv);
                ++np;
              }
            }
          }
        }
      }
      if (np > 5) {
        tmp[r] = sxy / (var * (np - 1.0));
        if (std::fabs(tmp[r]) > 1e-12) any = true;
      } else {
        tmp[r] = 0.0;
      }
    }
    if (any) {
      for (int r = 0; r < nOff; ++r) acf_sum[r] += tmp[r];
      ++count;
    }
  }
  if (count == 0) return List::create(_["ok"] = false);
  std::vector<double> avg(nOff);
  for (int r = 0; r < nOff; ++r) avg[r] = acf_sum[r] / count;
  NumericVector avg_vec(nOff);
  for (int r = 0; r < nOff; ++r) avg_vec[r] = avg[r];
  double emp_half = NA_REAL;
  for (int r = 0; r < nOff; ++r) {
    if (avg[r] <= 0.5) {
      emp_half = radii[r];
      break;
    }
  }
  List fit = fit_acf_model_cpp(radii, avg_vec);
  if (!as<bool>(fit["ok"])) {
    if (std::isfinite(emp_half)) {
      return List::create(
        _["ok"] = true,
        _["d"] = 2.0 * emp_half,
        _["acf"] = avg_vec,
        _["r"] = radii,
        _["acf_model"] = NumericVector(),
        _["count"] = count
      );
    }
    fit["acf"] = avg_vec;
    fit["count"] = count;
    return fit;
  }
  fit["acf"] = avg_vec;
  fit["count"] = count;
  return fit;
}
