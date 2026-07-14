// #include "BrainGnomes.h"
#define RNIFTI_NIFTILIB_VERSION 2
#include <Rcpp.h>
#include <array>
#include <vector>
#include <queue>
#include <algorithm>
#include <cmath>
#include <limits>
#include <RNifti.h>
// #include "RNiftiAPI.h"

using namespace Rcpp;
using namespace RNifti;
typedef int64_t dim_t;
typedef double pixdim_t;

/**
 * @brief Generate 3D neighbor offsets according to AFNI/voxel connectivity convention.
 *
 * This function produces the relative (dx,dy,dz) offsets of all voxels that are
 * considered "neighbors" of a central voxel, under a chosen neighborhood size NN:
 *
 *   - NN = 1 → 6-connected neighborhood (faces only). Includes voxels that share a face
 *              with the center (|dx|+|dy|+|dz| = 1).
 *   - NN = 2 → 18-connected neighborhood (faces + edges). Includes face neighbors plus
 *              voxels that share an edge with the center (|dx|+|dy|+|dz| <= 2).
 *   - NN = 3 → 26-connected neighborhood (faces + edges + corners). Includes all voxels
 *              in the surrounding 3×3×3 cube, except the center itself.
 *
 * These offsets are typically used for connected-component labeling, morphological
 * operations, or neighborhood searches in 3D brain imaging volumes.
 *
 * @param NN Neighborhood type (1, 2, or 3).
 * @return Vector of 3-element integer arrays {dx, dy, dz} representing neighbor offsets.
 */
static std::vector<std::array<int,3>> get_neighbors(int NN) {
  std::vector<std::array<int,3>> offsets;
  offsets.reserve(26);  // maximum possible neighbors in 3D
  
  // Loop over all displacements in the 3x3x3 cube around the center
  for (int dx = -1; dx <= 1; ++dx) {
    for (int dy = -1; dy <= 1; ++dy) {
      for (int dz = -1; dz <= 1; ++dz) {
        
        // Skip the center voxel (0,0,0)
        if (dx == 0 && dy == 0 && dz == 0) continue;
        
        // Manhattan distance = number of axes stepped away
        int manhattan = std::abs(dx) + std::abs(dy) + std::abs(dz);
        
        // NN=1: only 6 face neighbors (manhattan == 1)
        // NN=2: 6 faces + 12 edges = 18 neighbors (manhattan <= 2)
        // NN=3: all 26 possible neighbors
        if ((NN == 1 && manhattan == 1) || (NN == 2 && manhattan <= 2) || NN == 3)
          offsets.push_back(std::array<int,3>{{dx, dy, dz}});
      }
    }
  }
  
  return offsets;
}

/**
 * @brief Extract the largest connected component from a 3D binary mask.
 *
 * This function finds all connected components (clusters) of nonzero voxels
 * in a 3D logical mask, using the specified neighborhood definition, and
 * returns a new mask that retains only the voxels belonging to the largest
 * component.
 *
 * Connectivity is defined by the @p NN parameter:
 *   - NN = 1 → 6-connected (faces only)
 *   - NN = 2 → 18-connected (faces + edges)
 *   - NN = 3 → 26-connected (faces + edges + corners)
 *
 * @param mask LogicalVector of length nx*ny*nz representing the 3D mask.
 * @param nx Number of voxels along the x-dimension.
 * @param ny Number of voxels along the y-dimension.
 * @param nz Number of voxels along the z-dimension.
 * @param NN Neighborhood connectivity (1, 2, or 3).
 * @return LogicalVector of length nx*ny*nz, where only the largest connected
 *         component is marked TRUE and all other voxels are FALSE.
 */
static LogicalVector largest_component(const LogicalVector &mask,
                                       int nx, int ny, int nz, int NN)
{
  const int nvox = nx * ny * nz;
  
  // Precompute neighbor offsets for the given NN
  const auto neighbors = get_neighbors(NN);
  
  // Track which voxels have already been visited
  LogicalVector visited(nvox, false);
  
  // Store sizes and voxel indices of each component
  std::vector<int> sizes;
  std::vector< std::vector<int> > components;
  sizes.reserve(128); components.reserve(128);
  
  // Loop over all voxels to find new seed points
  for (int i = 0; i < nvox; ++i) {
    // Skip background (0) voxels and voxels already assigned
    if (!mask[i] || visited[i]) continue;
    
    // Collect voxels belonging to this component
    std::vector<int> comp;
    comp.reserve(1024);
    
    // Breadth-first search (queue-based flood fill)
    std::queue<int> q;
    q.push(i);
    visited[i] = true;
    
    while (!q.empty()) {
      const int idx = q.front(); q.pop();
      comp.push_back(idx);
      
      // Convert linear index back to (x,y,z)
      const int x = idx % nx;
      const int y = (idx / nx) % ny;
      const int z =  idx / (nx * ny);
      
      // Check all neighbors
      for (const auto &off : neighbors) {
        const int xx = x + off[0], yy = y + off[1], zz = z + off[2];
        
        // Skip out-of-bounds neighbors
        if (xx < 0 || xx >= nx || yy < 0 || yy >= ny || zz < 0 || zz >= nz) continue;
        const int nidx = xx + yy * nx + zz * nx * ny;
        
        // If neighbor is part of mask and unvisited, add to queue
        if (mask[nidx] && !visited[nidx]) {
          visited[nidx] = true;
          q.push(nidx);
        }
      }
    }
    
    // Record size and voxel indices for this component
    sizes.push_back((int)comp.size());
    components.push_back(std::move(comp));
  }
  
  // Prepare output mask, initially all FALSE
  LogicalVector out(nvox, false);
  
  // If no components were found, return empty mask
  if (components.empty()) return out;
  
  // Find index of largest component
  const int li = (int)std::distance(sizes.begin(),
                  std::max_element(sizes.begin(), sizes.end()));
  
  // Mark voxels of the largest component TRUE
  for (int idx : components[li]) out[idx] = true;
  return out;
}

/**
 * @brief Apply 3D morphological operations (erosion or dilation) to a binary mask.
 *
 * This function performs iterative morphological filtering on a 3D logical mask
 * using the specified neighborhood definition (NN) and number of steps.
 *
 * - **Dilation**: background voxels (0) that touch any foreground neighbor (1)
 *   become foreground.
 * - **Erosion**: foreground voxels (1) are retained only if *all* neighbors
 *   are foreground; otherwise they become background (0).
 *
 * Connectivity is defined by the @p NN parameter:
 *   - NN = 1 → 6-connected (faces only)
 *   - NN = 2 → 18-connected (faces + edges)
 *   - NN = 3 → 26-connected (faces + edges + corners)
 *
 * Multiple iterations of erosion or dilation can be applied sequentially.
 *
 * @param mask Reference to a 1D LogicalVector representing the 3D binary mask
 *             (length must be nx*ny*nz). Modified in place.
 * @param nx Number of voxels along the x-dimension.
 * @param ny Number of voxels along the y-dimension.
 * @param nz Number of voxels along the z-dimension.
 * @param NN Neighborhood connectivity (1, 2, or 3).
 * @param steps Number of times to apply the erosion or dilation.
 * @param dilate If true, perform dilation; if false, perform erosion.
 */
static void morph(LogicalVector &mask, int nx, int ny, int nz,
                  int NN, int steps, bool dilate)
{
  if (steps <= 0) return;
  
  // Precompute neighbor offsets based on NN
  const auto neighbors = get_neighbors(NN);
  
  // Apply operation for the requested number of iterations
  for (int s = 0; s < steps; ++s) {
    LogicalVector prev = clone(mask);
    for (int z = 0; z < nz; ++z) {
      for (int y = 0; y < ny; ++y) {
        for (int x = 0; x < nx; ++x) {
          const int idx = x + y * nx + z * nx * ny;
          if (dilate) {
            // Dilation: turn a background voxel ON if any neighbor is ON
            if (!prev[idx]) {
              bool any_on = false;
              for (const auto &off : neighbors) {
                const int xx = x + off[0], yy = y + off[1], zz = z + off[2];
                if (xx < 0 || xx >= nx || yy < 0 || yy >= ny || zz < 0 || zz >= nz) continue;
                const int nidx = xx + yy * nx + zz * nx * ny;
                if (prev[nidx]) { any_on = true; break; }
              }
              if (any_on) mask[idx] = true;
            }
          } else {
            // Erosion: keep a foreground voxel ON only if all neighbors are ON
            if (prev[idx]) {
              bool all_on = true;
              for (const auto &off : neighbors) {
                const int xx = x + off[0], yy = y + off[1], zz = z + off[2];
                if (xx < 0 || xx >= nx || yy < 0 || yy >= ny || zz < 0 || zz >= nz) continue;
                const int nidx = xx + yy * nx + zz * nx * ny;
                if (!prev[nidx]) { all_on = false; break; }
              }
              if (!all_on) mask[idx] = false;
            }
          }
        }
      }
    }
  }
}

// Count AFNI NN2 neighbors, using edge replication at image boundaries. The
// 18 offsets comprise face- and edge-sharing voxels; clamping an out-of-bounds
// coordinate reproduces AFNI's boundary treatment.
static int count_peel_neighbors(const LogicalVector &mask,
                                int x, int y, int z,
                                int nx, int ny, int nz,
                                const std::vector<std::array<int,3>> &neighbors)
{
  int count = 0;
  const int nxy = nx * ny;
  for (const auto &off : neighbors) {
    const int xx = std::min(nx - 1, std::max(0, x + off[0]));
    const int yy = std::min(ny - 1, std::max(0, y + off[1]));
    const int zz = std::min(nz - 1, std::max(0, z + off[2]));
    if (mask[xx + yy * nx + zz * nxy]) ++count;
  }
  return count;
}

// AFNI-style peel/unpeel. A voxel is removed when fewer than 17 of its 18 NN2
// neighbors survive. The layer in which it was removed is retained, and only
// previously removed voxels connected to the surviving core are restored. For
// multiple peels, outer layers require at least two restored/surviving
// neighbors, matching AFNI's layer-aware restoration rule.
static void peel_and_restore(LogicalVector &mask,
                             int nx, int ny, int nz,
                             int npeel)
{
  const int nvox = nx * ny * nz;
  if (npeel < 1 || nvox < 27) return;

  const auto neighbors = get_neighbors(2);
  std::vector<int> removed_layer(nvox, 0);

  for (int layer = 1; layer <= npeel; ++layer) {
    for (int z = 0; z < nz; ++z) {
      for (int y = 0; y < ny; ++y) {
        for (int x = 0; x < nx; ++x) {
          const int idx = x + y * nx + z * nx * ny;
          if (mask[idx] && count_peel_neighbors(
                mask, x, y, z, nx, ny, nz, neighbors) < 17) {
            removed_layer[idx] = layer;
          }
        }
      }
    }
    for (int i = 0; i < nvox; ++i)
      if (removed_layer[i] > 0) mask[i] = false;
  }

  std::vector<int> restore_neighbors(nvox, 0);
  for (int layer = npeel; layer >= 1; --layer) {
    std::fill(restore_neighbors.begin(), restore_neighbors.end(), 0);
    for (int z = 0; z < nz; ++z) {
      for (int y = 0; y < ny; ++y) {
        for (int x = 0; x < nx; ++x) {
          const int idx = x + y * nx + z * nx * ny;
          if (removed_layer[idx] >= layer && !mask[idx]) {
            restore_neighbors[idx] = count_peel_neighbors(
              mask, x, y, z, nx, ny, nz, neighbors
            );
          }
        }
      }
    }

    const int minimum_to_restore = layer == npeel ? 1 : 2;
    for (int i = 0; i < nvox; ++i)
      if (restore_neighbors[i] >= minimum_to_restore) mask[i] = true;
  }
}

// Compute q-th quantile in-place via nth_element (q in [0,1])
static inline float quantile_nth(std::vector<float> &v, double q) {
  if (v.empty()) return 0.0f;
  if (q <= 0.0) return *std::min_element(v.begin(), v.end());
  if (q >= 1.0) return *std::max_element(v.begin(), v.end());
  const size_t n = v.size();
  double pos = q * (n - 1);
  size_t k = (size_t)std::floor(pos);
  size_t k2 = std::min(k + 1, n - 1);
  std::nth_element(v.begin(), v.begin() + k, v.end());
  float a = v[k];
  if (k2 == k) return a;
  std::nth_element(v.begin() + k + 1, v.begin() + k2, v.end());
  float b = v[k2];
  float frac = static_cast<float>(pos - k);
  return a + frac * (b - a);
}

// AFNI-style clip estimator. The clip is iteratively updated to clfrac times
// the median of positive finite values at or above the current clip. Starting
// from the larger of the positive 35th percentile and half the positive RMS
// avoids convergence to a near-zero background mode when most positive voxels
// are outside the brain.
static float clip_level_from_positive(const std::vector<float> &vals,
                                      float clfrac)
{
  std::vector<float> pos;
  pos.reserve(vals.size());
  double sumsq = 0.0;
  for (float v : vals) {
    if (v > 0.0f && std::isfinite(v)) {
      pos.push_back(v);
      sumsq += static_cast<double>(v) * static_cast<double>(v);
    }
  }
  if (pos.empty()) return 0.0f;

  if (!std::isfinite(clfrac) || clfrac <= 0.0f || clfrac >= 0.99f)
    clfrac = 0.5f;

  const float lower_upper_mass = quantile_nth(pos, 0.35);
  const float half_rms = 0.5f * static_cast<float>(
    std::sqrt(sumsq / static_cast<double>(pos.size()))
  );
  float clip = std::max(lower_upper_mass, half_rms);
  if (!std::isfinite(clip) || clip < 0.0f) clip = 0.0f;

  std::vector<float> upper;
  upper.reserve(pos.size());
  float previous = -1.0f;
  for (int iteration = 0; iteration < 66; ++iteration) {
    upper.clear();
    for (float v : pos) if (v >= clip) upper.push_back(v);
    if (upper.empty()) break;

    const float upper_median = quantile_nth(upper, 0.5);
    const float next = clfrac * upper_median;
    if (!std::isfinite(next) || next < 0.0f) break;

    const float tolerance = 1.0e-6f * std::max(1.0f, std::fabs(next));
    if (std::fabs(next - clip) <= tolerance) {
      clip = next;
      break;
    }

    // Protect against a discrete two-cycle in small or quantized images.
    if (previous >= 0.0f && std::fabs(next - previous) <= tolerance) {
      clip = 0.5f * (clip + next);
      break;
    }
    previous = clip;
    clip = next;
  }

  return clip;
}

struct GradualClipResult {
  std::vector<float> threshold;
  float fixed;
  float minimum;
  float maximum;
};

// Estimate a clip level within a rectangular region of a 3D image.
static float regional_clip_level(const std::vector<float> &vals,
                                 int nx, int ny,
                                 int xa, int xb,
                                 int ya, int yb,
                                 int za, int zb,
                                 float clfrac)
{
  std::vector<float> region;
  region.reserve(static_cast<size_t>(xb - xa + 1) *
                 static_cast<size_t>(yb - ya + 1) *
                 static_cast<size_t>(zb - za + 1));
  const int nxy = nx * ny;
  for (int z = za; z <= zb; ++z)
    for (int y = ya; y <= yb; ++y)
      for (int x = xa; x <= xb; ++x)
        region.push_back(vals[x + y * nx + z * nxy]);
  return clip_level_from_positive(region, clfrac);
}

// Construct a smooth spatial threshold by estimating the clip separately in
// eight overlapping regions around the intensity-weighted center and
// trilinearly interpolating between their centers. Local estimates are bounded
// below by one third of the global clip to prevent near-zero background-only
// regions from opening the mask.
static GradualClipResult gradual_clip_levels(const std::vector<float> &vals,
                                             int nx, int ny, int nz,
                                             float clfrac)
{
  const int nvox = nx * ny * nz;
  GradualClipResult result;
  result.fixed = clip_level_from_positive(vals, clfrac);
  result.threshold.assign(nvox, result.fixed);
  result.minimum = result.fixed;
  result.maximum = result.fixed;
  if (nvox == 0 || result.fixed <= 0.0f) return result;

  double weight_sum = 0.0;
  double x_sum = 0.0, y_sum = 0.0, z_sum = 0.0;
  const int nxy = nx * ny;
  for (int z = 0; z < nz; ++z) {
    for (int y = 0; y < ny; ++y) {
      for (int x = 0; x < nx; ++x) {
        const float value = vals[x + y * nx + z * nxy];
        if (value > 0.0f && std::isfinite(value)) {
          const double weight = static_cast<double>(value);
          weight_sum += weight;
          x_sum += weight * x;
          y_sum += weight * y;
          z_sum += weight * z;
        }
      }
    }
  }

  const int ic = weight_sum > 0.0 ?
    static_cast<int>(std::round(x_sum / weight_sum)) : (nx - 1) / 2;
  const int jc = weight_sum > 0.0 ?
    static_cast<int>(std::round(y_sum / weight_sum)) : (ny - 1) / 2;
  const int kc = weight_sum > 0.0 ?
    static_cast<int>(std::round(z_sum / weight_sum)) : (nz - 1) / 2;

  const int ox = std::max(1, static_cast<int>(std::round(0.01 * nx)));
  const int oy = std::max(1, static_cast<int>(std::round(0.01 * ny)));
  const int oz = std::max(1, static_cast<int>(std::round(0.01 * nz)));
  const int xlo_end = std::min(nx - 1, ic + ox);
  const int xhi_start = std::max(0, ic - ox);
  const int ylo_end = std::min(ny - 1, jc + oy);
  const int yhi_start = std::max(0, jc - oy);
  const int zlo_end = std::min(nz - 1, kc + oz);
  const int zhi_start = std::max(0, kc - oz);

  float clips[2][2][2];
  const float local_floor = result.fixed / 3.0f;
  for (int hz = 0; hz < 2; ++hz) {
    const int za = hz ? zhi_start : 0;
    const int zb = hz ? nz - 1 : zlo_end;
    for (int hy = 0; hy < 2; ++hy) {
      const int ya = hy ? yhi_start : 0;
      const int yb = hy ? ny - 1 : ylo_end;
      for (int hx = 0; hx < 2; ++hx) {
        const int xa = hx ? xhi_start : 0;
        const int xb = hx ? nx - 1 : xlo_end;
        clips[hx][hy][hz] = std::max(
          local_floor,
          regional_clip_level(vals, nx, ny, xa, xb, ya, yb, za, zb, clfrac)
        );
      }
    }
  }

  const double x0 = 0.5 * ic;
  const double x1 = 0.5 * (ic + nx - 1);
  const double y0 = 0.5 * jc;
  const double y1 = 0.5 * (jc + ny - 1);
  const double z0 = 0.5 * kc;
  const double z1 = 0.5 * (kc + nz - 1);
  const double dxi = x1 > x0 ? 1.0 / (x1 - x0) : 0.0;
  const double dyi = y1 > y0 ? 1.0 / (y1 - y0) : 0.0;
  const double dzi = z1 > z0 ? 1.0 / (z1 - z0) : 0.0;

  result.minimum = std::numeric_limits<float>::infinity();
  result.maximum = -std::numeric_limits<float>::infinity();
  for (int z = 0; z < nz; ++z) {
    const double wz = std::min(1.0, std::max(0.0, (z - z0) * dzi));
    for (int y = 0; y < ny; ++y) {
      const double wy = std::min(1.0, std::max(0.0, (y - y0) * dyi));
      for (int x = 0; x < nx; ++x) {
        const double wx = std::min(1.0, std::max(0.0, (x - x0) * dxi));
        float value = 0.0f;
        for (int hz = 0; hz < 2; ++hz) {
          const double az = hz ? wz : 1.0 - wz;
          for (int hy = 0; hy < 2; ++hy) {
            const double ay = hy ? wy : 1.0 - wy;
            for (int hx = 0; hx < 2; ++hx) {
              const double ax = hx ? wx : 1.0 - wx;
              value += static_cast<float>(ax * ay * az * clips[hx][hy][hz]);
            }
          }
        }
        result.threshold[x + y * nx + z * nxy] = value;
        result.minimum = std::min(result.minimum, value);
        result.maximum = std::max(result.maximum, value);
      }
    }
  }

  return result;
}

// Fill internal holes: set any 0-voxels not connected to the volume boundary to 1.
static void _fill_holes(LogicalVector &mask, int nx, int ny, int nz, int NN /*use 2*/)
{
  const int nvox = nx * ny * nz;
  const auto neighbors = get_neighbors(NN);
  
  // visited marks background (0) voxels that are reachable from boundary
  std::vector<uint8_t> visited(nvox, 0);
  std::queue<int> q;
  
  auto enqueue_if_bg = [&](int x, int y, int z) {
    if (x < 0 || x >= nx || y < 0 || y >= ny || z < 0 || z >= nz) return;
    const int idx = x + y * nx + z * nx * ny;
    if (!mask[idx] && !visited[idx]) { // background and not yet marked
      visited[idx] = 1;
      q.push(idx);
    }
  };
  
  // Seed from the entire boundary (all six faces)
  for (int x = 0; x < nx; ++x)
    for (int y = 0; y < ny; ++y) {
      enqueue_if_bg(x, y, 0);
      enqueue_if_bg(x, y, nz - 1);
    }
    for (int x = 0; x < nx; ++x)
      for (int z = 0; z < nz; ++z) {
        enqueue_if_bg(x, 0, z);
        enqueue_if_bg(x, ny - 1, z);
      }
      for (int y = 0; y < ny; ++y)
        for (int z = 0; z < nz; ++z) {
          enqueue_if_bg(0, y, z);
          enqueue_if_bg(nx - 1, y, z);
        }
        
        // Flood-fill from boundary background
        while (!q.empty()) {
          const int idx = q.front(); q.pop();
          const int x = idx % nx;
          const int y = (idx / nx) % ny;
          const int z =  idx / (nx * ny);
          for (const auto &off : neighbors) {
            const int xx = x + off[0], yy = y + off[1], zz = z + off[2];
            if (xx < 0 || xx >= nx || yy < 0 || yy >= ny || zz < 0 || zz >= nz) continue;
            const int nidx = xx + yy * nx + zz * nx * ny;
            if (!mask[nidx] && !visited[nidx]) {
              visited[nidx] = 1;
              q.push(nidx);
            }
          }
        }
        
        // Any background voxel NOT visited is an enclosed hole → fill it
        for (int i = 0; i < nvox; ++i) {
          if (!mask[i] && !visited[i]) mask[i] = true;
        }
}

//' Create an automatic brain mask from a NIfTI image (Rcpp implementation)
//'
//' This function mimics AFNI's \code{3dAutomask} logic to generate a binary
//' brain mask from a 3D or 4D NIfTI image. If the input is 4D, the time dimension
//' is collapsed by computing the mean image across frames. A robust clip level
//' is then estimated, voxels above threshold are retained, and morphological
//' clean-up steps are applied (largest component, peels, hole-filling, optional
//' erosion/dilation, optional superior–inferior cutoff).
//'
//' @name automask
//' @param image A `RNifti::NiftiImage` object containing a 3D or 4D volume or
//'   file path to a NIfTI object whose mask should be calculated
//' @param outfile Optional file path where the resulting mask should be saved as
//'   a NIfTI file. If `""` (default), no file is written.
//' @param clfrac Fraction of the median intensity above the current clip used
//'   by the iterative clip estimator. Smaller values produce larger masks.
//'   Default is 0.5.
//' @param NN Neighborhood connectivity used for the largest connected component
//'   search and optional morphology. Options are `1` (faces only, 6-neighbor),
//'   `2` (faces+edges, 18-neighbor), or `3` (faces+edges+corners,
//'   26-neighbor). Default is 2.
//' @param erode_steps Number of additional erosions to apply after main mask
//'   construction. Default is 0 (none).
//' @param dilate_steps Number of additional dilations to apply after main mask
//'   construction. Default is 0 (none).
//' @param SIhh Distance in millimeters below the most superior voxel of the mask
//'   to retain. Voxels inferior to this cutoff are set to zero. Default is 0
//'   (no cutoff).
//' @param peels Number of layer-aware peel/restore operations using the NN2
//'   neighborhood and AFNI's 17-of-18 survival rule. These remove thin
//'   protuberances while restoring boundary voxels connected to the surviving
//'   core. Default is 1, matching AFNI `3dAutomask`.
//' @param fill_holes Logical; if `TRUE`, interior holes in the mask are
//'   filled using NN=1 connectivity. Default is TRUE.
//'
//' @return A 3D RNifti mask object with dimensions \code{c(nx, ny, nz)} and
//'   values \code{0/1}. If `outfile` is provided, the mask is also written to disk as an unsigned
//'   8-bit (\code{DT_UINT8}) NIfTI file.
//'
//' @examples
//' \dontrun{
//'   library(RNifti)
//'   nii <- readNifti("sub-01_task-rest_bold.nii.gz")
//'   mask <- automask_rcpp(nii, outfile = "sub-01_mask.nii.gz")
//' }
//'
//' @details
//' The processing pipeline is as follows:
//' \enumerate{
//'   \item Collapse 4D inputs to a 3D mean volume.
//'   \item Compute an iterative global clip threshold, estimate local clip
//'     levels in eight overlapping regions, and apply the smoothly interpolated
//'     spatial threshold.
//'   \item Retain only the largest connected component (NN as specified).
//'   \item Apply AFNI-style 17-of-18 layer-aware peeling and restoration
//'     (\code{peels} times, NN2), then retain the largest face-connected
//'     surviving component.
//'   \item Optionally fill interior holes.
//'   \item Apply user-specified erosion/dilation steps (NN as specified).
//'   \item Apply optional superior–inferior cutoff (\code{SIhh}).
//' }
//'
//' @seealso \code{\link[RNifti]{readNifti}}, AFNI \code{3dAutomask}
//' @export

// [[Rcpp::export]]
Rcpp::RObject automask(SEXP img,
                        std::string outfile,
                        float clfrac = 0.5,
                        int NN = 2, // nearest neighbor definition used for largest connected component
                        int erode_steps = 0, // erode mask x times
                        int dilate_steps = 0, // dilate mask x times
                        float SIhh = 0.0, // clip x mm below most superior voxel
                        int peels = 1, // erode/dilate peeling
                        bool fill_holes = true)
{
  NiftiImage image(img); // convert SEXP to nifti image
  
  // Dimensions
  std::vector<dim_t> dim = image.dim();
  if (dim.size() < 3) stop("Input must be at least 3D");
  const int nx = (int)dim[0], ny = (int)dim[1], nz = (int)dim[2];
  const int nvox = nx * ny * nz;
  
  // Collapse any higher dims (time, etc.) into one "frames" dimension
  size_t nframes = 1;
  if (dim.size() > 3) {
    for (size_t d = 3; d < dim.size(); ++d) nframes *= (size_t)dim[d];
  }
  
  // Load data
  NiftiImageData data(image);
  
  // Build a 3D working volume as the mean across frames (accommodates 3D and 4D inputs)
  std::vector<float> mean3d(nvox, 0.0f);
  if (nframes == 1u) {
    for (int i = 0; i < nvox; ++i) {
      float v = static_cast<float>(data[i]);
      mean3d[i] = std::isfinite(v) ? v : 0.0f;
    }
  } else {
    for (size_t t = 0; t < nframes; ++t) {
      const size_t off = t * (size_t)nvox;
      for (int i = 0; i < nvox; ++i) {
        float v = static_cast<float>(data[off + (size_t)i]);
        if (std::isfinite(v)) mean3d[i] += v;
      }
    }
    const float invT = 1.0f / static_cast<float>(nframes);
    for (int i = 0; i < nvox; ++i) mean3d[i] *= invT;
  }
  
  // Threshold using an iterative fixed clip and a smooth spatial clip field
  if (NN < 1 || NN > 3) NN = 2;
  if (clfrac <= 0.0f) clfrac = 0.5f;

  const GradualClipResult clip = gradual_clip_levels(mean3d, nx, ny, nz, clfrac);
  Rcout << "fixed clip level: " << clip.fixed
        << "; gradual range: " << clip.minimum << " to " << clip.maximum
        << std::endl;
  
  LogicalVector mask(nvox);
  const bool valid_clip = std::isfinite(clip.fixed) && clip.fixed > 0.0f;
  for (int i = 0; i < nvox; ++i) {
    mask[i] = valid_clip && std::isfinite(mean3d[i]) && mean3d[i] > 0.0f &&
      mean3d[i] >= clip.threshold[i];
  }
  
  // Keep the largest connected component
  mask = largest_component(mask, nx, ny, nz, NN);
  
  // Peel thin attachments with AFNI's 17-of-18 NN2 survival rule, restore only
  // removed voxels connected to the surviving core, then recluster.
  if (peels > 0) {
    int pre_peel = sum(mask);
    peel_and_restore(mask, nx, ny, nz, peels);
    // AFNI reclusters with face connectivity after restoration.
    mask = largest_component(mask, nx, ny, nz, 1);
    Rcout << "Peeled mask " << peels << " times. Mask voxels before: " << pre_peel << ", after: " << sum(mask) << std::endl;
  }
  
  // Fill enclosed holes to tidy up mask. Use NN=1 for the most generous fill
  // NN=1 (6-connected): diagonal/edge leaks don’t count, so fewer interior zeros are reachable from the boundary.
  // Thus, more regions are considered enclosed holes and get filled. Result: fewer holes remain.
  if (fill_holes) {
    int pre_fill = sum(mask);
    _fill_holes(mask, nx, ny, nz, 1);
    Rcout << "Filled interior holes. Mask voxels before: " << pre_fill << ", after: " << sum(mask) << std::endl;
  }
  
  // Optional user-specified erode/dilate steps
  if (erode_steps > 0)  morph(mask, nx, ny, nz, NN, erode_steps, false);
  if (dilate_steps > 0) morph(mask, nx, ny, nz, NN, dilate_steps, true);
  
  // Optional SI cutoff: remove voxels inferior (below) a specified distance
  // relative to the superior-most voxel in the mask. The distance SIhh is
  // given in millimeters, so voxel spacing in z (dz) must be taken into account.
  if (SIhh > 0.0f) {
    // RNifti::NiftiImage::pixdim() gives voxel spacing (size) for each dimension.
    // Here index 2 corresponds to the z-axis spacing (dz, in mm).
    std::vector<pixdim_t> pix = image.pixdim();
    float dz = 1.0f;
    if (pix.size() >= 3) dz = std::fabs(static_cast<float>(pix[2]));
    
    // Find the highest (most superior) slice index containing any voxel in the mask.
    int top_z = -1;
    for (int z = 0; z < nz; ++z) {
      for (int y = 0; y < ny; ++y)
        for (int x = 0; x < nx; ++x) {
          const int idx = x + y * nx + z * nx * ny;
          if (mask[idx]) { if (z > top_z) top_z = z; }
        }
    }
    
    // If a valid top slice was found, compute the cutoff slice index.
    // cutoff_z is SIhh millimeters inferior to the top_z slice, rounded.
    if (top_z >= 0 && dz > 0.0f) {
      const int cutoff_z = top_z - static_cast<int>(std::floor(SIhh / dz + 0.5f));
      
      // Zero out all voxels below the cutoff slice (z < cutoff_z).
      for (int z = 0; z < std::max(0, cutoff_z); ++z) {
        for (int y = 0; y < ny; ++y)
          for (int x = 0; x < nx; ++x)
            mask[x + y * nx + z * nx * ny] = false;
      }
    }
  }
  
  // Return 3-D integer mask with a 3-D dim attribute (no 4-D mismatch)
  IntegerVector imask(nvox);
  for (int i = 0; i < nvox; ++i) imask[i] = mask[i] ? 1 : 0;
  imask.attr("dim") = IntegerVector::create(nx, ny, nz);
  Rcout << "Voxels in mask: " << sum(imask) << std::endl;
  
  // RNifti can copy an image, then update accepts RObject inputs with dim attributes,
  // in which case it will modify the dimensions of the image
  NiftiImage result(image, false);
  result.update(imask);
  
  // Save to output
  if (!outfile.empty()) result.toFile(outfile, DT_UINT8);
  
  // Rcout << "result dim " << result.nDims() << std::endl; // should be 3, not 4
  return result.toArrayOrPointer(true, "NIfTI image");
}


/*** R
nii <- "/Users/hallquist/Downloads/func/sub-221256_task-trust_run-1_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz"
mask <- automask(nii, outfile="mask.nii.gz", clfrac = 0.5, NN = 1, SIhh = 0, peels = 1, fill_holes = TRUE, dilate_steps = 1)

*/
