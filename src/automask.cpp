// #include "BrainGnomes.h"
#define RNIFTI_NIFTILIB_VERSION 2
#include <Rcpp.h>
#include <array>
#include <vector>
#include <queue>
#include <algorithm>
#include <cmath>
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

// Robust clip level using [qlow, qhigh] of positive finite voxels
static float clip_level_from_positive(const std::vector<float> &vals,
                                      float clfrac,
                                      double qlow = 0.02,
                                      double qhigh = 0.98)
{
  std::vector<float> pos;
  pos.reserve(vals.size());
  for (float v : vals) if (v > 0.0f && std::isfinite(v)) pos.push_back(v);
  if (pos.empty()) return 0.0f;
  
  // Guard against pathological volumes with very few positives
  if (pos.size() < 1000) {
    // Fall back to median as last resort
    size_t mid = pos.size()/2;
    std::nth_element(pos.begin(), pos.begin()+mid, pos.end());
    float med = pos[mid];
    if ((pos.size() % 2) == 0) {
      std::nth_element(pos.begin(), pos.begin()+mid-1, pos.end());
      med = 0.5f * (med + pos[mid-1]);
    }
    return std::max(0.0f, med * std::max(clfrac, 0.1f));
  }
  
  // Compute robust low/high percentiles
  float lo = quantile_nth(pos, qlow);
  float hi = quantile_nth(pos, qhigh);
  if (!std::isfinite(lo) || !std::isfinite(hi) || hi <= lo) {
    // Fallback to median if range is degenerate
    size_t mid = pos.size()/2;
    std::nth_element(pos.begin(), pos.begin()+mid, pos.end());
    float med = pos[mid];
    if ((pos.size() % 2) == 0) {
      std::nth_element(pos.begin(), pos.begin()+mid-1, pos.end());
      med = 0.5f * (med + pos[mid-1]);
    }
    return std::max(0.0f, med * std::max(clfrac, 0.1f));
  }
  
  // Interpolate within robust range
  clfrac = std::min(std::max(clfrac, 0.0f), 1.0f);
  return lo + clfrac * (hi - lo);
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
//' @param clfrac Fraction of the robust intensity range used to set the clip
//'   level for initial thresholding. Default is 0.5.
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
//' @param peels Number of "peel/unpeel" operations (erode then dilate with NN2
//'   neighborhood) applied to remove thin protuberances. Default is 1, matching
//'   AFNI `3dAutomask`.
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
//'   \item Compute robust clip threshold and apply initial thresholding.
//'   \item Retain only the largest connected component (NN as specified).
//'   \item Apply AFNI-style peel/unpeel (\code{peels} times, NN2).
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
  
  // Threshold via robust clip level
  if (NN < 1 || NN > 3) NN = 2;
  if (clfrac <= 0.0f) clfrac = 0.5f;
  
  const float clip = clip_level_from_positive(mean3d, clfrac, .05, .95);
  Rcout << "clip level: " << clip << std::endl;
  
  LogicalVector mask(nvox);
  for (int i = 0; i < nvox; ++i) mask[i] = (mean3d[i] >= clip);
  
  // Keep the largest connected component
  mask = largest_component(mask, nx, ny, nz, NN);
  
  // Peel (erode) the mask 'pp' times, then unpeel (dilate). Using NN2 neighborhoods,
  // clips off protuberances less than 2*pp voxels thick
  if (peels > 0) {
    int pre_peel = sum(mask);
    // Use NN=2 regardless of the main NN setting (matches 3dAutomask docs)
    morph(mask, nx, ny, nz, 2, peels, false); // erode
    morph(mask, nx, ny, nz, 2, peels, true);  // dilate
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
