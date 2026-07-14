#define RNIFTI_NIFTILIB_VERSION 2
#include <Rcpp.h>
#include <RNifti.h>

#include <algorithm>
#include <cmath>
#include <limits>
#include <string>
#include <vector>

using namespace Rcpp;
using namespace RNifti;

typedef int64_t dim_t;

namespace {

const double MAD_TO_SIGMA = 1.482602218505602;

double vector_median(std::vector<double> values) {
  if (values.empty()) return NA_REAL;

  const size_t n = values.size();
  const size_t middle = n / 2;
  std::nth_element(values.begin(), values.begin() + middle, values.end());
  const double upper = values[middle];

  if (n % 2 == 1) return upper;

  const double lower = *std::max_element(
    values.begin(), values.begin() + middle
  );
  return 0.5 * (lower + upper);
}

double raw_mad(const std::vector<double> &values, const double center) {
  if (values.empty() || !std::isfinite(center)) return NA_REAL;

  std::vector<double> deviations;
  deviations.reserve(values.size());
  for (const double value : values) {
    deviations.push_back(std::fabs(value - center));
  }
  return vector_median(deviations);
}

double robust_location(std::vector<double> values,
                       const std::string &method,
                       const double trim) {
  if (values.empty()) return NA_REAL;
  if (method == "median") return vector_median(values);

  std::sort(values.begin(), values.end());
  const size_t n_trim = static_cast<size_t>(
    std::floor(trim * static_cast<double>(values.size()))
  );
  const size_t first = n_trim;
  const size_t last = values.size() - n_trim;
  if (first >= last) return NA_REAL;

  long double total = 0.0;
  for (size_t i = first; i < last; ++i) total += values[i];
  return static_cast<double>(total / static_cast<long double>(last - first));
}

bool same_spatial_grid(const NiftiImage &reference,
                       const NiftiImage &candidate,
                       const double tolerance) {
  const std::vector<dim_t> reference_dim = reference.dim();
  const std::vector<dim_t> candidate_dim = candidate.dim();
  if (candidate_dim.size() != 3 || reference_dim.size() < 3) return false;

  for (int axis = 0; axis < 3; ++axis) {
    if (candidate_dim[axis] != reference_dim[axis]) return false;
  }

  const NiftiImage::Xform reference_transform = reference.xform();
  const NiftiImage::Xform candidate_transform = candidate.xform();
  const NiftiImage::Xform::Matrix &reference_xform =
    reference_transform.matrix();
  const NiftiImage::Xform::Matrix &candidate_xform =
    candidate_transform.matrix();
  for (int row = 0; row < 4; ++row) {
    for (int column = 0; column < 4; ++column) {
      const double difference = std::fabs(
        reference_xform(row, column) - candidate_xform(row, column)
      );
      if (!std::isfinite(difference) || difference > tolerance) return false;
    }
  }
  return true;
}

RObject nifti_from_numeric(const NiftiImage &source,
                           NumericVector values) {
  const nifti_image *source_pointer = source;
  NiftiImage result(nifti2_copy_nim_info(source_pointer), false);
  result.update(values);
  return result.toArrayOrPointer(true, "NIfTI image");
}

RObject nifti_from_integer(const NiftiImage &source,
                           IntegerVector values) {
  const nifti_image *source_pointer = source;
  NiftiImage result(nifti2_copy_nim_info(source_pointer), false);
  result.update(values);
  return result.toArrayOrPointer(true, "NIfTI image");
}

} // namespace

//' Measure a robust intensity location within a frozen reference core
//'
//' Computes the same voxelwise temporal location and spatial median used for
//' intensity normalization, but does not redefine or refine the supplied core.
//' This supports measuring the denominator after spatial preprocessing while
//' keeping voxel and frame selection fixed from the original positive-scale
//' BOLD image.
//'
//' @param img A 4D `RNifti::NiftiImage` object or path to a 4D BOLD NIfTI.
//' @param core_mask A 3D frozen reference-core mask on the same grid as `img`.
//' @param include_frames Optional logical vector with one value per volume.
//' @param baseline_method Either `"trimmed_mean"` (default) or `"median"`.
//' @param baseline_trim Fraction removed from each temporal tail for a trimmed
//'   mean. The default is 0.10.
//' @param min_valid_frames Minimum finite eligible observations per core voxel.
//' @param affine_tolerance Absolute tolerance for comparing NIfTI transforms.
//'
//' @return A list with `reference_location`, core and usable voxel counts,
//'   usable fraction, and eligible-frame count.
//'
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List measure_reference_location(
    SEXP img,
    SEXP core_mask,
    const Rcpp::Nullable<Rcpp::LogicalVector> &include_frames = R_NilValue,
    std::string baseline_method = "trimmed_mean",
    double baseline_trim = 0.10,
    int min_valid_frames = 20,
    double affine_tolerance = 1e-5) {

  if (baseline_method != "trimmed_mean" && baseline_method != "median") {
    stop("baseline_method must be 'trimmed_mean' or 'median'.");
  }
  if (!std::isfinite(baseline_trim) || baseline_trim < 0.0 ||
      baseline_trim >= 0.5) {
    stop("baseline_trim must be finite and in [0, 0.5).");
  }
  if (min_valid_frames < 1) stop("min_valid_frames must be positive.");
  if (!std::isfinite(affine_tolerance) || affine_tolerance < 0.0) {
    stop("affine_tolerance must be finite and nonnegative.");
  }

  NiftiImage image(img);
  const std::vector<dim_t> dimensions = image.dim();
  if (dimensions.size() != 4) stop("img must be a 4D NIfTI image.");

  const size_t nx = static_cast<size_t>(dimensions[0]);
  const size_t ny = static_cast<size_t>(dimensions[1]);
  const size_t nz = static_cast<size_t>(dimensions[2]);
  const size_t nt = static_cast<size_t>(dimensions[3]);
  const size_t nvox = nx * ny * nz;

  NiftiImage mask(core_mask);
  if (!same_spatial_grid(image, mask, affine_tolerance)) {
    stop("core_mask must be 3D and match the image dimensions and affine.");
  }

  LogicalVector eligible(nt, true);
  if (include_frames.isNotNull()) {
    LogicalVector supplied(include_frames);
    if (static_cast<size_t>(supplied.size()) != nt) {
      stop("include_frames must have one value per image volume.");
    }
    for (size_t t = 0; t < nt; ++t) {
      if (LogicalVector::is_na(supplied[t])) {
        stop("include_frames cannot contain missing values.");
      }
      eligible[t] = supplied[t];
    }
  }
  const int n_eligible = sum(eligible);
  if (n_eligible < min_valid_frames) {
    stop("Fewer than min_valid_frames volumes are eligible.");
  }

  NiftiImageData data(image);
  NiftiImageData mask_data(mask);
  std::vector<double> values;
  std::vector<double> voxel_locations;
  values.reserve(n_eligible);
  voxel_locations.reserve(nvox);

  int core_voxels = 0;
  for (size_t voxel = 0; voxel < nvox; ++voxel) {
    const double mask_value = static_cast<double>(mask_data[voxel]);
    if (!std::isfinite(mask_value) || mask_value <= 0.0) continue;
    ++core_voxels;

    values.clear();
    for (size_t t = 0; t < nt; ++t) {
      if (!eligible[t]) continue;
      const double value = static_cast<double>(data[t * nvox + voxel]);
      if (std::isfinite(value)) values.push_back(value);
    }
    if (values.size() < static_cast<size_t>(min_valid_frames)) continue;

    const double location = robust_location(
      values, baseline_method, baseline_trim
    );
    if (std::isfinite(location) && location > 0.0) {
      voxel_locations.push_back(location);
    }
  }

  if (core_voxels == 0) stop("The frozen reference core contains no positive voxels.");
  if (voxel_locations.empty()) {
    stop("No frozen-core voxels have a finite positive reference location.");
  }

  const int usable_core_voxels = static_cast<int>(voxel_locations.size());
  return List::create(
    _["reference_location"] = vector_median(voxel_locations),
    _["core_voxels"] = core_voxels,
    _["usable_core_voxels"] = usable_core_voxels,
    _["usable_core_fraction"] = static_cast<double>(usable_core_voxels) /
      static_cast<double>(core_voxels),
    _["eligible_frames"] = n_eligible
  );
}

//' Derive a conservative functional reference core from 4D BOLD data
//'
//' Refines a functional candidate mask by removing voxels with an invalid or
//' very low robust baseline, extreme robust relative temporal noise, or an
//' excessive fraction of abrupt signal changes. An optional second mask can be
//' supplied as a subtractive constraint. This function never dilates a mask or
//' fills holes.
//'
//' @param img A 4D `RNifti::NiftiImage` object or path to a 4D BOLD NIfTI.
//'   Intensities must retain a positive baseline; do not pass demeaned or
//'   residualized data whose voxelwise means have been removed.
//' @param candidate_mask Required 3D functional candidate mask, normally from
//'   `automask()` applied to the original positive-scale BOLD data with
//'   `fill_holes = FALSE` and `dilate_steps = 0`.
//' @param constraint_mask Optional 3D mask used only as a subtractive
//'   constraint. If `NULL` (the default), the candidate mask alone defines the
//'   population subject to temporal quality refinement.
//' @param include_frames Optional logical vector with one value per volume.
//'   `TRUE` marks steady-state, uncensored volumes eligible for estimation.
//'   Missing values are not allowed. If `NULL`, all volumes are eligible.
//' @param baseline_method Either `"trimmed_mean"` (default) or `"median"`.
//' @param baseline_trim Fraction removed from each tail when
//'   `baseline_method = "trimmed_mean"`. The default is 0.10.
//' @param baseline_floor_fraction Minimum baseline as a fraction of the spatial
//'   median baseline in the mask intersection. The default 0.20 is provisional
//'   and should be calibrated on representative data.
//' @param relative_noise_mad_cutoff Upper cutoff for log robust relative noise,
//'   expressed as spatial median plus this many scaled MADs. The default is 5.
//' @param spike_mad_cutoff A consecutive-volume change is called extreme when
//'   its absolute deviation from the median first difference exceeds this many
//'   scaled MADs. Both volumes bordering an extreme change are marked. Default
//'   is 6.
//' @param max_spike_fraction Maximum fraction of eligible volumes marked by an
//'   extreme consecutive-volume change. Default is 0.05.
//' @param min_valid_frames Minimum number of finite eligible volumes required
//'   for a voxel. At least two consecutive finite differences are also required.
//' @param affine_tolerance Absolute tolerance used when comparing the active
//'   4-by-4 NIfTI transforms of the image and masks.
//' @param outfile Optional path for the final uint8 reference-core mask. If
//'   `""`, no file is written.
//'
//' @return A list containing 3D RNifti images `core`, `agreement_mask`,
//'   `mask_membership`, `baseline`, `relative_noise_cv`, `spike_fraction`,
//'   `finite_fraction`, and `rejection_code`, plus scalar `thresholds`, voxel
//'   `counts`, and `mask_metrics`. `mask_membership` is coded 0 = neither mask,
//'   1 = mask A only, 2 = mask B only, and 3 = agreement. Temporal QA maps are
//'   computed over the candidate-mask union when a constraint is supplied so
//'   disagreement regions can be audited, but thresholds and the final core are
//'   derived strictly from the effective candidate population. With no
//'   constraint, `agreement_mask` is simply the candidate mask. Rejection codes
//'   are additive bit flags: 1 = outside the effective candidate population,
//'   2 = insufficient finite/consecutive observations, 4 =
//'   nonfinite or nonpositive baseline, 8 = baseline below the relative floor,
//'   16 = extreme relative noise, and 32 = excessive spike fraction.
//'
//' @details Robust relative noise is
//'   `1.4826 * median(abs(diff(y))) / (sqrt(2) * baseline)`, calculated only
//'   across consecutive eligible volumes. This median absolute first-difference
//'   estimator remains sensitive to sustained alternating noise, for which MAD
//'   around the median difference can collapse to zero. Its spatial upper
//'   threshold is estimated on the log scale after the baseline criteria have
//'   been applied. Spike fraction is the fraction of eligible volumes adjoining
//'   an extreme first difference, with the spike threshold based on MAD around
//'   the median difference. Threshold defaults are intentionally conservative
//'   draft values and require empirical calibration before pipeline use.
//'
//' @export
// [[Rcpp::export]]
Rcpp::List derive_reference_core(
    SEXP img,
    SEXP candidate_mask,
    SEXP constraint_mask = R_NilValue,
    const Rcpp::Nullable<Rcpp::LogicalVector> &include_frames = R_NilValue,
    std::string baseline_method = "trimmed_mean",
    double baseline_trim = 0.10,
    double baseline_floor_fraction = 0.20,
    double relative_noise_mad_cutoff = 5.0,
    double spike_mad_cutoff = 6.0,
    double max_spike_fraction = 0.05,
    int min_valid_frames = 20,
    double affine_tolerance = 1e-5,
    std::string outfile = "") {

  if (baseline_method != "trimmed_mean" && baseline_method != "median") {
    stop("baseline_method must be 'trimmed_mean' or 'median'.");
  }
  if (!std::isfinite(baseline_trim) || baseline_trim < 0.0 ||
      baseline_trim >= 0.5) {
    stop("baseline_trim must be finite and in [0, 0.5).");
  }
  if (!std::isfinite(baseline_floor_fraction) ||
      baseline_floor_fraction < 0.0 || baseline_floor_fraction >= 1.0) {
    stop("baseline_floor_fraction must be finite and in [0, 1).");
  }
  if (!std::isfinite(relative_noise_mad_cutoff) ||
      relative_noise_mad_cutoff <= 0.0) {
    stop("relative_noise_mad_cutoff must be finite and positive.");
  }
  if (!std::isfinite(spike_mad_cutoff) || spike_mad_cutoff <= 0.0) {
    stop("spike_mad_cutoff must be finite and positive.");
  }
  if (!std::isfinite(max_spike_fraction) || max_spike_fraction < 0.0 ||
      max_spike_fraction > 1.0) {
    stop("max_spike_fraction must be finite and in [0, 1].");
  }
  if (min_valid_frames < 3) stop("min_valid_frames must be at least 3.");
  if (!std::isfinite(affine_tolerance) || affine_tolerance < 0.0) {
    stop("affine_tolerance must be finite and nonnegative.");
  }

  NiftiImage image(img);
  const std::vector<dim_t> dimensions = image.dim();
  if (dimensions.size() != 4) stop("img must be a 4D NIfTI image.");

  const size_t nx = static_cast<size_t>(dimensions[0]);
  const size_t ny = static_cast<size_t>(dimensions[1]);
  const size_t nz = static_cast<size_t>(dimensions[2]);
  const size_t nt = static_cast<size_t>(dimensions[3]);
  const size_t nvox = nx * ny * nz;
  if (nt < static_cast<size_t>(min_valid_frames)) {
    stop("The image has fewer volumes than min_valid_frames.");
  }

  const bool has_constraint = constraint_mask != R_NilValue;
  SEXP effective_constraint = has_constraint ? constraint_mask : candidate_mask;
  NiftiImage first_mask(candidate_mask);
  NiftiImage second_mask(effective_constraint);
  if (!same_spatial_grid(image, first_mask, affine_tolerance) ||
      !same_spatial_grid(image, second_mask, affine_tolerance)) {
    stop("Candidate and constraint masks must be 3D and match the image dimensions and affine.");
  }

  LogicalVector eligible(nt, true);
  if (include_frames.isNotNull()) {
    LogicalVector supplied(include_frames);
    if (static_cast<size_t>(supplied.size()) != nt) {
      stop("include_frames must have one value per image volume.");
    }
    for (size_t t = 0; t < nt; ++t) {
      if (LogicalVector::is_na(supplied[t])) {
        stop("include_frames cannot contain missing values.");
      }
      eligible[t] = supplied[t];
    }
  }
  const int n_eligible = sum(eligible);
  if (n_eligible < min_valid_frames) {
    stop("Fewer than min_valid_frames volumes are eligible.");
  }

  NiftiImageData data(image);
  NiftiImageData first_mask_data(first_mask);
  NiftiImageData second_mask_data(second_mask);

  IntegerVector agreement(nvox, 0);
  IntegerVector mask_membership(nvox, 0);
  IntegerVector core(nvox, 0);
  IntegerVector rejection_code(nvox, 1);
  NumericVector baseline(nvox, NA_REAL);
  NumericVector relative_noise_cv(nvox, NA_REAL);
  NumericVector spike_fraction(nvox, NA_REAL);
  NumericVector finite_fraction(nvox, NA_REAL);

  agreement.attr("dim") = IntegerVector::create(nx, ny, nz);
  mask_membership.attr("dim") = IntegerVector::create(nx, ny, nz);
  core.attr("dim") = IntegerVector::create(nx, ny, nz);
  rejection_code.attr("dim") = IntegerVector::create(nx, ny, nz);
  baseline.attr("dim") = IntegerVector::create(nx, ny, nz);
  relative_noise_cv.attr("dim") = IntegerVector::create(nx, ny, nz);
  spike_fraction.attr("dim") = IntegerVector::create(nx, ny, nz);
  finite_fraction.attr("dim") = IntegerVector::create(nx, ny, nz);

  std::vector<bool> has_sufficient_data(nvox, false);
  std::vector<bool> has_positive_baseline(nvox, false);
  std::vector<double> positive_baselines;
  positive_baselines.reserve(nvox);

  int mask_a_count = 0;
  int mask_b_count = 0;
  int agreement_count = 0;
  int union_count = 0;

  // Reuse temporal work buffers across voxels to avoid repeated allocation on
  // typical whole-brain images with tens or hundreds of thousands of voxels.
  std::vector<double> values;
  std::vector<double> differences;
  std::vector<double> absolute_differences;
  std::vector<size_t> difference_left;
  std::vector<size_t> difference_right;
  std::vector<bool> spike_volume(nt, false);
  values.reserve(n_eligible);
  differences.reserve(n_eligible > 0 ? n_eligible - 1 : 0);
  absolute_differences.reserve(n_eligible > 0 ? n_eligible - 1 : 0);
  difference_left.reserve(n_eligible > 0 ? n_eligible - 1 : 0);
  difference_right.reserve(n_eligible > 0 ? n_eligible - 1 : 0);

  for (size_t voxel = 0; voxel < nvox; ++voxel) {
    const double first_value = static_cast<double>(first_mask_data[voxel]);
    const double second_value = static_cast<double>(second_mask_data[voxel]);
    const bool in_first = std::isfinite(first_value) && first_value > 0.0;
    const bool in_second = std::isfinite(second_value) && second_value > 0.0;
    if (in_first) ++mask_a_count;
    if (in_second) ++mask_b_count;
    if (!(in_first || in_second)) continue;

    ++union_count;
    mask_membership[voxel] = (in_first ? 1 : 0) + (in_second ? 2 : 0);

    const bool in_agreement = in_first && in_second;
    if (in_agreement) {
      agreement[voxel] = 1;
      rejection_code[voxel] = 0;
      ++agreement_count;
    }

    values.clear();
    differences.clear();
    absolute_differences.clear();
    difference_left.clear();
    difference_right.clear();

    bool previous_was_finite_eligible = false;
    size_t previous_t = 0;
    double previous_value = 0.0;

    for (size_t t = 0; t < nt; ++t) {
      if (!eligible[t]) {
        previous_was_finite_eligible = false;
        continue;
      }

      const double value = static_cast<double>(data[t * nvox + voxel]);
      if (!std::isfinite(value)) {
        previous_was_finite_eligible = false;
        continue;
      }

      values.push_back(value);
      if (previous_was_finite_eligible && t == previous_t + 1) {
        differences.push_back(value - previous_value);
        difference_left.push_back(previous_t);
        difference_right.push_back(t);
      }
      previous_was_finite_eligible = true;
      previous_t = t;
      previous_value = value;
    }

    finite_fraction[voxel] = static_cast<double>(values.size()) /
      static_cast<double>(n_eligible);
    if (values.size() < static_cast<size_t>(min_valid_frames) ||
        differences.size() < 2) {
      rejection_code[voxel] |= 2;
      continue;
    }
    has_sufficient_data[voxel] = true;

    const double voxel_baseline = robust_location(
      values, baseline_method, baseline_trim
    );
    baseline[voxel] = voxel_baseline;
    if (!std::isfinite(voxel_baseline) || voxel_baseline <= 0.0) {
      rejection_code[voxel] |= 4;
      continue;
    }
    has_positive_baseline[voxel] = true;
    if (in_agreement) positive_baselines.push_back(voxel_baseline);

    const double difference_median = vector_median(differences);
    const double centered_difference_mad = raw_mad(
      differences, difference_median
    );
    for (const double difference : differences) {
      absolute_differences.push_back(std::fabs(difference));
    }
    const double median_absolute_difference = vector_median(
      absolute_differences
    );
    const double robust_sigma = MAD_TO_SIGMA * median_absolute_difference /
      std::sqrt(2.0);
    relative_noise_cv[voxel] = robust_sigma / voxel_baseline;

    std::fill(spike_volume.begin(), spike_volume.end(), false);
    const double difference_cutoff = spike_mad_cutoff *
      MAD_TO_SIGMA * centered_difference_mad;
    const double numerical_tolerance = std::max(
      1e-12, std::fabs(voxel_baseline) * 1e-12
    );
    for (size_t i = 0; i < differences.size(); ++i) {
      const double deviation = std::fabs(differences[i] - difference_median);
      const bool is_spike = difference_cutoff > numerical_tolerance ?
        deviation > difference_cutoff : deviation > numerical_tolerance;
      if (is_spike) {
        spike_volume[difference_left[i]] = true;
        spike_volume[difference_right[i]] = true;
      }
    }

    int n_spike_volumes = 0;
    for (size_t t = 0; t < nt; ++t) {
      if (eligible[t] && spike_volume[t]) ++n_spike_volumes;
    }
    spike_fraction[voxel] = static_cast<double>(n_spike_volumes) /
      static_cast<double>(n_eligible);
  }

  if (agreement_count == 0) {
    stop(has_constraint ?
      "The candidate and constraint masks do not overlap." :
      "The candidate mask contains no positive voxels.");
  }
  if (positive_baselines.empty()) {
    stop("No agreement voxels have sufficient data and a positive baseline.");
  }

  const double spatial_baseline_median = vector_median(positive_baselines);
  const double baseline_floor = baseline_floor_fraction * spatial_baseline_median;

  std::vector<bool> passes_baseline_floor(nvox, false);
  std::vector<double> log_relative_noise;
  log_relative_noise.reserve(positive_baselines.size());
  const double relative_noise_floor = 1e-12;

  int sufficient_count = 0;
  int positive_baseline_count = 0;
  int baseline_floor_count = 0;
  for (size_t voxel = 0; voxel < nvox; ++voxel) {
    const bool in_agreement = agreement[voxel] != 0;
    if (in_agreement && has_sufficient_data[voxel]) ++sufficient_count;
    if (in_agreement && has_positive_baseline[voxel]) {
      ++positive_baseline_count;
    }
    if (!has_positive_baseline[voxel]) continue;

    if (baseline[voxel] < baseline_floor) {
      rejection_code[voxel] |= 8;
      continue;
    }
    passes_baseline_floor[voxel] = true;
    if (in_agreement) ++baseline_floor_count;

    const double noise = relative_noise_cv[voxel];
    if (in_agreement && std::isfinite(noise) && noise >= 0.0) {
      log_relative_noise.push_back(std::log(std::max(noise, relative_noise_floor)));
    }
  }

  if (log_relative_noise.empty()) {
    stop("No baseline-valid agreement voxels have finite relative noise.");
  }

  const double spatial_log_noise_median = vector_median(log_relative_noise);
  const double spatial_log_noise_mad = MAD_TO_SIGMA * raw_mad(
    log_relative_noise, spatial_log_noise_median
  );
  const double log_noise_threshold = spatial_log_noise_median +
    relative_noise_mad_cutoff * spatial_log_noise_mad;
  const double relative_noise_threshold = std::exp(log_noise_threshold);

  int noise_count = 0;
  int spike_count = 0;
  int core_count = 0;
  for (size_t voxel = 0; voxel < nvox; ++voxel) {
    if (!passes_baseline_floor[voxel]) continue;
    const bool in_agreement = agreement[voxel] != 0;

    const double noise = relative_noise_cv[voxel];
    const bool passes_noise = std::isfinite(noise) && noise >= 0.0 &&
      std::log(std::max(noise, relative_noise_floor)) <= log_noise_threshold;
    if (passes_noise) {
      if (in_agreement) ++noise_count;
    } else {
      rejection_code[voxel] |= 16;
    }

    const bool passes_spike = std::isfinite(spike_fraction[voxel]) &&
      spike_fraction[voxel] <= max_spike_fraction;
    if (passes_spike) {
      if (in_agreement) ++spike_count;
    } else {
      rejection_code[voxel] |= 32;
    }

    if (!(in_agreement && passes_noise && passes_spike)) continue;

    core[voxel] = 1;
    ++core_count;
  }

  if (core_count == 0) {
    warning("No voxels survived all reference-core criteria.");
  }

  // Copy only the NIfTI header, then install the 3D core payload. This avoids an
  // unnecessary deep copy of the full 4D image and ensures that none of the
  // returned RNifti objects share data storage.
  const nifti_image *image_pointer = image;
  NiftiImage core_image(nifti2_copy_nim_info(image_pointer), false);
  core_image.update(core);
  if (!outfile.empty()) core_image.toFile(outfile, DT_UINT8);
  RObject core_object = core_image.toArrayOrPointer(true, "NIfTI image");

  return List::create(
    _["core"] = core_object,
    _["agreement_mask"] = nifti_from_integer(core_image, agreement),
    _["mask_membership"] = nifti_from_integer(core_image, mask_membership),
    _["baseline"] = nifti_from_numeric(core_image, baseline),
    _["relative_noise_cv"] = nifti_from_numeric(core_image, relative_noise_cv),
    _["spike_fraction"] = nifti_from_numeric(core_image, spike_fraction),
    _["finite_fraction"] = nifti_from_numeric(core_image, finite_fraction),
    _["rejection_code"] = nifti_from_integer(core_image, rejection_code),
    _["thresholds"] = List::create(
      _["spatial_baseline_median"] = spatial_baseline_median,
      _["baseline_floor"] = baseline_floor,
      _["spatial_log_noise_median"] = spatial_log_noise_median,
      _["spatial_log_noise_mad"] = spatial_log_noise_mad,
      _["relative_noise_threshold"] = relative_noise_threshold,
      _["max_spike_fraction"] = max_spike_fraction
    ),
    _["counts"] = IntegerVector::create(
      _["mask_a"] = mask_a_count,
      _["mask_b"] = mask_b_count,
      _["mask_a_only"] = mask_a_count - agreement_count,
      _["mask_b_only"] = mask_b_count - agreement_count,
      _["agreement"] = agreement_count,
      _["union"] = union_count,
      _["sufficient_data"] = sufficient_count,
      _["positive_baseline"] = positive_baseline_count,
      _["baseline_floor"] = baseline_floor_count,
      _["relative_noise"] = noise_count,
      _["spike_fraction"] = spike_count,
      _["core"] = core_count
    ),
    _["mask_metrics"] = NumericVector::create(
      _["dice"] = 2.0 * static_cast<double>(agreement_count) /
        static_cast<double>(mask_a_count + mask_b_count),
      _["jaccard"] = static_cast<double>(agreement_count) /
        static_cast<double>(union_count),
      _["agreement_fraction_mask_a"] = static_cast<double>(agreement_count) /
        static_cast<double>(mask_a_count),
      _["agreement_fraction_mask_b"] = static_cast<double>(agreement_count) /
        static_cast<double>(mask_b_count),
      _["core_fraction_of_agreement"] = static_cast<double>(core_count) /
        static_cast<double>(agreement_count)
    ),
    _["used_constraint_mask"] = has_constraint
  );
}
