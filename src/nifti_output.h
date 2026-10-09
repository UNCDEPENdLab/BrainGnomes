#ifndef BRAINGNOMES_NIFTI_OUTPUT_H
#define BRAINGNOMES_NIFTI_OUTPUT_H

#include <algorithm>
#include <cmath>
#include <limits>

// Encode finite processed values in the requested integer storage type.
// Choose a binary scaling grid and round to its nearest point. Account for
// NIfTI-1's float header fields before encoding, so the persisted grid fits the
// output range. Direct typed stores avoid RNifti's 32-bit integer intermediate.
template <typename Integer>
RNifti::NiftiImageData encode_nifti_integer_data(
    const RNifti::NiftiImage &image, int datatype) {
  const RNifti::NiftiImageData source = image.data();
  long double minimum = std::numeric_limits<long double>::infinity();
  long double maximum = -minimum;
  for (size_t i = 0; i < source.length(); ++i) {
    const double value = source[i];
    if (!std::isfinite(value)) {
      Rcpp::stop("Cannot save non-finite processed values in integer NIfTI storage.");
    }
    minimum = std::min(minimum, static_cast<long double>(value));
    maximum = std::max(maximum, static_cast<long double>(value));
  }

  const bool nifti2 = image->nifti_type == NIFTI_FTYPE_NIFTI2_1 ||
    image->nifti_type == NIFTI_FTYPE_NIFTI2_2;
  const long double code_min = static_cast<long double>(std::numeric_limits<Integer>::lowest()) +
    (datatype == DT_INT32 ? 1.0L : 0.0L); // INT32_MIN is RNifti's missing-value sentinel.
  const long double code_max = static_cast<long double>(std::numeric_limits<Integer>::max());
  double slope = 1.0;
  double intercept = static_cast<double>(minimum);

  if (maximum != minimum) {
    const long double ideal = (maximum - minimum) / (code_max - code_min);
    long double step = std::exp2(std::ceil(std::log2(ideal)));
    // A NIfTI-1 slope cannot be smaller than its float header can represent.
    if (!nifti2) step = std::max(step,
      static_cast<long double>(std::numeric_limits<float>::denorm_min()));
    for (;;) {
      slope = static_cast<double>(step);
      const long double center = minimum + (maximum - minimum) / 2.0L;
      // Unsigned codes start at the minimum rather than their midpoint. This
      // avoids a huge cancelling intercept when the header imposes a slope
      // larger than the ideal grid (e.g. tiny values in NIfTI-1 UINT64).
      const long double offset = std::numeric_limits<Integer>::is_signed ?
        center - step * (code_min + code_max) / 2.0L :
        std::floor(minimum / step) * step;
      intercept = static_cast<double>(std::round(offset / step) * step);
      if (!nifti2) {
        slope = static_cast<float>(slope);
        intercept = static_cast<float>(intercept);
      }
      // Header rounding must not move an unsigned lower bound above the data.
      if (!std::numeric_limits<Integer>::is_signed && intercept > minimum) {
        intercept = nifti2 ? std::nextafter(intercept, -std::numeric_limits<double>::infinity()) :
          std::nextafter(static_cast<float>(intercept), -std::numeric_limits<float>::infinity());
      }
      if (!std::isfinite(slope) || slope <= 0 || !std::isfinite(intercept)) {
        Rcpp::stop("Processed values cannot be represented by integer NIfTI scaling.");
      }
      // Leave room for rounding at both endpoints, using the actual header grid.
      const long double low = (minimum - intercept) / slope;
      const long double high = (maximum - intercept) / slope;
      if (low >= code_min && high <= code_max) break;
      step *= 2.0L;
    }
  } else if (!nifti2) {
    intercept = static_cast<float>(intercept);
  }

  RNifti::NiftiImageData encoded(NULL, source.length(), datatype, slope, intercept);
  Integer *codes = static_cast<Integer *>(encoded.blob());
  for (size_t i = 0; i < source.length(); ++i) {
    const long double code = std::round((static_cast<long double>(source[i]) - intercept) / slope);
    // Clamp before casting: floating-point representations of 64-bit limits
    // can lie one past the integer maximum on platforms with double long double.
    if (code <= code_min) {
      codes[i] = std::numeric_limits<Integer>::lowest() + (datatype == DT_INT32 ? 1 : 0);
    } else if (code >= code_max) {
      codes[i] = std::numeric_limits<Integer>::max();
    } else {
      codes[i] = static_cast<Integer>(code);
    }
  }
  return encoded;
}

// Write an integer image using the encoded buffer and a private metadata copy.
// Keep the processed image unchanged and avoid copying encoded data on dispatch.
template <typename Integer>
inline void write_scaled_nifti_integer(const RNifti::NiftiImage &image,
                                       const std::string &outfile, int datatype) {
  const RNifti::NiftiImageData encoded = encode_nifti_integer_data<Integer>(image, datatype);
  RNifti::NiftiImage output(image, true);
  output.replaceData(encoded);
  output.toFile(outfile, datatype);
}

// Write processed data without mutating the floating-point image returned to R.
// Integer output retains the requested datatype and recalibrates its scaling;
// floating-point output follows RNifti's ordinary write path.
inline void write_nifti_preserving_datatype(const RNifti::NiftiImage &image,
                                           const std::string &outfile, int datatype) {
  if (outfile.empty()) return;
  switch (datatype) {
    case DT_INT8: write_scaled_nifti_integer<int8_t>(image, outfile, datatype); return;
    case DT_UINT8: write_scaled_nifti_integer<uint8_t>(image, outfile, datatype); return;
    case DT_INT16: write_scaled_nifti_integer<int16_t>(image, outfile, datatype); return;
    case DT_UINT16: write_scaled_nifti_integer<uint16_t>(image, outfile, datatype); return;
    case DT_INT32: write_scaled_nifti_integer<int32_t>(image, outfile, datatype); return;
    case DT_UINT32: write_scaled_nifti_integer<uint32_t>(image, outfile, datatype); return;
    case DT_INT64: write_scaled_nifti_integer<int64_t>(image, outfile, datatype); return;
    case DT_UINT64: write_scaled_nifti_integer<uint64_t>(image, outfile, datatype); return;
    default: image.toFile(outfile, datatype); return;
  }
}

#endif
