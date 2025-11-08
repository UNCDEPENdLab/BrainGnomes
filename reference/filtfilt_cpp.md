# Zero-Phase IIR Filtering via Forward and Reverse Filtering

Applies an IIR filter to a 1D numeric signal using forward and backward
passes to eliminate phase distortions, similar to
`scipy.signal.filtfilt`. This function implements a pure C++ version
using the Direct Form II Transposed structure, including optional
initial condition handling via steady-state initialization.

## Arguments

- x:

  A numeric vector representing the input time series.

- b:

  A numeric vector of numerator (feedforward) filter coefficients.

- a:

  A numeric vector of denominator (feedback) filter coefficients. Must
  have `a[0] == 1.0`.

- padlen:

  Number of samples to extend on each edge for padding. If `-1`
  (default), uses `3 * max(length(a), length(b))`.

- padtype:

  Type of padding at the signal boundaries. One of `"constant"`
  (default), `"odd"`, `"even"`, or `"zero"`.

- use_zi:

  Logical. If `TRUE` (default), use steady-state initial conditions to
  minimize transients.

## Value

A numeric vector of the same length as `x`, containing the filtered
signal.

## Details

The function applies the IIR filter in the forward direction, reverses
the result, and applies the filter again, then reverses the final
output. This approach removes phase delay. Padding is used to minimize
edge artifacts, and the filter state is optionally initialized with
values derived from the steady-state step response of the filter
(`lfilter_zi_arma()`).

## References

- Scipy Signal Documentation:
  <https://docs.scipy.org/doc/scipy/reference/generated/scipy.signal.filtfilt.html>

- Gustafsson, F. (1996). Determining the initial states in
  forward-backward filtering. IEEE Transactions on Signal Processing.
