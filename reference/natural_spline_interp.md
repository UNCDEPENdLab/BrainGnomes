# Cubic spline interpolation with natural spline and linear extrapolation

Performs natural cubic spline interpolation for given input values. This
function takes known data points `(x, y)` and evaluates the cubic spline
interpolation at specified output points `xout`. It uses a natural
spline formulation with zero second derivatives at the endpoints.

## Arguments

- x:

  A numeric vector of strictly increasing x-values (time or position).

- y:

  A numeric vector of y-values at each x (same length as x).

- xout:

  A numeric vector of points at which to interpolate.

## Value

A numeric vector of interpolated y-values at each point in `xout`.

## Details

The function performs cubic spline interpolation using a tridiagonal
system to solve for the spline coefficients. If `xout` values fall
outside the `x` range, the function uses linear extrapolation, mirror
R's `splinefun` with `method = "natural"`

## Examples

``` r
  x <- c(0, 1, 2, 3, 4)
  y <- c(0, 1, 0, 1, 0)
  xout <- seq(0, 4, by = 0.1)
  natural_spline_interp(x, y, xout)
#>  [1] 0.00000000 0.17071429 0.33714286 0.49500000 0.64000000 0.76785714
#>  [7] 0.87428571 0.95500000 1.00571429 1.02214286 1.00000000 0.93728571
#> [13] 0.84114286 0.72100000 0.58628571 0.44642857 0.31085714 0.18900000
#> [19] 0.09028571 0.02414286 0.00000000 0.02414286 0.09028571 0.18900000
#> [25] 0.31085714 0.44642857 0.58628571 0.72100000 0.84114286 0.93728571
#> [31] 1.00000000 1.02214286 1.00571429 0.95500000 0.87428571 0.76785714
#> [37] 0.64000000 0.49500000 0.33714286 0.17071429 0.00000000
```
