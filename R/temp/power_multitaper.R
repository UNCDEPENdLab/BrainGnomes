power_multitaper <- function(
    y, dt,
    nw = 3, k = NULL,
    pad_factor = 0.5, detrend = TRUE,
    centre = "Slepian", adaptive = TRUE, jackknife = FALSE,
    smooth_psd = TRUE
) {
  stopifnot(is.numeric(y), length(dt) == 1, is.finite(dt), dt > 0)
  # detrend <- match.arg(detrend)
  checkmate::assert_number(pad_factor, lower = 0, upper=100, null.ok = TRUE)
  if (is.null(pad_factor)) pad_factor <- 0.5 # 50% padding
  
  if (!requireNamespace("multitaper", quietly = TRUE))
    stop("Package 'multitaper' is required.")
  
  y <- as.numeric(y)
  if (any(!is.finite(y))) stop("Cannot process inputs with NA/Inf values.")
  if (length(y) < 5) stop("y is less than 5 timepoints")

  # optional linear detrend -- useful for removing drift prior to PSD
  if (isTRUE(detrend)) {
    t <- seq_along(y)
    y <- resid(stats::lm(y ~ t))
  }

  # guard against constant series
  if (stats::var(y) <= 2*.Machine$double.eps) {
    return(data.frame(f = numeric(), power = numeric()))
  }
  
  if (is.null(k)) k <- max(1L, floor(2 * nw - 1))
  
  n_fft <- 2^ceiling(log2((1+pad_factor) * length(y)))
  
  out <- multitaper::spec.mtm(
    y, k = k, nw = nw, nFFT = n_fft,
    centre = centre, adaptive = adaptive, jackknife = jackknife,
    plot = FALSE, returnZeroFreq = TRUE, deltat=dt
  )
  
  f_hz <- out$freq # in Hz because delta passed to spec.mtm
  p_db <- 10 * log10(out$spec) # convert to dB
  
  # Spectra tend to be much easier to look at when smoothed. Use a peak-preserving smoother
  # with a width of 1/20th of the series
  if (smooth_psd) {
    window_length <- 2 * floor((length(p_db) / 20) / 2) + 1 # 1/20th of the series
    p_db <- signal::sgolayfilt(p_db, p = 3, n = window_length)
    #p_db <- stats::filter(p_db, rep(1/5, 5), sides = 2)  # 5-point moving average
  }
  
  return(data.frame(f = f_hz, power = p_db))
  
}

smooth_downsample <- function(freq, power, n_bins = 250) {
  stopifnot(length(freq) == length(power))
  
  # Break into evenly spaced frequency bins
  bin_edges <- seq(min(freq), max(freq), length.out = n_bins + 1)
  bin_centers <- 0.5 * (bin_edges[-1] + bin_edges[-length(bin_edges)])
  
  # Bin means
  power_mean <- numeric(n_bins)
  for (i in seq_len(n_bins)) {
    in_bin <- freq >= bin_edges[i] & freq < bin_edges[i + 1]
    power_mean[i] <- if (any(in_bin)) mean(power[in_bin]) else NA
  }
  
  data.frame(f = bin_centers, power = power_mean)
}