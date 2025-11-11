mtm_bandpower <- function(
    y, dt,
    bands,                       # data.frame with low/high[/label], or named list of length-2 numerics
    nw = 4, k = NULL,
    detrend = c("none","linear"),
    centre = TRUE,
    adaptive = TRUE, jackknife = FALSE,
    pad_factor = 2,              # padding for nicer grids; doesn't change variance
    exclude_dc = TRUE,           # drop f=0
    total_band = NULL,           # e.g., c(0.01, 0.5); power for "relative_power"
    na_rm = TRUE,
    psd = NULL
) {
  stopifnot(length(dt) == 1, is.finite(dt), dt > 0)
  detrend <- match.arg(detrend)
  if (is.null(psd)) {
    stopifnot(is.numeric(y))
  }
  
  if (is.null(psd) && !requireNamespace("multitaper", quietly = TRUE))
    stop("Package 'multitaper' is required (install.packages('multitaper')).")
  
  # --- parse bands ---
  if (is.data.frame(bands)) {
    stopifnot(all(c("low","high") %in% names(bands)))
    labels <- if ("label" %in% names(bands)) bands$label else NULL
    bands_df <- data.frame(
      low  = as.numeric(bands$low),
      high = as.numeric(bands$high),
      label = if (is.null(labels)) sprintf("band_%02d", seq_len(nrow(bands))) else as.character(labels),
      stringsAsFactors = FALSE
    )
  } else if (is.list(bands) && length(bands) > 0) {
    nm <- names(bands)
    if (is.null(nm) || any(nm == "")) nm <- paste0("band_", seq_along(bands))
    bands_df <- do.call(rbind, lapply(seq_along(bands), function(i) {
      b <- as.numeric(bands[[i]])
      if (length(b) != 2) stop("Each band in the list must be length-2: c(low, high).")
      data.frame(low = min(b), high = max(b), label = nm[i], stringsAsFactors = FALSE)
    }))
  } else {
    stop("`bands` must be a data.frame with columns low/high[/label] or a named list of length-2 numerics.")
  }
  
  if (is.null(psd)) {
    # --- prep series ---
    y <- as.numeric(y)
    if (na_rm) y <- y[is.finite(y)]
    if (!length(y)) stop("All values are NA/Inf after filtering.")
    if (centre) y <- y - mean(y)
    if (detrend == "linear" && length(y) > 2) {
      t <- seq_along(y)
      y <- resid(stats::lm(y ~ t))
    }
    if (stats::var(y) <= .Machine$double.eps) {
      stop("Time series is (near) constant after preprocessing; cannot estimate PSD.")
    }
    
    # --- multitaper PSD ---
    if (is.null(k)) k <- max(1L, floor(2*nw - 1))
    N <- length(y)
    n_fft <- as.integer(ceiling(N * pad_factor))
    # if you prefer power-of-two FFTs, uncomment:
    # n_fft <- 2^ceiling(log2(n_fft))
    
    mt <- multitaper::spec.mtm(
      y,
      k = k, nw = nw,
      nFFT = n_fft,
      deltat = dt,                 # frequency in Hz
      centreWithSlepians = TRUE,   # Slepian centering
      adaptive = adaptive,
      jackknife = jackknife,
      plot = FALSE,
      returnZeroFreq = TRUE
    )
    
    f <- mt$freq             # Hz, from 0 .. Nyquist
    S <- mt$spec             # linear spectral density
    N_series <- N
    psd_meta_raw <- list(
      n = N_series,
      nw = nw,
      k = k,
      adaptive = adaptive,
      jackknife = jackknife,
      centred = centre,
      centre = centre,
      detrend = detrend,
      pad_factor = pad_factor
    )
  } else {
    if (!is.list(psd) || is.null(psd$freq) || is.null(psd$spec)) {
      stop("`psd` must be a list with elements `freq` and `spec`.")
    }
    f <- as.numeric(psd$freq)
    S <- as.numeric(psd$spec)
    if (!length(f) || length(f) != length(S)) {
      stop("`psd$freq` and `psd$spec` must be non-empty and of equal length.")
    }
    if (any(!is.finite(f)) || any(!is.finite(S))) {
      stop("`psd$freq` and `psd$spec` must contain only finite values.")
    }
    if (is.unsorted(f)) {
      ord <- order(f)
      f <- f[ord]
      S <- S[ord]
    }
    N_series <- if (is.list(psd$meta) && !is.null(psd$meta$n)) psd$meta$n else NA_integer_
    psd_meta_raw <- if (is.list(psd$meta)) psd$meta else list()
  }
  
  # Exclude DC bin (recommended for fMRI)
  if (exclude_dc) {
    keep <- f > 0
    f <- f[keep]; S <- S[keep]
  }
  
  nyq <- 1/(2*dt)
  
  # Interpolator for integration (linear)
  # S_fun <- stats::approxfun(f, S, rule = 2)  # constant extrapolation outside range
  # 
  # # Helper: integrate safely over [a,b], clipped to [min(f), max(f)]
  # integrate_band <- function(a, b) {
  #   lo <- max(min(f), a); hi <- min(max(f), b)
  #   if (!is.finite(lo) || !is.finite(hi) || hi <= lo) return(NA_real_)
  #   res <- try(stats::integrate(S_fun, lower = lo, upper = hi), silent = TRUE)
  #   if (inherits(res, "try-error")) return(NA_real_)
  #   as.numeric(res$value)
  # }
  
  integrate_band <- function(a, b) {
    # clip to PSD span
    a <- max(a, min(f)); b <- min(b, max(f))
    if (!is.finite(a) || !is.finite(b) || b <= a) return(0)  # return 0 power if no overlap
    
    # find indices whose bins intersect [a,b]
    idx <- which(f >= a & f <= b)
    if (length(idx) < 2L) {
      # interpolate endpoints and do a tiny trapezoid
      Sa <- approx(f, S, xout = a, rule = 2)$y
      Sb <- approx(f, S, xout = b, rule = 2)$y
      return(0.5 * (Sa + Sb) * (b - a))
    } else {
      # include band edges explicitly
      ff <- c(a, f[idx], b)
      SS <- c(approx(f, S, xout = a, rule = 2)$y, S[idx],
              approx(f, S, xout = b, rule = 2)$y)
      # trapezoidal area
      sum(0.5 * (SS[-1] + SS[-length(SS)]) * diff(ff))
    }
  }
  
  # Compute band powers
  bands_df$low  <- pmax(0, bands_df$low)
  bands_df$high <- pmin(nyq, bands_df$high)
  bands_df$power_linear <- vapply(
    seq_len(nrow(bands_df)),
    function(i) integrate_band(bands_df$low[i], bands_df$high[i]),
    numeric(1)
  )
  bands_df$power_db <- ifelse(is.finite(bands_df$power_linear) & bands_df$power_linear > 0,
                              10*log10(bands_df$power_linear),
                              NA_real_)
  
  # Relative power vs total band (default: total over the union of provided bands;
  # you can pass `total_band = c(0.01, nyq)` to normalize to a fixed range)
  if (is.null(total_band)) {
    tot_lo <- min(bands_df$low, na.rm = TRUE)
    tot_hi <- max(bands_df$high, na.rm = TRUE)
  } else {
    stopifnot(is.numeric(total_band), length(total_band) == 2)
    tot_lo <- max(0, min(total_band)); tot_hi <- min(nyq, max(total_band))
  }
  total_power <- integrate_band(tot_lo, tot_hi)
  bands_df$relative_power <- if (is.finite(total_power) && total_power > 0) {
    bands_df$power_linear / total_power
  } else {
    NA_real_
  }
  
  centre_meta <- if (!is.null(psd_meta_raw$centred)) {
    psd_meta_raw$centred
  } else if (!is.null(psd_meta_raw$centre)) {
    psd_meta_raw$centre
  } else {
    centre
  }
  psd_meta_out <- list(
    dt = dt,
    nyquist = nyq,
    n = N_series,
    nw = if (!is.null(psd_meta_raw$nw)) psd_meta_raw$nw else nw,
    k = if (!is.null(psd_meta_raw$k)) psd_meta_raw$k else k,
    adaptive = if (!is.null(psd_meta_raw$adaptive)) psd_meta_raw$adaptive else adaptive,
    jackknife = if (!is.null(psd_meta_raw$jackknife)) psd_meta_raw$jackknife else jackknife,
    centred = centre_meta,
    detrend = if (!is.null(psd_meta_raw$detrend)) psd_meta_raw$detrend else detrend,
    pad_factor = if (!is.null(psd_meta_raw$pad_factor)) psd_meta_raw$pad_factor else pad_factor,
    exclude_dc = exclude_dc
  )
  attr(bands_df, "psd_meta") <- psd_meta_out
  
  bands_df[, c("label","low","high","power_linear","power_db","relative_power")]
}
