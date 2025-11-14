#!/usr/bin/env Rscript

# -------------------------------------------------------------------------
# 3dSmoothnessChange.R
#   Hybrid smoothness estimator that supports three internal methods:
#     1) PSD (frequency-domain, legacy 3dSmoothnessChange behaviour)
#     2) Classic neighbour-difference FWHM (mirrors 3dFWHMx classic mode)
#     3) ACF mixed-model FWHM (Gaussian + exponential)
#   Single-dataset mode reports the current smoothness (per method) and the
#   PSD-predicted effect of adding a Gaussian kernel. Comparison mode takes
#   pre/post datasets, predicts the ΔFWHM via the PSD model, measures the
#   actual Δ via the requested method, and compares both the raw PSD error
#   and a method-specific calibration regression.
# -------------------------------------------------------------------------

suppressWarnings(suppressMessages({
  have_RNifti <- requireNamespace("RNifti", quietly = TRUE)
  have_pracma <- requireNamespace("pracma", quietly = TRUE)
}))

if (!have_RNifti) {
  stop("RNifti package is required for 3dSmoothnessChange.R.")
}

# Load shared helper functions (classic/ACF estimators)
get_script_dir <- function() {
  cmd <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd, value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  }
  "."
}

helper_path <- file.path(get_script_dir(), "smoothness_helpers.R")
if (!file.exists(helper_path)) {
  stop("Cannot locate smoothness_helpers.R (expected at ", helper_path, ")")
}
source(helper_path)
have_acf_cpp <- exists("acf_estimate_cpp")

load_acf_cpp <- function() {
  if (have_acf_cpp) return(TRUE)
  if (!requireNamespace("Rcpp", quietly = TRUE)) return(FALSE)
  cpp_path <- file.path(get_script_dir(), "acf_rcpp.cpp")
  if (!file.exists(cpp_path)) {
    alt <- file.path(get_script_dir(), "src", "R_scripts", "acf_rcpp.cpp")
    if (file.exists(alt)) cpp_path <- alt
  }
  if (!file.exists(cpp_path)) return(FALSE)
  ok <- tryCatch({
    Rcpp::sourceCpp(cpp_path)
    TRUE
  }, error = function(e) {
    warning("Failed to compile ACF Rcpp helpers: ", conditionMessage(e))
    FALSE
  })
  have_acf_cpp <<- ok && exists("acf_estimate_cpp")
  have_acf_cpp
}

pracma_fftn <- NULL
if (have_pracma) {
  ns <- asNamespace("pracma")
  if (exists("fftn", envir = ns, inherits = FALSE)) {
    pracma_fftn <- get("fftn", envir = ns)
  }
}

method_choices <- c("classic", "acf", "psd")
smoother_choices <- c("gaussian", "susan")
# Calibration regressions stratified by both the smoothing tool (Gaussian via
# 3dBlurInMask / 3dmerge vs. SUSAN) and whether the blur respected the analysis
# mask. Classic uses linear fits, PSD relies on quadratics to capture curvature,
# and ACF currently reuses the Gaussian coefficients (no SUSAN-specific data yet).
calibration_coeffs <- list(
  gaussian = list(
    classic = list(
      mask   = list(type = "linear", coeffs = c(-2.942579, 1.198781)),
      nomask = list(type = "linear", coeffs = c(-2.141319, 1.143082))
    ),
    acf = list(
      mask   = list(type = "linear", coeffs = c(-4.668769, 1.922866)),
      nomask = list(type = "linear", coeffs = c(-3.615025, 1.872935))
    ),
    psd = list(
      mask   = list(type = "poly", coeffs = c(-0.876887, 0.436634, -0.00816034)),
      nomask = list(type = "poly", coeffs = c(-0.882800, 0.529972, -0.00903182))
    )
  ),
  susan = list(
    classic = list(
      mask   = list(type = "poly", coeffs = c(-3.6270403, 1.4369376, -0.03108286)),
      nomask = list(type = "poly", coeffs = c(-3.6270403, 1.4369376, -0.03108286))
    ),
    acf = list(
      mask   = list(type = "poly", coeffs = c(-5.410268, 2.042638, -0.01314094)),
      nomask = list(type = "poly", coeffs = c(-5.410268, 2.042638, -0.01314094))
    ),
    psd = list(
      mask   = list(type = "poly", coeffs = c(-0.680609, 0.3994575, -0.00837159)),
      nomask = list(type = "poly", coeffs = c(-0.680609, 0.3994575, -0.00837159))
    )
  )
)

# Evaluate the calibration regression for a requested kernel FWHM (in mm)
predict_calibration <- function(model, kernel) {
  coeffs <- model$coeffs
  if (model$type == "linear") {
    return(coeffs[1] + coeffs[2] * kernel)
  } else if (model$type == "poly") {
    powers <- 0:(length(coeffs) - 1)
    return(sum(coeffs * kernel^powers))
  } else {
    stop("Unknown calibration model type: ", model$type)
  }
}

# Pick the correct calibration entry (mask vs. nomask) for the requested method
select_calibration <- function(smoother, method, used_mask) {
  smooth_entry <- calibration_coeffs[[smoother]]
  if (is.null(smooth_entry)) {
    die("No calibration table for smoother '%s'", smoother)
  }
  entry <- smooth_entry[[method]]
  if (is.null(entry)) die("No calibration coefficients for method '%s'", method)
  key <- if (used_mask) "mask" else "nomask"
  model <- entry[[key]]
  if (is.null(model)) {
    others <- entry[setdiff(names(entry), key)]
    if (!length(others)) {
      die("Calibration entry for method '%s' is malformed.", method)
    }
    warning(sprintf("Calibration '%s' missing for method %s; using fallback.",
                    key, method))
    return(others[[1]])
  }
  model
}

usage <- function() {
  cat(paste0(
    "Usage:\n",
    "  Single dataset:\n",
    "    3dSmoothnessChange.R -input resid.nii.gz -kernel_fwhm MM [-method classic|acf|psd]\n\n",
    "  Pre/Post comparison:\n",
    "    3dSmoothnessChange.R -pre pre.nii.gz -post post.nii.gz -kernel_fwhm MM [options]\n\n",
    "Options:\n",
    "  -input FILE        Residual dataset for single-mode prediction.\n",
    "  -pre/ -post FILE   Pre/post datasets for comparison mode.\n",
    "  -mask FILE         Optional 3D mask (otherwise uses -automask).\n",
    "  -automask          Build mask from nonzero finite voxels (default on).\n",
    "  -no_automask       Disable automask; mask must be supplied.\n",
    "  -kernel_fwhm MM    Gaussian kernel FWHM (required for -pre/-post runs).\n",
    "  -kernel_sigma MM   Kernel sigma; overrides -kernel_fwhm.\n",
    "  -method NAME       classic (default), acf, or psd.\n",
    "  -smoother NAME     gaussian (default) or susan (selects calibration family).\n",
    "                     (ACF mode shells out to 3dFWHMx by default; ensure it is in $PATH.)\n",
    "  -acf_radius MM     ACF search radius (default 20 mm).\n",
    "  -use_internal_acf  Use the built-in R ACF estimator (default calls 3dFWHMx).\n",
    "  -match_tol MM      Tolerance for Δ comparisons (default 0.5; Inf disables).\n",
    "  -polydeg N         Voxelwise polynomial detrend order (default 3).\n",
    "  -no_demean         Skip voxelwise demeaning (use with caution).\n",
    "  -trim_frac F       Tail-trimming fraction for PSD weighting (default 0.02).\n",
    "  -no_tail_weight    Give each volume equal weight (skip ex-Gaussian weighting).\n",
    "  -max_volumes N     Use only first N time points (default all).\n",
    "  -peraxis           Report PSD per-axis gradients (PSD method only).\n",
    "  -no_unif           Skip MAD normalization (classic/ACF only).\n",
    "  -used_blur_mask    Indicate blurring was performed inside the mask (mask-based calibration).\n",
    "  -out FILE          Write tab-delimited summary to FILE.\n",
    "  -exgauss_out FILE  Write ex-Gaussian weights (PSD mode only).\n",
    "  -quiet             Suppress progress messages.\n"
  ))
}

die <- function(...) {
  cat("ERROR:", sprintf(...), "\n")
  quit(status = 1, save = "no")
}

msg <- function(..., quiet = FALSE) if (!quiet) cat(..., "\n")

volume_limit <- function(nt, max_volumes) {
  if (!is.finite(max_volumes) || max_volumes <= 0) return(nt)
  max(1L, min(nt, as.integer(max_volumes)))
}

afni_limit_path <- function(path, total_volumes, used_volumes) {
  if (is.null(path) || !nzchar(path)) return(path)
  if (!is.finite(total_volumes) || total_volumes <= 1L) return(path)
  if (!is.finite(used_volumes) || used_volumes <= 0) return(path)
  if (used_volumes >= total_volumes) return(path)
  paste0(path, "[0..", used_volumes - 1L, "]")
}

read_nifti <- function(path, max_volumes = Inf, label = NULL, quiet = FALSE,
                       load_data = TRUE) {
  if (is.null(path) || !nzchar(path)) {
    die("Dataset path is missing.")
  }
  if (!file.exists(path)) {
    die("Dataset '%s' not found.", path)
  }
  if (is.null(label)) label <- basename(path)
  hdr <- RNifti::niftiHeader(path)
  pix <- as.numeric(hdr$pixdim[2:4])
  pix[!is.finite(pix) | pix <= 0] <- 1
  dim_vec <- as.integer(hdr$dim)
  ndim <- dim_vec[1]
  ndim_use <- max(3L, min(ndim, length(dim_vec) - 1L))
  dims <- as.integer(dim_vec[seq_len(ndim_use) + 1L])
  if (length(dims) < 3L) {
    die("Dataset '%s' has invalid dimensions.", path)
  }
  dims_spatial <- dims[1:3]
  n_vox <- prod(dims_spatial)
  nt_total <- if (length(dims) >= 4L) dims[4] else 1L
  nt_use <- if (length(dims) >= 4L) volume_limit(nt_total, max_volumes) else 1L
  if (length(dims) >= 4L && nt_use < nt_total) {
    msg(sprintf("[%s] Limiting analysis to first %d/%d time points.",
                label, nt_use, nt_total),
        quiet = quiet)
  }
  vols <- NULL
  data <- NULL
  if (load_data) {
    if (length(dims) >= 4L && nt_total > 1L) {
      vols <- seq_len(nt_use)
    }
    raw <- if (is.null(vols)) RNifti::readNifti(path) else RNifti::readNifti(path, volumes = vols)
    raw <- ensure_4d(raw)
    dims_raw <- dim(raw)
    if (!all(dims_raw[1:3] == dims_spatial)) {
      die("Dataset '%s' spatial dims mismatch header.", path)
    }
    nt_use <- dims_raw[4]
    data <- matrix(as.numeric(raw), nrow = n_vox, ncol = nt_use)
  }
  list(
    data = data,
    pixdim = pix,
    total_volumes = nt_total,
    used_volumes = if (length(dims) >= 4L) nt_use else 1L,
    spatial_dim = dims_spatial,
    dims = dims,
    n_voxels = n_vox
  )
}

parse_args <- function(argv) {
  opts <- list(
    input = NULL,
    pre = NULL,
    post = NULL,
    mask = NULL,
    automask = TRUE,
    kernel_fwhm = NA_real_,
    kernel_sigma = NA_real_,
    method = "classic",
    smoother = "gaussian",
    acf_radius = 20,
    match_tol = 0.5,
    polydeg = 3L,
    demean = TRUE,
    trim_frac = 0.02,
    tail_weight = TRUE,
    max_volumes = Inf,
    peraxis = FALSE,
    unif = TRUE,
    used_blur_mask = FALSE,
    use_internal_acf = FALSE,
    out = NULL,
    exgauss_out = NULL,
    quiet = FALSE
  )
  i <- 1L
  while (i <= length(argv)) {
    key <- argv[i]
    val <- if (i < length(argv)) argv[i + 1L] else NA
    if (key %in% c("-h", "-help", "--help")) {
      usage(); quit(save = "no")
    } else if (key %in% c("-input", "-dset")) {
      opts$input <- val; i <- i + 1L
    } else if (key == "-pre") {
      opts$pre <- val; i <- i + 1L
    } else if (key == "-post") {
      opts$post <- val; i <- i + 1L
    } else if (key == "-mask") {
      opts$mask <- val; i <- i + 1L
    } else if (key == "-automask") {
      opts$automask <- TRUE
    } else if (key == "-no_automask") {
      opts$automask <- FALSE
    } else if (key == "-kernel_fwhm") {
      opts$kernel_fwhm <- as.numeric(val); i <- i + 1L
    } else if (key == "-kernel_sigma") {
      opts$kernel_sigma <- as.numeric(val); i <- i + 1L
    } else if (key == "-method") {
      opts$method <- tolower(val); i <- i + 1L
    } else if (key == "-smoother") {
      opts$smoother <- tolower(val); i <- i + 1L
    } else if (key == "-acf_radius") {
      opts$acf_radius <- as.numeric(val); i <- i + 1L
    } else if (key == "-match_tol") {
      opts$match_tol <- as.numeric(val); i <- i + 1L
    } else if (key == "-polydeg") {
      opts$polydeg <- as.integer(val); i <- i + 1L
    } else if (key == "-no_demean") {
      opts$demean <- FALSE
    } else if (key == "-trim_frac") {
      opts$trim_frac <- as.numeric(val); i <- i + 1L
    } else if (key == "-no_tail_weight") {
      opts$tail_weight <- FALSE
    } else if (key == "-max_volumes") {
      opts$max_volumes <- as.integer(val); i <- i + 1L
    } else if (key == "-peraxis") {
      opts$peraxis <- TRUE
    } else if (key == "-no_unif") {
      opts$unif <- FALSE
    } else if (key == "-used_blur_mask") {
      opts$used_blur_mask <- TRUE
    } else if (key == "-no_used_blur_mask") {
      opts$used_blur_mask <- FALSE
    } else if (key == "-use_internal_acf") {
      opts$use_internal_acf <- TRUE
    } else if (key == "-no_use_internal_acf") {
      opts$use_internal_acf <- FALSE
    } else if (key == "-out") {
      opts$out <- val; i <- i + 1L
    } else if (key == "-exgauss_out") {
      opts$exgauss_out <- val; i <- i + 1L
    } else if (key == "-quiet") {
      opts$quiet <- TRUE
    } else {
      die("Unknown option '%s'", key)
    }
    i <- i + 1L
  }
  opts
}

# Build an analysis mask (either supplied, or via automask fallback)
build_mask <- function(mask_path, target_dim, automask, data_mat = NULL) {
  if (!is.null(mask_path)) {
    m <- RNifti::readNifti(mask_path)
    mask_dims <- dim(m)
    if (length(mask_dims) == 4L) {
      m <- apply(m != 0 & is.finite(m), c(1, 2, 3), any)
    } else {
      m <- (m != 0) & is.finite(m)
    }
    if (!all(dim(m)[1:3] == target_dim[1:3])) {
      die("Mask dims %s do not match data dims %s",
          paste(dim(m)[1:3], collapse = "x"),
          paste(target_dim[1:3], collapse = "x"))
    }
    return(m)
  }
  if (!automask) die("Mask is required when -no_automask is used.")
  if (is.null(data_mat)) {
    die("Automask requested but data are unavailable to derive it.")
  }
  if (nrow(data_mat) != prod(target_dim)) {
    die("Automask data rows (%d) do not match spatial dimensions %s",
        nrow(data_mat), paste(target_dim, collapse = "x"))
  }
  mask_vec <- apply(is.finite(data_mat) & data_mat != 0, 1L, any)
  array(mask_vec, dim = target_dim)
}

# Guarantee 4D shape for all datasets (adds a singleton time axis if needed)
ensure_4d <- function(arr) {
  dims <- dim(arr)
  if (is.null(dims)) die("Dataset lacks dimension metadata.")
  if (length(dims) == 3L) {
    dim(arr) <- c(dims, 1L)
  } else if (length(dims) != 4L) {
    die("Dataset must be 3D or 4D; got dims %s", paste(dims, collapse = "x"))
  }
  arr
}

fftfreq <- function(n, delta) {
  if (n %% 2 == 0) {
    pos <- 0:(n/2 - 1)
    neg <- rev(-seq_len(n/2))
  } else {
    pos <- 0:((n - 1)/2)
    neg <- rev(-seq_len((n - 1)/2))
  }
  2 * pi * c(pos, neg) / (n * delta)
}

fftn3d <- function(vol) {
  if (!is.null(pracma_fftn)) return(pracma_fftn(vol))
  arr <- vol
  dims <- dim(arr)
  for (axis in seq_along(dims)) {
    perm <- c(axis, setdiff(seq_along(dims), axis))
    tmp <- aperm(arr, perm)
    dim_tmp <- dim(tmp)
    mat <- matrix(tmp, nrow = dims[axis])
    mat <- stats::mvfft(mat)
    tmp <- array(mat, dim = dim_tmp)
    arr <- aperm(tmp, order(perm))
  }
  arr
}

fit_exgauss <- function(vals, trim_frac = 0.02) {
  v <- vals[is.finite(vals)]
  n <- length(v)
  if (n < 10) {
    return(list(mu = NA, sigma = NA, tau = NA,
                mean = if (n) mean(v) else NA,
                var = if (n > 1) var(v) else NA,
                skew = NA))
  }
  if (trim_frac > 0 && trim_frac < 0.5) {
    qs <- stats::quantile(v, probs = c(trim_frac, 1 - trim_frac), names = FALSE)
    v <- v[v >= qs[1] & v <= qs[2]]
  }
  m <- mean(v); s2 <- stats::var(v)
  if (!is.finite(s2) || s2 <= 0) {
    return(list(mu = m, sigma = 0, tau = 0, mean = m, var = s2, skew = NA))
  }
  centered <- v - m
  skew <- mean(centered^3) / (s2^(3/2))
  if (!is.finite(skew)) skew <- 0
  skew <- max(skew, 0)
  tau <- if (skew > 0) (0.5 * skew)^(1/3) * sqrt(s2) else 0
  sigma2 <- max(s2 - tau^2, 1e-9)
  list(mu = m - tau, sigma = sqrt(sigma2), tau = tau,
       mean = m, var = s2, skew = skew)
}

volume_weight <- function(fit, enable = TRUE) {
  if (!enable) return(1)
  if (is.na(fit$tau) || is.na(fit$sigma)) return(1)
  denom <- max(fit$sigma, 1e-6)
  1 / (1 + fit$tau / denom)
}

# Construct orthonormal polynomial trends (optionally including mean column)
build_trend_matrix <- function(nt, degree, demean) {
  cols <- list()
  tvec <- seq_len(nt)
  if (demean) cols[[length(cols) + 1L]] <- rep(1, nt)
  if (degree >= 1) {
    rng <- range(tvec)
    scaled <- if (diff(rng) > 0) {
      2 * (tvec - mean(tvec)) / diff(rng)
    } else {
      rep(0, nt)
    }
    poly_mat <- stats::poly(scaled, degree = degree, raw = FALSE, simple = FALSE)
    for (p in seq_len(degree)) {
      cols[[length(cols) + 1L]] <- poly_mat[, p]
    }
  }
  if (!length(cols)) return(NULL)
  do.call(cbind, cols)
}

# Apply per-voxel detrending using the orthonormal basis above
detrend_voxels <- function(mat, degree, demean = TRUE) {
  nt <- ncol(mat)
  if (nt == 0) return(mat)
  X <- build_trend_matrix(nt, degree, demean)
  if (is.null(X)) return(mat)
  XtX <- crossprod(X)
  coeff <- solve(XtX, crossprod(X, t(mat)))
  fitted <- t(X %*% coeff)
  mat - fitted
}

# Detrend a voxel-by-time matrix
detrend_matrix <- function(mat, degree, demean) {
  detrend_voxels(mat, degree = degree, demean = demean)
}

# Classic/ACF normalization: scale voxels by temporal MAD to equalize variance
mad_scale_matrix <- function(mat, mask_vec) {
  mad_vals <- apply(mat, 1L, stats::mad, constant = 1.4826, na.rm = TRUE)
  mad_vals[!is.finite(mad_vals) | mad_vals <= 1e-6] <- 1
  if (!is.null(mask_vec)) mad_vals[!mask_vec] <- 1
  mat / mad_vals
}

# Core PSD pipeline: detrend, FFT per time point, accumulate gradient energy,
# and derive pre/post smoothness plus optional per-axis outputs.
run_estimation <- function(arr, pixdim, mask, kernel_sigma, opts,
                           dims_spat = NULL, label = "input",
                           save_fits = FALSE, return_data = FALSE) {
  mat <- NULL
  if (is.matrix(arr)) {
    if (is.null(dims_spat)) {
      die("[%s] Spatial dimensions are required when passing matrix data.", label)
    }
    dims <- c(dims_spat, ncol(arr))
    mat <- arr
  } else {
    arr <- ensure_4d(arr)
    dims <- dim(arr)
    dims_spat <- dims[1:3]
    mat <- matrix(as.numeric(arr), nrow = prod(dims_spat), ncol = dims[4])
  }
  n_vox <- prod(dims_spat)
  nt <- ncol(mat)
  if (nt < 1) die("[%s] Dataset has zero time points.", label)
  mask_vec <- as.vector(mask)
  if (length(mask_vec) != n_vox) die("[%s] Mask mismatch.", label)
  if (!any(mask_vec)) die("[%s] Mask is empty.", label)

  msg(sprintf("[%s] Dataset dims: %d x %d x %d x %d",
              label, dims_spat[1], dims_spat[2], dims_spat[3], nt),
      quiet = opts$quiet)
  msg(sprintf("[%s] Mask voxels : %d", label, sum(mask_vec)),
      quiet = opts$quiet)

  mat[!mask_vec, ] <- 0
  if (opts$demean || opts$polydeg > 0) {
    mat[mask_vec, ] <- detrend_voxels(mat[mask_vec, , drop = FALSE],
                                      degree = opts$polydeg,
                                      demean = opts$demean)
  }

  nt_use <- volume_limit(nt, opts$max_volumes)
  mat_use <- mat[, seq_len(nt_use), drop = FALSE]
  msg(sprintf("[%s] Using first %d/%d time points.", label, nt_use, nt),
      quiet = opts$quiet)

  kx <- fftfreq(dims_spat[1], pixdim[1])
  ky <- fftfreq(dims_spat[2], pixdim[2])
  kz <- fftfreq(dims_spat[3], pixdim[3])
  kx2 <- array(0, dim = dims_spat); ky2 <- array(0, dim = dims_spat); kz2 <- array(0, dim = dims_spat)
  for (ix in seq_along(kx)) kx2[ix, , ] <- kx[ix]^2
  for (iy in seq_along(ky)) ky2[, iy, ] <- ky[iy]^2
  for (iz in seq_along(kz)) kz2[, , iz] <- kz[iz]^2
  k2 <- kx2 + ky2 + kz2
  kernel_transfer <- exp(- (kernel_sigma^2) * k2)

  pre_energy <- post_energy <- pre_grad <- post_grad <- 0
  pre_grad_axes <- post_grad_axes <- c(0, 0, 0)
  weight_sum <- 0
  fits <- if (save_fits) vector("list", nt_use) else NULL

  for (t in seq_len(nt_use)) {
    vol <- array(mat_use[, t], dim = dims_spat)
    vol[!mask] <- 0
    fit <- fit_exgauss(vol[mask], trim_frac = opts$trim_frac)
    w <- volume_weight(fit, enable = opts$tail_weight)
    fft_vol <- fftn3d(vol)
    pow <- Mod(fft_vol)^2
    pre_energy <- pre_energy + w * sum(pow)
    pg <- w * sum(pow * k2)
    pre_grad <- pre_grad + pg
    pow_k <- pow * kernel_transfer
    post_energy <- post_energy + w * sum(pow_k)
    post_grad <- post_grad + w * sum(pow_k * k2)
    if (opts$peraxis) {
      pre_grad_axes <- pre_grad_axes + w * c(sum(pow * kx2), sum(pow * ky2), sum(pow * kz2))
      post_grad_axes <- post_grad_axes + w * c(sum(pow_k * kx2),
                                              sum(pow_k * ky2),
                                              sum(pow_k * kz2))
    }
    weight_sum <- weight_sum + w
    if (save_fits) fits[[t]] <- c(volume = t, fit$mu, fit$sigma, fit$tau,
                                  fit$mean, fit$var, fit$skew, weight = w)
    if (!opts$quiet && (t %% 10 == 0 || t == nt_use)) {
      msg(sprintf("[%s] Processed volume %d/%d", label, t, nt_use),
          quiet = opts$quiet)
    }
  }

  if (weight_sum <= 0 || pre_grad <= 0 || post_grad <= 0) {
    die("[%s] Invalid gradient energy; check data or mask.", label)
  }

  pre_energy <- pre_energy / weight_sum
  post_energy <- post_energy / weight_sum
  pre_grad <- pre_grad / weight_sum
  post_grad <- post_grad / weight_sum

  fwhm_pre <- sqrt(4 * log(2) * pre_energy / pre_grad)
  fwhm_post <- sqrt(4 * log(2) * post_energy / post_grad)
  delta <- fwhm_post - fwhm_pre
  kernel_equiv <- sqrt(max(0, fwhm_post^2 - fwhm_pre^2))

  fits_matrix <- NULL
  if (save_fits && length(fits)) {
    fits_matrix <- do.call(rbind, fits)
    colnames(fits_matrix) <- c("volume", "mu", "sigma", "tau",
                               "mean", "var", "skew", "weight")
  }

  peraxis <- NULL
  if (opts$peraxis) {
    pre_grad_axes <- pre_grad_axes / weight_sum
    post_grad_axes <- post_grad_axes / weight_sum
    fwhm_axes_pre <- fwhm_axes_post <- rep(NA_real_, 3)
    valid_pre <- pre_grad_axes > 0
    valid_post <- post_grad_axes > 0
    fwhm_axes_pre[valid_pre] <- sqrt(4 * log(2) * pre_energy / pre_grad_axes[valid_pre])
    fwhm_axes_post[valid_post] <- sqrt(4 * log(2) * post_energy / post_grad_axes[valid_post])
    peraxis <- list(
      pre = fwhm_axes_pre,
      post = fwhm_axes_post,
      delta = fwhm_axes_post - fwhm_axes_pre,
      geom_pre = geom_mean_safe(fwhm_axes_pre),
      geom_post = geom_mean_safe(fwhm_axes_post),
      geom_delta = geom_mean_safe(fwhm_axes_post) - geom_mean_safe(fwhm_axes_pre)
    )
  }

  processed_arr <- if (return_data) array(mat_use, dim = c(dims_spat, nt_use)) else NULL

  list(
    fwhm_pre = fwhm_pre,
    fwhm_post = fwhm_post,
    delta = delta,
    kernel_equiv = kernel_equiv,
    weight_sum = weight_sum,
    volumes_used = nt_use,
    fits = fits_matrix,
    peraxis = peraxis,
    processed = processed_arr
  )
}

write_exgauss <- function(mat, path) {
  if (is.null(mat) || is.null(path)) return()
  utils::write.table(mat, file = path, sep = ",",
                     row.names = FALSE, col.names = TRUE, quote = FALSE)
}

# Classic/ACF preprocessing entry point (shared by both methods)
prepare_classic_data <- function(mat, mask, opts) {
  proc <- detrend_matrix(mat, opts$polydeg, opts$demean)
  mask_vec <- if (is.null(mask)) NULL else as.vector(mask)
  if (opts$unif) proc <- mad_scale_matrix(proc, mask_vec)
  proc
}

# Wrapper returning the geometric-mean FWHM from the classic neighbour differences
measure_classic <- function(mat, mask, vox_mm, dims_spat) {
  arr4d <- array(mat, dim = c(dims_spat, ncol(mat)))
  res <- estimate_classic_fwhm(arr4d, mask, vox_mm)
  list(value = res$geom, details = res)
}

# Wrapper returning the single-parameter Gaussian-equivalent FWHM from the ACF fit
measure_acf_internal <- function(mat, mask, vox_mm, opts, dims_spat, use_cpp = FALSE) {
  arr4d <- array(mat, dim = c(dims_spat, ncol(mat)))
  res <- estimate_acf_fwhm(arr4d, mask, vox_mm,
                           radius_mm = opts$acf_radius,
                           use_cpp = use_cpp)
  if (!isTRUE(res$ok)) {
    msg("ACF estimator failed to converge; returning NA.",
        quiet = opts$quiet)
  }
  list(value = res$fwhm, details = res)
}

parse_fwhmx_stdout <- function(lines) {
  if (!length(lines)) return(NULL)
  for (idx in seq(length(lines), 1L)) {
    line <- trimws(lines[idx])
    if (!nzchar(line)) next
    tokens <- strsplit(line, "[[:space:]]+")[[1]]
    if (!length(tokens)) next
    nums <- suppressWarnings(as.numeric(tokens))
    if (length(nums) >= 4 && all(is.finite(nums[1:4]))) {
      return(list(fxyz = nums[1:3], fwhm = nums[4]))
    }
  }
  NULL
}

run_3dfwhmx_acf <- function(dset_path, opts, label = "input") {
  bin <- Sys.which("3dFWHMx")
  if (!nzchar(bin)) {
    die("3dFWHMx executable not found in PATH (needed for ACF external mode).")
  }
  if (is.null(dset_path) || !file.exists(dset_path)) {
    die("[%s] Dataset '%s' not found for 3dFWHMx.",
        label, if (is.null(dset_path)) "<NULL>" else dset_path)
  }
  args <- character()
  if (!is.null(opts$mask)) {
    args <- c(args, "-mask", opts$mask)
  } else if (opts$automask) {
    args <- c(args, "-automask")
  }
  detrend_order <- max(0L, opts$polydeg)
  args <- c(args, "-detrend", as.character(detrend_order))
  args <- c(args, "-acf", "NULL", "-overwrite", "-input", dset_path)
  res <- tryCatch(
    system2(bin, args, stdout = TRUE, stderr = TRUE),
    error = function(e) {
      structure(character(), status = 1L, stderr = e$message)
    }
  )
  out_lines <- res
  err_lines <- attr(res, "stderr")
  if (is.null(err_lines)) err_lines <- character()
  exit_code <- attr(res, "status")
  if (is.null(exit_code)) exit_code <- 0L
  if (!opts$quiet && length(err_lines)) {
    cat(paste(err_lines, collapse = "\n"), "\n")
  }
  if (exit_code != 0) {
    die("[%s] 3dFWHMx failed (exit %d). Output:\n%s",
        label, exit_code, paste(c(out_lines, err_lines), collapse = "\n"))
  }
  parsed <- parse_fwhmx_stdout(out_lines)
  if (is.null(parsed)) {
    die("[%s] Could not parse 3dFWHMx output.", label)
  }
  list(fwhm = parsed$fwhm, fxyz = parsed$fxyz,
       stdout = out_lines, stderr = err_lines)
}

measure_acf_external <- function(dset_path, opts, label = "input") {
  res <- run_3dfwhmx_acf(dset_path, opts, label = label)
  list(value = res$fwhm, details = res)
}

main <- function() {
  argv <- commandArgs(trailingOnly = TRUE)
  if (!length(argv)) {
    usage()
    quit(save = "no")
  }
  opts <- parse_args(argv)
  if (!(opts$method %in% method_choices)) {
    die("Invalid -method '%s' (choose %s)", opts$method,
        paste(method_choices, collapse = ", "))
  }
  if (!(opts$smoother %in% smoother_choices)) {
    die("Invalid -smoother '%s' (choose %s)", opts$smoother,
        paste(smoother_choices, collapse = ", "))
  }
  if (opts$polydeg < 0) die("-polydeg must be >= 0.")
  if (!is.finite(opts$acf_radius) || opts$acf_radius <= 0)
    die("-acf_radius must be > 0.")

  kernel_sigma <- NA_real_
  if (!is.na(opts$kernel_sigma)) {
    if (opts$kernel_sigma <= 0) die("Kernel sigma must be > 0.")
    kernel_sigma <- opts$kernel_sigma
  } else if (!is.na(opts$kernel_fwhm)) {
    if (opts$kernel_fwhm <= 0) die("Kernel FWHM must be > 0.")
    kernel_sigma <- opts$kernel_fwhm / (2 * sqrt(2 * log(2)))
  }

  normalize_opt_path <- function(path) {
    if (is.null(path)) return(NULL)
    tryCatch(normalizePath(path, mustWork = FALSE),
             error = function(e) path)
  }
  opts$input <- normalize_opt_path(opts$input)
  opts$pre <- normalize_opt_path(opts$pre)
  opts$post <- normalize_opt_path(opts$post)
  opts$mask <- normalize_opt_path(opts$mask)

  if (is.null(opts$pre) && !is.null(opts$input)) {
    opts$pre <- opts$input
  }
  compare_mode <- !is.null(opts$pre) && !is.null(opts$post)
  if (!compare_mode && is.null(opts$input)) {
    die("Need -input or -pre/-post.")
  }
  if (compare_mode && is.na(kernel_sigma)) {
    die("-kernel_fwhm or -kernel_sigma is required when using -pre/-post.")
  }

  method_label <- toupper(opts$method)
  cal_model <- select_calibration(opts$smoother, opts$method, opts$used_blur_mask)
  use_cpp_acf <- FALSE
  if (opts$method == "acf" && opts$use_internal_acf) {
    use_cpp_acf <- load_acf_cpp()
    if (!use_cpp_acf && !opts$quiet) {
      msg("Falling back to pure R ACF estimator (Rcpp helpers unavailable).",
          quiet = opts$quiet)
    } else if (use_cpp_acf && !opts$quiet) {
      msg("Using Rcpp-accelerated ACF estimator.", quiet = opts$quiet)
    }
  }

  need_psd_prediction <- !is.na(kernel_sigma) && kernel_sigma > 0
  need_data_single <- (opts$method != "acf") || opts$use_internal_acf || need_psd_prediction

  if (!compare_mode) {
    nif <- read_nifti(opts$input, opts$max_volumes, label = "input",
                      quiet = opts$quiet, load_data = need_data_single)
    data_mat <- mask_arr <- NULL
    if (need_data_single) {
      data_mat <- nif$data
      mask_arr <- build_mask(opts$mask, nif$spatial_dim, opts$automask, data_mat)
    }
    psd_prediction <- NULL
    if (opts$method == "psd") {
      psd_sigma <- if (is.na(kernel_sigma)) 0 else kernel_sigma
      res <- run_estimation(data_mat, nif$pixdim, mask_arr, psd_sigma, opts,
                            dims_spat = nif$spatial_dim,
                            label = "input", save_fits = !is.null(opts$exgauss_out))
      value <- res$fwhm_pre
      if (!is.null(opts$exgauss_out) && !is.null(res$fits)) {
        write_exgauss(res$fits, opts$exgauss_out)
      }
      if (psd_sigma > 0) psd_prediction <- res
    } else if (opts$method == "acf" && !opts$use_internal_acf) {
      dset_path <- afni_limit_path(opts$input, nif$total_volumes, nif$used_volumes)
      meas <- measure_acf_external(dset_path, opts, label = "input")
      value <- meas$value
    } else {
      proc <- prepare_classic_data(data_mat, mask_arr, opts)
      meas <- if (opts$method == "classic")
        measure_classic(proc, mask_arr, nif$pixdim, nif$spatial_dim)
      else
        measure_acf_internal(proc, mask_arr, nif$pixdim, opts,
                             dims_spat = nif$spatial_dim, use_cpp = use_cpp_acf)
      value <- meas$value
    }
    msg(sprintf("%s measured FWHM             : %.4f mm", method_label, value),
        quiet = opts$quiet)
    if (!is.null(opts$out)) {
      utils::write.table(data.frame(method = opts$method, fwhm_mm = value),
                         file = opts$out, quote = FALSE,
                         row.names = FALSE, sep = "\t")
    }
    if (need_psd_prediction) {
      if (is.null(psd_prediction)) {
        psd_prediction <- run_estimation(data_mat, nif$pixdim, mask_arr,
                                         kernel_sigma, opts,
                                         dims_spat = nif$spatial_dim,
                                         label = "input_psd", save_fits = FALSE)
      }
      kernel_mm <- 2 * sqrt(2 * log(2)) * kernel_sigma
      msg(sprintf("PSD predicted FWHM (kernel %.2f mm): %.4f -> %.4f mm",
                  kernel_mm, psd_prediction$fwhm_pre, psd_prediction$fwhm_post),
          quiet = opts$quiet)
      msg(sprintf("PSD predicted ΔFWHM              : %.4f mm",
                  psd_prediction$delta),
          quiet = opts$quiet)
    }
    return(invisible())
  }

  scenario_label <- if (opts$used_blur_mask) "masked" else "unmasked"
  msg(sprintf("Using %s calibration (%s smoother, %s blurring).",
              method_label, toupper(opts$smoother), scenario_label),
      quiet = opts$quiet)
  need_data_compare <- (opts$method != "acf") || opts$use_internal_acf
  pre_nif <- read_nifti(opts$pre, opts$max_volumes, label = "pre",
                        quiet = opts$quiet, load_data = need_data_compare)
  post_nif <- read_nifti(opts$post, opts$max_volumes, label = "post",
                         quiet = opts$quiet, load_data = need_data_compare)
  if (any(abs(post_nif$pixdim[1:3] - pre_nif$pixdim[1:3]) > 1e-6)) {
    die("Pre/post voxel sizes differ.")
  }
  if (!all(post_nif$spatial_dim == pre_nif$spatial_dim)) {
    die("Pre/post spatial dimensions differ.")
  }
  pre_mat <- post_mat <- mask_arr <- NULL
  if (need_data_compare) {
    pre_mat <- pre_nif$data
    post_mat <- post_nif$data
    mask_arr <- build_mask(opts$mask, pre_nif$spatial_dim, opts$automask, pre_mat)
    if (!all(dim(mask_arr) == pre_nif$spatial_dim)) {
      die("Mask dims %s do not match data dims %s",
          paste(dim(mask_arr), collapse = "x"),
          paste(pre_nif$spatial_dim, collapse = "x"))
    }
    if (!all(dim(mask_arr) == post_nif$spatial_dim)) {
      die("Post dataset dims %s do not match pre/mask dims %s",
          paste(post_nif$spatial_dim, collapse = "x"),
          paste(dim(mask_arr), collapse = "x"))
    }
  }

  if (opts$method == "psd") {
    pre_meas <- run_estimation(pre_mat, pre_nif$pixdim, mask_arr, 0, opts,
                               dims_spat = pre_nif$spatial_dim,
                               label = "pre", save_fits = !is.null(opts$exgauss_out))
    post_meas <- run_estimation(post_mat, post_nif$pixdim, mask_arr, 0, opts,
                                dims_spat = post_nif$spatial_dim,
                                label = "post", save_fits = !is.null(opts$exgauss_out))
    if (!is.null(opts$exgauss_out)) {
      write_exgauss(pre_meas$fits, paste0(opts$exgauss_out, ".pre.csv"))
      write_exgauss(post_meas$fits, paste0(opts$exgauss_out, ".post.csv"))
    }
    measured_pre <- pre_meas$fwhm_pre
    measured_post <- post_meas$fwhm_pre
  } else if (opts$method == "acf" && !opts$use_internal_acf) {
    pre_path <- afni_limit_path(opts$pre, pre_nif$total_volumes, pre_nif$used_volumes)
    post_path <- afni_limit_path(opts$post, post_nif$total_volumes, post_nif$used_volumes)
    meas_pre <- measure_acf_external(pre_path, opts, label = "pre")
    meas_post <- measure_acf_external(post_path, opts, label = "post")
    measured_pre <- meas_pre$value
    measured_post <- meas_post$value
  } else {
    pre_proc <- prepare_classic_data(pre_mat, mask_arr, opts)
    post_proc <- prepare_classic_data(post_mat, mask_arr, opts)
    meas_pre <- if (opts$method == "classic")
      measure_classic(pre_proc, mask_arr, pre_nif$pixdim, pre_nif$spatial_dim)
    else
      measure_acf_internal(pre_proc, mask_arr, pre_nif$pixdim, opts,
                           dims_spat = pre_nif$spatial_dim,
                           use_cpp = use_cpp_acf)
    meas_post <- if (opts$method == "classic")
      measure_classic(post_proc, mask_arr, post_nif$pixdim, post_nif$spatial_dim)
    else
      measure_acf_internal(post_proc, mask_arr, post_nif$pixdim, opts,
                           dims_spat = post_nif$spatial_dim,
                           use_cpp = use_cpp_acf)
    measured_pre <- meas_pre$value
    measured_post <- meas_post$value
  }

  delta_observed <- measured_post - measured_pre
  kernel_requested <- 2 * sqrt(2 * log(2)) * kernel_sigma
  delta_cal_expected <- predict_calibration(cal_model, kernel_requested)
  diff_cal <- delta_observed - delta_cal_expected
  tol <- opts$match_tol
  within_cal <- if (is.finite(tol)) (abs(diff_cal) <= tol) else NA

  msg(sprintf("%s measured pre/post FWHM      : %.4f -> %.4f mm",
              method_label, measured_pre, measured_post),
      quiet = opts$quiet)
  msg(sprintf("%s observed ΔFWHM              : %.4f mm", method_label, delta_observed),
      quiet = opts$quiet)
  msg(sprintf("Calibrated expected Δ (%s)       : %.4f mm", method_label, delta_cal_expected),
      quiet = opts$quiet)
  msg(sprintf("Calibrated Δ difference          : %.4f mm", diff_cal),
      quiet = opts$quiet)

  if (opts$method == "psd" && !is.null(pre_meas$peraxis) && !is.null(post_meas$peraxis)) {
    msg(sprintf("Per-axis PSD observed Δ (x y z) : %s mm",
                paste(sprintf("%.4f", post_meas$peraxis$pre - pre_meas$peraxis$pre),
                      collapse = " ")),
        quiet = opts$quiet)
  }

  if (is.finite(tol)) {
    verdict <- if (isTRUE(within_cal)) "YES" else "NO"
    msg(sprintf("Calibrated verdict  : %s (|Δobs-Δcal| = %.4f mm; tol = %.4f mm)",
                verdict, abs(diff_cal), tol),
        quiet = opts$quiet)
  } else {
    msg(sprintf("Calibrated verdict  : tolerance disabled (Δ diff = %.4f mm)",
                diff_cal), quiet = opts$quiet)
  }

  if (!is.null(opts$out)) {
    tab <- data.frame(
      method = opts$method,
      measured_pre_mm = measured_pre,
      measured_post_mm = measured_post,
      observed_delta_mm = delta_observed,
      calibrated_expected_delta_mm = delta_cal_expected,
      calibrated_delta_difference_mm = diff_cal,
      tolerance_mm = tol,
      match_calibrated = within_cal
    )
    utils::write.table(tab, file = opts$out, quote = FALSE,
                       row.names = FALSE, sep = "\t")
  }
}

main()
