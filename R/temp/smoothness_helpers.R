#!/usr/bin/env Rscript

# Helper functions shared by 3dSmoothnessChange.R and 3dFWHMx_classic.R

#' Numerically stable sample variance with guardrails
#'
#' @param x Numeric vector.
#' @return Finite sample variance or `NA_real_` when insufficient data.
var_sample <- function(x) {
  if (length(x) < 2) return(NA_real_)
  v <- var(x)
  if (!is.finite(v) || v <= 0) NA_real_ else as.numeric(v)
}

#' Estimate axis-wise FWHM from first-difference variance
#'
#' @param vol3d Numeric array (x, y, z) representing one 3D volume.
#' @param mask3d Logical array selecting voxels to include.
#' @param vox_mm Numeric length-3 vector of voxel sizes in millimeters.
#' @return Numeric length-3 vector of FWHM per axis; -1 indicates failure.
compute_fwhm_1dif <- function(vol3d, mask3d, vox_mm) {
  stopifnot(length(dim(vol3d)) == 3L, all(dim(vol3d) == dim(mask3d)))
  nx <- dim(vol3d)[1]; ny <- dim(vol3d)[2]; nz <- dim(vol3d)[3]
  total_mask <- mask3d & is.finite(vol3d)
  nmask <- sum(total_mask)
  if (is.na(nmask) || nmask < 9) return(rep(-1, 3))
  vdat <- var_sample(as.numeric(vol3d[total_mask]))
  if (is.na(vdat) || vdat <= 0) return(rep(-1, 3))
  fx <- fy <- fz <- -1
  if (nx > 1) {
    pairs <- mask3d[1:(nx-1), , , drop = FALSE] & mask3d[2:nx, , , drop = FALSE]
    if (any(pairs)) {
      diffs <- vol3d[2:nx, , , drop = FALSE] - vol3d[1:(nx-1), , , drop = FALSE]
      vxx <- var_sample(as.numeric(diffs[pairs]))
      if (is.finite(vxx)) {
        arg <- 1 - 0.5 * (vxx / vdat)
        if (is.finite(arg) && arg > 0 && arg < 1) {
          fx <- 2.35482 * sqrt(-1 / (4 * log(arg))) * vox_mm[1]
        }
      }
    }
  }
  if (ny > 1) {
    pairs <- mask3d[ , 1:(ny-1), , drop = FALSE] & mask3d[ , 2:ny, , drop = FALSE]
    if (any(pairs)) {
      diffs <- vol3d[ , 2:ny, , drop = FALSE] - vol3d[ , 1:(ny-1), , drop = FALSE]
      vyy <- var_sample(as.numeric(diffs[pairs]))
      if (is.finite(vyy)) {
        arg <- 1 - 0.5 * (vyy / vdat)
        if (is.finite(arg) && arg > 0 && arg < 1) {
          fy <- 2.35482 * sqrt(-1 / (4 * log(arg))) * vox_mm[2]
        }
      }
    }
  }
  if (nz > 1) {
    pairs <- mask3d[ , , 1:(nz-1), drop = FALSE] & mask3d[ , , 2:nz, drop = FALSE]
    if (any(pairs)) {
      diffs <- vol3d[ , , 2:nz, drop = FALSE] - vol3d[ , , 1:(nz-1), drop = FALSE]
      vzz <- var_sample(as.numeric(diffs[pairs]))
      if (is.finite(vzz)) {
        arg <- 1 - 0.5 * (vzz / vdat)
        if (is.finite(arg) && arg > 0 && arg < 1) {
          fz <- 2.35482 * sqrt(-1 / (4 * log(arg))) * vox_mm[3]
        }
      }
    }
  }
  c(fx, fy, fz)
}

#' Pre-compute voxel offsets used to sample the ACF
#'
#' @param dx,dy,dz Physical voxel spacing along each axis (mm).
#' @param radius_mm Maximum distance for ACF evaluation.
#' @param dims Integer vector of volume dimensions (x, y, z).
#' @param is2D Flag indicating whether data are effectively 2D.
#' @return List with `offs` (matrix of integer offsets) and `rads` (distances).
build_acf_offsets <- function(dx, dy, dz, radius_mm, dims, is2D = FALSE) {
  nx <- dims[1]; ny <- dims[2]; nz <- dims[3]
  ix_max <- floor(radius_mm / dx)
  iy_max <- floor(radius_mm / dy)
  iz_max <- if (is2D) 0L else floor(radius_mm / dz)
  offs <- list()
  rads <- numeric(0)
  for (di in -ix_max:ix_max) {
    for (dj in -iy_max:iy_max) {
      for (dk in -iz_max:iz_max) {
        if (di == 0L && dj == 0L && dk == 0L) next
        if (is2D && dk != 0L) next
        r <- sqrt((di*dx)^2 + (dj*dy)^2 + (dk*dz)^2)
        if (r <= radius_mm + 1e-6) {
          offs[[length(offs)+1L]] <- c(di, dj, dk)
          rads <- c(rads, r)
        }
      }
    }
  }
  if (!length(offs)) return(list(offs = matrix(integer(0), ncol = 3), rads = numeric(0)))
  ord <- order(rads)
  offm <- do.call(rbind, offs)[ord, , drop = FALSE]
  rads <- rads[ord]
  NCLU_GOAL <- 666L; NCLU_BASE <- 111L
  n <- nrow(offm)
  if (n > NCLU_GOAL) {
    # Keep dense sampling near the origin; thin farther offsets
    base_idx <- seq_len(NCLU_BASE)
    rest <- (NCLU_BASE + 1L):n
    dp <- max(1L, round((n - NCLU_BASE) / (NCLU_GOAL - NCLU_BASE)))
    keep_rest <- rest[seq(1, length(rest), by = dp)]
    keep <- c(base_idx, keep_rest)
    offm <- offm[keep, , drop = FALSE]
    rads <- rads[keep]
  }
  list(offs = offm, rads = rads)
}

#' Empirical ACF for a single 3D volume
#'
#' @param vol3d Numeric array (x, y, z) for one volume.
#' @param mask3d Logical mask restricting valid voxels.
#' @param offs Matrix of integer offsets (columns: di, dj, dk).
#' @param rads Numeric vector of physical distances (unused, for alignment).
#' @return Numeric vector of empirical autocorrelation values per offset.
acf_for_volume <- function(vol3d, mask3d, offs, rads) {
  nx <- dim(vol3d)[1]; ny <- dim(vol3d)[2]; nz <- dim(vol3d)[3]
  m <- mask3d & is.finite(vol3d)
  nmask <- sum(m)
  if (is.na(nmask) || nmask < 9 || !any(is.finite(vol3d[m]))) return(rep(0, length(rads)))
  fbar <- mean(vol3d[m])
  fvar <- var(as.numeric(vol3d[m]))
  if (!is.finite(fvar) || fvar <= 0) return(rep(0, length(rads)))
  res <- numeric(length(rads))
  for (idx in seq_len(nrow(offs))) {
    di <- offs[idx,1]; dj <- offs[idx,2]; dk <- offs[idx,3]
    if (di >= 0) { i1 <- 1:(nx-di); i2 <- (1+di):nx } else { i1 <- (1-di):nx; i2 <- 1:(nx+di) }
    if (dj >= 0) { j1 <- 1:(ny-dj); j2 <- (1+dj):ny } else { j1 <- (1-dj):ny; j2 <- 1:(ny+dj) }
    if (dk >= 0) { k1 <- 1:(nz-dk); k2 <- (1+dk):nz } else { k1 <- (1-dk):nz; k2 <- 1:(nz+dk) }
    if (!length(i1) || !length(j1) || !length(k1)) { res[idx] <- 0; next }
    a <- vol3d[i1, j1, k1, drop = FALSE]
    b <- vol3d[i2, j2, k2, drop = FALSE]
    ma <- m[i1, j1, k1, drop = FALSE]
    mb <- m[i2, j2, k2, drop = FALSE]
    pair_mask <- ma & mb
    np <- sum(pair_mask)
    if (is.na(np) || np <= 5) { res[idx] <- 0; next }
    av <- as.numeric(a[pair_mask]) - fbar
    bv <- as.numeric(b[pair_mask]) - fbar
    res[idx] <- sum(av * bv) / (fvar * max(1, np - 1))
  }
  res
}

#' Fit AFNI's mixed Gaussian+exponential ACF model
#'
#' @param r Distances (mm).
#' @param acf_vals Empirical ACF values for those distances.
#' @return List containing fit parameters and modeled ACF when successful.
fit_acf_model <- function(r, acf_vals) {
  ok <- is.finite(r) & is.finite(acf_vals)
  r <- r[ok]; a <- acf_vals[ok]
  if (!length(r)) return(list(ok = FALSE))
  ord <- order(r)
  r <- r[ord]; a <- pmax(pmin(a[ord], 1), 0)
  for (i in seq_along(a)[-1]) {
    if (a[i] > a[i - 1]) a[i] <- a[i - 1]
  }
  tail_len <- max(4L, floor(length(r) * 0.3))
  tail_idx <- seq.int(length(r) - tail_len + 1L, length(r))
  tail_vals <- a[tail_idx]
  tail_r <- r[tail_idx]
  keep <- tail_vals > 1e-4
  tail_vals <- tail_vals[keep]
  tail_r <- tail_r[keep]
  if (length(tail_vals) >= 3L) {
    fit <- lm(log(tail_vals) ~ tail_r)
    slope <- coef(fit)[2]
    intercept <- coef(fit)[1]
    c_est <- if (is.finite(slope) && slope < 0) -1 / slope else max(r) / 3
    mix <- exp(intercept)
    mix <- min(0.99, max(0.01, mix))
    a_est <- 1 - mix
  } else {
    c_est <- max(r) / 3
    a_est <- 0.3
  }
  obj <- function(par) {
    aa <- par[1]; bb <- par[2]
    if (aa <= 0 || aa >= 1 || bb <= 0) return(1e9)
    pred <- aa * exp(-0.5 * (r*r)/(bb*bb)) + (1 - aa) * exp(-r/c_est)
    sum((pred - a)^2)
  }
  grid_a <- seq(0.1, 0.9, length.out = 9)
  grid_b <- seq(max(r)/10, max(r), length.out = 9)
  grid <- expand.grid(grid_a, grid_b)
  vals <- apply(grid, 1, obj)
  best <- grid[which.min(vals), ]
  step <- c(0.1, max(r) * 0.2)
  for (iter in 1:10) {
    cand <- rbind(
      best,
      c(best[1] + step[1], best[2]),
      c(best[1] - step[1], best[2]),
      c(best[1], best[2] + step[2]),
      c(best[1], best[2] - step[2])
    )
    cand[,1] <- pmin(0.99, pmax(0.01, cand[,1]))
    cand[,2] <- pmax(0.05, cand[,2])
    vals <- apply(cand, 1, obj)
    idx <- which.min(vals)
    best <- cand[idx, ]
    step <- step * 0.7
  }
  a_hat <- best[1]; b_hat <- best[2]
  model <- a_hat * exp(-0.5 * (r*r)/(b_hat*b_hat)) + (1 - a_hat) * exp(-r/c_est)
  f <- function(x) a_hat * exp(-0.5 * (x*x)/(b_hat*b_hat)) + (1 - a_hat) * exp(-x/c_est) - 0.5
  upper <- max(r) * 4
  if (f(0) < 0 || f(upper) > 0) return(list(ok = FALSE))
  root <- tryCatch(uniroot(f, c(0, upper))$root, error = function(e) NA_real_)
  if (!is.finite(root)) return(list(ok = FALSE))
  list(ok = TRUE, a = a_hat, b = b_hat, c = c_est, d = 2 * root,
       r = r, acf_emp = a, acf_model = model)
}

#' Geometric mean that tolerates non-finite and non-positive entries
#'
#' @param x Numeric vector of values to average.
#' @return Geometric mean or `NA_real_` when no valid entries.
geom_mean_safe <- function(x) {
  x <- x[is.finite(x) & x > 0]
  if (!length(x)) NA_real_ else exp(mean(log(x)))
}

#' Classical FWHM estimate based on per-axis finite differences
#'
#' @param arr4d Numeric array (x, y, z, t) of residuals.
#' @param mask3d Logical mask of valid voxels.
#' @param vox_mm Numeric vector of voxel sizes (mm).
#' @param agg Aggregation method for across-axis summary (default geometric mean).
#' @return List with per-volume axes, aggregated axes, and overall FWHM.
estimate_classic_fwhm <- function(arr4d, mask3d, vox_mm, agg = "geom") {
  dims <- dim(arr4d)
  nt <- dims[4]
  per_axis <- matrix(NA_real_, nrow = nt, ncol = 3)
  for (t in seq_len(nt)) {
    per_axis[t, ] <- compute_fwhm_1dif(arr4d[,,,t], mask3d, vox_mm)
  }
  geom_axes <- apply(per_axis, 2, geom_mean_safe)
  overall <- geom_mean_safe(geom_axes)
  list(per_axis = per_axis, geom_axes = geom_axes, geom = overall)
}

#' Estimate smoothness from the spatial ACF across a 4D residual set
#'
#' @param arr4d Numeric residual array (x, y, z, t).
#' @param mask3d Logical mask of valid voxels.
#' @param vox_mm Voxel sizes (mm).
#' @param radius_mm Maximum distance to include in ACF estimation.
#' @param use_cpp Whether to use compiled helper (when available).
#' @return List containing fitted FWHM, ACF samples, and fit metadata.
estimate_acf_fwhm <- function(arr4d, mask3d, vox_mm, radius_mm = 20,
                              use_cpp = FALSE) {
  dims <- dim(arr4d)
  nt <- dims[4]
  offs <- build_acf_offsets(vox_mm[1], vox_mm[2], vox_mm[3],
                            radius_mm, dims[1:3], dims[3] == 1)
  if (nrow(offs$offs) == 0) return(list(fwhm = NA_real_, ok = FALSE))
  radii <- offs$rads
  acf_vals <- NULL
  used_cpp <- FALSE
  if (use_cpp && exists("acf_estimate_cpp", mode = "function")) {
    dims_full <- c(dims[1], dims[2], dims[3], nt)
    res <- tryCatch(
      acf_estimate_cpp(as.numeric(arr4d), as.logical(mask3d),
                       as.integer(dims_full), offs$offs, offs$rads),
      error = function(e) list(ok = FALSE, error = conditionMessage(e))
    )
    if (isTRUE(res$ok) && length(res$acf)) {
      acf_vals <- res$acf
      radii <- res$r
      used_cpp <- TRUE
    } else {
      err_msg <- if (!is.null(res$error)) res$error else "no details"
      warning("ACF Rcpp estimator failed (", err_msg,
              "); falling back to R implementation.", call. = FALSE)
    }
  }
  if (is.null(acf_vals)) {
    acf_sum <- rep(0, length(offs$rads))
    count <- 0
    for (t in seq_len(nt)) {
      vals <- acf_for_volume(arr4d[,,,t], mask3d, offs$offs, offs$rads)
      if (!all(vals == 0)) {
        acf_sum <- acf_sum + vals
        count <- count + 1
      }
    }
    if (count == 0) return(list(fwhm = NA_real_, ok = FALSE))
    acf_vals <- acf_sum / count
  }
  fit <- fit_acf_model(radii, acf_vals)
  if (!fit$ok) {
    return(list(fwhm = NA_real_, ok = FALSE, acf = acf_vals, radii = radii))
  }
  list(fwhm = fit$d, ok = TRUE, acf = acf_vals,
       radii = fit$r, model = fit$acf_model,
       a = fit$a, b = fit$b, c = fit$c, cpp = used_cpp)
}

#' Median across the time dimension for each voxel
#'
#' @param arr4d Numeric array (x, y, z, t).
#' @return 3D array of voxel-wise medians.
median_over_time <- function(arr4d) {
  apply(arr4d, c(1, 2, 3), median, na.rm = TRUE)
}

#' MAD across the time dimension for each voxel
#'
#' @param arr4d Numeric array (x, y, z, t).
#' @return 3D array of voxel-wise scaled MAD values.
mad_over_time <- function(arr4d) {
  apply(arr4d, c(1, 2, 3), mad, constant = 1.4826, na.rm = TRUE)
}
