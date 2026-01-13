zscore <- function(x) {
  xs <- scale(x, center = TRUE, scale = TRUE)
  as.numeric(xs[, 1L])
}

build_mixing_matrix <- function(n_time = 80L) {
  t <- seq_len(n_time)
  base1 <- zscore(sin(t / 7))
  base2 <- zscore(cos(t / 9))
  base3 <- zscore(sin(t / 3 + 0.2))
  base4 <- zscore(cos(t / 5 + 0.1))
  mix <- cbind(
    base1,
    zscore(0.55 * base1 + 0.25 * base2 + 0.2 * base4),
    zscore(base2 + 0.15 * base3),
    zscore(0.35 * base1 + 0.35 * base2 + 0.3 * base3),
    zscore(rnorm(n_time))
  )
  mix
}

test_that("apply_aroma differentiates aggressive and non-aggressive denoising for NIfTI input", {
  skip_if_not_installed("RNifti")

  set.seed(42)
  tmpdir <- tempdir()

  mixing <- build_mixing_matrix(100L)
  noise_ics <- c(2L, 4L) # regress only a subset of the available components
  mix_path <- file.path(tmpdir, "mixing.tsv")
  data.table::fwrite(as.data.frame(mixing), mix_path, sep = "\t", col.names = FALSE)

  signal <- 1000 +
    35 * mixing[, 2] +
    25 * mixing[, 4] +
    15 * mixing[, 5] +
    rnorm(nrow(mixing), sd = 3)

  arr <- array(signal, dim = c(1, 1, 1, length(signal)))
  in_path <- file.path(tmpdir, "input.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(arr), in_path)

  nonagg_path <- file.path(tmpdir, "nonagg.nii.gz")
  agg_path <- file.path(tmpdir, "agg.nii.gz")

  apply_aroma(
    in_file = in_path,
    out_file = nonagg_path,
    mixing_file = mix_path,
    noise_ics = noise_ics,
    overwrite = TRUE,
    nonaggressive = TRUE
  )

  apply_aroma(
    in_file = in_path,
    out_file = agg_path,
    mixing_file = mix_path,
    noise_ics = noise_ics,
    overwrite = TRUE,
    nonaggressive = FALSE
  )

  resid_nonagg <- as.vector(RNifti::readNifti(nonagg_path))
  resid_agg <- as.vector(RNifti::readNifti(agg_path))

  expect_lt(var(resid_agg), var(resid_nonagg))

  corr_nonagg_c2 <- abs(cor(resid_nonagg, mixing[, 2]))
  corr_agg_c2 <- abs(cor(resid_agg, mixing[, 2]))
  corr_nonagg_c4 <- abs(cor(resid_nonagg, mixing[, 4]))
  corr_agg_c4 <- abs(cor(resid_agg, mixing[, 4]))

  # correlation of residuals and c2/c4 in exclusive/aggressive mode should be ~0
  expect_lt(abs(corr_agg_c2), 1e-6)
  expect_lt(abs(corr_agg_c4), 1e-6)
  expect_lt(corr_agg_c2, corr_nonagg_c2)
  expect_lt(corr_agg_c4, corr_nonagg_c4)
})

test_that("apply_aroma removes AROMA components from confounds using lmfit_residuals_mat", {
  skip_if_not_installed("RNifti")

  set.seed(101)
  tmpdir <- tempdir()

  mixing <- build_mixing_matrix(90L)
  noise_ics <- c(2L, 4L)
  mix_path <- file.path(tmpdir, "mixing.tsv")
  data.table::fwrite(as.data.frame(mixing), mix_path, sep = "\t", col.names = FALSE)

  conf_df <- data.frame(
    conf_a = mixing[, 2] + 0.3 * mixing[, 5] + rnorm(nrow(mixing), sd = 0.05),
    conf_b = mixing[, 4] + 0.4 * mixing[, 3],
    conf_c = mixing[, 1] - 0.2 * mixing[, 4]
  )
  conf_dir <- file.path(tmpdir, "confounds")
  dir.create(conf_dir, showWarnings = FALSE)
  conf_path <- file.path(conf_dir, "sub-01_task-test_desc-confounds_timeseries.tsv")
  data.table::fwrite(conf_df, conf_path, sep = "\t")

  base_cfg <- list(
    motion_filter = list(enable = FALSE),
    scrubbing = list(enable = FALSE, apply = FALSE, add_to_confounds = FALSE),
    confound_regression = list(
      enable = TRUE,
      columns = colnames(conf_df),
      noproc_columns = NULL
    ),
    confound_calculate = list(
      enable = FALSE,
      columns = character(0),
      noproc_columns = character(0),
      demean = FALSE
    ),
    apply_aroma = list(enable = TRUE, nonaggressive = TRUE),
    temporal_filter = list(low_pass_hz = NULL, high_pass_hz = NULL, method = "butterworth"),
    bids_desc = "pp",
    tr = 1,
    overwrite = TRUE
  )

  proc_files <- list(
    confounds = conf_path,
    melodic_mix = mix_path,
    noise_ics = noise_ics
  )

  run_confounds <- function(nonaggressive_flag, out_dir) {
    cfg <- base_cfg
    cfg$apply_aroma$nonaggressive <- nonaggressive_flag
    output_bids_info <- list(
      subject = "01",
      task = "rest",
      description = "denoise",
      suffix = "bold",
      ext = ".nii.gz",
      directory = out_dir
    )
    processing_sequence <- "apply_aroma"
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

    regress_path <- postprocess_confounds(
      proc_files = proc_files,
      cfg = cfg,
      processing_sequence = processing_sequence,
      output_bids_info = output_bids_info,
      fsl_img = NULL,
      lg = NULL
    )

    regs_dt <- data.table::fread(regress_path)
    colnames(regs_dt) <- c("intercept", colnames(conf_df))
    regs_mat <- as.matrix(regs_dt)[, -1, drop = FALSE]
    regs_mat
  }

  regs_nonagg <- run_confounds(TRUE, file.path(tmpdir, "nonagg"))
  regs_agg <- run_confounds(FALSE, file.path(tmpdir, "agg"))

  var_nonagg <- apply(regs_nonagg, 2, var)
  var_agg <- apply(regs_agg, 2, var)
  expect_true(all(var_agg <= var_nonagg + 1e-6))

  corr_nonagg_a <- abs(cor(regs_nonagg[, "conf_a"], mixing[, noise_ics[1]]))
  corr_agg_a <- abs(cor(regs_agg[, "conf_a"], mixing[, noise_ics[1]]))
  expect_lt(corr_agg_a, corr_nonagg_a)

  corr_nonagg_b <- abs(cor(regs_nonagg[, "conf_b"], mixing[, noise_ics[2]]))
  corr_agg_b <- abs(cor(regs_agg[, "conf_b"], mixing[, noise_ics[2]]))
  expect_lt(corr_agg_b, corr_nonagg_b)
  
  # correlation of residuals and c2/c4 in exclusive/aggressive mode should be ~0
  expect_lt(abs(corr_agg_a), 1e-6)
  expect_lt(abs(corr_agg_b), 1e-6)
})
