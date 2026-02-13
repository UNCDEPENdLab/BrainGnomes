test_that("postprocess_confounds handles scrubbing, column expansion, and noproc columns", {
  skip_if_not_installed("RNifti")

  tmpdir <- tempfile("postproc_confounds_")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  n_time <- 8L
  set.seed(123)
  conf_df <- data.frame(
    rot_x = sin(seq_len(n_time) / 10),
    rot_y = cos(seq_len(n_time) / 9),
    rot_z = sin(seq_len(n_time) / 8),
    trans_x = seq(0.1, 0.3, length.out = n_time),
    trans_y = seq(-0.2, 0.2, length.out = n_time),
    trans_z = rnorm(n_time, sd = 0.01),
    framewise_displacement = c(0.15, 0.22, 0.65, 0.12, 0.58, 0.77, 0.19, 0.61),
    white_matter = seq(-0.4, 0.4, length.out = n_time),
    csf = seq(0.3, -0.3, length.out = n_time),
    global_signal = c(NA_real_, rnorm(n_time - 1, sd = 0.05)),
    cosine_01 = cos(seq_len(n_time)),
    cosine_02 = sin(seq_len(n_time)),
    filler = rnorm(n_time)
  )

  conf_path <- file.path(tmpdir, "sub-01_task-rest_desc-confounds_timeseries.tsv")
  data.table::fwrite(conf_df, conf_path, sep = "\t")

  cfg <- list(
    motion_filter = list(enable = FALSE),
    scrubbing = list(
      enable = TRUE,
      expression = c(fdspikes = "framewise_displacement > 0.5"),
      apply = FALSE,
      add_to_confounds = TRUE
    ),
    confound_regression = list(
      enable = TRUE,
      columns = c("6p", "white_matter"),
      noproc_columns = "global_signal"
    ),
    confound_calculate = list(
      enable = TRUE,
      columns = c("cosine_<1-2>", "white_matter"),
      noproc_columns = "framewise_displacement",
      demean = TRUE
    ),
    apply_aroma = list(enable = FALSE, nonaggressive = TRUE),
    temporal_filter = list(low_pass_hz = NULL, high_pass_hz = NULL, method = "butterworth"),
    bids_desc = "pp",
    tr = 1,
    overwrite = TRUE
  )

  output_bids_info <- list(
    subject = "01",
    task = "rest",
    description = "denoise",
    suffix = "bold",
    ext = ".nii.gz",
    directory = tmpdir
  )

  proc_files <- list(
    confounds = conf_path,
    melodic_mix = NULL,
    noise_ics = integer(0)
  )

  regress_path <- postprocess_confounds(
    proc_files = proc_files,
    cfg = cfg,
    processing_sequence = character(0),
    output_bids_info = output_bids_info,
    fsl_img = NULL,
    lg = NULL
  )

  expect_true(file.exists(regress_path))

  confound_file <- construct_bids_filename(
    modifyList(output_bids_info, list(suffix = "confounds", ext = ".tsv")),
    full.names = TRUE
  )
  scrub_file <- construct_bids_filename(
    modifyList(output_bids_info, list(suffix = "scrub", ext = ".tsv")),
    full.names = TRUE
  )
  censor_file <- get_censor_file(output_bids_info)

  expect_true(file.exists(confound_file))
  expect_true(file.exists(scrub_file))
  expect_true(file.exists(censor_file))

  # confound_calculate output: demeaned requested columns + original FD + spikes
  calc_dt <- data.table::fread(confound_file)
  calc_matrix <- as.matrix(calc_dt)
  expected_calc <- expand_confound_columns(cfg$confound_calculate$columns, names(conf_df))
  expect_equal(
    unname(calc_matrix[, seq_along(expected_calc), drop = FALSE]),
    unname(sweep(
      as.matrix(conf_df[, expected_calc, drop = FALSE]),
      2,
      colMeans(conf_df[, expected_calc, drop = FALSE])
    )),
    tolerance = 1e-10
  )

  fd_column <- calc_matrix[, length(expected_calc) + 1L]
  expect_equal(fd_column, conf_df$framewise_displacement)

  spike_indices <- which(conf_df$framewise_displacement > 0.5)
  spike_matrix <- calc_matrix[, -(seq_len(length(expected_calc) + 1L)), drop = FALSE]
  expect_equal(ncol(spike_matrix), length(spike_indices))
  if (ncol(spike_matrix) > 0) {
    apply(spike_matrix, 2, function(col) expect_equal(sum(col), 1))
    spike_rows <- apply(spike_matrix, 2, function(col) which(col == 1))
    expect_equal(unname(spike_rows), spike_indices)
  }

  # scrub and censor files line up with detected spikes
  scrub_dt <- data.table::fread(scrub_file)
  expect_equal(dim(scrub_dt), c(n_time, length(spike_indices)))
  censor_vals <- readLines(censor_file)
  expect_identical(
    censor_vals,
    as.character(ifelse(conf_df$framewise_displacement > 0.5, 0, 1))
  )

  # regression file: intercept + expanded columns + noproc global signal with NA replaced by 0
  reg_dt <- data.table::fread(regress_path)
  reg_df <- as.data.frame(reg_dt)
  reg_cols <- expand_confound_columns(cfg$confound_regression$columns, names(conf_df))
  colnames(reg_df) <- c("intercept", reg_cols, "global_signal")
  expect_true(all(reg_df$intercept == 1))

  centered_means <- colMeans(reg_df[, reg_cols, drop = FALSE])
  expect_true(all(abs(centered_means) < 1e-10))

  expected_global_signal <- replace(conf_df$global_signal, is.na(conf_df$global_signal), 0)
  expect_equal(reg_df$global_signal, expected_global_signal)
})

test_that("postprocess_confounds recomputes framewise displacement when only noproc columns are requested", {
  tmpdir <- tempfile("postproc_confounds_fd_")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  n_time <- 40L
  conf_df <- data.frame(
    rot_x = sin(seq_len(n_time) / 4),
    rot_y = cos(seq_len(n_time) / 5),
    rot_z = sin(seq_len(n_time) / 6),
    trans_x = seq(0.05, 0.15, length.out = n_time),
    trans_y = seq(-0.1, 0.1, length.out = n_time),
    trans_z = seq(0.2, -0.2, length.out = n_time),
    framewise_displacement = rep(0.3, n_time),
    white_matter = seq(-0.5, 0.5, length.out = n_time),
    csf = c(seq(0.4, -0.4, length.out = n_time - 1), NA_real_)
  )

  conf_path <- file.path(tmpdir, "sub-01_task-rest_desc-confounds_timeseries.tsv")
  data.table::fwrite(conf_df, conf_path, sep = "\t")

  cfg <- list(
    motion_filter = list(enable = TRUE, filter_type = "notch", bandstop_min_bpm = 12, bandstop_max_bpm = 18),
    scrubbing = list(enable = FALSE, apply = FALSE, add_to_confounds = FALSE, head_radius = 40),
    confound_regression = list(
      enable = TRUE,
      columns = NULL,
      noproc_columns = c("framewise_displacement", "csf")
    ),
    confound_calculate = list(
      enable = TRUE,
      columns = NULL,
      noproc_columns = c("framewise_displacement", "white_matter"),
      demean = FALSE
    ),
    apply_aroma = list(enable = FALSE, nonaggressive = TRUE),
    temporal_filter = list(low_pass_hz = NULL, high_pass_hz = NULL, method = "butterworth"),
    bids_desc = "pp",
    tr = 0.8,
    overwrite = TRUE
  )

  output_bids_info <- list(
    subject = "99",
    task = "rest",
    description = "denoise",
    suffix = "bold",
    ext = ".nii.gz",
    directory = tmpdir
  )

  proc_files <- list(
    confounds = conf_path,
    melodic_mix = NULL,
    noise_ics = integer(0)
  )

  motion_cols <- c("rot_x", "rot_y", "rot_z", "trans_x", "trans_y", "trans_z")
  filtered_df <- filter_confounds(
    confounds_df = conf_df,
    tr = cfg$tr,
    filter_type = cfg$motion_filter$filter_type,
    bandstop_min_bpm = cfg$motion_filter$bandstop_min_bpm,
    bandstop_max_bpm = cfg$motion_filter$bandstop_max_bpm,
    columns = motion_cols,
    lg = lgr::get_logger_glue("BrainGnomes.test.motion")
  )
  expected_fd <- framewise_displacement(
    motion = filtered_df[, motion_cols, drop = FALSE],
    head_radius = cfg$scrubbing$head_radius,
    columns = motion_cols
  )

  regress_path <- postprocess_confounds(
    proc_files = proc_files,
    cfg = cfg,
    processing_sequence = character(0),
    output_bids_info = output_bids_info,
    fsl_img = NULL,
    lg = NULL
  )

  expect_true(file.exists(regress_path))

  confound_file <- construct_bids_filename(
    modifyList(output_bids_info, list(suffix = "confounds", ext = ".tsv")),
    full.names = TRUE
  )
  reg_dt <- data.table::fread(regress_path)
  calc_dt <- data.table::fread(confound_file)
  reg_df <- as.data.frame(reg_dt)

  # confound_calculate output should contain recomputed FD followed by white matter
  expect_equal(ncol(calc_dt), 2)
  expect_equal(calc_dt[[1]], expected_fd, tolerance = 1e-6)
  expect_equal(calc_dt[[2]], conf_df$white_matter)
  expect_false(isTRUE(all.equal(conf_df$framewise_displacement, expected_fd)))

  # regression file contains intercept + FD + csf (with NA replaced by 0)
  colnames(reg_df) <- c("intercept", "framewise_displacement", "csf")
  expect_true(all(reg_df$intercept == 1))
  expect_equal(reg_df$framewise_displacement, expected_fd, tolerance = 1e-6)
  expect_equal(
    reg_df$csf,
    replace(conf_df$csf, is.na(conf_df$csf), 0)
  )
})

test_that("postprocess_confounds honors framewise_displacement_unfiltered requests", {
  tmpdir <- tempfile("postproc_confounds_fd_unfiltered_")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  n_time <- 40L
  conf_df <- data.frame(
    rot_x = sin(seq_len(n_time) / 4),
    rot_y = cos(seq_len(n_time) / 5),
    rot_z = sin(seq_len(n_time) / 6),
    trans_x = seq(0.05, 0.15, length.out = n_time),
    trans_y = seq(-0.1, 0.1, length.out = n_time),
    trans_z = seq(0.2, -0.2, length.out = n_time),
    framewise_displacement = rep(0.3, n_time)
  )
  conf_path <- file.path(tmpdir, "sub-01_task-rest_desc-confounds_timeseries.tsv")
  data.table::fwrite(conf_df, conf_path, sep = "\t")

  cfg <- list(
    motion_filter = list(enable = TRUE, filter_type = "notch", bandstop_min_bpm = 12, bandstop_max_bpm = 18),
    scrubbing = list(enable = FALSE, apply = FALSE, add_to_confounds = FALSE, head_radius = 40),
    confound_regression = list(enable = FALSE, columns = NULL, noproc_columns = NULL),
    confound_calculate = list(
      enable = TRUE,
      columns = NULL,
      noproc_columns = "framewise_displacement_unfiltered",
      demean = FALSE,
      include_header = TRUE
    ),
    apply_aroma = list(enable = FALSE, nonaggressive = TRUE),
    temporal_filter = list(low_pass_hz = NULL, high_pass_hz = NULL, method = "butterworth"),
    bids_desc = "pp",
    tr = 0.8,
    overwrite = TRUE
  )

  output_bids_info <- list(
    subject = "99",
    task = "rest",
    description = "denoise",
    suffix = "bold",
    ext = ".nii.gz",
    directory = tmpdir
  )
  proc_files <- list(confounds = conf_path, melodic_mix = NULL, noise_ics = integer(0))

  postprocess_confounds(
    proc_files = proc_files,
    cfg = cfg,
    processing_sequence = character(0),
    output_bids_info = output_bids_info,
    fsl_img = NULL,
    lg = NULL
  )

  confound_file <- construct_bids_filename(
    modifyList(output_bids_info, list(suffix = "confounds", ext = ".tsv")),
    full.names = TRUE
  )
  calc_dt <- data.table::fread(confound_file)
  expect_equal(colnames(calc_dt), "framewise_displacement")
  expect_equal(calc_dt$framewise_displacement, conf_df$framewise_displacement, tolerance = 1e-6)
})

test_that("postprocess_confounds skips processing when confound data are empty", {
  skip_if_not_installed("RNifti")

  tmpdir <- tempfile("postproc_confounds_empty_")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  conf_df <- data.frame(rot_x = numeric(0))
  conf_path <- file.path(tmpdir, "sub-01_task-rest_desc-confounds_timeseries.tsv")
  data.table::fwrite(conf_df, conf_path, sep = "\t")

  cfg <- list(
    motion_filter = list(enable = FALSE),
    scrubbing = list(enable = FALSE, apply = FALSE, add_to_confounds = FALSE),
    confound_regression = list(enable = TRUE, columns = "rot_x", noproc_columns = NULL, prefix = "r"),
    confound_calculate = list(enable = FALSE, columns = NULL, noproc_columns = NULL, demean = TRUE),
    apply_aroma = list(enable = FALSE, nonaggressive = TRUE),
    temporal_filter = list(low_pass_hz = NULL, high_pass_hz = NULL, method = "butterworth"),
    bids_desc = "pp",
    tr = 1,
    overwrite = TRUE
  )

  output_bids_info <- list(
    subject = "01",
    task = "rest",
    description = "denoise",
    suffix = "bold",
    ext = ".nii.gz",
    directory = tmpdir
  )

  proc_files <- list(
    confounds = conf_path,
    melodic_mix = NULL,
    noise_ics = integer(0)
  )

  expect_warning(
    regress_path <- postprocess_confounds(
      proc_files = proc_files,
      cfg = cfg,
      processing_sequence = character(0),
      output_bids_info = output_bids_info,
      fsl_img = NULL,
      lg = NULL
    ),
    "Confound columns were requested but no usable data were found",
    fixed = TRUE
  )

  expect_null(regress_path)

  reg_file <- construct_bids_filename(
    modifyList(output_bids_info, list(suffix = "regressors", ext = ".tsv")),
    full.names = TRUE
  )
  expect_false(file.exists(reg_file))
})

test_that("postprocess_confounds handles empty confound data with calc/reg enabled", {
  skip_if_not_installed("RNifti")

  tmpdir <- tempfile("postproc_confounds_empty_calc_")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  conf_df <- data.frame(rot_x = numeric(0), framewise_displacement = numeric(0))
  conf_path <- file.path(tmpdir, "sub-01_task-rest_desc-confounds_timeseries.tsv")
  data.table::fwrite(conf_df, conf_path, sep = "\t")

  cfg <- list(
    motion_filter = list(enable = FALSE),
    scrubbing = list(enable = FALSE, apply = FALSE, add_to_confounds = FALSE),
    confound_regression = list(enable = TRUE, columns = "rot_x", noproc_columns = NULL, prefix = "r"),
    confound_calculate = list(enable = TRUE, columns = "framewise_displacement", noproc_columns = NULL, demean = TRUE),
    apply_aroma = list(enable = FALSE, nonaggressive = TRUE),
    temporal_filter = list(low_pass_hz = NULL, high_pass_hz = NULL, method = "butterworth"),
    bids_desc = "pp",
    tr = 1,
    overwrite = TRUE
  )

  output_bids_info <- list(
    subject = "01",
    task = "rest",
    description = "denoise",
    suffix = "bold",
    ext = ".nii.gz",
    directory = tmpdir
  )

  proc_files <- list(
    confounds = conf_path,
    melodic_mix = NULL,
    noise_ics = integer(0)
  )

  expect_warning(
    regress_path <- postprocess_confounds(
      proc_files = proc_files,
      cfg = cfg,
      processing_sequence = character(0),
      output_bids_info = output_bids_info,
      fsl_img = NULL,
      lg = NULL
    ),
    "Confound columns were requested but no usable data were found",
    fixed = TRUE
  )

  expect_null(regress_path)

  reg_file <- construct_bids_filename(
    modifyList(output_bids_info, list(suffix = "regressors", ext = ".tsv")),
    full.names = TRUE
  )
  confound_file <- construct_bids_filename(
    modifyList(output_bids_info, list(suffix = "confounds", ext = ".tsv")),
    full.names = TRUE
  )
  expect_false(file.exists(reg_file))
  expect_false(file.exists(confound_file))
})
