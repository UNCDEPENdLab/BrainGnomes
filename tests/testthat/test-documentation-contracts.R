get_topic_rd <- function(topic) {
  source_file <- testthat::test_path("..", "..", "man", paste0(topic, ".Rd"))
  if (file.exists(source_file)) {
    return(tools::parse_Rd(source_file))
  }

  help_file <- utils::help(topic, package = "BrainGnomes")
  if (!length(help_file)) stop("Missing installed help topic: ", topic)
  utils:::.getHelpFile(help_file)
}

rd_tag_text <- function(rd, tag) {
  matching <- vapply(rd, function(x) identical(attr(x, "Rd_tag"), tag), logical(1))
  paste(unlist(rd[matching]), collapse = "")
}

get_vignette_sources <- function() {
  source_dir <- testthat::test_path("..", "..", "vignettes")
  if (!dir.exists(source_dir)) {
    source_dir <- system.file("doc", package = "BrainGnomes")
  }
  list.files(source_dir, pattern = "[.]Rmd$", full.names = TRUE)
}

test_that("website reference groups cover the public workflow by task", {
  path <- testthat::test_path("..", "..", "_pkgdown.yml")
  skip_if_not(file.exists(path), "website configuration is not installed with the package")
  groups <- yaml::read_yaml(path)$reference
  expect_identical(vapply(groups[1:3], `[[`, character(1), "title"),
    c("Start and configure a project", "Preview and run work", "Monitor, diagnose, and recover"))
  expect_true("setup_project" %in% groups[[1]]$contents)
  expect_true("run_project" %in% groups[[2]]$contents)
  expect_true("diagnose_project" %in% groups[[3]]$contents)
  topics <- unlist(lapply(groups, `[[`, "contents"), use.names = FALSE)
  expect_true(all(getNamespaceExports("BrainGnomes") %in% topics))
  expect_false("diagnose_pipeline" %in% topics)
  expect_identical(anyDuplicated(topics), 0L)
})

test_that("exported native-backed help topics contain real usage signatures", {
  expected_formals <- list(
    automask = c(
      "img", "outfile", "clfrac", "NN", "erode_steps", "dilate_steps",
      "SIhh", "peels", "fill_holes"
    ),
    filtfilt_cpp = c("x", "b", "a", "padlen", "padtype", "use_zi"),
    image_quantile = c("in_file", "brain_mask", "quantiles", "exclude_zero"),
    lmfit_residuals_4d = c(
      "infile", "X", "include_rows", "add_intercept", "outfile", "internal",
      "preserve_mean", "set_mean", "regress_cols", "exclusive"
    ),
    natural_spline_4d = c(
      "infile", "t_interpolate", "edge_nn", "outfile", "internal"
    ),
    natural_spline_interp = c("x", "y", "xout"),
    remove_nifti_volumes = c("infile", "remove_tpts", "outfile")
  )

  namespace <- asNamespace("BrainGnomes")
  for (topic in names(expected_formals)) {
    rd <- get_topic_rd(topic)
    usage <- rd_tag_text(rd, "\\usage")
    expect_true(nzchar(usage), info = paste(topic, "must have a usage section"))
    expect_match(usage, paste0("\\b", topic, "\\("), info = topic)
    expect_identical(
      names(formals(get(topic, envir = namespace))),
      expected_formals[[topic]],
      info = topic
    )
  }

  expect_identical(formals(automask)$outfile, "")
})

test_that("corrected public examples and scientific output contracts stay documented", {
  automask_rd <- paste(unlist(get_topic_rd("automask")), collapse = "")
  quantile_rd <- paste(unlist(get_topic_rd("image_quantile")), collapse = "")
  run_project_topic <- get_topic_rd("run_project")
  run_project_rd <- paste(unlist(run_project_topic), collapse = "")
  extract_rd <- paste(unlist(get_topic_rd("extract_rois")), collapse = "")

  expect_match(automask_rd, "mask <- automask\\(")
  expect_false(grepl("automask_rcpp", automask_rd, fixed = TRUE))
  expect_match(quantile_rd, "quantiles = 0.5", fixed = TRUE)
  expect_false(grepl('image_quantile("bold.nii.gz", 0.5)', quantile_rd, fixed = TRUE))
  expect_false(grepl("prompt = TRUE", run_project_rd, fixed = TRUE))
  expect_match(run_project_rd, 'steps = "fmriprep"', fixed = TRUE)
  supported_stages <- c(
    "flywheel_sync", "bids_conversion", "mriqc", "fmriprep", "aroma",
    "postprocess", "extract_rois"
  )
  for (stage in supported_stages) {
    expect_match(run_project_rd, stage, fixed = TRUE)
  }
  run_project_seealso <- rd_tag_text(run_project_topic, "\\seealso")
  expect_match(run_project_seealso, "run_bids_validation", fixed = TRUE)
  expect_match(run_project_seealso, "get_run_provenance", fixed = TRUE)
  expect_match(extract_rd, "range from -1 to 1", fixed = TRUE)
  expect_match(extract_rd, "use NA on the diagonal", fixed = TRUE)
  expect_false(grepl("becomes 15", extract_rd, fixed = TRUE))
})

test_that("Quickstart renders CLI help from the installed command", {
  quickstart_path <- testthat::test_path("..", "..", "vignettes", "braingnomes_quickstart.Rmd")
  if (!file.exists(quickstart_path)) {
    quickstart_path <- system.file("doc", "braingnomes_quickstart.Rmd", package = "BrainGnomes")
  }
  expect_true(nzchar(quickstart_path) && file.exists(quickstart_path))
  quickstart <- readLines(quickstart_path, warn = FALSE)
  quickstart_text <- paste(quickstart, collapse = "\n")

  expect_match(quickstart_text, 'system.file("BrainGnomes", package = "BrainGnomes")', fixed = TRUE)
  expect_match(quickstart_text, "system2(", fixed = TRUE)
  expect_false(grepl("run_project <project_directory|config.yaml> -steps", quickstart_text, fixed = TRUE))

  flow_path <- testthat::test_path(
    "..", "..", "inst", "extdata", "braingnomes_flow.png"
  )
  if (!file.exists(flow_path)) {
    flow_path <- system.file(
      "extdata", "braingnomes_flow.png", package = "BrainGnomes"
    )
  }
  expect_true(nzchar(flow_path) && file.exists(flow_path))
  article_flow_path <- file.path(
    dirname(quickstart_path), "braingnomes_flow.png"
  )
  if (dir.exists(testthat::test_path("..", "..", "vignettes"))) {
    expect_true(file.exists(article_flow_path))
    expect_identical(
      unname(tools::md5sum(article_flow_path)), unname(tools::md5sum(flow_path))
    )
  }
  expect_match(quickstart_text, 'flow_png <- "braingnomes_flow.png"', fixed = TRUE)
  expect_match(quickstart_text, 'fig.alt="BrainGnomes pipeline flow"', fixed = TRUE)

  optional_heading <- regexpr(
    "# Optional inspection and automation tools", quickstart_text, fixed = TRUE
  )[[1L]]
  expect_gt(optional_heading, 0L)
  primary_workflow <- substr(quickstart_text, 1L, optional_heading - 1L)
  expect_match(primary_workflow, "scfg <- setup_project()", fixed = TRUE)
  expect_match(primary_workflow, "run <- run_project(scfg)", fixed = TRUE)
  expect_match(primary_workflow, "inspect_project(scfg)", fixed = TRUE)
  expect_match(primary_workflow, "diagnose_project(scfg)", fixed = TRUE)
  expect_false(grepl("diagnose_pipeline(scfg)", primary_workflow, fixed = TRUE))
  expect_false(grepl("validate_project_config(", primary_workflow, fixed = TRUE))
  expect_false(grepl("doctor(", primary_workflow, fixed = TRUE))
  expect_false(grepl("plan_project(", primary_workflow, fixed = TRUE))

  expect_match(quickstart_text, "## Config: inspect or validate YAML", fixed = TRUE)
  expect_match(quickstart_text, "## Doctor: inspect the submission environment", fixed = TRUE)
  expect_match(quickstart_text, "## Plan: inspect or persist resolved work", fixed = TRUE)
  expect_match(
    quickstart_text,
    "A plan is an optional view of the request model\nthat `run_project()` resolves internally",
    fixed = TRUE
  )
  expect_match(quickstart_text, "scope-realization.json", fixed = TRUE)
  expect_match(quickstart_text, "immutable job\\s+manifest")
  expect_match(quickstart_text, "runtime\\s+receipt")
  expect_match(quickstart_text, "get_run_provenance(scfg, run$run_id)", fixed = TRUE)
  expect_match(
    quickstart_text,
    "BrainGnomes provenance /project/my_study --run=latest --format=json",
    fixed = TRUE
  )
  expect_match(
    quickstart_text,
    "retry_run <- retry_project_run(scfg, run$run_id, dry_run = FALSE)",
    fixed = TRUE
  )
  expect_match(quickstart_text, "A retry does not resume or change the original run", fixed = TRUE)
  expect_match(quickstart_text, "include_blocked = TRUE", fixed = TRUE)
  expect_match(
    quickstart_text,
    'BrainGnomes retry /project/my_study --run="$run_id" --yes',
    fixed = TRUE
  )
  expect_match(
    quickstart_text,
    "BrainGnomes diagnose /project/my_study --interactive",
    fixed = TRUE
  )
  expect_false(grepl(
    "BrainGnomes diagnose /project/my_study --run=latest --interactive",
    quickstart_text, fixed = TRUE
  ))
})

test_that("applied-user recovery documentation explains safe run-based actions", {
  diagnosis_path <- testthat::test_path(
    "..", "..", "vignettes", "diagnosing_pipeline.Rmd"
  )
  if (!file.exists(diagnosis_path)) {
    diagnosis_path <- system.file(
      "doc", "diagnosing_pipeline.Rmd", package = "BrainGnomes"
    )
  }
  expect_true(nzchar(diagnosis_path) && file.exists(diagnosis_path))
  diagnosis_text <- paste(readLines(diagnosis_path, warn = FALSE), collapse = "\n")
  diagnosis_plain <- gsub("[[:space:]]+", " ", diagnosis_text)

  expected_guidance <- c(
    "# Choose the run you mean",
    "# Inspect one run without prompts",
    "get_run_provenance(scfg, run_id)",
    "# Retry failed work after correcting the cause",
    "A retry creates a **new run**",
    "retry_plan <- retry_project_run(scfg, run_id, dry_run = TRUE)",
    "retry_run <- retry_project_run(scfg, run_id, dry_run = FALSE)",
    "include_blocked = TRUE",
    "# Cancel work that is still active",
    'BrainGnomes retry /project/my_study --run="$run_id" --yes'
  )
  for (guidance in expected_guidance) {
    expect_match(diagnosis_text, guidance, fixed = TRUE)
  }
  expect_false(grepl(
    "validation can otherwise trigger interactive correction prompts",
    diagnosis_text, fixed = TRUE
  ))
  expect_match(
    diagnosis_plain,
    "does not delete data, logs, or completed outputs",
    fixed = TRUE
  )

  retry_rd <- paste(unlist(get_topic_rd("retry_project_run")), collapse = "")
  cancel_rd <- paste(unlist(get_topic_rd("cancel_project_run")), collapse = "")
  retry_plain <- gsub("[[:space:]]+", " ", retry_rd)
  cancel_plain <- gsub("[[:space:]]+", " ", cancel_rd)
  expect_match(retry_plain, "does not change the original run", fixed = TRUE)
  expect_match(retry_plain, "submission begins immediately", fixed = TRUE)
  expect_match(retry_plain, "could not run because an earlier", fixed = TRUE)
  expect_match(cancel_plain, "does not delete project data", fixed = TRUE)
  expect_match(cancel_plain, "immediately sends cancellation requests", fixed = TRUE)
})

test_that("vignette metadata and local assets remain publication-ready", {
  vignette_files <- get_vignette_sources()
  expect_gt(length(vignette_files), 0L)

  for (vignette_file in vignette_files) {
    lines <- readLines(vignette_file, warn = FALSE)
    vignette_text <- paste(lines, collapse = "\n")
    info <- basename(vignette_file)

    frontmatter_end <- which(lines[-1L] == "---")[[1L]] + 1L
    frontmatter <- lines[seq_len(frontmatter_end)]
    date_lines <- grep(
      '^date: "[0-9]{2} [A-Z][a-z]{2} [0-9]{4}"$',
      frontmatter,
      value = TRUE
    )
    expect_true(
      length(date_lines) == 1L,
      info = paste(info, "must have one literal date")
    )

    expect_false(
      grepl("\u3010F:|\u2020L[0-9]", vignette_text, perl = TRUE),
      info = paste(info, "must not contain internal source-citation markers")
    )
    expect_false(
      grepl("[(]in progress[)]|vignette[^\\n]*in progress", vignette_text,
        ignore.case = TRUE, perl = TRUE
      ),
      info = paste(info, "must not contain a stale in-progress label")
    )

    image_markup <- regmatches(
      vignette_text,
      gregexpr("!\\[[^]]*\\]\\([^)]+\\)", vignette_text, perl = TRUE)
    )[[1L]]
    if (length(image_markup) && !identical(image_markup, character(0))) {
      image_targets <- sub("^.*\\]\\(([^)[:space:]]+).*$", "\\1", image_markup)
      image_targets <- image_targets[!grepl(
        "^[a-z][a-z0-9+.-]*:", image_targets, ignore.case = TRUE
      )]
      for (image_target in image_targets) {
        expect_true(
          file.exists(file.path(dirname(vignette_file), image_target)),
          info = paste(info, "references missing image", image_target)
        )
      }
    }
  }
})

test_that("motion QC guide covers summaries, exports, and scrubbing boundaries", {
  motion_qc_path <- testthat::test_path("..", "..", "vignettes", "motion_qc.Rmd")
  if (!file.exists(motion_qc_path)) {
    motion_qc_path <- system.file("doc", "motion_qc.Rmd", package = "BrainGnomes")
  }
  expect_true(nzchar(motion_qc_path) && file.exists(motion_qc_path))
  motion_qc_text <- paste(readLines(motion_qc_path, warn = FALSE), collapse = "\n")

  expected_guidance <- c(
    "calculate_motion_outliers(",
    "# Raw FD threshold summaries",
    "include_filtered = TRUE",
    "fd_filt_",
    "all(is.na(skipped_qc",
    "# Export a QC or exclusion table",
    "output_file = summary_file",
    "# Relationship to postprocessing scrubbing",
    "does **not** submit jobs"
  )
  for (guidance in expected_guidance) {
    expect_match(motion_qc_text, guidance, fixed = TRUE)
  }
})
