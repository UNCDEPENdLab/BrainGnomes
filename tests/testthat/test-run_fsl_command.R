test_that("run_fsl_command errors when FSLDIR cannot be resolved", {
  old_fsldir <- Sys.getenv("FSLDIR", unset = NA_character_)
  on.exit({
    if (is.na(old_fsldir)) Sys.unsetenv("FSLDIR") else Sys.setenv(FSLDIR = old_fsldir)
  }, add = TRUE)
  Sys.unsetenv("FSLDIR")

  local_mocked_bindings(
    system2 = function(...) character(0),
    .package = "base",
    .env = baseenv()
  )

  expect_error(
    run_fsl_command("fslmaths in.nii.gz -Tmean out.nii.gz", run = FALSE, echo = FALSE, use_lgr = FALSE),
    regexp = "Cannot resolve FSLDIR"
  )
})

test_that("run_fsl_command errors when resolved FSLDIR is missing fslconf", {
  fake_fsldir <- tempfile("fake_fsldir_")
  dir.create(fake_fsldir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(fake_fsldir, recursive = TRUE, force = TRUE), add = TRUE)

  expect_error(
    run_fsl_command(
      "fslmaths in.nii.gz -Tmean out.nii.gz",
      fsldir = fake_fsldir,
      run = FALSE,
      echo = FALSE,
      use_lgr = FALSE
    ),
    regexp = "Resolved FSLDIR is invalid"
  )
})

test_that("run_fsl_command falls back to feat path when FSLDIR is unset", {
  old_fsldir <- Sys.getenv("FSLDIR", unset = NA_character_)
  on.exit({
    if (is.na(old_fsldir)) Sys.unsetenv("FSLDIR") else Sys.setenv(FSLDIR = old_fsldir)
  }, add = TRUE)
  Sys.unsetenv("FSLDIR")

  fake_fsldir <- tempfile("fsl_")
  dir.create(file.path(fake_fsldir, "bin"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(fake_fsldir, "etc", "fslconf"), recursive = TRUE, showWarnings = FALSE)
  writeLines("#!/bin/sh", file.path(fake_fsldir, "etc", "fslconf", "fsl.sh"))
  fake_feat <- file.path(fake_fsldir, "bin", "feat")
  writeLines("#!/bin/sh", fake_feat)
  on.exit(unlink(fake_fsldir, recursive = TRUE, force = TRUE), add = TRUE)

  local_mocked_bindings(
    system2 = function(command, args = character(), ...) {
      if (identical(command, "bash") && length(args) >= 2L && identical(args[[1]], "-lc")) {
        cmd <- args[[2]]
        if (grepl("command -v feat", cmd, fixed = TRUE)) return(fake_feat)
        return(character(0))
      }
      character(0)
    },
    .package = "base",
    .env = baseenv()
  )

  rc <- run_fsl_command(
    "fslmaths in.nii.gz -Tmean out.nii.gz",
    run = FALSE,
    echo = FALSE,
    use_lgr = FALSE
  )
  expect_equal(as.numeric(rc), 0)
  expect_identical(
    normalizePath(Sys.getenv("FSLDIR"), winslash = "/", mustWork = FALSE),
    normalizePath(fake_fsldir, winslash = "/", mustWork = FALSE)
  )
})

test_that("run_fsl_command accepts singularity FSLDIR that is host-invisible", {
  fake_img <- tempfile("fsl_", fileext = ".sif")
  file.create(fake_img)
  on.exit(unlink(fake_img, force = TRUE), add = TRUE)

  old_fsldir <- Sys.getenv("FSLDIR", unset = NA_character_)
  on.exit({
    if (is.na(old_fsldir)) Sys.unsetenv("FSLDIR") else Sys.setenv(FSLDIR = old_fsldir)
  }, add = TRUE)
  Sys.setenv(FSLDIR = "/host/fsl")

  local_mocked_bindings(
    system2 = function(command, args = character(), stdout = "", stderr = "", ...) {
      if (identical(command, "singularity") &&
          length(args) >= 4L &&
          identical(args[[1]], "exec") &&
          identical(args[[3]], "printenv") &&
          identical(args[[4]], "FSLDIR")) {
        return("/opt/fsl-6.0.7.16")
      }
      if (identical(command, "singularity") &&
          length(args) >= 4L &&
          identical(args[[1]], "exec") &&
          identical(args[[3]], "test") &&
          identical(args[[4]], "-f")) {
        return(0L)
      }
      character(0)
    },
    .package = "base",
    .env = baseenv()
  )

  rc <- run_fsl_command(
    "fslmaths in.nii.gz -Tmean out.nii.gz",
    run = FALSE,
    echo = FALSE,
    use_lgr = FALSE,
    fsl_img = fake_img
  )
  expect_equal(as.numeric(rc), 0)
  expect_identical(Sys.getenv("FSLDIR"), "/host/fsl")
})
