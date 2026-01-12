test_that("resample_template_to_img falls back to a managed env when Python is not writable", {
  skip_on_os("windows")
  skip_if_not_installed("reticulate")

  pkg_state <- new.env(parent = emptyenv())
  pkg_state$installed <- character()

  local_mocked_bindings(
    py_available = function(initialize = FALSE) FALSE,
    py_module_available = function(module) module %in% pkg_state$installed,
    py_install = function(...) stop("py_install should not be called in this test"),
    py_require = function(pkgs, ...) {
      pkg_state$installed <- unique(c(pkg_state$installed, pkgs))
      invisible()
    },
    source_python = function(file, envir = parent.frame(), convert = TRUE) {
      if (!is.null(envir)) {
        assign("resample_template_to_bold", function(in_file, output, ...) output, envir = envir)
      }
      invisible(NULL)
    },
    .package = "reticulate"
  )

  env_root <- tempfile("pyenv")
  dir.create(file.path(env_root, "bin"), recursive = TRUE)
  file.create(file.path(env_root, "bin", "python"))
  Sys.chmod(env_root, "0555")

  old_env <- Sys.getenv(c("RETICULATE_PYTHON", "HOME"), unset = NA)
  on.exit({
    Sys.chmod(env_root, "0755")
    unlink(env_root, recursive = TRUE, force = TRUE)
    for (nm in names(old_env)) {
      val <- old_env[[nm]]
      if (is.na(val)) {
        Sys.unsetenv(nm)
      } else {
        do.call(Sys.setenv, setNames(list(val), nm))
      }
    }
  }, add = TRUE)

  Sys.setenv(
    RETICULATE_PYTHON = file.path(env_root, "bin", "python"),
    HOME = tempdir()
  )

  in_file <- tempfile(
    "sub-01_task-test_space-MNI152NLin6Asym_desc-preproc_bold",
    fileext = ".nii.gz"
  )
  writeBin(raw(0), in_file)

  out <- NULL
  expect_warning(
    out <- resample_template_to_img(in_file, install_dependencies = TRUE, overwrite = TRUE, lg = NULL),
    "not writable"
  )

  expect_true(grepl("templatemask\\.nii\\.gz$", out))
  expect_setequal(pkg_state$installed, c("nibabel", "nilearn", "templateflow"))
})
