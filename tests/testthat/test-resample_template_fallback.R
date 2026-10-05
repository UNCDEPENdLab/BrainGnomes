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

test_that("resample_template_to_img forwards cohort-qualified template queries", {
  skip_if_not_installed("reticulate")

  captured <- new.env(parent = emptyenv())

  local_mocked_bindings(
    py_available = function(initialize = FALSE) TRUE,
    py_module_available = function(module) TRUE,
    source_python = function(file, envir = parent.frame(), convert = TRUE) {
      if (!is.null(envir)) {
        assign("resample_template_to_bold", function(in_file, output, template_space = NULL,
                                                     template_cohort = NULL, ...) {
          captured$template_space <- template_space
          captured$template_cohort <- template_cohort
          output
        }, envir = envir)
      }
      invisible(NULL)
    },
    .package = "reticulate"
  )

  tmp_dir <- tempfile("resample_template_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  in_file <- file.path(
    tmp_dir,
    "sub-01_task-test_space-MNIPediatricAsym_cohort-2_desc-preproc_bold.nii.gz"
  )
  file.create(in_file)

  out <- resample_template_to_img(in_file, install_dependencies = FALSE, overwrite = TRUE, lg = NULL)

  expect_identical(captured$template_space, "MNIPediatricAsym")
  expect_identical(captured$template_cohort, 2L)
  expect_true(grepl("cohort-2", basename(out), fixed = TRUE))
})

test_that("Python template resampling preserves target xforms and codes", {
  skip_if_not_installed("reticulate")
  required_modules <- c("nibabel", "nilearn", "numpy", "templateflow")
  available <- vapply(
    required_modules, reticulate::py_module_available, logical(1)
  )
  skip_if_not(
    all(available),
    paste("Python modules unavailable:", paste(required_modules[!available], collapse = ", "))
  )

  script <- system.file(
    "fetch_matched_template_image.py", package = "BrainGnomes"
  )
  expect_true(file.exists(script))

  tmp_dir <- tempfile("resample_xforms_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  reference_file <- file.path(
    tmp_dir,
    "sub-01_task-test_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz"
  )
  template_file <- file.path(tmp_dir, "template_mask.nii.gz")
  output_file <- file.path(tmp_dir, "resampled_mask.nii.gz")

  np <- reticulate::import("numpy", convert = FALSE)
  nib <- reticulate::import("nibabel", convert = FALSE)
  reference_affine <- diag(4)
  reference_affine[1:3, 1:3] <- diag(c(2.4, 2.5, 2.6))
  reference_affine[1:3, 4] <- c(-70, -90, -60)
  reference_affine_py <- np$array(reference_affine)
  reference <- nib$Nifti1Image(
    np$zeros(reticulate::tuple(8L, 9L, 7L, 3L), dtype = "float32"),
    reference_affine_py
  )
  reference$set_qform(reference_affine_py, code = 4L)
  reference$set_sform(reference_affine_py, code = 4L)
  nib$save(reference, reference_file)

  template_affine_py <- np$array(diag(c(1, 1, 1, 1)))
  template <- nib$Nifti1Image(
    np$ones(reticulate::tuple(5L, 6L, 4L), dtype = "uint8"),
    template_affine_py
  )
  nib$save(template, template_file)

  module <- reticulate::import_from_path(
    "fetch_matched_template_image", path = dirname(script), convert = FALSE
  )
  reticulate::py_set_attr(
    module,
    "fetch_template_image",
    reticulate::r_to_py(
      function(template, resolution, suffix, desc = NULL,
               extension = ".nii.gz", cohort = NULL) template_file
    )
  )
  module$resample_template_to_bold(
    in_file = reference_file,
    output = output_file,
    template_space = "MNI152NLin2009cAsym",
    interpolation = "nearest"
  )

  observed <- nib$load(output_file)
  observed_qform <- observed$get_qform(coded = TRUE)
  observed_sform <- observed$get_sform(coded = TRUE)
  reference_qform <- reference$get_qform(coded = TRUE)
  reference_sform <- reference$get_sform(coded = TRUE)

  observed_qform <- reticulate::py_to_r(observed_qform)
  observed_sform <- reticulate::py_to_r(observed_sform)
  reference_qform <- reticulate::py_to_r(reference_qform)
  reference_sform <- reticulate::py_to_r(reference_sform)

  expect_identical(as.integer(observed_qform[[2L]]), 4L)
  expect_identical(as.integer(observed_sform[[2L]]), 4L)
  expect_equal(
    observed_qform[[1L]], reference_qform[[1L]], tolerance = 1e-7
  )
  expect_equal(
    observed_sform[[1L]], reference_sform[[1L]], tolerance = 1e-7
  )
})

test_that("Python resampling never reads reference voxel data", {
  skip_if_not_installed("reticulate")
  required_modules <- c("nibabel", "nilearn", "numpy")
  available <- vapply(
    required_modules, reticulate::py_module_available, logical(1)
  )
  skip_if_not(all(available), "Python image-resampling modules unavailable")
  script <- system.file("fetch_matched_template_image.py", package = "BrainGnomes")
  module <- reticulate::import_from_path(
    "fetch_matched_template_image", path = dirname(script), convert = FALSE
  )
  tmp_dir <- tempfile("resample_header_only_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  reticulate::py_run_string(paste(c(
    "import os",
    "import numpy as np",
    "import nibabel as nib",
    "from unittest.mock import patch",
    "",
    "# A long 4D image whose geometry is valid but whose voxels cannot be read.",
    "class UnreadableReference:",
    "    shape = (8, 9, 7, 10000)",
    "    dtype = np.dtype('float32')",
    "    def __array__(self, *args, **kwargs):",
    "        raise AssertionError('Reference voxel data were accessed')",
    "    def __getitem__(self, key):",
    "        raise AssertionError('Reference voxel data were accessed')",
    "",
    "def check_header_only_resampling(module, directory):",
    "    # Assert header-only access and identical resampled voxels and transforms.",
    "    affine = np.diag([2., 2., 2., 1.])",
    "    reference = nib.Nifti1Image(np.zeros((8, 9, 7), dtype=np.float32), affine)",
    "    reference.set_qform(affine, code=4)",
    "    reference.set_sform(affine, code=4)",
    "    reference._dataobj = UnreadableReference()",
    "    reference.header.set_data_shape(reference.shape)",
    "    source_file = os.path.join(directory, 'source.nii.gz')",
    "    output_file = os.path.join(directory, 'output.nii.gz')",
    "    source = nib.Nifti1Image(np.ones((5, 6, 4), dtype=np.uint8), np.eye(4))",
    "    nib.save(source, source_file)",
    "    real_load = nib.load",
    "    def guarded_load(path, *args, **kwargs):",
    "        return reference if path == 'unreadable_reference' else real_load(path, *args, **kwargs)",
    "    with patch.object(module.nib, 'load', side_effect=guarded_load):",
    "        module.resample_image_to_reference(source_file, 'unreadable_reference', output_file)",
    "    observed = real_load(output_file)",
    "    from nilearn.image import resample_to_img",
    "    small_reference = nib.Nifti1Image(np.zeros((8, 9, 7), dtype=np.float32), affine)",
    "    expected = resample_to_img(source, small_reference, interpolation='nearest',",
    "                               force_resample=True, copy_header=True)",
    "    np.testing.assert_array_equal(observed.get_fdata(), expected.get_fdata())",
    "    assert observed.shape == (8, 9, 7)",
    "    assert int(observed.get_qform(coded=True)[1]) == 4",
    "    assert int(observed.get_sform(coded=True)[1]) == 4",
    "    return True"
  ), collapse = "\n"))
  expect_true(reticulate::py_to_r(
    reticulate::py$check_header_only_resampling(module, tmp_dir)
  ))
})
