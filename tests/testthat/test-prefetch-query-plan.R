test_that("resolve_prefetch_query_plan returns parsed summary from plan step", {
  root <- tempfile("prefetch_plan_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  script_path <- file.path(root, "prefetch_wrapper.py")
  writeLines("print('unused')", script_path)
  container_path <- file.path(root, "fake_container.sif")
  file.create(container_path)
  tf_home <- file.path(root, "templateflow")
  dir.create(tf_home, recursive = TRUE, showWarnings = FALSE)

  fake_runtime <- "/usr/bin/fake-runtime"

  local_mocked_bindings(
    find_container_runtime = function() fake_runtime,
    run_prefetch_query_plan_command = function(runtime, cmd_args, env) {
      expect_identical(runtime, fake_runtime)
      expect_true("--plan-only" %in% cmd_args)
      expect_true("--summary-json" %in% cmd_args)
      expect_true(any(grepl("--output-spaces", cmd_args, fixed = TRUE)))
      expect_true(any(grepl("MNI152NLin2009cAsym", cmd_args, fixed = TRUE)))
      expect_true("TEMPLATEFLOW_HOME" %in% names(env))
      expect_true("APPTAINERENV_TEMPLATEFLOW_HOME" %in% names(env))
      expect_identical(
        unname(env[["TEMPLATEFLOW_HOME"]]),
        normalizePath(tf_home, winslash = "/", mustWork = FALSE)
      )
      expect_identical(
        unname(env[["APPTAINERENV_TEMPLATEFLOW_HOME"]]),
        normalizePath(tf_home, winslash = "/", mustWork = FALSE)
      )

      summary_idx <- match("--summary-json", cmd_args)
      summary_file <- cmd_args[[summary_idx + 1L]]
      jsonlite::write_json(
        list(
          query_signature = "abc123sig",
          query_count = 4L,
          queries = list(
            list(template = "MNI152NLin2009cAsym", params = list(suffix = "T1w")),
            list(template = "MNI152NLin2009cAsym", params = list(suffix = "mask")),
            list(template = "MNI152NLin2009cAsym", params = list(suffix = "T2w")),
            list(
              template = "MNI152NLin2009cAsym",
              params = list(suffix = "probseg", label = "CSF")
            )
          )
        ),
        path = summary_file,
        auto_unbox = TRUE
      )

      character(0)
    }
  )

  plan <- resolve_prefetch_query_plan(
    container_path = container_path,
    script_path = script_path,
    requested_spaces = "MNI152NLin2009cAsym",
    templateflow_home = tf_home
  )

  expect_type(plan, "list")
  expect_identical(plan$query_signature, "abc123sig")
  expect_equal(plan$query_count, 4L)
  expect_length(plan$queries, 4L)
  expect_true(any(vapply(plan$queries, function(x) identical(x$params$suffix, "T1w"), logical(1))))
  expect_true(any(vapply(plan$queries, function(x) identical(x$params$suffix, "mask"), logical(1))))
  expect_true(any(vapply(plan$queries, function(x) identical(x$params$suffix, "T2w"), logical(1))))
  expect_true(any(vapply(plan$queries, function(x) identical(x$params$suffix, "probseg"), logical(1))))
})

test_that("resolve_prefetch_query_plan returns NULL when no container runtime is available", {
  root <- tempfile("prefetch_plan_no_runtime_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  script_path <- file.path(root, "prefetch_wrapper.py")
  writeLines("print('unused')", script_path)
  container_path <- file.path(root, "fake_container.sif")
  file.create(container_path)
  tf_home <- file.path(root, "templateflow")
  dir.create(tf_home, recursive = TRUE, showWarnings = FALSE)

  local_mocked_bindings(
    find_container_runtime = function() NULL
  )

  expect_null(resolve_prefetch_query_plan(
    container_path = container_path,
    script_path = script_path,
    requested_spaces = "MNI152NLin2009cAsym",
    templateflow_home = tf_home
  ))
})

test_that("resolve_prefetch_query_plan returns NULL when plan command exits nonzero", {
  root <- tempfile("prefetch_plan_nonzero_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  script_path <- file.path(root, "prefetch_wrapper.py")
  writeLines("print('unused')", script_path)
  container_path <- file.path(root, "fake_container.sif")
  file.create(container_path)
  tf_home <- file.path(root, "templateflow")
  dir.create(tf_home, recursive = TRUE, showWarnings = FALSE)

  local_mocked_bindings(
    find_container_runtime = function() "/usr/bin/fake-runtime",
    run_prefetch_query_plan_command = function(runtime, cmd_args, env) {
      structure("prefetch failed", status = 1L)
    }
  )

  expect_null(resolve_prefetch_query_plan(
    container_path = container_path,
    script_path = script_path,
    requested_spaces = "MNI152NLin2009cAsym",
    templateflow_home = tf_home
  ))
})

test_that("resolve_prefetch_query_plan returns NULL when summary file is missing", {
  root <- tempfile("prefetch_plan_missing_summary_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  script_path <- file.path(root, "prefetch_wrapper.py")
  writeLines("print('unused')", script_path)
  container_path <- file.path(root, "fake_container.sif")
  file.create(container_path)
  tf_home <- file.path(root, "templateflow")
  dir.create(tf_home, recursive = TRUE, showWarnings = FALSE)

  local_mocked_bindings(
    find_container_runtime = function() "/usr/bin/fake-runtime",
    run_prefetch_query_plan_command = function(runtime, cmd_args, env) {
      character(0)
    }
  )

  expect_null(resolve_prefetch_query_plan(
    container_path = container_path,
    script_path = script_path,
    requested_spaces = "MNI152NLin2009cAsym",
    templateflow_home = tf_home
  ))
})

test_that("resolve_prefetch_query_plan returns NULL when summary json is malformed", {
  root <- tempfile("prefetch_plan_bad_json_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  script_path <- file.path(root, "prefetch_wrapper.py")
  writeLines("print('unused')", script_path)
  container_path <- file.path(root, "fake_container.sif")
  file.create(container_path)
  tf_home <- file.path(root, "templateflow")
  dir.create(tf_home, recursive = TRUE, showWarnings = FALSE)

  local_mocked_bindings(
    find_container_runtime = function() "/usr/bin/fake-runtime",
    run_prefetch_query_plan_command = function(runtime, cmd_args, env) {
      summary_idx <- match("--summary-json", cmd_args)
      summary_file <- cmd_args[[summary_idx + 1L]]
      writeLines("{not-valid-json", summary_file)
      character(0)
    }
  )

  expect_null(resolve_prefetch_query_plan(
    container_path = container_path,
    script_path = script_path,
    requested_spaces = "MNI152NLin2009cAsym",
    templateflow_home = tf_home
  ))
})

test_that("resolve_prefetch_query_plan forwards conditional CIFTI defaults flag", {
  root <- tempfile("prefetch_plan_cifti_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  script_path <- file.path(root, "prefetch_wrapper.py")
  writeLines("print('unused')", script_path)
  container_path <- file.path(root, "fake_container.sif")
  file.create(container_path)
  tf_home <- file.path(root, "templateflow")
  dir.create(tf_home, recursive = TRUE, showWarnings = FALSE)

  local_mocked_bindings(
    find_container_runtime = function() "/usr/bin/fake-runtime",
    run_prefetch_query_plan_command = function(runtime, cmd_args, env) {
      expect_true("--include-cifti-defaults" %in% cmd_args)

      summary_idx <- match("--summary-json", cmd_args)
      summary_file <- cmd_args[[summary_idx + 1L]]
      jsonlite::write_json(
        list(query_signature = "sig-cifti", query_count = 1L, queries = list()),
        path = summary_file,
        auto_unbox = TRUE
      )

      character(0)
    }
  )

  plan <- resolve_prefetch_query_plan(
    container_path = container_path,
    script_path = script_path,
    requested_spaces = "MNI152NLin2009cAsym",
    templateflow_home = tf_home,
    include_cifti_defaults = TRUE
  )

  expect_identical(plan$query_signature, "sig-cifti")
  expect_equal(plan$query_count, 1L)
})
