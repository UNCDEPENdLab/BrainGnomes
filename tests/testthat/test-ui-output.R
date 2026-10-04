test_that("CLI instruction blocks normalize source indentation", {
  expect_identical(
    normalize_cli_block("\n    First line\n      indented detail\n\n    Last line\n"),
    "First line\n  indented detail\n\nLast line"
  )
  expect_null(normalize_cli_block("\n  \n"))
})

test_that("CLI instruction blocks wrap prose and retain list structure", {
  old_options <- options(
    cli.width = 54,
    cli.unicode = FALSE,
    cli.num_colors = 1,
    cli.dynamic = FALSE
  )
  on.exit(options(old_options), add = TRUE)

  output <- capture.output(cli_instruction(
    "
      This prose was authored across two source lines but should be rendered as
      one width-aware paragraph.

      Processing steps:
        - First item
        - Second item

      Outputs:
        1. Summary file
           One row per scan.
        2. Detail file
           One row per region.

      /data/dicom/
      |-- sub-01/
      |   |-- ses-01/
    ",
    before = FALSE
  ), type = "message")

  expect_true(any(grepl("^\\* First item$", output)))
  expect_true(any(grepl("^[[:space:]]*1\\. Summary file One row per scan\\.$", output)))
  expect_true(any(grepl("^[[:space:]]*2\\. Detail file One row per region\\.$", output)))
  expect_true(any(output == "|   |-- ses-01/"))
  expect_false(any(grepl("^      This prose", output)))
})

test_that("prompt instructions do not add a blank line before input", {
  old_options <- options(cli.num_colors = 1, cli.dynamic = FALSE)
  on.exit(options(old_options), add = TRUE)
  observed_prompt <- NULL

  local_mocked_bindings(
    console_input_available = function() TRUE,
    getline = function(prompt) {
      observed_prompt <<- prompt
      ""
    },
    .package = "BrainGnomes"
  )

  output <- capture.output(result <- prompt_input(
    prompt = "Optional arguments",
    instruct = "Press Enter when done.",
    type = "character", required = FALSE
  ), type = "message")

  expect_true(is.na(result))
  expect_identical(output, c("", "Press Enter when done."))
  expect_identical(observed_prompt, "Optional arguments (Press Enter to skip)\n> ")
})

test_that("CLI argument builders place the input marker directly after instructions", {
  old_options <- options(cli.num_colors = 1, cli.dynamic = FALSE)
  on.exit(options(old_options), add = TRUE)
  observed_prompt <- NULL

  local_mocked_bindings(
    getline = function(prompt) {
      observed_prompt <<- prompt
      ""
    },
    .package = "BrainGnomes"
  )

  output <- capture.output(result <- build_cli_args(
    instruct = "Specify optional arguments. Press Enter when done."
  ), type = "message")

  expect_length(result, 0L)
  expect_identical(output, c("", "Specify optional arguments. Press Enter when done."))
  expect_identical(observed_prompt, "> ")
})

test_that("prompt headings and input remain usable through the TTY-safe reader", {
  old_options <- options(
    cli.width = 60,
    cli.unicode = FALSE,
    cli.num_colors = 1,
    cli.dynamic = FALSE
  )
  on.exit(options(old_options), add = TRUE)
  observed_prompt <- NULL

  local_mocked_bindings(
    console_input_available = function() TRUE,
    getline = function(prompt) {
      observed_prompt <<- prompt
      ""
    },
    .package = "BrainGnomes"
  )

  output <- capture.output(result <- prompt_input(
    prompt = "Continue?",
    instruct = "This explanation is wrapped by cli.",
    heading = "BIDS validation",
    type = "flag",
    default = TRUE
  ), type = "message")

  expect_true(result)
  expect_true(any(grepl("BIDS validation", output, fixed = TRUE)))
  expect_true(any(output == "This explanation is wrapped by cli."))
  expect_identical(
    observed_prompt,
    "Continue? (yes/no; Press Enter to accept default: yes)\n> "
  )
})

test_that("headless initialization never enters the interactive prompt layer", {
  root <- tempfile("headless-ui-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  local_mocked_bindings(
    prompt_input = function(...) stop("interactive prompt was called"),
    .package = "BrainGnomes"
  )

  cfg <- setup_project(
    project_name = "headless",
    project_directory = root,
    interactive = FALSE
  )
  expect_true(file.exists(file.path(root, "project_config.yaml")))
  expect_false(any(vapply(
    supported_project_steps(),
    function(step) isTRUE(cfg[[step]]$enable),
    logical(1)
  )))
})
