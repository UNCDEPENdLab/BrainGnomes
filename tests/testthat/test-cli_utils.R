# tests for CLI utility functions

library(testthat)

# args_to_df handles equals and space-separated values

test_that("args_to_df parses CLI arguments", {
  args <- c("--foo=1", "--bar 2 3", "-x=hello")
  df <- args_to_df(args)
  expect_equal(nrow(df), 3)
  expect_equal(df$lhs, c("foo", "bar", "x"))
  expect_equal(df$rhs, c("1", "2 3", "hello"))
  expect_equal(df$has_equals, c(TRUE, FALSE, TRUE))
  expect_equal(df$nhyphens, c(2, 2, 1))
})

# parse_cli_args builds nested list

test_that("parse_cli_args converts CLI arguments to nested list", {
  args <- c("--a/b=10", "--c=d", "--flag e f", "--flag2='e f'")
  res <- parse_cli_args(args)
  expect_equal(res$a$b, 10)
  expect_equal(res$c, "d")
  expect_equal(res$flag, c("e", "f")) # unquoted spaces parsed into vector
  expect_equal(res$flag2, "e f") # quoted spaces parsed into string
})

# values containing '=' are parsed correctly
test_that("parse_cli_args handles embedded equals signs", {
  args <- c("--foo=bar=baz", "--bar='a = b'")
  res <- parse_cli_args(args)
  expect_equal(res$foo, "bar=baz")
  expect_equal(res$bar, "a = b")
})

test_that("parse_cli_args treats bare flags as TRUE", {
  res <- parse_cli_args(c("--status FAILED", "--cascade"))
  expect_equal(res$status, "FAILED")
  expect_true(isTRUE(res$cascade))
})

test_that("parse_cli_args respects explicit FALSE and NULL for flags", {
  expect_false(parse_cli_args(c("--cascade=FALSE"))$cascade)
  expect_null(parse_cli_args(c("--cascade=NULL"))$cascade)
})

test_that("CLI values retain negative numbers and trailing equals signs", {
  result <- parse_cli_args(c("--offsets=-1 -2.5 0 1", "--token=abc==", "--empty=", "--flag"))
  expect_equal(result$offsets, c(-1, -2.5, 0, 1))
  expect_identical(result$token, "abc==")
  expect_identical(result$empty, "")
  expect_true(result$flag)
  expect_equal(parse_cli_args("--cutoff -0.5 --enabled")$cutoff, -0.5)
  expect_identical(set_nested_values("a/b=key==")$a$b, "key==")
})

test_that("shell path options preserve spaces and quotes without changing vector options", {
  paths <- c("/tmp/project  with spaces/config.yaml", "/tmp/user's project/config.yaml")
  for (path in paths) {
    for (option in list(c("--input", path), paste0("--input=", path))) {
      result <- parse_cli_path_args(c(option, "--processing_steps=filter regress", "--flag"))
      expect_identical(result$input, path)
      expect_identical(result$processing_steps, c("filter", "regress"))
      expect_true(result$flag)
    }
  }
  result <- parse_cli_path_args(c("--sqlite_db", "", "--job_id=1", "--status=STARTED"))
  expect_identical(result$sqlite_db, "")
  generated <- nested_list_to_args(list(input = "/tmp/a b", processing_steps = c("filter", "regress")))
  expect_identical(parse_cli_path_args(generated), parse_cli_args(generated))
})

# nested_list_to_args round-trips with parse_cli_args

test_that("nested_list_to_args creates expected CLI strings", {
  lst <- list(a = list(b = 1, c = 2), d = "hey")
  args <- nested_list_to_args(lst)
  expect_true(all(c("--a/b='1'", "--a/c='2'", "--d='hey'") %in% args))
  collapsed <- nested_list_to_args(lst, collapse = TRUE)
  expect_true(grepl("--a/b='1'", collapsed, fixed = TRUE))
  expect_true(grepl("--d='hey'", collapsed, fixed = TRUE))
})

# set_cli_options merges and updates CLI args

test_that("set_cli_options updates and adds options", {
  base <- c("--foo=1", "--bar=2")
  new <- c("--foo=3", "--baz=4")
  result <- set_cli_options(base, new, collapse = TRUE)
  # expect_equal(result, c("--foo=3", "--bar=2", "--baz=4"))
  expect_equal(result, c("--foo=3 --baz=4 --bar=2"))
})

test_that("fmriprep_cli_requests_cifti_defaults detects requested CIFTI output", {
  expect_false(fmriprep_cli_requests_cifti_defaults(NULL))
  expect_false(fmriprep_cli_requests_cifti_defaults(""))
  expect_false(fmriprep_cli_requests_cifti_defaults("--output-spaces MNI152NLin2009cAsym"))
  expect_false(fmriprep_cli_requests_cifti_defaults("--cifti-output FALSE"))
  expect_false(fmriprep_cli_requests_cifti_defaults("--cifti-output=off"))

  expect_true(fmriprep_cli_requests_cifti_defaults("--cifti-output 91k"))
  expect_true(fmriprep_cli_requests_cifti_defaults("--dummy=1 --cifti-output=170k"))
})
