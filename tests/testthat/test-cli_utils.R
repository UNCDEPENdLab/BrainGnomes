# tests for CLI utility functions

library(testthat)

# args_to_df handles equals and space-separated values

test_that("args_to_df parses CLI arguments", {
  args <- c("--foo=1", "--bar", "2 3", "-x=hello")
  df <- args_to_df(args)
  expect_equal(nrow(df), 3)
  expect_equal(df$lhs, c("foo", "bar", "x"))
  expect_equal(df$rhs, c("1", "2 3", "hello"))
  expect_equal(df$has_equals, c(TRUE, FALSE, TRUE))
  expect_equal(df$nhyphens, c(2, 2, 1))
})

# parse_cli_args builds nested list

test_that("parse_cli_args converts CLI arguments to nested list", {
  args <- c("--a/b=10", "--c=d", "--flag e f")
  res <- parse_cli_args(args)
  expect_equal(res$a$b, 10)
  expect_equal(res$c, "d")
  expect_equal(res$flag, "e f")
})

# nested_list_to_args round-trips with parse_cli_args

test_that("nested_list_to_args creates expected CLI strings", {
  lst <- list(a = list(b = 1, c = 2), d = "hey")
  args <- nested_list_to_args(lst)
  expect_true(all(c("--a/b='1'", "--a/c='2'", "--d=hey") %in% args))
  collapsed <- nested_list_to_args(lst, collapse = TRUE)
  expect_true(grepl("--a/b='1'", collapsed, fixed = TRUE))
  expect_true(grepl("--d=hey", collapsed, fixed = TRUE))
})

# set_cli_options merges and updates CLI args

test_that("set_cli_options updates and adds options", {
  base <- c("--foo=1", "--bar=2")
  new <- c("--foo=3", "--baz=4")
  result <- set_cli_options(base, new)
  expect_equal(result, c("--foo=3", "--bar=2", "--baz=4"))
})

