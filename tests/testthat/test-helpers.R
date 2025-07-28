test_that("get_nested_values returns expected values for simplify = TRUE and FALSE", {
  nested_list <- list(
    enable = TRUE,
    postproc1 = list(a = 1, b = 2, c = 3),
    postproc2 = list(a = "x", b = "y", c = "z"),
    postproc3 = list(a = 3.5, b = 4.5)
  )
  
  # Test simplify = TRUE with atomic values of same type (numeric)
  result1 <- get_nested_values(nested_list, c("postproc1/a", "postproc1/b", "postproc1/c"), simplify = TRUE)
  expect_type(result1, "double")
  expect_equal(result1, c("postproc1/a" = 1, "postproc1/b" = 2, "postproc1/c" = 3))
  
  # Test simplify = TRUE with mixed types (numeric and character)
  result2 <- get_nested_values(nested_list, c("postproc1/a", "postproc2/a", "postproc3/a"), simplify = TRUE)
  expect_type(result2, "list")
  expect_equal(names(result2), c("postproc1/a", "postproc2/a", "postproc3/a"))
  expect_equal(result2[[1]], 1)
  expect_equal(result2[[2]], "x")
  expect_equal(result2[[3]], 3.5)
  
  # Test simplify = FALSE always returns list
  result3 <- get_nested_values(nested_list, c("postproc1/b", "postproc2/b"), simplify = FALSE)
  expect_type(result3, "list")
  expect_equal(result3[[1]], 2)
  expect_equal(result3[[2]], "y")
  
  # Test with a single key string and atomic return
  result4 <- get_nested_values(nested_list, "postproc3/a", simplify = TRUE)
  expect_type(result4, "double")
  expect_equal(result4, c("postproc3/a" = 3.5))
  
  # Test with a single key string and list return
  result5 <- get_nested_values(nested_list, "postproc3", simplify = TRUE)
  expect_type(result5, "list")
  expect_equal(result5[[1L]], 3.5)
  
  # Test with a missing key
  result6 <- get_nested_values(nested_list, "postproc1/z", simplify = TRUE)
  expect_true(is.na(result6) || is.null(result6[[1]]))
})