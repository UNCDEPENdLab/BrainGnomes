test_that("native interactive menus pass named titles and text-mode settings", {
  local_mocked_bindings(interactive = function() TRUE, .package = "base")
  local_mocked_bindings(
    menu = function(choices, graphics = FALSE, title = NULL) {
      expect_identical(title, "Choose a stream")
      expect_false(graphics)
      2L
    },
    select.list = function(choices, preselect = NULL, multiple = FALSE,
                           title = NULL, graphics = TRUE) {
      expect_identical(title, "Choose a stream")
      expect_false(graphics)
      expect_false(multiple)
      choices[2L]
    },
    .package = "utils"
  )
  expect_identical(menu_safe(c("first", "second"), "Choose a stream"), 2L)
  expect_identical(select_list_safe(c("first", "second"), "Choose a stream"), "second")
})

test_that("output-space editing exits on menu cancellation", {
  local_mocked_bindings(menu_safe = function(...) 0L, .package = "BrainGnomes")
  expect_identical(choose_fmriprep_spaces("T1w fsaverage"), "T1w fsaverage")
})

test_that("text prompts stop cleanly when terminal input is cancelled", {
  local_mocked_bindings(getline = function(...) NULL,
    console_input_available = function() TRUE, .package = "BrainGnomes")
  expect_error(prompt_input(instruct = "Enter a name", type = "character"), "Input cancelled")
  expect_identical(read_multiline_input(), character())
})
