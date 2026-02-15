# --- get_job_type ---

test_that("get_job_type classifies setup jobs", {
  expect_equal(get_job_type("fsaverage_setup"), "Setup")
  expect_equal(get_job_type("prefetch_templates_MNI"), "Setup")
  expect_equal(get_job_type("flywheel_sync_proj1"), "Setup")
})

test_that("get_job_type classifies pipeline steps", {
  expect_equal(get_job_type("bids_conversion_sub-01"), "BIDS Conversion")
  expect_equal(get_job_type("bids_validation_sub-01"), "BIDS Validation")
  expect_equal(get_job_type("mriqc_sub-01_ses-1"), "MRIQC")
  expect_equal(get_job_type("fmriprep_sub-01_ses-1"), "fMRIPrep")
  expect_equal(get_job_type("aroma_sub-01_ses-1"), "ICA-AROMA")
  expect_equal(get_job_type("extract_rois_sub-01_ses-1"), "ROI Extraction")
})

test_that("get_job_type classifies postprocess streams", {
  expect_equal(get_job_type("postprocess_default_sub-01_ses-1"), "Postprocess default")
  expect_equal(get_job_type("postprocess_gsr_sub-01"), "Postprocess gsr")
})

test_that("get_job_type returns Other for unrecognized names", {
  expect_equal(get_job_type("something_else"), "Other")
  expect_equal(get_job_type("unknown_job_name"), "Other")
})

# --- get_status_symbol ---

test_that("get_status_symbol returns a non-empty string for all known statuses", {
  statuses <- c("COMPLETED", "QUEUED", "STARTED", "FAILED", "FAILED_BY_EXT")
  symbols <- get_status_symbol(statuses)
  expect_length(symbols, length(statuses))
  for (s in symbols) {
    expect_true(nzchar(s))
  }
})

test_that("get_status_symbol handles unknown status with fallback", {
  result <- get_status_symbol("UNKNOWN")
  expect_true(nzchar(result))
  # Should contain "?" as the fallback symbol
  expect_true(grepl("\\?", result))
})

test_that("get_status_symbol is vectorized", {
  result <- get_status_symbol(c("COMPLETED", "FAILED"))
  expect_length(result, 2)
})

# --- get_status_color ---

test_that("get_status_color returns a non-empty string for all known statuses", {
  statuses <- c("COMPLETED", "QUEUED", "STARTED", "FAILED", "FAILED_BY_EXT")
  colored <- get_status_color(statuses)
  expect_length(colored, length(statuses))
  for (s in colored) {
    expect_true(nzchar(s))
  }
})

test_that("get_status_color passes through unrecognized status as-is", {
  result <- get_status_color("UNKNOWN")
  # Should contain the original status text
  expect_true(grepl("UNKNOWN", result))
})

test_that("get_status_color is vectorized", {
  result <- get_status_color(c("COMPLETED", "STARTED", "FAILED"))
  expect_length(result, 3)
})

# --- get_all_nodes ---

test_that("get_all_nodes returns single node for leaf", {
  skip_if_not_installed("data.tree")
  root <- data.tree::Node$new("leaf")
  nodes <- get_all_nodes(root)
  expect_length(nodes, 1)
  expect_equal(nodes[[1]]$name, "leaf")
})

test_that("get_all_nodes collects all nodes in a tree", {
  skip_if_not_installed("data.tree")
  root <- data.tree::Node$new("top")
  child1 <- root$AddChild("child1")
  child2 <- root$AddChild("child2")
  grandchild <- child1$AddChild("grandchild")

  nodes <- get_all_nodes(root)
  node_names <- vapply(nodes, function(n) n$name, character(1))
  expect_length(nodes, 4)
  expect_setequal(node_names, c("top", "child1", "child2", "grandchild"))
})

test_that("get_all_nodes handles deep nesting", {
  skip_if_not_installed("data.tree")
  root <- data.tree::Node$new("L0")
  current <- root
  for (i in seq_len(5)) {
    current <- current$AddChild(paste0("L", i))
  }

  nodes <- get_all_nodes(root)
  expect_length(nodes, 6) # L0 through L5
})

# --- upstream node lookup (FAILED_BY_EXT path) ---

test_that("Traverse with filterFun finds upstream parent by id", {
  skip_if_not_installed("data.tree")
  root <- data.tree::Node$new("sequence")
  parent <- root$AddChild("parent_job")
  parent$id <- "10"
  parent$status <- "FAILED"
  child <- root$AddChild("child_job")
  child$id <- "11"
  child$parent_id <- "10"
  child$status <- "FAILED_BY_EXT"

  # This is the pattern used in diagnose_pipeline for FAILED_BY_EXT lookup
  upstream_matches <- data.tree::Traverse(child$root, filterFun = function(n) {
    !is.null(n$id) && n$id == child$parent_id
  })
  upstream_node <- if (length(upstream_matches) > 0L) upstream_matches[[1]] else NULL

  expect_false(is.null(upstream_node))
  expect_equal(upstream_node$name, "parent_job")
  expect_equal(upstream_node$id, "10")
})

test_that("Traverse returns empty list when parent id not found", {
  skip_if_not_installed("data.tree")
  root <- data.tree::Node$new("sequence")
  child <- root$AddChild("orphan_job")
  child$id <- "20"
  child$parent_id <- "999"  # non-existent

  upstream_matches <- data.tree::Traverse(child$root, filterFun = function(n) {
    !is.null(n$id) && n$id == child$parent_id
  })
  upstream_node <- if (length(upstream_matches) > 0L) upstream_matches[[1]] else NULL

  expect_null(upstream_node)
})

test_that("FindNode with filterFun errors (confirming old API was wrong)", {
  skip_if_not_installed("data.tree")
  root <- data.tree::Node$new("sequence")
  child <- root$AddChild("job1")
  child$id <- "1"

  expect_error(
    data.tree::FindNode(root, filterFun = function(n) !is.null(n$id)),
    "unused argument"
  )
})
