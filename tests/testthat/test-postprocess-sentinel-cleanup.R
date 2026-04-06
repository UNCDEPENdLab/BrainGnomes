test_that("postprocess sentinels clean up array handoff artifacts", {
  resolve_pkg_file <- function(inst_rel, fallback_rel) {
    inst_path <- system.file(inst_rel, package = "BrainGnomes")
    if (nzchar(inst_path) && file.exists(inst_path)) {
      return(normalizePath(inst_path, mustWork = TRUE))
    }
    fallback <- do.call(testthat::test_path, as.list(c("..", "..", "inst", fallback_rel)))
    normalizePath(fallback, mustWork = TRUE)
  }

  scripts <- c("postprocess_sentinel.sbatch", "postprocess_sentinel.pbs")

  for (script in scripts) {
    script_path <- resolve_pkg_file(
      inst_rel = file.path("hpc_scripts", script),
      fallback_rel = c("hpc_scripts", script)
    )
    lines <- readLines(script_path, warn = FALSE)
    text <- paste(lines, collapse = "\n")

    expect_match(text, "cleanup_postprocess_sidecars\\(\\)")
    expect_match(text, "rm -f \\\"\\$cleanup_filelist_path\\\" \\\"\\$sentinel_meta_file\\\" \\\"\\$sentinel_jid_file\\\" \\\"\\$cleanup_summary_tsv\\\"")
    expect_match(text, "rm -f \\\"\\$cleanup_task_status_dir\\\"/\\*\\.env")
    expect_match(text, "rmdir \\\"\\$cleanup_task_status_dir\\\"")
    expect_match(text, "cleanup_postprocess_sidecars\\n  exit 1", perl = TRUE)
    expect_true(any(grepl("cleanup_postprocess_sidecars", tail(lines, 25))))
  }
})
