test_that("postprocess sentinels clean up array handoff artifacts", {
  script_dir <- test_path("..", "..", "inst", "hpc_scripts")
  scripts <- c("postprocess_sentinel.sbatch", "postprocess_sentinel.pbs")

  for (script in scripts) {
    lines <- readLines(file.path(script_dir, script), warn = FALSE)
    text <- paste(lines, collapse = "\n")

    expect_match(text, "cleanup_postprocess_sidecars\\(\\)")
    expect_match(text, "rm -f \\\"\\$cleanup_filelist_path\\\" \\\"\\$sentinel_meta_file\\\" \\\"\\$sentinel_jid_file\\\" \\\"\\$cleanup_summary_tsv\\\"")
    expect_match(text, "rm -f \\\"\\$cleanup_task_status_dir\\\"/\\*\\.env")
    expect_match(text, "rmdir \\\"\\$cleanup_task_status_dir\\\"")
    expect_match(text, "cleanup_postprocess_sidecars\\n  exit 1", perl = TRUE)
    expect_true(any(grepl("cleanup_postprocess_sidecars", tail(lines, 25))))
  }
})
