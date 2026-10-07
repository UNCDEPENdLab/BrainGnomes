#!/usr/bin/env Rscript
# Minimal tracking bridge: no BrainGnomes namespace or mutable installed helper.
argv <- commandArgs(trailingOnly = TRUE)
if (length(argv) < 5L) stop("Missing worker bootstrap arguments.")
source(argv[[1L]], local = TRUE)
if (argv[[2L]] == "bind") {
  invisible(bind_job_submission(argv[[3L]], argv[[4L]], argv[[5L]]))
} else if (argv[[2L]] == "fail") {
  record_job_bootstrap_failure(argv[[3L]], argv[[4L]], argv[[5L]],
                               as.integer(argv[[6L]]), argv[[7L]],
                               identical(argv[[8L]], "array"))
} else {
  stop("Unknown worker bootstrap operation.")
}
