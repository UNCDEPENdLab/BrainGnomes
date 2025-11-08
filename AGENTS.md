# Repository Guidelines

## Project Structure & Module Organization

- `R/`: R source files; exported functions documented with roxygen2.
- `src/`: C++ code via Rcpp/RcppArmadillo; compiled during check.
- `tests/testthat/`: unit tests (testthat 3rd edition). Top-level
  `tests/testthat.R` runs the suite.
- `inst/`: scripts and assets (e.g., HPC `.sbatch`, CLI helpers, Python
  utilities).
- `man/`, `NAMESPACE`, `DESCRIPTION`: package metadata and generated
  docs.
- `vignettes/`: long-form examples; built with knitr/rmarkdown.

## Build, Test, and Development Commands

- Install deps: `Rscript -e 'install.packages(c("devtools"))'` (runtime
  deps come from `DESCRIPTION`).
- Document: `Rscript -e 'devtools::document()'` (updates `man/` and
  `NAMESPACE`).
- Test fast: `Rscript -e 'devtools::test()'`.
- Full check: `Rscript -e 'devtools::check()'` or
  `R CMD build . && R CMD check --as-cran BrainGnomes_*.tar.gz`.
- Site/docs: `Rscript -e 'pkgdown::build_site()'` (optional).

## Coding Style & Naming Conventions

- R: 2-space indent, no tabs; prefer `snake_case` for functions/objects;
  internal helpers start with `.`.
- C++ (`src/`): follow tidy, minimal-include style; expose via Rcpp
  attributes; keep headers local.
- Roxygen2 for docs; include `@export`, `@param`, `@return`, and
  examples where practical.
- File names: group by feature (e.g., `utils_misc.R`, `extract_rois.R`).

## Testing Guidelines

- Framework: `testthat` (edition 3). Place tests in `tests/testthat/` as
  `test-*.R`.
- Write small, deterministic tests; avoid HPC or external I/O when
  possible; use temp files.
- Run locally with `devtools::test()`; full CI parity via
  `devtools::check()`.

## Commit & Pull Request Guidelines

- Commits: imperative mood, concise subject (≤72 chars), meaningful body
  when needed.
- Before PR: run `devtools::document()`, `devtools::test()`, and
  `devtools::check()`; update `NEWS.md` for user-facing changes.
- PRs should include: clear description, linked issues, test updates,
  and any relevant screenshots/outputs for changes to CLI or results.

## Security & Configuration Tips

- Do not commit data, PHI, or large binaries; use `.gitignore` and
  `inst/` for example scripts only.
- Python tools use `reticulate`; set `RETICULATE_PYTHON` to your
  environment if needed.
- HPC scripts live in `inst/hpc_scripts/`; validate resources locally or
  on a test queue before wide use.
