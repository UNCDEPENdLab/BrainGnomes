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

## HPC / Container Testing

- Distinguish unit tests from HPC-backed integration tests. Prefer local
  `testthat` tests with mocked scheduler/container behavior for pure R
  logic, and reserve Slurm-backed runs for code paths that truly depend
  on `singularity`/`apptainer`, scheduler environment, or
  compute-node-only filesystems.
- For container-dependent debugging, prefer an interactive Slurm
  allocation via `srun --pty ... bash` over debugging on the login node.
  This is the default way to reproduce and inspect
  `singularity`/`apptainer` behavior incrementally.
- Inside an interactive allocation, run focused tests rather than the
  full suite when possible,
  e.g. `Rscript -e 'devtools::test(filter = "prefetch-templateflow-integration-srun")'`
  or another narrow `filter`.
- Mirror production runtime assumptions when testing interactively: bind
  the same paths, set the same `APPTAINERENV_*` / `SINGULARITYENV_*`
  variables, use the same scratch/log directories, and disable
  networking if the production script does so.
- Treat the batch scripts in `inst/hpc_scripts/` as the source of truth
  for container invocation details. When reproducing
  fMRIPrep/MRIQC/AROMA/FSL behavior, copy the bind mounts, environment
  variables, and container command shape from the relevant
  `.sbatch`/`.pbs` script rather than inventing a simplified command.
- Use `srun --pty` first to isolate runtime/container issues quickly.
  Use `sbatch` afterward when the bug may depend on non-interactive
  scheduler behavior, submission wrappers, or batch-script-only logic.
- Keep HPC-backed integration tests opt-in with explicit environment
  guards and `skip_if()` checks so regular `devtools::test()` remains
  fast and safe on machines without Slurm or container runtimes.
- Follow the pattern in
  `tests/testthat/test-prefetch-templateflow-integration-srun.R` for
  future Slurm-backed integration tests that need a real compute node
  and container runtime.
- A typical interactive workflow is:
  `srun --pty -N 1 -n 1 --mem=16g --time=01:00:00 bash` then load any
  required module/runtime and run a focused
  `Rscript -e 'devtools::test(filter = "...")'` command inside the
  allocation.

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
