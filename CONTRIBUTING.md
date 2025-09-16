# Contributing

Thanks for your interest in improving BrainGnomes! This guide summarizes how to develop locally, what CI runs, and what we expect in pull requests. For details on structure, style, and commands, see AGENTS.md.

## Getting Started
- Install tooling: `Rscript -e 'install.packages(c("devtools"))'`.
- Set up docs: `Rscript -e 'devtools::document()'`.
- Run tests: `Rscript -e 'devtools::test()'`.
- Full package check: `Rscript -e 'devtools::check()'`.

## Workflow
- Branch naming: `feature/<short-topic>` or `fix/<short-topic>`.
- Keep changes focused and small; include roxygen2 updates with code changes.
- Add/adjust tests in `tests/testthat/` (files named `test-*.R`).

## CI Expectations
- GitHub Actions run R CMD check on PRs (`.github/workflows/R-CMD-check.yaml`).
- Docs build with pkgdown on main (`.github/workflows/pkgdown.yaml`).
- PRs must pass CI and resolve all warnings/errors from `devtools::check()`.

## Commit & PR Checklist
- Commits: imperative subject (≤72 chars); use a descriptive body when needed.
- Before opening a PR:
  - `devtools::document()` and `devtools::test()` pass locally.
  - `devtools::check()` passes (no ERRORs/WARNINGs; NOTE only when unavoidable and explained).
  - Tests added/updated for behavior changes.
  - `NEWS.md` updated for user-facing changes.
- In the PR description: link related issues, summarize changes, note any interface changes, include brief outputs/screenshots when helpful.

## Security & Data
- Do not commit PHI or large binaries. Use example configs only (see `tests/example_config.yaml`).

## Reference
- Repo guidelines: `AGENTS.md`
- README and examples: `README.md`
- Package site: https://uncdependlab.github.io/BrainGnomes/
