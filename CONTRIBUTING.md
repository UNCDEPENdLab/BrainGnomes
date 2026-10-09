# Contributing

Thanks for your interest in improving BrainGnomes! This guide summarizes how to develop locally, what CI runs, and what we expect in pull requests. For details on structure, style, and commands, see [AGENTS.md](AGENTS.md).

## Getting Started

- Install tooling: `Rscript -e 'install.packages(c("devtools"))'`.
- Set up docs: `Rscript -e 'devtools::document()'`.
- Run tests: `Rscript -e 'devtools::test()'`.
- Site preview: `Rscript -e 'pkgdown::build_site()'` (install `pkgdown` first).
- Full package check: `Rscript -e 'devtools::check()'`.

## Workflow

- Branch naming: `feature/<short-topic>` or `fix/<short-topic>`.
- Keep changes focused and small; include roxygen2 updates with code changes.
- Update every edited vignette to a literal date after its author, using
  `date: "DD Mon YYYY"`. Keep vignette builds self-contained and put article
  images under `vignettes/` so pkgdown can copy them. Keep installed-vignette
  resource paths valid as well.
- Add/adjust tests in `tests/testthat/` (files named `test-*.R`).

## CI Expectations

- GitHub Actions run R CMD check on PRs (`.github/workflows/R-CMD-check.yaml`).
- pkgdown builds documentation on pull requests; successful non-PR runs on the
  main branch or a published release deploy the site (`.github/workflows/pkgdown.yaml`).
- PRs must pass CI and resolve package-check errors and warnings. Report any
  environment limitations explicitly, including missing system utilities.

## Commit & PR Checklist

- Commits: imperative subject (≤72 chars); use a descriptive body when needed.
- Before opening a PR:
  - `devtools::document()` and `devtools::test()` pass locally.
  - `devtools::check()` passes (no ERRORs/WARNINGs; NOTE only when unavoidable and explained).
  - Tests added/updated for behavior changes.
  - `NEWS.md` updated for user-facing changes.
- In the PR description: link related issues, summarize changes, note any interface changes, include brief outputs/screenshots when helpful.

## Security & Data

- Do not commit PHI, study data, or large binaries. The installed placeholder
  configuration is `inst/extdata/example_project_config.yaml`; test fixtures
  belong under `tests/`.

## Reference

- Repo guidelines: `AGENTS.md`
- README and examples: `README.md`
- Package site: https://hallquistlab.github.io/BrainGnomes/
