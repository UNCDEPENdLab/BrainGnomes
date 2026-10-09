# Study QC inventory and dashboard

BrainGnomes can collect existing QC evidence into a study inventory,
export it as TSV tables and an R object, and render a Quarto dashboard.
The dashboard provides searchable React tables, interactive plots, and
expandable details linking outputs to their diagnostics and recorded
workflow attempts.

## Collect a snapshot

``` r

library(BrainGnomes)
scfg <- load_project("/path/to/project")
qc <- collect_qc_inventory(scfg)
qc

# Optional: include participants whose data have not arrived yet.
qc <- collect_qc_inventory(
  scfg,
  subjects = data.frame(sub_id = c("001", "002"), ses_id = "baseline")
)

subset(qc$inventory, output_status == "missing")
subset(qc$workflow, status %in% c("FAILED", "BLOCKED", "NOT_TRACKED"))
```

Collection reads the configuration, tracking database, file metadata,
and small diagnostic files. It does not read image arrays, rerun
processing, submit work, or update the database. By default, job states
come from the tracking database. Use `refresh = TRUE` to also request
the existing read-only scheduler comparison for active jobs. The report
identifies whether that comparison was performed.

The returned `bg_qc_inventory` object includes these tables:

| Table | Meaning |
|----|----|
| `acquisitions` | Discovered raw BOLD files, preserving participant, session, task, run, acquisition, direction, echo, and other filename entities |
| `inventory` | Discovered/expected derivative records by stage and stream, including source/atlas paths, availability, saved audit status, and convenient numeric QC columns |
| `workflow` | Configured participant/session/stage expectations and current tracked work units, including project setup jobs |
| `metrics` | Numeric values with units, definitions, source files, and available/missing observation counts |
| `regions` | Per-label usable voxel proportions, retention status, and recorded exclusion reasons |
| `evidence` | Source/report/diagnostic paths, availability, size, and modification time |
| `checks` | Individual saved postprocessing validation messages and statuses |
| `jobs`, `attempts` | Historical job records and logical attempts, preserving current-attempt indicators |
| `manifest_files` | Exact paths in historical output manifests and current size/mtime comparisons |
| `active`, `reconciliation` | Active-job timing and optional scheduler comparisons |
| `issues` | Collection errors and ambiguous or unsupported evidence |

`record_id` joins derivative-related tables **within a snapshot**.
Snapshot IDs, collection times, schema version, and package version are
in `qc$metadata`. They also travel with the TSV bundle in
`snapshot.json`. Use `readRDS("inventory.rds")` to recover all R column
types. When importing TSVs, read BIDS identifier columns as character
strings to preserve leading zeros.

## Render the dashboard

Rendering requires [Quarto 1.4 or
newer](https://quarto.org/docs/dashboards/) and the optional R packages
below. Inventory collection and TSV/RDS export remain available without
these rendering dependencies.

``` r

install.packages(c("reactable", "plotly", "htmltools", "htmlwidgets", "knitr", "rmarkdown"))

html <- render_qc_dashboard(qc, "/path/to/reports/qc-snapshot-01")
browseURL(html)

# Export only the machine-readable snapshot to a separate directory.
write_qc_inventory(qc, "/path/to/exports/qc-snapshot-01")
```

Each export requires a new or empty directory. Render a new snapshot
when the study changes; an existing report remains a record of what was
observed at its collection time. If Quarto fails, the RDS and TSV
exports remain available and `render.log` contains the failure details.
Use a new directory for a subsequent rendering attempt.

The four dashboard pages cover study progress, derivative exploration,
QC metrics, and workflow/evidence details. Tables use
[reactable](https://glin.github.io/reactable/), with client-side
filtering, sorting, pagination, and expandable rows. Plotly charts
provide hover values and selection among metric/stage/stream/task/atlas
groups. Grouped metric plots deduplicate shared diagnostic files so
multiple derivative representations do not multiply a source
observation. Missing values remain visible in tables.

The HTML embeds its JavaScript and styles and does not require a running
R session, web server, or network connection for those interactions.
Keep the HTML with its neighboring TSV/RDS files for downloads. Evidence
links point to original project files using paths relative to the report
directory: moving only the report does not copy upstream HTML, its
assets, logs, or imaging data. Browser or hosting policies may restrict
links to local files. The table retains the full source paths so users
can still locate the evidence.

## Interpret counts and metrics

Workflow units, acquisitions, and derivative records have different
denominators. A participant-level fMRIPrep job can cover several
sessions and acquisitions. A BOLD acquisition can generate several
spatial representations, postprocessing streams, atlas time series, and
connectivity matrices. The dashboard labels these counts separately.

Expected native derivative paths are resolved from discovered upstream
images and the **current stream configuration**, following the
production filename conventions. Before an upstream image exists, the
configured work remains in the workflow table; output spaces and
multi-echo combinations are not guessed. The subject scope includes
discovered files and BIDS participant directories, tracked subjects,
saved run subject tables, and any explicitly supplied subjects.
Historical scope remains visible even if the current configuration has
changed. Existing native outputs are retained when their upstream images
are no longer available; unavailable stream/source attribution is left
missing.

A file marked `available` exists and is nonempty. A completed job
describes its recorded workflow outcome. A saved audit marked `passed`
describes its recorded computational check. These observations do not
establish scientific image quality. Audit timestamps remain available
because an audit can outlive the derivative it describes. Output
manifests provide exact historical path associations, using size and
modification time checks rather than content hashes. A subsequent failed
attempt does not acquire ownership of an older output.

The `attention` field highlights missing/empty derivatives, invalid or
failed audits, malformed diagnostics, saved ROI loss, shared destination
paths, and failed/blocked/cancelled workflow context. `workflow_status`,
`workflow_scope`, and `workflow_run_id` identify the current
participant/session-level context; they do not identify the producing
attempt for the derivative. It is a navigation aid and does not define
analysis inclusion or exclusion.

- `mean_fd` summarizes finite `framewise_displacement` values in the
  original fMRIPrep confounds TSV, in millimeters. The metric table
  records the finite and missing frame counts. Postprocessed FD may have
  been filtered, demeaned, or censored; it is not substituted for this
  source metric.
- MRIQC IQMs retain separate names: `mriqc_fd_mean`, `mriqc_tsnr`,
  `mriqc_dvars_std`, and `mriqc_gcor`. These are imported summaries with
  their [upstream
  definitions](https://mriqc.readthedocs.io/en/stable/iqms/bold.html).
  Their observation denominators are unavailable when not recorded by
  MRIQC.
- Censor metrics count ones in the saved binary mask, where one means
  retain. A saved mask does not establish that physical volume removal
  was executed.
- ROI metrics summarize the recorded extraction retention rule and
  usable voxel proportions. The regional table preserves label-level
  losses and their recorded reasons, including `outside_mask`,
  `invalid_bold`, and `below_threshold`. Losses are specific to the
  atlas and input stream.

Optional diagnostics are not generated by collection. Enable native
`validate_postproc_steps` or ROI `save_diagnostics` in the processing
configuration when those records are wanted for future processing.
Previously missing evidence remains unavailable. No human ratings,
review lifecycle, inclusion decisions, or automatic motion thresholds
are implemented.

The exports contain participant identifiers and source paths. Share them
with the same access controls as the study’s existing reports.
