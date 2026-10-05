#' Export a QC inventory as RDS, TSV tables, and a snapshot description
#'
#' @param x A `bg_qc_inventory` returned by [collect_qc_inventory()].
#' @param output_dir A new or empty output directory. Existing snapshots are
#'   protected from replacement; use a new directory when refreshing a report.
#' @return Invisibly returns the absolute path to the exported directory.
#' @details Each data-frame component becomes a TSV with quoted text and `NA`
#'   for missing values. `inventory.rds` preserves the complete R object;
#'   `snapshot.json` records schema, snapshot ID, collection time, and project.
#'   IDs join tables within that snapshot. Reports contain participant IDs and
#'   source paths and should be shared with the same care as their source data.
#' @export
write_qc_inventory <- function(x, output_dir) {
  checkmate::assert_class(x, "bg_qc_inventory")
  assert_provenance_metadata(x, "qc_inventory")
  checkmate::assert_string(output_dir, min.chars = 1L)
  if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE,
      no.. = TRUE))) stop("output_dir must be new or empty; use a new snapshot directory.", call. = FALSE)
  if (!dir.exists(output_dir) && !dir.create(output_dir, recursive = TRUE)) {
    stop("Cannot create QC output directory: ", output_dir, call. = FALSE)
  }
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  for (name in names(x)) if (is.data.frame(x[[name]])) {
    data.table::fwrite(x[[name]], file.path(output_dir, paste0(name, ".tsv")),
      sep = "\t", quote = "auto", na = "NA")
  }
  saveRDS(x, file.path(output_dir, "inventory.rds"))
  jsonlite::write_json(x$metadata, file.path(output_dir, "snapshot.json"),
    pretty = TRUE, auto_unbox = TRUE, na = "null")
  invisible(output_dir)
}

#' Render a Quarto dashboard for a study QC snapshot
#'
#' @param x A `bg_qc_inventory` returned by [collect_qc_inventory()].
#' @param output_dir A new or empty directory for the HTML and its downloadable
#'   RDS/TSV snapshot. Keep these files together when sharing the dashboard.
#' @param title Optional dashboard title. Defaults to the project name.
#' @param quarto Path to the Quarto executable; defaults to the executable on
#'   `PATH`. Quarto 1.4 or newer is required.
#' @param quiet Suppress Quarto progress output.
#' @return Invisibly returns the absolute path to `index.html`.
#' @details Rendering requires optional packages `reactable`, `plotly`,
#'   `htmltools`, `htmlwidgets`, `knitr`, and `rmarkdown`. Collection and TSV/RDS
#'   export do not require these packages or Quarto. JavaScript and styles are
#'   embedded in the HTML; search, filtering, row details, and plots work without
#'   a server or network connection. Linked evidence remains in its source
#'   location, using paths relative to the dashboard. Moving only the dashboard
#'   preserves its displayed snapshot but does not copy upstream reports, logs,
#'   or images. No human ratings or analysis decisions are collected.
#' @seealso [collect_qc_inventory()], [write_qc_inventory()]
#' @export
render_qc_dashboard <- function(x, output_dir, title = NULL,
                                quarto = Sys.which("quarto"), quiet = TRUE) {
  checkmate::assert_class(x, "bg_qc_inventory")
  checkmate::assert_string(title, null.ok = TRUE)
  checkmate::assert_flag(quiet)
  needed <- c("reactable", "plotly", "htmltools", "htmlwidgets", "knitr", "rmarkdown")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install dashboard packages: ", paste(missing, collapse = ", "), call. = FALSE)
  if (!checkmate::test_string(quarto) || !nzchar(quarto) || !file.exists(quarto)) {
    stop("Quarto >= 1.4 is required to render HTML. TSV/RDS export is available with write_qc_inventory().", call. = FALSE)
  }
  version <- system2(quarto, "--version", stdout = TRUE, stderr = TRUE)
  if (!length(version) || !grepl("^[0-9]+\\.[0-9]+", version[1L]) ||
      utils::compareVersion(version[1L], "1.4") < 0L) {
    stop("Quarto >= 1.4 is required for dashboards.", call. = FALSE)
  }
  output_dir <- write_qc_inventory(x, output_dir)
  if (is.null(title)) title <- paste(x$metadata$project, "| Study QC")
  widgets <- qc_dashboard_widgets(x, output_dir)
  # Serialize already-built widgets: Quarto can render the snapshot without
  # reconnecting to SQLite, rescanning data, or loading a different package build.
  saveRDS(widgets, file.path(output_dir, "dashboard-widgets.rds"))
  asset_dir <- system.file("qc_dashboard", package = "BrainGnomes", mustWork = TRUE)
  file.copy(file.path(asset_dir, c("dashboard.css", "theme.scss")), output_dir)
  template <- readLines(file.path(asset_dir, "dashboard.qmd"), warn = FALSE)
  # Quarto uses YAML 1.2 booleans; R's YAML emitter otherwise writes yes/no.
  # Only the user-supplied title needs YAML escaping; the remaining header is fixed.
  header <- c("---", yaml::as.yaml(list(title = title)),
    "format:", "  dashboard:", "    theme: [cosmo, theme.scss]", "    orientation: rows",
    "    embed-resources: true", "    css: dashboard.css", "    scrolling: true",
    "execute:", "  echo: false", "  warning: false", "  message: false",
    "params:", "  snapshot: inventory.rds", "  widgets: dashboard-widgets.rds", "---")
  qmd <- file.path(output_dir, "dashboard.qmd")
  writeLines(c(header, template), qmd)
  # Use the same R installation and library paths as the calling session.
  old_r <- Sys.getenv("QUARTO_R", unset = NA_character_)
  old_libs <- Sys.getenv("R_LIBS", unset = NA_character_)
  old_tmpdir <- Sys.getenv("TMPDIR", unset = NA_character_)
  on.exit({
    if (is.na(old_r)) Sys.unsetenv("QUARTO_R") else Sys.setenv(QUARTO_R = old_r)
    if (is.na(old_libs)) Sys.unsetenv("R_LIBS") else Sys.setenv(R_LIBS = old_libs)
    if (is.na(old_tmpdir)) Sys.unsetenv("TMPDIR") else Sys.setenv(TMPDIR = old_tmpdir)
  }, add = TRUE)
  Sys.setenv(QUARTO_R = R.home("bin"), R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  # R may have fallen back from an unavailable cluster TMPDIR. Give Quarto
  # that same writable temporary directory rather than its stale environment.
  Sys.setenv(TMPDIR = tempdir())
  args <- c("render", shQuote(qmd), "--output", "index.html", "--execute-dir",
    shQuote(output_dir), if (quiet) "--quiet")
  log <- file.path(output_dir, "render.log")
  # Quarto's resource bundler also resolves generated JavaScript from the CLI
  # working directory, independently of the R chunk execution directory.
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(output_dir)
  status <- system2(quarto, args, stdout = log, stderr = log)
  if (!quiet) cat(readLines(log, warn = FALSE), sep = "\n")
  html <- file.path(output_dir, "index.html")
  if (!identical(status, 0L) || !file.exists(html)) {
    stop("Quarto rendering failed. The TSV/RDS snapshot is retained; see ", log, call. = FALSE)
  }
  invisible(html)
}

#' Compute portable, URL-encoded paths relative to the dashboard directory
#' @keywords internal
#' @noRd
qc_evidence_href <- function(path, output_dir) {
  target <- strsplit(normalizePath(path, winslash = "/", mustWork = FALSE), "/", fixed = TRUE)[[1L]]
  base <- strsplit(normalizePath(output_dir, winslash = "/", mustWork = FALSE), "/", fixed = TRUE)[[1L]]
  common <- 0L
  while (common < min(length(target), length(base)) &&
      identical(target[common + 1L], base[common + 1L])) common <- common + 1L
  parts <- c(rep("..", length(base) - common), target[seq_along(target) > common])
  paste(vapply(parts, utils::URLencode, character(1), reserved = TRUE), collapse = "/")
}

#' Render safe evidence links and visibly distinguish missing targets
#' @keywords internal
#' @noRd
qc_evidence_links <- function(evidence, output_dir) {
  if (!nrow(evidence)) return(htmltools::tags$p(class = "qc-muted", "No linked evidence available."))
  htmltools::tags$ul(class = "qc-evidence", lapply(seq_len(nrow(evidence)), function(i) {
    item <- evidence[i, , drop = FALSE]
    label <- paste(gsub("_", " ", item$kind), "\u00b7", basename(item$path))
    htmltools::tags$li(if (isTRUE(item$exists)) {
      htmltools::tags$a(href = qc_evidence_href(item$path, output_dir),
        target = "_blank", rel = "noopener", label)
    } else htmltools::tags$span(class = "qc-muted", paste("Missing:", label)),
      htmltools::tags$small(class = "qc-path", item$path))
  }))
}

#' Build a consistently styled React table with optional expandable details
#' @keywords internal
#' @noRd
qc_reactable <- function(data, columns = NULL, details = NULL, page_size = 12L) {
  rownames(data) <- NULL
  labels <- c(sub_id = "Participant", ses_id = "Session", run_id = "Workflow run",
    output_status = "File", diagnostics_status = "Diagnostics", mean_fd = "Mean FD (mm)")
  defaults <- stats::setNames(lapply(names(data), function(name) {
    label <- if (name %in% names(labels)) labels[[name]] else tools::toTitleCase(gsub("_", " ", name))
    definition <- reactable::colDef(name = label, na = "Unavailable", minWidth = 110,
      cell = if (is.character(data[[name]])) qc_text_cell else NULL,
      style = list(whiteSpace = "nowrap", overflow = "hidden", textOverflow = "ellipsis"),
      headerStyle = list(whiteSpace = "nowrap"))
    # Apply missing-value text only to real data columns. Applying it through
    # defaultColDef also labels React's synthetic row-expander column.
    override <- columns[[name]]
    if (!is.null(override)) definition <- utils::modifyList(definition,
      override[!vapply(override, is.null, logical(1))])
    definition
  }), names(data))
  reactable::reactable(data, columns = defaults, details = details,
    searchable = TRUE, filterable = TRUE, highlight = TRUE, striped = FALSE, wrap = FALSE,
    showPageSizeOptions = TRUE, defaultPageSize = page_size,
    pageSizeOptions = unique(c(page_size, 25L, 50L)), showSortable = TRUE,
    defaultColDef = reactable::colDef(minWidth = 110),
    theme = reactable::reactableTheme(color = "#183044", backgroundColor = "#ffffff",
      borderColor = "#e3e9ee", highlightColor = "#eaf5f4",
      headerStyle = list(background = "#f3f7f9", fontWeight = 600),
      searchInputStyle = list(borderRadius = "8px")),
    language = reactable::reactableLang(searchPlaceholder = "Search this table...",
      noData = "No records in this snapshot"))
}

#' Keep long table text compact while making the complete value available on hover
#' @keywords internal
#' @noRd
qc_text_cell <- function(value) {
  value <- qc_scalar(value)
  if (is.na(value)) return("Unavailable")
  htmltools::tags$span(title = value, value)
}

#' Give status values readable badges without equating availability with quality
#' @keywords internal
#' @noRd
qc_status_badge <- function(value) {
  tone <- if (value %in% c("COMPLETED", "available", "passed", "matches_size_and_mtime")) "teal"
    else if (value %in% c("FAILED", "BLOCKED", "failed", "error", "invalid", "missing", "changed")) "red"
    else "neutral"
  htmltools::tags$span(class = paste("qc-badge", paste0("qc-", tone)),
    gsub("_", " ", value))
}

#' Summarize tracked and expected workflow units in a stacked status chart
#' @keywords internal
#' @noRd
qc_workflow_plot <- function(workflow) {
  if (!nrow(workflow)) return(htmltools::tags$p("No configured or tracked workflow units yet."))
  labels <- paste(workflow$stage, ifelse(is.na(workflow$stream), "", workflow$stream))
  counts <- as.data.frame(table(stage = labels, status = workflow$status), stringsAsFactors = FALSE)
  counts <- counts[counts$Freq > 0L, , drop = FALSE]
  colors <- c(COMPLETED = "#168b80", RUNNING = "#468acf", QUEUED = "#93bfe8",
    FAILED = "#cf5360", BLOCKED = "#b36e35", CANCELLED = "#9b779d",
    NOT_TRACKED = "#c8d1d9", UNKNOWN = "#8a98a5")
  plot <- plotly::plot_ly(x = counts$Freq, y = counts$stage, color = counts$status,
    colors = colors, type = "bar", orientation = "h", hovertemplate = "%{y}<br>%{x} workflow units<extra></extra>")
  plotly::layout(plot, barmode = "stack", xaxis = list(title = "Workflow units"),
    yaxis = list(title = "", automargin = TRUE), margin = list(l = 20, r = 20, t = 12, b = 40),
    paper_bgcolor = "transparent", plot_bgcolor = "transparent",
    legend = list(orientation = "h", y = -0.2))
}

#' Plot comparable metric groups with an explicit selector and denominators
#' @keywords internal
#' @noRd
qc_metrics_plot <- function(metrics, inventory) {
  if (!nrow(metrics)) return(htmltools::tags$p("No numeric QC evidence available. Missing diagnostics are listed in the inventory."))
  joined <- merge(metrics, inventory[c("record_id", "sub_id", "ses_id", "run", "stage",
    "stream", "input_stream", "atlas", "task", "space", "resolution")],
    by = "record_id", all.x = TRUE, sort = FALSE)
  # The same confounds/ROI table can support several spaces or output products.
  # Plot each evidence source once within its stream/atlas/task group.
  group_columns <- c("metric", "stage", "stream", "input_stream", "task", "space", "resolution", "atlas")
  joined <- joined[!duplicated(joined[c("source_file", group_columns)]), ]
  joined <- joined[is.finite(joined$value), , drop = FALSE]
  if (!nrow(joined)) return(htmltools::tags$p("Metric files were found, but all values are unavailable."))
  group_fields <- joined[group_columns]
  # Archived upstream inputs can leave observed ROI products without a known
  # atlas path. Keep their diagnostic directories separate instead of pooling
  # every unknown atlas into one distribution.
  unknown_atlas <- joined$stage == "extract_rois" & is.na(joined$atlas)
  group_fields$atlas[unknown_atlas] <- dirname(joined$source_file[unknown_atlas])
  group_fields[] <- lapply(group_fields, function(x) ifelse(is.na(x), "", x))
  groups <- do.call(paste, c(group_fields, sep = " / "))
  labels <- unique(groups)
  # Keep full paths in grouping keys, but shorten the visible selector labels.
  # Distinct full atlas identities still select distinct traces.
  display_labels <- vapply(labels, function(label) {
    parts <- strsplit(label, " / ", fixed = TRUE)[[1L]]
    paste(basename(parts[nzchar(parts)]), collapse = " / ")
  }, character(1))
  display_labels <- make.unique(display_labels)
  plot <- plotly::plot_ly()
  for (i in seq_along(labels)) {
    df <- joined[groups == labels[i], , drop = FALSE]
    hover <- paste0("sub-", df$sub_id, " | session ", df$ses_id, " | run ", df$run,
      "<br>", df$value, " ", df$unit, "<br>Observed: ", df$n_observations,
      "; missing: ", df$n_missing)
    plot <- plotly::add_trace(plot, x = df$sub_id, y = df$value,
      type = "scatter", mode = "markers", name = labels[i], text = hover,
      hoverinfo = "text", visible = i == 1L,
      marker = list(color = "#168b80", size = 10, opacity = 0.85))
  }
  buttons <- lapply(seq_along(labels), function(i) list(method = "restyle",
    args = list("visible", seq_along(labels) == i), label = display_labels[i]))
  plotly::layout(plot, showlegend = FALSE,
    xaxis = list(title = "Participant", type = "category"),
    yaxis = list(title = "Value (units shown on hover)", rangemode = "tozero"),
    updatemenus = list(list(buttons = buttons, x = 0, y = 1.22, xanchor = "left")),
    margin = list(t = 85, b = 55, l = 65, r = 15),
    paper_bgcolor = "transparent", plot_bgcolor = "transparent")
}

#' Build all dashboard components from one immutable inventory snapshot
#' @keywords internal
#' @noRd
qc_dashboard_widgets <- function(x, output_dir) {
  inventory <- x$inventory
  status_column <- reactable::colDef(cell = qc_status_badge, minWidth = 135)
  visible <- c("sub_id", "ses_id", "task", "run", "stage", "stream", "space",
    "workflow_status", "output_status", "validation_status", "diagnostics_status", "attention")
  columns <- stats::setNames(lapply(names(inventory), function(name) {
    if (name == "validation_status") reactable::colDef(name = "Saved audit", cell = qc_status_badge, minWidth = 135)
    else if (name == "workflow_status") reactable::colDef(name = "Workflow context", cell = qc_status_badge, minWidth = 145)
    else if (name %in% c("output_status", "diagnostics_status")) status_column
    else reactable::colDef(show = name %in% visible, minWidth = 110)
  }), names(inventory))
  details <- function(index) {
    row <- inventory[index, , drop = FALSE]
    evidence <- x$evidence[!is.na(x$evidence$record_id) & x$evidence$record_id == row$record_id, ]
    metrics <- x$metrics[x$metrics$record_id == row$record_id, , drop = FALSE]
    checks <- x$checks[x$checks$record_id == row$record_id, , drop = FALSE]
    # Show exact manifest associations separately from broad workflow context.
    manifests <- x$manifest_files[x$manifest_files$path == row$derivative_file, , drop = FALSE]
    htmltools::tags$div(class = "qc-detail",
      htmltools::tags$h4(basename(row$derivative_file)),
      htmltools::tags$p("Source: ", row$source_file),
      htmltools::tags$p("Atlas: ", row$atlas, " | Modified: ", row$modified_at),
      htmltools::tags$p("Input stream: ", row$input_stream,
        " | Destination association: ", row$association_status),
      htmltools::tags$p("Workflow scope: ", row$workflow_scope,
        " | Current workflow run: ", row$workflow_run_id,
        ". This is workflow context, not an assertion of which attempt produced this file."),
      qc_evidence_links(evidence, output_dir),
      if (nrow(metrics)) qc_reactable(metrics[c("metric", "value", "unit", "definition", "n_observations", "n_missing")], page_size = 5L),
      if (nrow(checks)) qc_reactable(checks[c("step", "status", "message")], page_size = 5L),
      if (nrow(manifests)) qc_reactable(manifests[c("job_id", "run_id", "attempt", "is_current_attempt", "status")], page_size = 5L))
  }
  job_fields <- c("job_id", "sequence_id", "sub_id", "ses_id", "stage", "stream",
    "attempt", "lifecycle_status", "is_current_attempt", "failure_category", "exit_code")
  job_details <- function(index) {
    job <- x$jobs[index, , drop = FALSE]
    fields <- c("stdout_log", "stderr_log", "job_manifest_path", "runtime_receipt_path")
    paths <- vapply(fields, function(field) qc_scalar(job[[field]]), character(1))
    paths <- paths[!is.na(paths) & nzchar(paths)]
    if (!is.na(job$sequence_id)) paths <- c(paths, run_provenance = file.path(
      x$metadata$log_directory, "runs", job$sequence_id, "provenance.json"))
    evidence <- data.frame(kind = names(paths), path = unname(paths), exists = file.exists(paths))
    qc_evidence_links(evidence, output_dir)
  }
  evidence_cols <- list(path = reactable::colDef(minWidth = 400,
    cell = function(value, index) {
      if (isTRUE(x$evidence$exists[index])) htmltools::tags$a(
        href = qc_evidence_href(value, output_dir), target = "_blank", rel = "noopener", value)
      else value
    }))
  list(inventory = qc_reactable(inventory, columns, details),
    acquisitions = qc_reactable(x$acquisitions),
    workflow = qc_reactable(x$workflow, list(status = status_column)),
    workflow_plot = qc_workflow_plot(x$workflow),
    metrics = qc_reactable(merge(x$metrics, inventory[c("record_id", "sub_id", "ses_id", "task", "run", "stage", "stream", "input_stream", "atlas")],
      by = "record_id", all.x = TRUE)),
    metric_plot = qc_metrics_plot(x$metrics, inventory),
    evidence = qc_reactable(x$evidence, evidence_cols),
    checks = qc_reactable(x$checks),
    regions = qc_reactable(merge(x$regions,
      inventory[c("record_id", "sub_id", "ses_id", "run", "task", "stream", "input_stream", "atlas")],
      by = "record_id", all.x = TRUE)),
    jobs = qc_reactable(x$jobs[job_fields], details = job_details),
    manifests = qc_reactable(x$manifest_files),
    issues = qc_reactable(x$issues),
    active = qc_reactable(x$active), reconciliation = qc_reactable(x$reconciliation))
}
