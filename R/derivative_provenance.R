# Portable records are separate from the project database. A sidecar describes
# the producer of a file; attempts to reuse or replace it have their own records.
#' Resolve derivative companion locations
#'
#' @param file Derivative NIfTI or TSV path, which need not exist.
#' @return Named list of sidecar and companion-directory paths.
#' @noRd
derivative_provenance_paths <- function(file) {
  stem <- sub("\\.(nii(\\.gz)?|tsv(\\.gz)?)$", "", file, ignore.case = TRUE)
  list(json = paste0(stem, ".json"), directory = paste0(stem, ".provenance"))
}

#' Read an optional provenance JSON file
#'
#' @param file Candidate JSON path; a missing file is allowed.
#' @return Parsed list, or NULL for an unavailable file. Malformed JSON raises an error.
#' @noRd
derivative_provenance_read_json <- function(file) {
  if (!checkmate::test_file_exists(file)) return(NULL)
  jsonlite::read_json(file, simplifyVector = FALSE)
}

#' Identify a source or output using the run-provenance fingerprint format
#'
#' @param file File to identify.
#' @param role Semantic role within the processing record.
#' @param checksum_cache Optional run-compatible checksum cache path.
#' @return Named list with original path, size, timestamp, and MD5 checksum.
#' @noRd
derivative_provenance_fingerprint <- function(file, role = "source", checksum_cache = NULL) {
  row <- fingerprint_run_artifact(role, file, checksum_cache)
  as.list(row[1L, , drop = FALSE])
}

#' Preserve JSON arrays when they contain zero or one elements
#'
#' @param value Vector of values in their intended order.
#' @return Unnamed list, serialized as a JSON array.
#' @noRd
derivative_provenance_array <- function(value) unname(as.list(value))

#' Capture image geometry without loading voxel data
#'
#' @param file Candidate NIfTI path.
#' @return NIfTI dimension, spacing, unit, and transform fields, or NULL if unavailable.
#' @noRd
derivative_provenance_geometry <- function(file) {
  if (!grepl("\\.nii(\\.gz)?$", file)) return(NULL)
  suppressWarnings(tryCatch({
    h <- RNifti::niftiHeader(file)
    h[c("dim", "pixdim", "xyzt_units", "qform_code", "sform_code",
        "quatern_b", "quatern_c", "quatern_d", "qoffset_x", "qoffset_y",
        "qoffset_z", "srow_x", "srow_y", "srow_z")]
  }, error = function(e) NULL))
}

#' Describe a published intensity reference without embedding voxel arrays
#'
#' @param reference Runtime intensity-reference list, possibly containing a map.
#' @param core_file Published reference-core NIfTI path.
#' @param sidecar_file Published intensity-reference JSON path.
#' @param scale_file Published PSC multiplier NIfTI path, if available.
#' @return Compact QA/settings list with relative companion paths and scale-map
#'   header geometry. Temporary automasks and in-memory images are excluded.
#' @noRd
intensity_reference_provenance <- function(reference, core_file, sidecar_file,
                                           scale_file) {
  reference$scale_map <- NULL
  reference$automask_file <- NULL
  reference$core_file <- basename(core_file)
  reference$sidecar_file <- basename(sidecar_file)
  reference$scale_file <- if (checkmate::test_file_exists(scale_file)) {
    basename(scale_file)
  } else NULL
  reference$scale_geometry <- if (!is.null(reference$scale_file)) {
    derivative_provenance_geometry(scale_file)
  } else NULL
  reference
}

#' Locate the nearest containing dataset description
#'
#' @param file Source or derivative path.
#' @return Absolute dataset directory, or NULL if no ancestor describes a dataset.
#' @noRd
derivative_provenance_dataset_root <- function(file) {
  path <- normalizePath(dirname(file), winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(path, "dataset_description.json"))) return(path)
    parent <- dirname(path)
    if (identical(path, parent)) return(NULL)
    path <- parent
  }
}

#' Identify the native R environment and available FSL executables
#'
#' @return Run-compatible software identities plus local FSL paths/version when
#'   discoverable without executing a tool. Configuration does not prove use.
#' @noRd
derivative_provenance_software <- function() {
  identity <- run_software_identity()
  fsl_dir <- Sys.getenv("FSLDIR", unset = "")
  version_file <- if (nzchar(fsl_dir)) file.path(fsl_dir, "etc", "fslversion") else ""
  identity$AvailableExternalTools <- list(FSL = list(
    Executables = as.list(Sys.which(c("fslmaths", "susan"))),
    Version = if (checkmate::test_file_exists(version_file)) {
      paste(readLines(version_file, warn = FALSE), collapse = " ")
    } else NULL
  ))
  identity
}

#' Embed available execution records without querying SQLite
#'
#' @return Run, work-unit attempt, manifest, receipt, and scheduler-task identities.
#'   Missing records remain NULL; a receipt describes startup verification,
#'   not successful completion of the derivative.
#' @noRd
derivative_provenance_context <- function() {
  manifest_file <- Sys.getenv("BG_JOB_MANIFEST", unset = "")
  manifest <- derivative_provenance_read_json(manifest_file)
  receipt_file <- if (nzchar(manifest_file)) {
    file.path(dirname(manifest_file), "runtime-receipt.json")
  } else ""
  receipt <- derivative_provenance_read_json(receipt_file)
  run_file <- if (nzchar(manifest_file)) {
    file.path(dirname(dirname(dirname(manifest_file))), "provenance.json")
  } else ""
  run <- derivative_provenance_read_json(run_file)
  list(
    RunID = manifest$run_id,
    WorkUnit = manifest$logical_work_unit,
    Attempt = manifest$logical_work_unit$attempt,
    ContractID = manifest$contract_id,
    Manifest = if (!is.null(manifest)) list(
      File = derivative_provenance_fingerprint(manifest_file, "job_manifest"),
      Record = manifest[c("schema_version", "contract_id", "run_id",
                          "logical_work_unit", "scheduler", "artifacts")]
    ) else NULL,
    Receipt = if (!is.null(receipt)) list(
      File = derivative_provenance_fingerprint(receipt_file, "runtime_receipt"), Record = receipt
    ) else NULL,
    Run = if (!is.null(run)) list(
      File = derivative_provenance_fingerprint(run_file, "run_provenance"),
      Request = run$request, Software = run$software,
      Configuration = run$configuration[c(
        "snapshot_file", "snapshot_checksum_algorithm", "snapshot_checksum"
      )]
    ) else NULL,
    SchedulerTask = as.list(Sys.getenv(c(
      "SLURM_JOB_ID", "SLURM_ARRAY_JOB_ID", "SLURM_ARRAY_TASK_ID",
      "PBS_JOBID", "PBS_ARRAYID", "PBS_ARRAY_INDEX"
    ), unset = NA_character_))
  )
}

# Only report/citation assets are copied, never the upstream imaging dataset.
# Relative HTML/CSS resources remain relative to their original report root.
#' Collect supplied upstream citations and report resources
#'
#' @param file Input derivative path.
#' @param metadata Parsed input sidecar, if present.
#' @return Named list mapping bundle-relative asset paths to original files.
#' @noRd
derivative_provenance_upstream_assets <- function(file, metadata) {
  files <- list()
  # Register an existing upstream asset. `path` is the candidate original file;
  # `relative` is its preserved path within the companion directory. Missing
  # optional assets are ignored. Updates the map and returns NULL invisibly.
  add <- function(path, relative) {
    if (checkmate::test_file_exists(path)) {
      files[[relative]] <<- normalizePath(path, winslash = "/", mustWork = TRUE)
    }
    invisible(NULL)
  }
  sidecar <- derivative_provenance_paths(file)$json
  add(sidecar, file.path("upstream", basename(sidecar)))
  if (!is.null(metadata$BrainGnomes)) {
    derivative_provenance_check_companions(file, metadata)
    for (asset in metadata$BrainGnomes$Assets) {
      add(file.path(dirname(file), asset$File),
          file.path("upstream", asset$File))
    }
    return(files)
  }
  root <- derivative_provenance_dataset_root(file)
  if (is.null(root)) return(files)
  add(file.path(root, "dataset_description.json"),
      "upstream/dataset_description.json")
  for (ext in c("bib", "md", "html", "tex")) {
    for (dir in c(root, file.path(root, "logs"))) {
      path <- file.path(dir, paste0("CITATION.", ext))
      add(path, file.path("upstream", substring(path, nchar(root) + 2L)))
    }
  }
  subject <- as.list(extract_bids_info(file))$subject
  if (checkmate::test_string(subject) && !is.na(subject)) {
    report <- file.path(root, paste0("sub-", subject, ".html"))
    add(report, file.path("upstream", basename(report)))
    # Follow local resources referenced by reports, including CSS dependencies.
    queue <- report[file.exists(report)]
    seen <- character()
    while (length(queue)) {
      path <- queue[[1L]]
      queue <- queue[-1L]
      if (path %in% seen) next
      seen <- c(seen, path)
      if (!grepl("\\.(html|css|svg)$", path, ignore.case = TRUE)) next
      content <- paste(readLines(path, warn = FALSE), collapse = "\n")
      matches <- regmatches(content, gregexpr(
        "(?:src|href|data)\\s*=\\s*[\"'][^\"']+[\"']|url\\([^)]*\\)",
        content, perl = TRUE, ignore.case = TRUE
      ))[[1L]]
      targets <- sub("^(?:src|href|data)\\s*=\\s*[\"']|^url\\([\"']?", "",
        matches, perl = TRUE, ignore.case = TRUE)
      targets <- sub("[\"')]+$", "", targets)
      for (target in targets) {
        if (grepl("^([[:alnum:]+.-]+:|/|#)", target)) next
        target <- utils::URLdecode(sub("[?#].*$", "", target))
        # Reports may also link to downloadable data. Only report resources
        # belong in this bundle; never follow a link to an imaging dataset.
        if (!grepl("\\.(html|css|js|json|svg|png|jpe?g|gif|ico|woff2?|ttf|webp)$",
                   target, ignore.case = TRUE)) next
        resolved <- normalizePath(file.path(dirname(path), target),
                                  winslash = "/", mustWork = FALSE)
        if (startsWith(resolved, paste0(root, "/")) && file.exists(resolved) &&
            !dir.exists(resolved)) {
          add(resolved, file.path("upstream", substring(resolved, nchar(root) + 2L)))
          queue <- c(queue, resolved)
        }
      }
    }
  }
  files
}

#' Initialize a mutable collector for one derivative attempt
#'
#' @param file Intended output derivative path.
#' @param source Input image path, or NULL when unavailable.
#' @param stage Native stage label.
#' @param requested Selected requested processing settings.
#' @return Environment containing the evolving record, source metadata, and assets.
#' @noRd
derivative_provenance_new <- function(file, source, stage, requested = list()) {
  tracker <- new.env(parent = emptyenv())
  tracker$file <- file
  tracker$id <- uuid::UUIDgenerate()
  tracker$assets <- list()
  tracker$metadata <- if (!is.null(source)) derivative_provenance_read_json(derivative_provenance_paths(source)$json) else NULL
  if (!is.null(tracker$metadata$BrainGnomes)) {
    read_derivative_provenance(source) # Refuse a stale producer record.
  }
  if (!is.null(source)) tracker$assets <- derivative_provenance_upstream_assets(source, tracker$metadata)
  tracker$record <- list(
    SchemaVersion = "brain-gnomes-derivative-v1", RecordID = tracker$id,
    RecordedAt = run_provenance_timestamp(), Stage = stage, Status = "requested",
    Requested = requested, Resolved = list(), Operations = list(),
    SourceFiles = if (!is.null(source)) list(derivative_provenance_fingerprint(source, "bold")) else list(),
    SourceMetadata = tracker$metadata,
    SourceDataset = if (!is.null(source)) {
      root <- derivative_provenance_dataset_root(source)
      if (!is.null(root)) derivative_provenance_read_json(file.path(root, "dataset_description.json"))
    } else NULL,
    Software = derivative_provenance_software(), Execution = derivative_provenance_context(),
    InputGeometry = if (!is.null(source)) derivative_provenance_geometry(source) else NULL,
    HumanReviewRequired = TRUE
  )
  tracker
}

#' Add resolved input file identities to an attempt
#'
#' @param tracker Mutable derivative collector.
#' @param files Named list of input paths; absent optional files are ignored.
#' @return NULL invisibly; updates the collector in place.
#' @noRd
derivative_provenance_sources <- function(tracker, files) {
  for (name in names(files)) {
    file <- files[[name]]
    if (checkmate::test_file_exists(file)) {
      tracker$record$SourceFiles[[length(tracker$record$SourceFiles) + 1L]] <-
        derivative_provenance_fingerprint(file, name)
    }
  }
  invisible(NULL)
}

#' Record an operation transition while preserving sequence order
#'
#' @param tracker Mutable derivative collector.
#' @param name Operation name.
#' @param status Requested, running, executed, skipped, failed, or reused state.
#' @param parameters Resolved parameters or, for reuse, the current request only.
#' @param origin Optional identity of a reused intermediate.
#' @param message Optional explanation of the operation state.
#' @return NULL invisibly; updates an active operation or appends a new occurrence.
#' @noRd
derivative_provenance_operation <- function(tracker, name, status, parameters = list(),
                          origin = NULL, message = NULL) {
  # Complete only the active occurrence; forced sequences may repeat a step.
  index <- length(tracker$record$Operations) + 1L
  if (index > 1L) {
    last <- tracker$record$Operations[[index - 1L]]
    if (identical(last$Name, name) && last$Status %in% c("requested", "running")) {
      index <- index - 1L
    }
  }
  tracker$record$Operations[[index]] <- list(
    Name = name, Status = status, Parameters = parameters,
    ParameterEvidence = if (status == "reused") "requested_only" else "runtime",
    Origin = origin, Message = message
  )
  invisible(NULL)
}

#' Persist an attempt separately from the derivative producer record
#'
#' @param tracker Mutable derivative collector.
#' @param status Overall attempt outcome.
#' @param error Failure or skip explanation, if applicable.
#' @return Path to the attempt JSON. Failed attempts mark active operations failed.
#' @noRd
derivative_provenance_attempt <- function(tracker, status, error = NULL) {
  record <- tracker$record
  record$Status <- status
  record$Error <- error
  record$IntendedDerivative <- normalizePath(tracker$file, winslash = "/", mustWork = FALSE)
  # An operation that returned successfully remains executed even if a later
  # operation or validation failed. Only in-flight work becomes failed.
  if (status == "failed") {
    record$Operations <- lapply(record$Operations, function(op) {
      if (identical(op$Status, "running")) { op$Status <- "failed"; op$Message <- error }
      op
    })
  }
  paths <- derivative_provenance_paths(tracker$file)
  file <- file.path(paths$directory, paste0("attempt-", tracker$id, ".json"))
  write_json_atomic(record, file)
  file
}

#' Embed the resolved censor vector as retained and excluded indices
#'
#' @param file Censor text file containing one zero or one per volume.
#' @param n_input Optional count of input image volumes.
#' @return List with availability, file identity, convention, and one-based indices.
#' @noRd
derivative_provenance_censor <- function(file, n_input = NULL) {
  if (!checkmate::test_file_exists(file)) return(list(Available = FALSE))
  values <- suppressWarnings(as.integer(readLines(file, warn = FALSE)))
  if (anyNA(values) || any(!values %in% c(0L, 1L))) stop("Invalid censor vector: ", file)
  list(
    Available = TRUE, File = derivative_provenance_fingerprint(file, "censor"),
    Convention = "1 = retained; 0 = censored", IndexBase = 1L,
    InputVolumes = n_input, CensorVolumes = length(values),
    RetainedIndices = derivative_provenance_array(which(values == 1L)),
    CensoredIndices = derivative_provenance_array(which(values == 0L))
  )
}

#' Describe recorded execution for human review
#'
#' @param record Resolved producer record; requested settings alone are insufficient.
#' @return Draft methods paragraph with explicit reuse and validation limitations.
#' @noRd
derivative_provenance_methods <- function(record) {
  version <- record$Software$braingnomes$version
  if (identical(record$Status, "unknown")) {
    return("This derivative was reused without a verified producer record. Its generating settings and original sources are unknown; the current request is not evidence of how it was produced.")
  }
  sentences <- paste0("Native ", record$Stage, " was performed with BrainGnomes ", version, ".")
  for (op in record$Operations) {
    p <- op$Parameters
    if (op$Status != "executed") {
      sentences <- c(sentences, paste0(
        "Operation ", op$Name, " was ", op$Status,
        if (op$Status == "reused") "; its originally applied parameters require review of the recorded origin" else "",
        "."
      ))
      next
    }
    description <- switch(op$Name,
      apply_mask = "A spatial mask was applied",
      spatial_smooth = paste0("Spatial smoothing used FSL SUSAN with a ", p$fwhm_mm, " mm FWHM kernel"),
      intensity_normalize = paste0("Intensity normalization used ",
        if (identical(p$mode, "run_scalar")) "run-wise scalar" else "voxelwise baseline",
        " scaling with target ", p$target,
        "; denominator safeguards and reference calibration are recorded in the sidecar"),
      apply_aroma = paste0("ICA-AROMA components were removed using ",
        if (isTRUE(p$nonaggressive)) "nonaggressive" else "aggressive", " regression"),
      temporal_filter = paste0("Temporal filtering used ", p$method,
        " with high-pass setting ", if (is.null(p$high_pass_hz)) "disabled" else paste(p$high_pass_hz, "Hz"),
        " and low-pass setting ", if (is.null(p$low_pass_hz)) "disabled" else paste(p$low_pass_hz, "Hz")),
      confound_regression = paste0("Nuisance regression used the recorded design columns (",
        paste(unlist(p$columns), collapse = ", "),
        ") with coefficients fitted on retained samples and temporal means preserved"),
      scrub_interpolate = paste0(length(record$Resolved$Censor$CensoredIndices),
        " censored samples were interpolated using natural cubic splines before subsequent enabled operations"),
      scrub_timepoints = paste0(length(record$Resolved$Censor$CensoredIndices),
        " censored samples were removed from the output"),
      copy_input = "No image transformations were executed and the input image was copied",
      roi_extraction = paste0("Regional signals were summarized using ", p$reduction,
        " aggregation; regions failing the recorded voxel-retention criterion were represented by missing values"),
      connectivity = if (isTRUE(p$all_missing)) "No usable regions remained; an all-missing connectivity matrix was written" else paste0("Regional connectivity used ", p$method, " correlations",
        if (isTRUE(p$fisher_z)) " followed by Fisher's z transformation (missing diagonal)" else ""),
      roi_diagnostics = "Regional voxel-retention diagnostics were exported",
      paste0("Operation ", op$Name, " was executed")
    )
    sentences <- c(sentences, paste0(description, "."))
  }
  if (!is.null(record$Resolved$CensorPolicy$expression)) {
    sentences <- c(sentences, paste0("The censor rule was `",
      record$Resolved$CensorPolicy$expression, "`; its resolved volume indices are recorded in the sidecar."))
  }
  if (!is.null(record$Resolved$Atlas)) {
    sentences <- c(sentences, paste0("The atlas was ",
      basename(record$Resolved$Atlas$Original$path), " (",
      length(record$Resolved$Atlas$Labels), " labels); its checksum, grid, and resampling status are recorded in the sidecar."))
  }
  upstream <- record$SourceMetadata$BrainGnomes$MethodsText
  if (checkmate::test_string(upstream)) {
    sentences <- c(sentences, paste0("Upstream recorded processing: ", upstream))
  }
  if (is.null(upstream)) {
    generators <- record$SourceDataset$GeneratedBy
    for (generator in generators) {
      if (is.list(generator) && checkmate::test_string(generator$Name)) {
        sentences <- c(sentences, paste0("The upstream dataset identifies ", generator$Name,
          " ", value_or_default(generator$Version, "(version unrecorded)"),
          " as a producing application; supplied upstream methods and reports should also be reviewed."))
      }
    }
  }
  if (!is.null(record$Validation) && any(vapply(record$Validation, function(x) {
    x$status %in% c("failed", "error")
  }, logical(1)))) {
    sentences <- c(sentences, "One or more operation validation checks failed or raised an error; completion does not indicate successful validation.")
  }
  paste(sentences, collapse = " ")
}

#' Combine relevant software citations with supplied upstream BibTeX
#'
#' @param tracker Collector with executed operations and upstream assets.
#' @return Unnamed list of BibTeX blocks; supplied upstream text is preserved.
#' @noRd
derivative_provenance_bibliography <- function(tracker) {
  package_bib <- paste(utils::toBibtex(utils::citation("BrainGnomes")), collapse = "\n")
  package_bib <- sub("@([[:alpha:]]+)\\{,", "@\\1{BrainGnomes_package,", package_bib)
  parts <- c(
    unlist(tracker$record$SourceMetadata$BrainGnomes$BibliographyParts, use.names = FALSE),
    package_bib
  )
  ops <- tracker$record$Operations
  executed <- vapply(ops, function(op) identical(op$Status, "executed"), logical(1))
  names <- vapply(ops[executed], function(op) op$Name, character(1))
  fsl_used <- any(vapply(ops[executed], function(op) {
    op$Name %in% c("apply_mask", "spatial_smooth", "intensity_normalize") ||
      (identical(op$Name, "temporal_filter") && identical(op$Parameters$method, "fslmaths"))
  }, logical(1)))
  if (fsl_used) {
    parts <- c(parts, paste0(
      "@article{BrainGnomes_FSL_2012,\n",
      "  author = {Jenkinson, Mark and Beckmann, Christian F. and Behrens, Timothy E. J. and Woolrich, Mark W. and Smith, Stephen M.},\n",
      "  title = {{FSL}}, journal = {NeuroImage}, year = {2012},\n",
      "  volume = {62}, number = {2}, pages = {782--790}, doi = {10.1016/j.neuroimage.2011.09.015}\n}"
    ))
  }
  if ("apply_aroma" %in% names) parts <- c(parts, paste0(
    "@article{BrainGnomes_AROMA_2015,\n",
    "  author = {Pruim, Raimon H. R. and Mennes, Maarten and van Rooij, Daan and Llera, Alberto and Buitelaar, Jan K. and Beckmann, Christian F.},\n",
    "  title = {{ICA-AROMA}: A robust {ICA}-based strategy for removing motion artifacts from {fMRI} data},\n",
    "  journal = {NeuroImage}, year = {2015}, volume = {112}, pages = {267--277}, doi = {10.1016/j.neuroimage.2015.02.064}\n}"
  ))
  for (asset in tracker$assets) {
    if (grepl("\\.bib$", asset) && file.exists(asset)) {
      parts <- c(parts, paste(readLines(asset, warn = FALSE), collapse = "\n"))
    }
  }
  # Identify fMRIPrep from supplied metadata, never from an input filename alone.
  # Keep upstream BibTeX verbatim; add the core paper only when it is absent.
  generators <- tracker$record$SourceDataset$GeneratedBy
  fmriprep_used <- any(vapply(generators, function(generator) {
    is.list(generator) && identical(tolower(generator$Name), "fmriprep")
  }, logical(1)))
  if (fmriprep_used && !any(grepl("10.1038/s41592-018-0235-4", parts, fixed = TRUE))) {
    parts <- c(parts, paste0(
      "@article{BrainGnomes_fMRIPrep_2019,\n",
      "  author = {Esteban, Oscar and Markiewicz, Christopher J. and Blair, Ross W. and Moodie, Craig A. and Isik, A. Ilkay and Erramuzpe, Asier and Kent, James D. and Goncalves, Mathias and DuPre, Elizabeth and Snyder, Madeleine and Oya, Hiroyuki and Ghosh, Satrajit S. and Wright, Jessey and Durnez, Joke and Poldrack, Russell A. and Gorgolewski, Krzysztof J.},\n",
      "  title = {{fMRIPrep}: a robust preprocessing pipeline for functional {MRI}},\n",
      "  journal = {Nature Methods}, year = {2019}, volume = {16}, number = {1}, pages = {111--116}, doi = {10.1038/s41592-018-0235-4}\n}"
    ))
  }
  derivative_provenance_array(unique(parts[nzchar(parts)]))
}

#' Publish a derivative sidecar and its readable companions
#'
#' @param tracker Collector for an existing derivative with resolved execution details.
#' @return Companion paths; also updates the collector with the published record.
#' @noRd
derivative_provenance_publish <- function(tracker) {
  file <- tracker$file
  checkmate::assert_file_exists(file)
  paths <- derivative_provenance_paths(file)
  record <- tracker$record
  # Include namespaces loaded by the actual operation (for example corpcor),
  # while retaining configured container identities resolved by the caller.
  record$Software <- modifyList(record$Software, derivative_provenance_software())
  record$Status <- if (identical(record$Status, "unknown")) "unknown" else "completed"
  record$Derivative <- derivative_provenance_fingerprint(file, "derivative")
  record$AttemptFile <- file.path(basename(paths$directory), paste0("attempt-", tracker$id, ".json"))
  record$OutputGeometry <- derivative_provenance_geometry(file)
  record$MethodsText <- derivative_provenance_methods(record)
  record$BibliographyParts <- derivative_provenance_bibliography(tracker)
  record$Assets <- list()
  for (relative in names(tracker$assets)) {
    source <- tracker$assets[[relative]]
    dest <- file.path(paths$directory, relative)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(source, dest, overwrite = TRUE)) stop("Cannot preserve provenance asset: ", source)
    record$Assets[[length(record$Assets) + 1L]] <- list(
      File = file.path(basename(paths$directory), relative),
      Original = derivative_provenance_fingerprint(source, "upstream_asset")
    )
  }
  metadata <- list(Description = paste("BrainGnomes", record$Stage, "derivative"))
  # Keep applicable standard metadata. BrainGnomes-specific source identities
  # live in the extension until a portable export supplies DatasetLinks.
  for (field in c("SpatialReference")) {
    if (!is.null(tracker$metadata[[field]])) metadata[[field]] <- tracker$metadata[[field]]
  }
  if (identical(record$Stage, "postprocessing") && !is.null(record$Resolved$TR)) {
    metadata$RepetitionTime <- record$Resolved$TR
  }
  dir.create(paths$directory, recursive = TRUE, showWarnings = FALSE)
  methods_file <- file.path(paths$directory, "methods.md")
  bib_file <- file.path(paths$directory, "bibliography.bib")
  writeLines(c(
    "# Processing methods (requires human review)", "", record$MethodsText, "",
    "Review the resolved settings, operation states, validation results, and upstream reports before using this text in a publication.",
    "", "The JSON sidecar contains the complete portable record. Upstream citations and reports are preserved under upstream/ when available."
  ), methods_file, useBytes = TRUE)
  writeLines(unlist(record$BibliographyParts), bib_file, useBytes = TRUE)
  # Identify generated prose as well as supplied upstream assets so incomplete
  # or edited companion directories cannot silently become verified exports.
  record$Companions <- lapply(c(methods_file, bib_file), function(path) list(
    File = file.path(basename(paths$directory), basename(path)),
    Original = derivative_provenance_fingerprint(path, "generated_companion")
  ))
  metadata$BrainGnomes <- record
  write_json_atomic(metadata, paths$json)
  tracker$record <- record
  c(paths$json, methods_file, bib_file,
    vapply(record$Assets, function(x) file.path(dirname(file), x$File), character(1)))
}

#' Record reuse without attributing current settings to an older derivative
#'
#' @param tracker Collector whose intended derivative already exists.
#' @return Sidecar path invisibly. A verified producer is unchanged; a legacy
#'   derivative receives an explicitly unknown producer record.
#' @noRd
derivative_provenance_reuse <- function(tracker) {
  paths <- derivative_provenance_paths(tracker$file)
  existing <- derivative_provenance_read_json(paths$json)
  if (!is.null(existing$BrainGnomes)) {
    read_derivative_provenance(tracker$file)
    tracker$record$ReusedRecordID <- existing$BrainGnomes$RecordID
    derivative_provenance_attempt(tracker, "reused")
    return(invisible(paths$json))
  }
  # A current request cannot reconstruct the settings of an unrecorded file.
  derivative_provenance_attempt(tracker, "reused")
  tracker$record$Status <- "unknown"
  tracker$record$SoftwareRole <- "recorder; producer software unknown"
  tracker$record$Requested <- NULL
  tracker$record$Resolved <- list()
  tracker$record$Operations <- list()
  tracker$record$SourceFiles <- list()
  tracker$record$SourceMetadata <- NULL
  tracker$record$SourceDataset <- NULL
  tracker$assets <- list()
  tracker$metadata <- NULL
  derivative_provenance_publish(tracker)
}

#' Publish an individual ROI output from the common extraction record
#'
#' @param tracker Collector shared by outputs of one atlas extraction.
#' @param file Written or reused ROI TSV path.
#' @param reused Whether this output predated the current extraction.
#' @param operation Optional output-specific operation, such as connectivity.
#' @param parameters Resolved output-specific parameters.
#' @return Paths to the sidecar and all companions, including attempt records.
#' @noRd
derivative_provenance_roi_output <- function(tracker, file, reused, operation = NULL, parameters = list()) {
  output <- new.env(parent = emptyenv())
  output$file <- file
  output$id <- uuid::UUIDgenerate()
  output$metadata <- tracker$metadata
  output$assets <- tracker$assets
  output$record <- tracker$record
  output$record$RecordID <- output$id
  if (!is.null(operation)) {
    if (operation == "roi_diagnostics") output$record$Operations <- list()
    derivative_provenance_operation(output, operation, "executed", parameters)
  }
  if (isTRUE(reused)) {
    derivative_provenance_reuse(output)
  } else {
    # Invalidate the prior producer even if replacement bytes happen to match.
    unlink(derivative_provenance_paths(file)$json)
    derivative_provenance_publish(output)
    derivative_provenance_attempt(output, "completed")
  }
  paths <- derivative_provenance_paths(file)
  c(paths$json, list.files(paths$directory, recursive = TRUE, full.names = TRUE))
}

#' Verify that saved companions are complete and confined to the derivative directory
#'
#' @param file Derivative path used as the root for relative companion links.
#' @param metadata Parsed derivative sidecar.
#' @return NULL invisibly; missing, changed, or escaping assets raise an error.
#' @noRd
derivative_provenance_check_companions <- function(file, metadata) {
  root <- normalizePath(dirname(file), winslash = "/", mustWork = TRUE)
  for (asset in c(metadata$BrainGnomes$Assets, metadata$BrainGnomes$Companions,
                  metadata$BrainGnomes$ExportedAttempts)) {
    target <- normalizePath(file.path(root, asset$File), winslash = "/", mustWork = FALSE)
    if (!startsWith(target, paste0(root, "/")) || !checkmate::test_file_exists(target)) {
      stop("Missing or unsafe provenance asset: ", asset$File, call. = FALSE)
    }
    if (!identical(unname(tools::md5sum(target)), asset$Original$checksum)) {
      stop("Provenance asset does not match its recorded checksum: ", asset$File, call. = FALSE)
    }
  }
  invisible(NULL)
}

#' Read the portable record associated with a derivative
#'
#' Reads the JSON sidecar without consulting a project database. The recorded
#' derivative checksum must match the file; an unsupported schema or stale
#' sidecar is an error. Original source paths need not be accessible.
#' @param file Path to a native postprocessed NIfTI or ROI TSV derivative.
#' @return A list containing standard metadata and the versioned `BrainGnomes`
#'   extension, including methods text, source identities, and execution records.
#' @export
read_derivative_provenance <- function(file) {
  checkmate::assert_file_exists(file)
  metadata <- derivative_provenance_read_json(derivative_provenance_paths(file)$json)
  if (!identical(metadata$BrainGnomes$SchemaVersion, "brain-gnomes-derivative-v1")) {
    stop("No supported BrainGnomes derivative provenance sidecar: ", file, call. = FALSE)
  }
  if (!identical(metadata$BrainGnomes$Derivative$checksum, unname(tools::md5sum(file)))) {
    stop("Derivative does not match its recorded checksum: ", file, call. = FALSE)
  }
  metadata
}

#' Export a derivative with its portable processing record
#'
#' Copies a derivative, its JSON sidecar, methods text, bibliography, and saved
#' upstream assets to a new directory. Reading the copy does not require SQLite
#' or access to the original study. The export is a portable bundle, not a claim
#' that all inputs or software environments have been archived.
#'
#' @param file Path to a derivative with BrainGnomes provenance.
#' @param output_dir New directory for the exported bundle. It must not exist.
#' @param datalad Optional named list of externally supplied `dataset_id`,
#'   `commit`, and/or `annex_key` identifiers. These are recorded as user-supplied
#'   references; no DataLad command is executed or identifier verified.
#' @return Paths to the exported derivative, sidecar, methods, bibliography,
#'   and dataset description, invisibly.
#' @export
export_derivative_provenance <- function(file, output_dir, datalad = NULL) {
  metadata <- read_derivative_provenance(file)
  derivative_provenance_check_companions(file, metadata)
  checkmate::assert_string(output_dir, min.chars = 1L)
  if (file.exists(output_dir)) stop("Export directory already exists: ", output_dir, call. = FALSE)
  if (!is.null(datalad)) {
    checkmate::assert_list(datalad, names = "unique")
    if (!length(datalad) || any(!names(datalad) %in% c("dataset_id", "commit", "annex_key")) ||
        !all(vapply(datalad, checkmate::test_string, logical(1), min.chars = 1L))) {
      stop("datalad must contain named dataset_id, commit, and/or annex_key strings.", call. = FALSE)
    }
  }
  dir.create(dirname(output_dir), recursive = TRUE, showWarnings = FALSE)
  staging <- tempfile(".derivative-export-", tmpdir = dirname(output_dir))
  dir.create(staging)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  source_paths <- derivative_provenance_paths(file)
  dest <- file.path(staging, basename(file))
  if (!file.copy(file, dest)) stop("Cannot copy derivative: ", file)
  # Copy declared companions and attempt records, excluding stale or unrelated
  # additions to the directory. Report-relative paths retain their hierarchy.
  attempts <- file.path(basename(source_paths$directory), list.files(
    source_paths$directory, pattern = "^attempt-[[:alnum:]-]+\\.json$"
  ))
  companions <- vapply(c(metadata$BrainGnomes$Assets, metadata$BrainGnomes$Companions),
    function(asset) asset$File, character(1))
  source_root <- normalizePath(dirname(file), winslash = "/", mustWork = TRUE)
  metadata$BrainGnomes$ExportedAttempts <- list()
  for (relative in unique(c(companions, attempts))) {
    source <- normalizePath(file.path(source_root, relative), winslash = "/", mustWork = TRUE)
    if (!startsWith(source, paste0(source_root, "/"))) {
      stop("Unsafe provenance companion: ", relative, call. = FALSE)
    }
    target <- file.path(staging, relative)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(source, target)) stop("Cannot copy provenance companion: ", source)
    if (relative %in% attempts) {
      metadata$BrainGnomes$ExportedAttempts[[length(metadata$BrainGnomes$ExportedAttempts) + 1L]] <- list(
        File = relative, Original = derivative_provenance_fingerprint(source, "derivative_attempt")
      )
    }
  }
  derivative_provenance_check_companions(dest, metadata)
  if (!is.null(datalad)) {
    metadata$BrainGnomes$ExternalReferences <-
      list(DataLad = datalad, Verification = "user_supplied_not_verified")
  }
  description <- list(
    Name = "BrainGnomes portable derivative export", BIDSVersion = "1.11.1",
    DatasetType = "derivative", GeneratedBy = list(list(
      Name = "BrainGnomes", Version = as.character(utils::packageVersion("BrainGnomes")),
      Description = "Portable export; derivative production is documented in its sidecar",
      CodeURL = "https://github.com/HallquistLab/BrainGnomes"
    ))
  )
  # Sources uses BIDS URIs only when the original dataset root is identifiable.
  # Re-exports must not retain URI keys from a previous dataset description.
  metadata$Sources <- NULL
  links <- list()
  sources <- character()
  for (source in metadata$BrainGnomes$SourceFiles) {
    root <- derivative_provenance_dataset_root(source$path)
    if (is.null(root)) next
    key <- paste0("source", length(links) + 1L)
    links[[key]] <- paste0("file://", utils::URLencode(root))
    sources <- c(sources, paste0("bids:", key, ":", substring(source$path, nchar(root) + 2L)))
  }
  if (length(links)) {
    description$DatasetLinks <- links
    metadata$Sources <- derivative_provenance_array(sources)
  }
  write_json_atomic(metadata, derivative_provenance_paths(dest)$json)
  write_json_atomic(description, file.path(staging, "dataset_description.json"))
  if (!file.rename(staging, output_dir)) stop("Cannot publish export directory: ", output_dir)
  invisible(list(
    derivative = file.path(output_dir, basename(file)),
    sidecar = file.path(output_dir, basename(source_paths$json)),
    methods = file.path(output_dir, basename(source_paths$directory), "methods.md"),
    bibliography = file.path(output_dir, basename(source_paths$directory), "bibliography.bib"),
    dataset_description = file.path(output_dir, "dataset_description.json")
  ))
}
