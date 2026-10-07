#' Enumerate only package/runtime code and necessary installed resources
#' @param package_path Source or installed BrainGnomes directory.
#' @param source Whether this is a development source package.
#' @return Relative files to snapshot; excludes user data and build artifacts.
#' @noRd
job_runtime_package_files <- function(package_path, source) {
  directories <- if (source) c("R", "inst", "data") else c("R", "Meta", "libs", "extdata", "data", "hpc_scripts")
  files <- c("DESCRIPTION", "NAMESPACE", unlist(lapply(directories, function(directory) {
    paste0(directory, "/", list.files(file.path(package_path, directory), recursive = TRUE,
                                      full.names = FALSE, all.files = FALSE))
  }), use.names = FALSE))
  if (source) {
    files <- c(files, paste0("src/", list.files(file.path(package_path, "src"),
                                               pattern = "[.](so|dll|dylib)$", full.names = FALSE)))
  } else {
    files <- c(files, list.files(package_path, pattern = "([.](R|py)$|^shell_functions$)", full.names = FALSE))
  }
  files <- unique(files[file.exists(file.path(package_path, files))])
  files[!dir.exists(file.path(package_path, files))]
}

#' Copy a code tree without following user data or overwriting sealed files
#' @param source_root Original code directory.
#' @param destination Destination directory.
#' @param files Relative files to copy.
#' @return NULL, invisibly; fails if any copy fails.
#' @noRd
copy_job_runtime_files <- function(source_root, destination, files) {
  for (relative in files) {
    target <- file.path(destination, relative)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(file.path(source_root, relative), target, overwrite = FALSE, copy.mode = TRUE)) {
      stop("Cannot snapshot runtime file: ", relative, call. = FALSE)
    }
  }
  invisible(NULL)
}

#' Permit owner-only edits to newly copied runtime files
#' @param files Copied file paths owned by this preparation.
#' @return NULL, invisibly; preserves existing group and other access bits.
#' @noRd
make_job_runtime_files_writable <- function(files) {
  modes <- bitwOr(as.integer(file.info(files)$mode), as.integer(as.octmode("0200")))
  Sys.chmod(files, as.octmode(modes))
  invisible(NULL)
}

#' Seal code without making private external helpers publicly readable
#' @param files Prepared runtime file paths.
#' @return NULL, invisibly; removes write bits and retains original read/execute access.
#' @noRd
seal_job_runtime_files <- function(files) {
  modes <- bitwAnd(as.integer(file.info(files)$mode), as.integer(as.octmode("0555")))
  Sys.chmod(files, as.octmode(modes))
  invisible(NULL)
}

#' Seal a reusable per-run BrainGnomes runtime, without copying its dependencies
#' @param contract Allocated attempt contract.
#' @param env_variables Worker environment.
#' @return Runtime paths and source fingerprints, or a previously sealed bundle.
#' @noRd
prepare_job_runtime_bundle <- function(contract, env_variables) {
  inherited <- env_variables[["BG_RUNTIME_BUNDLE"]]
  if (checkmate::test_string(inherited) && file.exists(file.path(inherited, "runtime.json"))) {
    return(jsonlite::read_json(file.path(inherited, "runtime.json"), simplifyVector = TRUE))
  }
  package_path <- getNamespaceInfo(asNamespace("BrainGnomes"), "path")
  source <- !file.exists(file.path(package_path, "Meta", "package.rds"))
  files <- job_runtime_package_files(package_path, source)
  helper_root <- if (source) file.path(package_path, "inst") else package_path
  configured_helpers <- env_variables[["pkg_dir"]]
  if (is.character(configured_helpers) && length(configured_helpers) == 1L && is.na(configured_helpers)) {
    configured_helpers <- Sys.getenv("pkg_dir", unset = NA_character_)
  }
  if (checkmate::test_string(configured_helpers) && file.exists(file.path(configured_helpers, "shell_functions"))) {
    helper_root <- configured_helpers
  }
  helpers <- list.files(helper_root, recursive = TRUE, full.names = FALSE)
  helpers <- helpers[grepl("([.](R|py|sbatch|pbs)$|(^|/)shell_functions$)", helpers)]
  originals <- c(file.path(package_path, files), file.path(helper_root, helpers))
  fingerprints <- unname(tools::md5sum(originals))
  # Read/execute permissions affect safe reuse; write bits are deliberately
  # stripped when sealing and must not prevent an inherited bundle's reuse.
  source_modes <- bitwAnd(as.integer(file.info(originals)$mode), as.integer(as.octmode("0555")))
  signature <- tempfile()
  on.exit(unlink(signature), add = TRUE)
  writeLines(c(files, helpers, fingerprints, as.character(source_modes), as.character(getRversion())), signature)
  key <- unname(tools::md5sum(signature))
  base <- file.path(dirname(dirname(contract$directory)), "runtime")
  bundle <- file.path(base, key)
  descriptor <- file.path(bundle, "runtime.json")
  if (file.exists(descriptor)) return(jsonlite::read_json(descriptor, simplifyVector = TRUE))
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  staging <- tempfile("runtime-", tmpdir = base)
  dir.create(staging)
  # Failed preparations are retained for diagnosis; never delete a bundle
  # that another concurrent submitter might already have begun using.
  library <- file.path(staging, "library")
  copy_job_runtime_files(package_path, file.path(library, "BrainGnomes"), files)
  copy_job_runtime_files(helper_root, file.path(staging, "helpers"), helpers)
  copied <- c(file.path(library, "BrainGnomes", files), file.path(staging, "helpers", helpers))
  if (!identical(unname(tools::md5sum(copied)), fingerprints)) {
    stop("Runtime copy does not match its source fingerprints.", call. = FALSE)
  }
  copy_job_runtime_files(if (source) file.path(package_path, "inst") else package_path,
                         staging, c("worker_bootstrap.R", "worker_runtime_load.R"))
  core_functions <- c("job_submission_connect", "job_submission_transaction", "bind_job_submission", "record_job_bootstrap_failure")
  core <- unlist(lapply(core_functions, function(name) {
    c(paste0(name, " <- ", paste(deparse(get(name, envir = asNamespace("BrainGnomes"))), collapse = "\n")), "")
  }), use.names = FALSE)
  writeLines(core, file.path(staging, "tracking-runtime.R"))
  # Package entrypoints need the pinned namespace even under --vanilla. Source
  # builds are supported for development without consulting mutable checkout R.
  entrypoints <- helpers[grepl("[.]R$", helpers)]
  for (relative in entrypoints) {
    path <- file.path(staging, "helpers", relative)
    make_job_runtime_files_writable(path)
    lines <- readLines(path, warn = FALSE)
    first <- if (length(lines) && grepl("^#!", lines[[1L]])) 1L else 0L
    prelude <- 'if (nzchar(Sys.getenv("BG_RUNTIME_LOADER"))) source(Sys.getenv("BG_RUNTIME_LOADER"), local = TRUE)'
    writeLines(c(head(lines, first), prelude, if (first) lines[-1L] else lines), path)
  }
  current_modes <- bitwAnd(as.integer(file.info(originals)$mode), as.integer(as.octmode("0555")))
  if (!identical(unname(tools::md5sum(originals)), fingerprints) || !identical(current_modes, source_modes)) {
    stop("Runtime source changed during preparation; submission was not attempted.", call. = FALSE)
  }
  runtime <- list(schema_version = "brain-gnomes-worker-runtime-v1", directory = bundle,
                  library = file.path(bundle, "library"),
                  source = if (source) file.path(bundle, "library", "BrainGnomes") else NULL,
                  helpers = file.path(bundle, "helpers"),
                  loader = file.path(bundle, "worker_runtime_load.R"),
                  package_version = as.character(utils::packageVersion("BrainGnomes")),
                  r_version = as.character(getRversion()),
                  dependency_libraries = .libPaths(),
                  source_files = originals, source_checksums = fingerprints, source_modes = source_modes)
  write_job_contract_once(runtime, file.path(staging, "runtime.json"), "Runtime bundle")
  sealed <- list.files(staging, recursive = TRUE, full.names = FALSE)
  sealed <- sealed[!dir.exists(file.path(staging, sealed))]
  writeLines(paste0(unname(tools::md5sum(file.path(staging, sealed))), "  ", sealed),
             file.path(staging, "checksums.md5"))
  seal_job_runtime_files(file.path(staging, c(sealed, "checksums.md5")))
  if (!file.rename(staging, bundle)) {
    if (!file.exists(descriptor)) stop("Cannot publish runtime bundle.", call. = FALSE)
  }
  jsonlite::read_json(descriptor, simplifyVector = TRUE)
}

#' Snapshot worker execution files and generate its independent bootstrap
#' @param contract Allocated attempt contract.
#' @param script Worker script path or one-line command.
#' @param scheduler Normalized scheduler command.
#' @param env_variables Named worker environment.
#' @return Updated script, environment, and runtime metadata.
#' @noRd
prepare_job_execution <- function(contract, script, scheduler, env_variables) {
  if (is.null(env_variables)) env_variables <- character()
  runtime <- prepare_job_runtime_bundle(contract, as.list(env_variables))
  env_variables["BG_RUNTIME_BUNDLE"] <- runtime$directory
  env_variables["BG_RUNTIME_LIBRARY"] <- runtime$library
  env_variables["BG_RUNTIME_SOURCE"] <- if (is.null(runtime$source)) "" else runtime$source
  env_variables["BG_RUNTIME_LOADER"] <- runtime$loader
  env_variables["pkg_dir"] <- runtime$helpers
  env_variables["R_LIBS_USER"] <- paste(unique(c(runtime$library, runtime$dependency_libraries)), collapse = .Platform$path.sep)
  env_variables["BG_ATTEMPT_ID"] <- contract$contract_id
  execution_dir <- file.path(contract$directory, "execution")
  dir.create(execution_dir, recursive = TRUE, showWarnings = FALSE)
  # Copy only executable helper inputs, not containers, configuration data,
  # output paths, or other potentially large/identifying project materials.
  executable_names <- c("insert_tracked_job_path", "upd_job_status_path", "add_parent_path",
                        "postprocess_rscript", "postprocess_image_sched_script", "extract_rscript",
                        "extract_sched_script", "prefetch_script")
  for (name in intersect(executable_names, names(env_variables))) {
    original <- env_variables[[name]]
    if (is.na(original)) original <- Sys.getenv(name, unset = NA_character_)
    if (is.na(original) || !file.exists(original)) stop("Runtime helper is missing: ", name, call. = FALSE)
    helper_relative <- basename(original)
    if (name %in% c("postprocess_image_sched_script", "extract_sched_script")) {
      helper_relative <- file.path("hpc_scripts", basename(original))
    }
    bundled <- file.path(runtime$helpers, helper_relative)
    # A caller's external override remains supported and is sealed separately.
    known <- normalizePath(original, winslash = "/", mustWork = TRUE) %in%
      normalizePath(runtime$source_files, winslash = "/", mustWork = FALSE)
    if (known && file.exists(bundled)) {
      env_variables[[name]] <- bundled
    } else {
      destination <- file.path(execution_dir, name)
      copy_job_runtime_files(dirname(original), destination, basename(original))
      env_variables[[name]] <- file.path(destination, basename(original))
      if (grepl("[.]R$", original)) {
        make_job_runtime_files_writable(env_variables[[name]])
        code <- readLines(env_variables[[name]], warn = FALSE)
        first <- if (length(code) && grepl("^#!", code[[1L]])) 1L else 0L
        writeLines(c(head(code, first),
                     'if (nzchar(Sys.getenv("BG_RUNTIME_LOADER"))) source(Sys.getenv("BG_RUNTIME_LOADER"), local = TRUE)',
                     if (first) code[-1L] else code), env_variables[[name]])
      }
    }
  }
  payload_dir <- file.path(execution_dir, "payload")
  dir.create(payload_dir)
  payload <- file.path(payload_dir, if (file.exists(script)) basename(script) else "command.sh")
  if (file.exists(script)) {
    copy_job_runtime_files(dirname(script), payload_dir, basename(script))
    original_lines <- readLines(script, warn = FALSE)
    if (grepl("[.]R$", script, ignore.case = TRUE)) {
      make_job_runtime_files_writable(payload)
      first <- if (length(original_lines) && grepl("^#!", original_lines[[1L]])) 1L else 0L
      writeLines(c(head(original_lines, first),
                   'if (nzchar(Sys.getenv("BG_RUNTIME_LOADER"))) source(Sys.getenv("BG_RUNTIME_LOADER"), local = TRUE)',
                   if (first) original_lines[-1L] else original_lines), payload)
    }
  } else {
    original_lines <- c("#!/bin/sh", script)
    writeLines(original_lines, payload)
    Sys.chmod(payload, "0600")
  }
  interpreter <- if (grepl("[.]R$", payload, ignore.case = TRUE)) file.path(R.home("bin"), "Rscript") else {
    if (length(original_lines) && grepl("^#!", original_lines[[1L]])) {
      sub("^#![[:space:]]*", "", original_lines[[1L]])
    } else "/bin/sh"
  }
  # Preserve interpreter arguments (e.g. /usr/bin/env bash) and scheduler
  # directives, but install our independent handler before the payload starts.
  first_command <- which(nzchar(trimws(original_lines)) & !grepl("^[[:space:]]*#", original_lines))
  leading_lines <- if (length(first_command)) head(original_lines, first_command[[1L]] - 1L) else original_lines
  header <- leading_lines[grepl("^#(SBATCH|PBS)([[:space:]]|$)", leading_lines)]
  wrapper <- file.path(execution_dir, "worker.sh")
  receipts <- file.path(contract$directory, "bootstrap")
  dir.create(receipts)
  setup <- c(
    paste0("BG_ATTEMPT_ID=", shQuote(contract$contract_id)),
    paste0("BG_JOB_MANIFEST=", shQuote(contract$manifest_path)),
    paste0("BG_CONTRACT_DIRECTORY=", shQuote(dirname(contract$directory))),
    paste0("sqlite_db=", shQuote(if (!"sqlite_db" %in% names(env_variables)) "" else env_variables[["sqlite_db"]])),
    paste0("BG_RUNTIME_BUNDLE=", shQuote(runtime$directory)),
    paste0("BG_RUNTIME_LIBRARY=", shQuote(runtime$library)),
    paste0("BG_RUNTIME_SOURCE=", shQuote(if (is.null(runtime$source)) "" else runtime$source)),
    paste0("BG_RUNTIME_LOADER=", shQuote(runtime$loader)),
    paste0("pkg_dir=", shQuote(runtime$helpers)),
    paste0("R_LIBS_USER=", shQuote(env_variables[["R_LIBS_USER"]])),
    "export BG_ATTEMPT_ID BG_JOB_MANIFEST BG_CONTRACT_DIRECTORY BG_RUNTIME_BUNDLE BG_RUNTIME_LIBRARY BG_RUNTIME_SOURCE BG_RUNTIME_LOADER pkg_dir R_LIBS_USER sqlite_db",
    # Every copied worker uses exactly these sealed helper paths, including
    # dynamic children submitted with an inherited environment.
    vapply(intersect(executable_names, names(env_variables)), function(name) {
      paste0("export ", name, "=", shQuote(env_variables[[name]]))
    }, character(1))
  )
  bridge <- file.path(runtime$directory, "worker_bootstrap.R")
  core <- file.path(runtime$directory, "tracking-runtime.R")
  rscript <- file.path(R.home("bin"), "Rscript")
  checksum_file <- file.path(runtime$directory, "checksums.md5")
  expected_checksums <- unname(tools::md5sum(checksum_file))
  execution_files <- list.files(execution_dir, recursive = TRUE, full.names = FALSE)
  execution_files <- execution_files[!dir.exists(file.path(execution_dir, execution_files))]
  execution_checksums <- file.path(execution_dir, "checksums.md5")
  writeLines(paste0(unname(tools::md5sum(file.path(execution_dir, execution_files))), "  ", execution_files),
             execution_checksums)
  expected_execution <- unname(tools::md5sum(execution_checksums))
  # Local jobs may run inside an allocation, and TORQUE can inherit Slurm
  # variables. Bind only IDs from the backend that actually launched this job.
  job_id_line <- switch(scheduler,
    sbatch = 'bg_job_id="${SLURM_ARRAY_JOB_ID:-${SLURM_JOB_ID:-$$}}"',
    qsub = 'bg_job_id="${PBS_JOBID:-$$}"',
    sh = 'bg_job_id=$$',
    stop("Unsupported worker scheduler: ", scheduler, call. = FALSE)
  )
  task_id_line <- switch(scheduler,
    sbatch = 'bg_task_id="${SLURM_ARRAY_TASK_ID:-allocation}"',
    qsub = 'bg_task_id="${PBS_ARRAYID:-${PBS_ARRAY_INDEX:-allocation}}"',
    sh = 'bg_task_id=allocation'
  )
  lines <- c("#!/bin/bash", header, setup,
    job_id_line,
    'BG_WORKER_JOB_ID="$bg_job_id"; export BG_WORKER_JOB_ID',
    task_id_line,
    'case "$bg_task_id" in *[!0-9a-zA-Z_-]*) bg_task_id=unknown;; esac',
    paste0("bg_receipt=", shQuote(paste0(receipts, "/failure-")), '"${bg_task_id}.tsv"'),
    'bg_phase=bootstrap', 'bg_child=',
    # The failure receipt uses fixed, non-executable fields and shell builtins.
    # It is emitted before any R-based best-effort update, even if R is broken.
    'bg_bootstrap_exit() {', '  bg_code=$?', '  trap - EXIT',
    '  if [ "$bg_phase" = bootstrap ] && [ "$bg_code" -ne 0 ]; then',
    '    bg_temp="${bg_receipt}.tmp.$$"',
    '    if printf "brain-gnomes-bootstrap-v1\\t%s\\t%s\\t%s\\t%s\\n" "$BG_ATTEMPT_ID" "$bg_job_id" "$bg_task_id" "$bg_code" > "$bg_temp"; then',
    '      mv -- "$bg_temp" "$bg_receipt" || printf "Cannot publish bootstrap receipt\\n" >&2',
    '    fi', '    bg_array=allocation',
    '    [ "$bg_task_id" = allocation ] || bg_array=array',
    paste0("    [ -z \"${sqlite_db:-}\" ] || ", shQuote(rscript), " ", shQuote(bridge), " ", shQuote(core),
           ' fail "$sqlite_db" "$BG_ATTEMPT_ID" "$bg_job_id" "$bg_code" "$bg_receipt" "$bg_array" || :'),
    '  fi', '  exit "$bg_code"', '}', 'trap bg_bootstrap_exit EXIT',
    'bg_signal() { [ -z "$bg_child" ] || kill -"$1" "$bg_child" 2>/dev/null || :; exit "$2"; }',
    "trap 'bg_signal TERM 143' TERM", "trap 'bg_signal INT 130' INT", "trap 'bg_signal HUP 129' HUP",
    paste0('[ -z "${sqlite_db:-}" ] || ', shQuote(rscript), " ", shQuote(bridge), " ", shQuote(core),
           ' bind "$sqlite_db" "$BG_ATTEMPT_ID" "$bg_job_id" || exit $?'),
    paste0('[ "$(md5sum ', shQuote(checksum_file), ' | cut -d " " -f 1)" = ',
           shQuote(expected_checksums), ' ] || exit 1'),
    paste0("(cd ", shQuote(runtime$directory), " && md5sum -c checksums.md5 >/dev/null) || exit 1"),
    paste0("[ -r ", shQuote(payload), " ] && [ -r ", shQuote(file.path(runtime$helpers, "shell_functions")), " ] || exit 1"),
    paste0('[ "$(md5sum ', shQuote(execution_checksums), ' | cut -d " " -f 1)" = ',
           shQuote(expected_execution), ' ] || exit 1'),
    paste0("(cd ", shQuote(execution_dir), " && md5sum -c checksums.md5 >/dev/null) || exit 1"),
    paste0(shQuote(rscript), " --vanilla -e ",
           shQuote('source(Sys.getenv("BG_RUNTIME_LOADER")); loadNamespace("BrainGnomes")'), " || exit $?"),
    'bg_phase=payload', paste(interpreter, shQuote(payload), '&'), 'bg_child=$!',
    'wait "$bg_child"', 'exit $?')
  writeLines(lines, wrapper)
  seal_job_runtime_files(file.path(execution_dir, c(execution_files, "checksums.md5")))
  wrapper_mode <- bitwOr(as.integer(file.info(payload)$mode), as.integer(as.octmode("0100")))
  Sys.chmod(wrapper, as.octmode(wrapper_mode))
  list(script = wrapper, environment = env_variables, runtime = runtime,
       payload = payload, bootstrap_receipts = receipts)
}
