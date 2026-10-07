# Load only the sealed BrainGnomes runtime; external dependencies stay in the
# explicitly recorded submission-time library paths rather than being copied.
runtime_library <- Sys.getenv("BG_RUNTIME_LIBRARY", unset = "")
runtime_source <- Sys.getenv("BG_RUNTIME_SOURCE", unset = "")
if (nzchar(runtime_library)) .libPaths(c(runtime_library, .libPaths()))
runtime_package <- if (nzchar(runtime_source)) runtime_source else file.path(runtime_library, "BrainGnomes")
if (!dir.exists(runtime_package)) stop("Sealed BrainGnomes runtime is missing.")
if (isNamespaceLoaded("BrainGnomes")) {
  loaded_package <- getNamespaceInfo(asNamespace("BrainGnomes"), "path")
  if (!identical(normalizePath(loaded_package), normalizePath(runtime_package))) {
    stop("An unpinned BrainGnomes namespace was preloaded before worker startup.")
  }
}
if (nzchar(runtime_source)) {
  if (!requireNamespace("pkgload", quietly = TRUE)) stop("Development runtime requires pkgload.")
  if (!isNamespaceLoaded("BrainGnomes")) pkgload::load_all(runtime_source, quiet = TRUE, compile = FALSE)
} else {
  loadNamespace("BrainGnomes", lib.loc = runtime_library)
}
