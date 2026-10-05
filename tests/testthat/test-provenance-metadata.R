test_that("metadata writers reject voxel payloads without replacing existing records", {
  root <- tempfile("metadata-writers-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  image <- RNifti::asNifti(array(seq_len(24), c(2, 3, 4)))
  payloads <- list(
    image, RNifti::asNifti(image, internal = TRUE), unclass(image),
    array(seq_len(24), c(2, 3, 4)),
    jsonlite::fromJSON(jsonlite::toJSON(array(1:8, c(2, 2, 2))), simplifyVector = FALSE),
    list(scale_map = as.numeric(image)), list(voxel_values = matrix(1:24, 4)),
    matrix(seq_len(24), 4),
    jsonlite::fromJSON(jsonlite::toJSON(matrix(seq_len(24), 4)), simplifyVector = FALSE),
    as.raw(1:4),
    structure(list(count = 24L), image = image)
  )
  writers <- list(
    json = function(value, file) write_json_atomic(value, file),
    yaml = function(value, file) write_yaml_atomic(value, file),
    validation = function(value, file) write_postproc_validation_summary(file, value),
    contract = function(value, file) write_job_contract_once(value, file, "contract")
  )
  for (name in names(writers)) {
    file <- file.path(root, paste0(name, ".json"))
    for (payload in payloads) {
      expect_error(writers[[name]](list(Resolved = list(payload = payload)), file),
        "not permitted in provenance")
      expect_false(file.exists(file))
    }
    if (name != "contract") {
      writers[[name]](list(Status = "completed"), file)
      checksum <- tools::md5sum(file)
      expect_error(writers[[name]](list(Resolved = list(image = image)), file),
        "not permitted in provenance")
      expect_identical(tools::md5sum(file), checksum)
    }
  }
  expect_setequal(list.files(root), c("json.json", "yaml.json", "validation.json"))
})

test_that("headers, sample locations, and aggregate QA survive the metadata guard", {
  root <- tempfile("metadata-header-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  file <- file.path(root, "image.nii.gz")
  RNifti::writeNifti(array(1:24, c(2, 3, 4)), file)
  metadata <- list(
    SourceFile = file, Geometry = derivative_provenance_geometry(file),
    Transform = diag(4), sampled_indices = c(1L, 12L, 24L),
    normalized_coords = matrix(seq(0, 1, length.out = 9), ncol = 3),
    voxel_count = 24L, max_absolute_error = 0.01,
    Records = list(list(count = 3, Geometry = list(Transform = diag(4))))
  )
  expect_no_error(write_json_atomic(metadata, file.path(root, "metadata.json")))
  parsed <- derivative_provenance_read_json(file.path(root, "metadata.json"))
  expect_equal(unlist(parsed$sampled_indices), c(1, 12, 24))
  expect_length(parsed$normalized_coords, 3L)
  expect_lt(file.info(file.path(root, "metadata.json"))$size, 3000)
})

test_that("QC inventory snapshots reject image payloads before creating output files", {
  root <- tempfile("qc-image-payload-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  inventory <- structure(list(metadata = list(project = "test"),
    UnexpectedImage = RNifti::asNifti(array(1:8, c(2, 2, 2)))),
    class = "bg_qc_inventory")
  expect_error(write_qc_inventory(inventory, root), "qc_inventory\\$UnexpectedImage")
  expect_false(dir.exists(root))
})
