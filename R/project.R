#' Create a lightweight SDTM project
#'
#' Creates isolated metadata, mapping, code, and output directories beneath a
#' project root. The approved raw-data directory may remain outside the project.
#'
#' @param root Project root directory.
#' @param in_dir Approved raw source-data directory. This may be an external or
#'   network path and does not need to exist when the project is created.
#'
#' @return A `clinical_sdtm_project` list containing exactly `root`, `in_dir`,
#'   `metadata_dir`, `mapping_dir`, `code_dir`, and `out_dir`.
#' @export
#'
#' @examples
#' \dontrun{
#' project <- sdtm_project_create(
#'   root = "C:/ClinicalAI/ABC-PHARM-01",
#'   in_dir = "\\\\clinicalai_synthetic_test_datasets"
#' )
#' }
sdtm_project_create <- function(root, in_dir) {
  assert_single_path(root, "root")
  assert_single_path(in_dir, "in_dir")

  if (file.exists(root) && !dir.exists(root)) {
    stop(sprintf("Project root exists but is not a directory: %s", root), call. = FALSE)
  }
  if (!dir.exists(root) &&
      !dir.create(root, recursive = TRUE, showWarnings = FALSE)) {
    stop(sprintf("Could not create project root directory: %s", root), call. = FALSE)
  }

  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  project <- list(
    root = root,
    in_dir = in_dir,
    metadata_dir = file.path(root, "metadata"),
    mapping_dir = file.path(root, "mappings"),
    code_dir = file.path(root, "code"),
    out_dir = file.path(root, "output")
  )

  for (directory in unname(unlist(project[c(
    "metadata_dir", "mapping_dir", "code_dir", "out_dir"
  )]))) {
    if (!dir.exists(directory) &&
        !dir.create(directory, recursive = TRUE, showWarnings = FALSE)) {
      stop(sprintf("Could not create project directory: %s", directory), call. = FALSE)
    }
  }

  marker_file <- file.path(root, ".clinicalai-sdtm-project")
  if (!file.exists(marker_file)) {
    project_id <- paste0(
      "sdtm-",
      format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"),
      "-",
      Sys.getpid()
    )
    writeLines(
      c(
        paste0("project_id: ", project_id),
        paste0("created_utc: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
      ),
      marker_file,
      useBytes = TRUE
    )
  }

  class(project) <- c("clinical_sdtm_project", "list")
  project
}

assert_single_path <- function(path, label) {
  if (!is.character(path) || length(path) != 1 || is.na(path) ||
      !nzchar(trimws(path))) {
    stop(sprintf("%s must be one non-empty character path.", label), call. = FALSE)
  }
  invisible(TRUE)
}

validate_sdtm_project <- function(project) {
  expected_fields <- c(
    "root", "in_dir", "metadata_dir", "mapping_dir", "code_dir", "out_dir"
  )
  if (!inherits(project, "clinical_sdtm_project") ||
      !identical(names(project), expected_fields)) {
    stop("project must be created by sdtm_project_create().", call. = FALSE)
  }
  for (field in expected_fields) {
    assert_single_path(project[[field]], paste0("project$", field))
  }
  invisible(TRUE)
}
