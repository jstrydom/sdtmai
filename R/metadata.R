# Maximum distinct values retained for a low-cardinality source column.
METADATA_DISTINCT_VALUE_LIMIT <- 20L

#' Generate deterministic source metadata
#'
#' Profiles CSV and SAS7BDAT files without using AI. Distinct values are kept
#' only for conservative low-cardinality columns: at most 20 non-missing values,
#' excluding identifier-like names and practical free-text columns.
#'
#' @param project Optional project created by [sdtm_project_create()].
#' @param input_dir Source-data directory when `project` is not supplied.
#' @param output_file Optional JSON output path. With a project, defaults to
#'   `file.path(project$metadata_dir, "meta_data.json")`.
#'
#' @return The generated metadata as a list, invisibly when written to a file.
#' @export
#'
#' @examples
#' \dontrun{
#' project <- sdtm_project_create("study", "external/raw")
#' metadata <- sdtm_generate_metadata(project = project)
#' }
sdtm_generate_metadata <- function(
    project = NULL,
    input_dir = NULL,
    output_file = NULL) {
  if (!is.null(project)) {
    validate_sdtm_project(project)
    if (!is.null(input_dir)) {
      stop("Supply project or input_dir, not both.", call. = FALSE)
    }
    input_dir <- project$in_dir
    if (is.null(output_file)) {
      output_file <- file.path(project$metadata_dir, "meta_data.json")
    }
  }

  assert_single_path(input_dir, "input_dir")
  if (!dir.exists(input_dir)) {
    stop(sprintf("Input directory does not exist: %s", input_dir), call. = FALSE)
  }
  if (!is.null(output_file)) {
    assert_single_path(output_file, "output_file")
  }

  source_files <- list.files(
    input_dir,
    pattern = "\\.(csv|sas7bdat)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  source_files <- source_files[order(basename(source_files), method = "radix")]
  if (length(source_files) == 0) {
    stop("Input directory contains no supported .csv or .sas7bdat source files.", call. = FALSE)
  }

  dataset_names <- tools::file_path_sans_ext(basename(source_files))
  if (anyDuplicated(dataset_names)) {
    duplicate_names <- unique(dataset_names[duplicated(dataset_names)])
    stop(
      sprintf(
        "Input directory contains duplicate dataset name(s): %s",
        paste(duplicate_names, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  metadata <- lapply(source_files, profile_source_file)

  if (!is.null(output_file)) {
    output_directory <- dirname(output_file)
    if (!dir.exists(output_directory) &&
        !dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)) {
      stop(sprintf("Could not create metadata output directory: %s", output_directory), call. = FALSE)
    }
    jsonlite::write_json(
      metadata,
      output_file,
      auto_unbox = TRUE,
      pretty = TRUE,
      null = "null",
      na = "null"
    )
    return(invisible(metadata))
  }

  metadata
}

profile_source_file <- function(file_path) {
  file_name <- basename(file_path)
  reader <- resolve_source_reader(file_name)
  source_data <- if (identical(reader, "readr::read_csv")) {
    readr::read_csv(
      file_path,
      show_col_types = FALSE,
      progress = FALSE,
      name_repair = "minimal"
    )
  } else {
    haven::read_sas(file_path, .name_repair = "minimal")
  }

  column_names <- names(source_data)
  column_types <- vapply(source_data, metadata_column_type, character(1))
  distinct_values <- list()
  for (column_name in column_names) {
    values <- source_data[[column_name]]
    if (include_metadata_distinct_values(column_name, values)) {
      distinct_values[[column_name]] <- sort(
        unique(values[!is.na(values)]),
        na.last = TRUE
      )
    }
  }

  list(
    dataset = tools::file_path_sans_ext(file_name),
    file_name = file_name,
    file_path = normalizePath(file_path, winslash = "/", mustWork = TRUE),
    columns = column_names,
    row_count = nrow(source_data),
    column_types = as.list(column_types),
    distinct_values = distinct_values
  )
}

resolve_source_reader <- function(file_name) {
  extension <- tolower(tools::file_ext(file_name))
  readers <- c(csv = "readr::read_csv", sas7bdat = "haven::read_sas")
  if (!extension %in% names(readers)) {
    stop(
      sprintf("Unsupported source file format: %s", file_name),
      call. = FALSE
    )
  }
  unname(readers[[extension]])
}

metadata_column_type <- function(values) {
  if (inherits(values, "Date")) return("Date")
  if (inherits(values, c("POSIXct", "POSIXlt"))) return("POSIXct")
  if (is.factor(values)) return("factor")
  if (is.logical(values)) return("logical")
  if (is.integer(values)) return("integer")
  if (is.numeric(values)) return("numeric")
  if (is.character(values)) return("character")
  class(values)[1]
}

include_metadata_distinct_values <- function(column_name, values) {
  identifier_pattern <- paste0(
    "(^|_)(id|identifier|subjectid|recordid|projectid|uuid|guid)(_|$)|",
    "(id|identifier|uuid|guid)$"
  )
  if (grepl(identifier_pattern, column_name, ignore.case = TRUE, perl = TRUE)) {
    return(FALSE)
  }

  non_missing <- values[!is.na(values)]
  if (length(non_missing) == 0) {
    return(FALSE)
  }
  distinct_count <- length(unique(non_missing))
  if (distinct_count > METADATA_DISTINCT_VALUE_LIMIT) {
    return(FALSE)
  }
  if (is.character(non_missing)) {
    text_width <- nchar(non_missing, type = "chars", allowNA = FALSE)
    if (max(text_width) > 200 || mean(text_width) > 80) {
      return(FALSE)
    }
  }

  is.atomic(non_missing) && !is.raw(non_missing) && !is.complex(non_missing)
}
