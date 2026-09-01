assert_input_file <- function(path, label) {
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    stop(sprintf("%s path must be one non-empty character value.", label), call. = FALSE)
  }
  if (!file.exists(path)) {
    stop(sprintf("Required %s file does not exist: %s", label, path), call. = FALSE)
  }
  invisible(TRUE)
}

read_approved_study_metadata <- function(metadata_path) {
  study_metadata <- tryCatch(
    jsonlite::fromJSON(metadata_path, simplifyVector = FALSE),
    error = function(error) {
      stop(sprintf("Study metadata JSON could not be parsed: %s", conditionMessage(error)), call. = FALSE)
    }
  )

  if (!is.list(study_metadata) || length(study_metadata) == 0) {
    stop("Study metadata must contain at least one dataset.", call. = FALSE)
  }

  required_fields <- c(
    "dataset", "file_name", "columns", "row_count", "column_types", "distinct_values"
  )
  dataset_names <- character(length(study_metadata))
  columns_by_dataset <- vector("list", length(study_metadata))
  study_inventory <- study_metadata

  for (dataset_index in seq_along(study_metadata)) {
    dataset_metadata <- study_metadata[[dataset_index]]
    missing_fields <- setdiff(required_fields, names(dataset_metadata))

    if (length(missing_fields) > 0) {
      dataset_label <- dataset_metadata$dataset
      if (is.null(dataset_label)) {
        dataset_label <- paste0("entry ", dataset_index)
      }
      stop(
        sprintf(
          "Study metadata %s is missing required field(s): %s",
          dataset_label,
          paste(missing_fields, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    dataset_names[dataset_index] <- dataset_metadata$dataset
    columns_by_dataset[[dataset_index]] <- unlist(
      dataset_metadata$columns,
      use.names = FALSE
    )

    study_inventory[[dataset_index]]$file_path <- NULL
    if (length(study_inventory[[dataset_index]]$distinct_values) == 0) {
      study_inventory[[dataset_index]]$distinct_values <- NULL
    }
  }

  if (anyDuplicated(dataset_names)) {
    duplicate_names <- unique(dataset_names[duplicated(dataset_names)])
    stop(
      sprintf("Study metadata contains duplicate dataset name(s): %s", paste(duplicate_names, collapse = ", ")),
      call. = FALSE
    )
  }

  names(columns_by_dataset) <- dataset_names

  list(
    metadata = study_metadata,
    inventory = study_inventory,
    dataset_names = dataset_names,
    columns_by_dataset = columns_by_dataset
  )
}

normalize_sdtm_domain <- function(domain) {
  if (!is.character(domain) || length(domain) != 1 || is.na(domain) ||
      !nzchar(trimws(domain))) {
    stop("domain must be one non-empty character value.", call. = FALSE)
  }
  toupper(trimws(domain))
}

read_sdtmig_domain_reference <- function(sdtmig, domain) {
  assert_input_file(sdtmig, "SDTMIG workbook")
  domain <- normalize_sdtm_domain(domain)
  workbook_sheets <- readxl::excel_sheets(sdtmig)
  if (!all(c("Variables", "Datasets") %in% workbook_sheets)) {
    stop("SDTMIG workbook must contain Variables and Datasets sheets.", call. = FALSE)
  }

  sdtmig_variables <- readxl::read_excel(sdtmig, sheet = "Variables")
  sdtmig_datasets <- readxl::read_excel(sdtmig, sheet = "Datasets")

  required_variable_columns <- c(
    "Dataset Name", "Variable Name", "Variable Label", "Type",
    "CDISC CT Codelist Code(s)", "Codelist Submission Value(s)",
    "Described Value Domain(s)", "Value List", "Role", "CDISC Notes", "Core"
  )
  required_dataset_columns <- c("Dataset Name", "Dataset Label", "Structure")

  if (!all(required_variable_columns %in% names(sdtmig_variables))) {
    stop("SDTMIG Variables sheet is missing one or more required columns.", call. = FALSE)
  }
  if (!all(required_dataset_columns %in% names(sdtmig_datasets))) {
    stop("SDTMIG Datasets sheet is missing one or more required columns.", call. = FALSE)
  }

  variable_domains <- unique(trimws(as.character(
    sdtmig_variables[["Dataset Name"]][!is.na(sdtmig_variables[["Dataset Name"]])]
  )))
  dataset_domains <- unique(trimws(as.character(
    sdtmig_datasets[["Dataset Name"]][!is.na(sdtmig_datasets[["Dataset Name"]])]
  )))
  available_domains <- intersect(variable_domains[nzchar(variable_domains)],
                                 dataset_domains[nzchar(dataset_domains)])
  if (!domain %in% available_domains) {
    stop(
      sprintf(
        "Unsupported SDTM domain '%s'. The supplied SDTMIG does not contain this domain.",
        domain
      ),
      call. = FALSE
    )
  }

  domain_variables <- sdtmig_variables[
    trimws(as.character(sdtmig_variables[["Dataset Name"]])) == domain &
      !is.na(sdtmig_variables[["Dataset Name"]]),
    ,
    drop = FALSE
  ]
  domain_dataset <- sdtmig_datasets[
    trimws(as.character(sdtmig_datasets[["Dataset Name"]])) == domain &
      !is.na(sdtmig_datasets[["Dataset Name"]]),
    ,
    drop = FALSE
  ]

  permitted_targets <- as.character(domain_variables[["Variable Name"]])
  if (any(is.na(permitted_targets) | !nzchar(trimws(permitted_targets)))) {
    stop(sprintf("SDTMIG domain '%s' contains a blank target variable name.", domain), call. = FALSE)
  }
  if (anyDuplicated(permitted_targets)) {
    duplicate_targets <- unique(permitted_targets[duplicated(permitted_targets)])
    stop(
      sprintf(
        "SDTMIG domain '%s' contains duplicate target variable(s): %s",
        domain,
        paste(duplicate_targets, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  list(
    domain = domain,
    variables = domain_variables,
    dataset = domain_dataset,
    permitted_targets = permitted_targets,
    available_domains = available_domains
  )
}

read_dm_sdtmig_reference <- function(sdtmig_path) {
  read_sdtmig_domain_reference(sdtmig_path, "DM")
}

read_treatment_arms_context <- function(treatment_arms_path) {
  treatment_arms_raw <- readxl::read_excel(
    treatment_arms_path,
    sheet = "Treatment Arms",
    col_names = FALSE
  )
  treatment_arms_header_row <- which(
    trimws(as.character(treatment_arms_raw[[1]])) == "Cohort"
  )[1]

  if (is.na(treatment_arms_header_row)) {
    stop("Could not find the Cohort header in the Treatment Arms sheet.", call. = FALSE)
  }
  if (treatment_arms_header_row >= nrow(treatment_arms_raw)) {
    stop("No treatment-arm rows follow the Cohort header.", call. = FALSE)
  }

  treatment_arms_column_names <- as.character(unlist(
    treatment_arms_raw[treatment_arms_header_row, ],
    use.names = FALSE
  ))
  treatment_arms <- treatment_arms_raw[
    seq.int(treatment_arms_header_row + 1, nrow(treatment_arms_raw)),
    ,
    drop = FALSE
  ]
  names(treatment_arms) <- treatment_arms_column_names
  treatment_arms <- treatment_arms[
    !is.na(treatment_arms[["Cohort"]]) &
      nzchar(trimws(as.character(treatment_arms[["Cohort"]]))),
    ,
    drop = FALSE
  ]

  if (nrow(treatment_arms) == 0) {
    stop("No treatment-arm rows were extracted from the Treatment Arms sheet.", call. = FALSE)
  }
  if (!all(c("Cohort", "ARMCD", "ARM") %in% names(treatment_arms))) {
    stop("Cleaned Treatment Arms data must contain Cohort, ARMCD, and ARM columns.", call. = FALSE)
  }

  treatment_arms
}

read_document_context <- function(path, label) {
  if (is.null(path)) return(character())
  assert_input_file(path, label)
  document_summary <- officer::docx_summary(officer::read_docx(path))
  document_text <- unique(trimws(as.character(document_summary[["text"]])))
  document_text[!is.na(document_text) & nzchar(document_text)]
}

read_sdtm_study_context <- function(
    protocol_path = NULL,
    acrf_path = NULL,
    treatment_arms_path = NULL) {
  treatment_arms <- if (is.null(treatment_arms_path)) {
    data.frame()
  } else {
    assert_input_file(treatment_arms_path, "Treatment Arms workbook")
    read_treatment_arms_context(treatment_arms_path)
  }
  list(
    protocol_context = read_document_context(protocol_path, "protocol"),
    annotated_crf_context = read_document_context(acrf_path, "annotated CRF"),
    treatment_arms = treatment_arms
  )
}

read_dm_study_context <- function(protocol_path, acrf_path, treatment_arms_path) {
  read_sdtm_study_context(protocol_path, acrf_path, treatment_arms_path)
}
