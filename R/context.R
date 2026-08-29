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

read_dm_sdtmig_reference <- function(sdtmig_path) {
  workbook_sheets <- readxl::excel_sheets(sdtmig_path)
  if (!all(c("Variables", "Datasets") %in% workbook_sheets)) {
    stop("SDTMIG workbook must contain Variables and Datasets sheets.", call. = FALSE)
  }

  sdtmig_variables <- readxl::read_excel(sdtmig_path, sheet = "Variables")
  sdtmig_datasets <- readxl::read_excel(sdtmig_path, sheet = "Datasets")

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

  dm_variables <- sdtmig_variables[
    sdtmig_variables[["Dataset Name"]] == "DM" & !is.na(sdtmig_variables[["Dataset Name"]]),
    ,
    drop = FALSE
  ]
  dm_dataset <- sdtmig_datasets[
    sdtmig_datasets[["Dataset Name"]] == "DM" & !is.na(sdtmig_datasets[["Dataset Name"]]),
    ,
    drop = FALSE
  ]

  if (nrow(dm_variables) == 0 || nrow(dm_dataset) == 0) {
    stop("SDTMIG workbook does not contain requested domain: DM", call. = FALSE)
  }

  list(
    variables = dm_variables,
    dataset = dm_dataset,
    permitted_targets = as.character(dm_variables[["Variable Name"]])
  )
}

read_dm_study_context <- function(protocol_path, acrf_path, treatment_arms_path) {
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

  protocol_summary <- officer::docx_summary(officer::read_docx(protocol_path))
  protocol_text <- protocol_summary[["text"]]
  protocol_dm_keywords <- paste(
    c(
      "ABC-PHARM-01", "Phase 1.*First-in-Human", "first-in-human.*single-center",
      "study design", "study population", "single ascending dose", "cohort structure",
      "participant flow", "five dose levels", "screened within", "Day 1",
      "study completion", "study duration", "demographic data", "informed consent",
      "enrollment", "cohort assignment", "planned enrollment", "subcutaneous.*dose"
    ),
    collapse = "|"
  )
  protocol_exclusion_keywords <- paste(
    c(
      "nonclinical package", "concomitant medications", "exploratory blood samples",
      "adverse event is any", "ClinicalTrials.gov", "PK analysis", "PK parameter",
      "pharmacokinetic parameter", "receptor.?occupancy assessment", "ADA assessment",
      "stopping rule", "dose escalation committee", "references"
    ),
    collapse = "|"
  )
  protocol_dm_row <- grepl(protocol_dm_keywords, protocol_text, ignore.case = TRUE) &
    !grepl(protocol_exclusion_keywords, protocol_text, ignore.case = TRUE)
  protocol_context <- protocol_text[protocol_dm_row]
  protocol_context <- unique(trimws(protocol_context[!is.na(protocol_context)]))
  protocol_context <- protocol_context[nzchar(protocol_context)]

  acrf_summary <- officer::docx_summary(officer::read_docx(acrf_path))
  acrf_text <- acrf_summary[["text"]]
  acrf_dm_keywords <- paste(
    c(
      "Enrollment", "Consent", "Demographics", "Randomisation", "Randomization",
      "Cohort Assignment", "Study Completion", "Annotation Index", "gl_enroll",
      "gl_dm", "gl_ds_rand", "RFICDTC", "ARMCD", "BRTHDTC", "AGEU",
      "ETHNIC", "RACE", "RFXSTDTC", "DM\\.ARM", "DM\\.SEX", "DM\\.AGE"
    ),
    collapse = "|"
  )
  acrf_dm_row <- grepl(acrf_dm_keywords, acrf_text, ignore.case = TRUE)
  acrf_context <- acrf_text[acrf_dm_row]
  acrf_context <- unique(trimws(acrf_context[!is.na(acrf_context)]))
  acrf_context <- acrf_context[nzchar(acrf_context)]

  if (length(protocol_context) == 0) {
    stop("No DM-relevant protocol context was extracted.", call. = FALSE)
  }
  if (length(acrf_context) == 0) {
    stop("No DM-relevant annotated CRF context was extracted.", call. = FALSE)
  }

  list(
    protocol_context = protocol_context,
    annotated_crf_context = acrf_context,
    treatment_arms = treatment_arms
  )
}
