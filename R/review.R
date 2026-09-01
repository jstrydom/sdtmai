#' Validate a programmer-reviewed SDTM mapping
#'
#' Reads a mapping review CSV produced by [sdtm_generate_mapping()], validates
#' the programmer's decisions against the approved source metadata and local
#' SDTMIG metadata, and returns a deterministic approved mapping table.
#'
#' @param mapping Path to the programmer-edited mapping review CSV.
#' @param metadata Path to the approved study metadata JSON.
#' @param sdtmig Path to the local SDTMIG workbook.
#' @param output_file Optional CSV path for the approved mapping table. Use
#'   `NULL` to return the result without writing a file.
#'
#' @return A list with class `clinical_sdtm_mapping_review` containing the
#'   domain, original review table, approved mapping, and unresolved review.
#' @export
#'
#' @examples
#' \dontrun{
#' review <- sdtm_validate_mapping(
#'   mapping = "output/DM_mapping_review.csv",
#'   metadata = "input/meta_data.json",
#'   sdtmig = "input/SDTMIG_v3.4.xlsx",
#'   output_file = "output/DM_mapping_approved.csv"
#' )
#' review$approved_mapping
#' }
sdtm_validate_mapping <- function(
    mapping,
    metadata,
    sdtmig,
    output_file = NULL) {
  assert_input_file(mapping, "mapping review CSV")
  assert_input_file(metadata, "study metadata")
  assert_input_file(sdtmig, "SDTMIG workbook")

  if (!is.null(output_file) &&
      (!is.character(output_file) || length(output_file) != 1 || !nzchar(output_file))) {
    stop("output_file must be NULL or one non-empty CSV path.", call. = FALSE)
  }

  review_table <- utils::read.csv(
    mapping,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character()
  )
  domain <- infer_review_domain(review_table)
  study <- read_approved_study_metadata(metadata)
  domain_reference <- read_sdtmig_domain_reference(sdtmig, domain)

  result <- validate_programmer_review(review_table, study, domain_reference)

  if (!is.null(output_file)) {
    output_directory <- dirname(output_file)
    if (!dir.exists(output_directory)) {
      dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
    }
    utils::write.csv(result$approved_mapping, output_file, row.names = FALSE, na = "")
  }

  result
}

split_review_sources <- function(value) {
  value <- trimws(as.character(value))
  if (!nzchar(value)) {
    return(character())
  }
  trimws(strsplit(value, ";", fixed = TRUE)[[1]])
}

infer_review_domain <- function(review_table) {
  if (!is.data.frame(review_table) || !"domain" %in% names(review_table)) {
    stop("Mapping review CSV is missing required column: domain", call. = FALSE)
  }
  row_domains <- toupper(trimws(as.character(review_table$domain)))
  if (any(is.na(row_domains) | !nzchar(row_domains))) {
    stop(
      "Mapping review CSV must contain exactly one non-empty domain in every row; found a blank domain.",
      call. = FALSE
    )
  }
  domains <- unique(row_domains)
  if (length(domains) != 1) {
    stop(
      sprintf(
        "Mapping review CSV must contain exactly one non-empty domain; found: %s",
        if (length(domains) == 0) "<none>" else paste(domains, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  domains[[1]]
}

validate_programmer_review <- function(review_table, study, domain_reference) {
  required_columns <- c(
    "domain", "target_variable", "target_label", "core", "role",
    "ai_disposition", "ai_mapping_type", "ai_source_datasets",
    "ai_source_variables", "ai_derivation", "ai_confidence",
    "ai_rationale", "ai_reason", "ai_evidence", "programmer_decision",
    "approved_source_datasets", "approved_source_variables",
    "approved_mapping_type", "approved_derivation", "programmer_comment"
  )
  missing_columns <- setdiff(required_columns, names(review_table))
  if (length(missing_columns) > 0) {
    stop(
      sprintf(
        "Mapping review CSV is missing required column(s): %s",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (nrow(review_table) == 0) {
    stop("Mapping review CSV contains no mapping rows.", call. = FALSE)
  }

  for (column_name in required_columns) {
    review_table[[column_name]] <- as.character(review_table[[column_name]])
    review_table[[column_name]][is.na(review_table[[column_name]])] <- ""
  }

  review_table$domain <- toupper(trimws(review_table$domain))
  review_table$target_variable <- trimws(review_table$target_variable)
  review_table$ai_disposition <- trimws(review_table$ai_disposition)
  review_table$programmer_decision <- trimws(review_table$programmer_decision)
  review_table$approved_mapping_type <- trimws(review_table$approved_mapping_type)

  domain <- infer_review_domain(review_table)
  if (!is.null(domain_reference$domain) && !identical(domain_reference$domain, domain)) {
    stop(
      sprintf("Mapping review domain '%s' does not match the loaded SDTMIG domain '%s'.", domain, domain_reference$domain),
      call. = FALSE
    )
  }

  invalid_targets <- unique(review_table$target_variable[
    !review_table$target_variable %in% domain_reference$permitted_targets
  ])
  if (length(invalid_targets) > 0) {
    stop(
      sprintf(
        "Target variable(s) not present in supplied %s SDTMIG metadata: %s",
        domain,
        paste(invalid_targets, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  duplicate_targets <- unique(review_table$target_variable[
    duplicated(review_table$target_variable)
  ])
  if (length(duplicate_targets) > 0) {
    stop(
      sprintf("Duplicate target variable(s) in mapping review: %s", paste(duplicate_targets, collapse = ", ")),
      call. = FALSE
    )
  }

  allowed_ai_dispositions <- c("Mapped", "Unresolved", "Not Applicable")
  invalid_ai_disposition_rows <- which(!review_table$ai_disposition %in% allowed_ai_dispositions)
  if (length(invalid_ai_disposition_rows) > 0) {
    stop(
      sprintf(
        "Invalid AI disposition for target(s): %s.",
        paste(review_table$target_variable[invalid_ai_disposition_rows], collapse = ", ")
      ),
      call. = FALSE
    )
  }

  allowed_decisions <- c("Accept", "Amend", "Resolve", "Not Applicable")
  invalid_decision_rows <- which(!review_table$programmer_decision %in% allowed_decisions)
  if (length(invalid_decision_rows) > 0) {
    invalid_decision_targets <- review_table$target_variable[invalid_decision_rows]
    stop(
      sprintf(
        "Invalid or blank programmer decision for target(s): %s. Allowed values are Accept, Amend, Resolve, and Not Applicable.",
        paste(invalid_decision_targets, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  allowed_mapping_types <- c("DIRECT", "ASSIGNED", "DERIVED", "MULTI_SOURCE")
  approved_mapping <- data.frame(
    domain = character(),
    target_variable = character(),
    target_label = character(),
    core = character(),
    role = character(),
    approved_disposition = character(),
    approved_mapping_type = character(),
    approved_source_datasets = character(),
    approved_source_variables = character(),
    approved_derivation = character(),
    programmer_comment = character(),
    stringsAsFactors = FALSE
  )

  for (row_index in seq_len(nrow(review_table))) {
    review_row <- review_table[row_index, , drop = FALSE]
    target_variable <- review_row$target_variable
    decision <- review_row$programmer_decision
    ai_disposition <- review_row$ai_disposition

    if (decision == "Accept" && ai_disposition != "Mapped") {
      stop(
        sprintf("Accept is only valid for an AI Mapped row; target '%s' is '%s'.", target_variable, ai_disposition),
        call. = FALSE
      )
    }
    if (decision == "Amend" && ai_disposition != "Mapped") {
      stop(
        sprintf("Amend is only valid for an AI Mapped row; target '%s' is '%s'.", target_variable, ai_disposition),
        call. = FALSE
      )
    }
    if (decision == "Resolve" && ai_disposition != "Unresolved") {
      stop(
        sprintf("Resolve is only valid for an AI Unresolved row; target '%s' is '%s'.", target_variable, ai_disposition),
        call. = FALSE
      )
    }

    target_reference <- domain_reference$variables[
      domain_reference$variables[["Variable Name"]] == target_variable,
      ,
      drop = FALSE
    ]

    if (decision == "Not Applicable") {
      programmer_mapping_fields <- c(
        review_row$approved_source_datasets,
        review_row$approved_source_variables,
        review_row$approved_mapping_type,
        review_row$approved_derivation
      )
      if (any(nzchar(trimws(programmer_mapping_fields)))) {
        stop(
          sprintf("Not Applicable row for target '%s' must leave all approved mapping fields blank.", target_variable),
          call. = FALSE
        )
      }
      final_mapping_type <- ""
      final_source_datasets <- character()
      final_source_variables <- character()
      final_derivation <- ""
      approved_disposition <- "Not Applicable"
    } else {
      approved_disposition <- "Mapped"

      if (decision == "Accept") {
        programmer_mapping_fields <- c(
          review_row$approved_source_datasets,
          review_row$approved_source_variables,
          review_row$approved_mapping_type,
          review_row$approved_derivation
        )
        if (any(nzchar(trimws(programmer_mapping_fields)))) {
          stop(
            sprintf("Accept row for target '%s' must leave all approved mapping fields blank.", target_variable),
            call. = FALSE
          )
        }
        final_mapping_type <- trimws(review_row$ai_mapping_type)
        source_dataset_text <- review_row$ai_source_datasets
        source_variable_text <- review_row$ai_source_variables
        final_derivation <- trimws(review_row$ai_derivation)
      } else {
        final_mapping_type <- review_row$approved_mapping_type
        source_dataset_text <- review_row$approved_source_datasets
        source_variable_text <- review_row$approved_source_variables
        final_derivation <- trimws(review_row$approved_derivation)
      }

      if (!final_mapping_type %in% allowed_mapping_types) {
        stop(
          sprintf(
            "Invalid approved mapping type for target '%s'. Allowed values are DIRECT, ASSIGNED, DERIVED, and MULTI_SOURCE.",
            target_variable
          ),
          call. = FALSE
        )
      }

      final_source_datasets <- split_review_sources(source_dataset_text)
      final_source_variables <- split_review_sources(source_variable_text)
      if (length(final_source_datasets) != length(final_source_variables)) {
        stop(
          sprintf(
            "Source dataset and variable counts differ for target '%s': %s dataset(s), %s variable(s).",
            target_variable,
            length(final_source_datasets),
            length(final_source_variables)
          ),
          call. = FALSE
        )
      }

      if (final_mapping_type == "DIRECT" && length(final_source_datasets) < 1) {
        stop(sprintf("DIRECT mapping for target '%s' requires at least one source pair.", target_variable), call. = FALSE)
      }
      if (final_mapping_type == "MULTI_SOURCE" && length(final_source_datasets) < 2) {
        stop(sprintf("MULTI_SOURCE mapping for target '%s' requires at least two source pairs.", target_variable), call. = FALSE)
      }
      if (final_mapping_type %in% c("DERIVED", "ASSIGNED") && !nzchar(final_derivation)) {
        stop(
          sprintf("%s mapping for target '%s' requires a non-empty derivation.", final_mapping_type, target_variable),
          call. = FALSE
        )
      }

      for (source_index in seq_along(final_source_datasets)) {
        source_dataset <- final_source_datasets[source_index]
        source_variable <- final_source_variables[source_index]
        if (!source_dataset %in% study$dataset_names) {
          stop(
            sprintf("Approved source dataset '%s' for target '%s' is not present in study metadata.", source_dataset, target_variable),
            call. = FALSE
          )
        }
        if (!source_variable %in% study$columns_by_dataset[[source_dataset]]) {
          stop(
            sprintf(
              "Approved source variable '%s' for dataset '%s' and target '%s' is not present in study metadata.",
              source_variable,
              source_dataset,
              target_variable
            ),
            call. = FALSE
          )
        }
      }
    }

    approved_mapping <- dplyr::bind_rows(approved_mapping, data.frame(
      domain = domain,
      target_variable = target_variable,
      target_label = as.character(target_reference[["Variable Label"]][1]),
      core = as.character(target_reference[["Core"]][1]),
      role = as.character(target_reference[["Role"]][1]),
      approved_disposition = approved_disposition,
      approved_mapping_type = final_mapping_type,
      approved_source_datasets = paste(final_source_datasets, collapse = ";"),
      approved_source_variables = paste(final_source_variables, collapse = ";"),
      approved_derivation = final_derivation,
      programmer_comment = trimws(review_row$programmer_comment),
      stringsAsFactors = FALSE
    ))
  }

  result <- list(
    domain = domain,
    review_table = review_table,
    approved_mapping = approved_mapping,
    unresolved_review = review_table[FALSE, , drop = FALSE]
  )
  class(result) <- c("clinical_sdtm_mapping_review", "list")
  result
}
