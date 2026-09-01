validate_mapping_response <- function(
    proposal,
    domain,
    permitted_targets,
    study_dataset_names,
    columns_by_dataset) {
  required_collections <- c("domain", "assessment", "mappings", "unresolved", "not_applicable")

  if (!is.list(proposal) || !all(required_collections %in% names(proposal))) {
    stop(
      "Malformed AI response: expected domain, assessment, mappings, unresolved, and not_applicable.",
      call. = FALSE
    )
  }
  if (!identical(proposal$domain, domain)) {
    stop(sprintf("AI response domain must exactly equal '%s'.", domain), call. = FALSE)
  }
  if (!is.list(proposal$assessment)) {
    stop("Malformed AI response: assessment must be a JSON object.", call. = FALSE)
  }
  if (!is.list(proposal$mappings) || !is.list(proposal$unresolved) ||
      !is.list(proposal$not_applicable)) {
    stop("Malformed AI response: disposition collections must be JSON arrays.", call. = FALSE)
  }

  allowed_mapping_types <- c("DIRECT", "ASSIGNED", "DERIVED", "MULTI_SOURCE")
  mapped_targets <- character()
  unresolved_targets <- character()
  not_applicable_targets <- character()

  for (mapping_index in seq_along(proposal$mappings)) {
    mapping <- proposal$mappings[[mapping_index]]
    if (!is.list(mapping) ||
        !all(c("target_variable", "sources", "mapping_type") %in% names(mapping))) {
      stop(
        sprintf("Malformed AI response: mappings item %s is missing required fields.", mapping_index),
        call. = FALSE
      )
    }
    target_variable <- mapping$target_variable

    if (!is.character(target_variable) || length(target_variable) != 1 ||
        !target_variable %in% permitted_targets) {
      stop(
        sprintf(
          "AI returned target variable '%s', but it is not present in the supplied %s SDTMIG metadata.",
          paste(target_variable, collapse = ", "),
          domain
        ),
        call. = FALSE
      )
    }
    mapped_targets <- c(mapped_targets, target_variable)

    if (!is.character(mapping$mapping_type) || length(mapping$mapping_type) != 1 ||
        !mapping$mapping_type %in% allowed_mapping_types) {
      stop(
        sprintf(
          "AI returned invalid mapping_type for target '%s'. Allowed values are DIRECT, ASSIGNED, DERIVED, and MULTI_SOURCE.",
          target_variable
        ),
        call. = FALSE
      )
    }

    if (!is.list(mapping$sources)) {
      stop(sprintf("AI sources for target '%s' must be a JSON array.", target_variable), call. = FALSE)
    }

    for (source_index in seq_along(mapping$sources)) {
      if (!is.list(mapping$sources[[source_index]]) ||
          !all(c("dataset", "variable") %in% names(mapping$sources[[source_index]]))) {
        stop(
          sprintf("Malformed AI response: source %s for target '%s' must contain dataset and variable.", source_index, target_variable),
          call. = FALSE
        )
      }
      source_dataset <- mapping$sources[[source_index]]$dataset
      source_variable <- mapping$sources[[source_index]]$variable

      if (!is.character(source_dataset) || length(source_dataset) != 1 ||
          !source_dataset %in% study_dataset_names) {
        stop(
          sprintf(
            "AI returned source dataset '%s', but it is not present in approved study metadata.",
            paste(source_dataset, collapse = ", ")
          ),
          call. = FALSE
        )
      }
      if (!is.character(source_variable) || length(source_variable) != 1 ||
          !source_variable %in% columns_by_dataset[[source_dataset]]) {
        stop(
          sprintf(
            "AI returned source variable '%s' for dataset '%s', but that variable is not present in approved study metadata.",
            paste(source_variable, collapse = ", "),
            source_dataset
          ),
          call. = FALSE
        )
      }
    }
  }

  for (unresolved_index in seq_along(proposal$unresolved)) {
    if (!is.list(proposal$unresolved[[unresolved_index]]) ||
        !"target_variable" %in% names(proposal$unresolved[[unresolved_index]])) {
      stop(
        sprintf("Malformed AI response: unresolved item %s is missing target_variable.", unresolved_index),
        call. = FALSE
      )
    }
    target_variable <- proposal$unresolved[[unresolved_index]]$target_variable
    if (!is.character(target_variable) || length(target_variable) != 1 ||
        !target_variable %in% permitted_targets) {
      stop(
        sprintf(
          "AI returned unresolved target variable '%s', but it is not present in the supplied %s SDTMIG metadata.",
          paste(target_variable, collapse = ", "),
          domain
        ),
        call. = FALSE
      )
    }
    unresolved_targets <- c(unresolved_targets, target_variable)
  }

  for (not_applicable_index in seq_along(proposal$not_applicable)) {
    if (!is.list(proposal$not_applicable[[not_applicable_index]]) ||
        !all(c("target_variable", "reason", "evidence") %in%
             names(proposal$not_applicable[[not_applicable_index]]))) {
      stop(
        sprintf("Malformed AI response: not_applicable item %s is missing required fields.", not_applicable_index),
        call. = FALSE
      )
    }
    target_variable <- proposal$not_applicable[[not_applicable_index]]$target_variable
    if (!is.character(target_variable) || length(target_variable) != 1 ||
        !target_variable %in% permitted_targets) {
      stop(
        sprintf(
          "AI returned not-applicable target variable '%s', but it is not present in the supplied %s SDTMIG metadata.",
          paste(target_variable, collapse = ", "),
          domain
        ),
        call. = FALSE
      )
    }
    not_applicable_targets <- c(not_applicable_targets, target_variable)
  }

  duplicate_mapped <- unique(mapped_targets[duplicated(mapped_targets)])
  duplicate_unresolved <- unique(unresolved_targets[duplicated(unresolved_targets)])
  duplicate_not_applicable <- unique(not_applicable_targets[duplicated(not_applicable_targets)])

  if (length(duplicate_mapped) > 0) {
    stop(
      sprintf("AI returned duplicate target variable(s) within mappings: %s", paste(duplicate_mapped, collapse = ", ")),
      call. = FALSE
    )
  }
  if (length(duplicate_unresolved) > 0) {
    stop(
      sprintf("AI returned duplicate target variable(s) within unresolved: %s", paste(duplicate_unresolved, collapse = ", ")),
      call. = FALSE
    )
  }
  if (length(duplicate_not_applicable) > 0) {
    stop(
      sprintf(
        "AI returned duplicate target variable(s) within not_applicable: %s",
        paste(duplicate_not_applicable, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  cross_disposition_targets <- unique(c(
    intersect(mapped_targets, unresolved_targets),
    intersect(mapped_targets, not_applicable_targets),
    intersect(unresolved_targets, not_applicable_targets)
  ))
  if (length(cross_disposition_targets) > 0) {
    stop(
      sprintf(
        "AI returned target variable(s) in more than one disposition: %s",
        paste(cross_disposition_targets, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  returned_targets <- c(mapped_targets, unresolved_targets, not_applicable_targets)
  missing_targets <- setdiff(permitted_targets, returned_targets)
  if (length(missing_targets) > 0) {
    stop(
      sprintf(
        "AI response omitted target variable(s) required by the supplied %s SDTMIG metadata: %s",
        domain,
        paste(missing_targets, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

build_mapping_review_table <- function(proposal, domain_reference) {
  domain <- normalize_sdtm_domain(proposal$domain)
  domain_variables <- domain_reference$variables
  review_table <- data.frame(
    domain = character(),
    target_variable = character(),
    target_label = character(),
    core = character(),
    role = character(),
    ai_disposition = character(),
    ai_mapping_type = character(),
    ai_source_datasets = character(),
    ai_source_variables = character(),
    ai_derivation = character(),
    ai_confidence = character(),
    ai_rationale = character(),
    ai_reason = character(),
    ai_evidence = character(),
    programmer_decision = character(),
    approved_source_datasets = character(),
    approved_source_variables = character(),
    approved_mapping_type = character(),
    approved_derivation = character(),
    programmer_comment = character(),
    stringsAsFactors = FALSE
  )

  for (mapping_index in seq_along(proposal$mappings)) {
    mapping <- proposal$mappings[[mapping_index]]
    target_row <- domain_variables[
      domain_variables[["Variable Name"]] == mapping$target_variable,
      ,
      drop = FALSE
    ]
    source_datasets <- character()
    source_variables <- character()

    for (source_index in seq_along(mapping$sources)) {
      source_datasets <- c(source_datasets, mapping$sources[[source_index]]$dataset)
      source_variables <- c(source_variables, mapping$sources[[source_index]]$variable)
    }

    derivation <- mapping$derivation_description
    if (is.null(derivation)) derivation <- ""
    confidence <- mapping$confidence
    if (is.null(confidence)) confidence <- ""
    rationale <- mapping$rationale
    if (is.null(rationale)) rationale <- ""

    review_table <- dplyr::bind_rows(review_table, data.frame(
      domain = domain,
      target_variable = mapping$target_variable,
      target_label = as.character(target_row[["Variable Label"]][1]),
      core = as.character(target_row[["Core"]][1]),
      role = as.character(target_row[["Role"]][1]),
      ai_disposition = "Mapped",
      ai_mapping_type = mapping$mapping_type,
      ai_source_datasets = paste(source_datasets, collapse = ";"),
      ai_source_variables = paste(source_variables, collapse = ";"),
      ai_derivation = derivation,
      ai_confidence = confidence,
      ai_rationale = rationale,
      ai_reason = "",
      ai_evidence = "",
      programmer_decision = "",
      approved_source_datasets = "",
      approved_source_variables = "",
      approved_mapping_type = "",
      approved_derivation = "",
      programmer_comment = "",
      stringsAsFactors = FALSE
    ))
  }

  for (unresolved_index in seq_along(proposal$unresolved)) {
    unresolved <- proposal$unresolved[[unresolved_index]]
    target_row <- domain_variables[
      domain_variables[["Variable Name"]] == unresolved$target_variable,
      ,
      drop = FALSE
    ]
    reason <- unresolved$reason
    if (is.null(reason)) reason <- ""

    review_table <- dplyr::bind_rows(review_table, data.frame(
      domain = domain,
      target_variable = unresolved$target_variable,
      target_label = as.character(target_row[["Variable Label"]][1]),
      core = as.character(target_row[["Core"]][1]),
      role = as.character(target_row[["Role"]][1]),
      ai_disposition = "Unresolved",
      ai_mapping_type = "",
      ai_source_datasets = "",
      ai_source_variables = "",
      ai_derivation = "",
      ai_confidence = "",
      ai_rationale = "",
      ai_reason = reason,
      ai_evidence = "",
      programmer_decision = "",
      approved_source_datasets = "",
      approved_source_variables = "",
      approved_mapping_type = "",
      approved_derivation = "",
      programmer_comment = "",
      stringsAsFactors = FALSE
    ))
  }

  for (not_applicable_index in seq_along(proposal$not_applicable)) {
    not_applicable <- proposal$not_applicable[[not_applicable_index]]
    target_row <- domain_variables[
      domain_variables[["Variable Name"]] == not_applicable$target_variable,
      ,
      drop = FALSE
    ]
    reason <- not_applicable$reason
    if (is.null(reason)) reason <- ""
    evidence <- not_applicable$evidence
    if (is.null(evidence)) evidence <- ""

    review_table <- dplyr::bind_rows(review_table, data.frame(
      domain = domain,
      target_variable = not_applicable$target_variable,
      target_label = as.character(target_row[["Variable Label"]][1]),
      core = as.character(target_row[["Core"]][1]),
      role = as.character(target_row[["Role"]][1]),
      ai_disposition = "Not Applicable",
      ai_mapping_type = "",
      ai_source_datasets = "",
      ai_source_variables = "",
      ai_derivation = "",
      ai_confidence = "",
      ai_rationale = "",
      ai_reason = reason,
      ai_evidence = evidence,
      programmer_decision = "",
      approved_source_datasets = "",
      approved_source_variables = "",
      approved_mapping_type = "",
      approved_derivation = "",
      programmer_comment = "",
      stringsAsFactors = FALSE
    ))
  }

  review_table
}

build_dm_review_table <- function(proposal, dm_variables) {
  build_mapping_review_table(proposal, list(variables = dm_variables))
}
