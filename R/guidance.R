read_mapping_guidance <- function(
    mapping_guidance = NULL,
    domain,
    permitted_targets = NULL) {
  if (is.null(mapping_guidance)) {
    return(list())
  }
  assert_input_file(mapping_guidance, "mapping guidance YAML")
  extension <- tolower(tools::file_ext(mapping_guidance))
  if (!extension %in% c("yaml", "yml")) {
    stop("mapping_guidance must be a .yaml or .yml file.", call. = FALSE)
  }

  guidance_file <- tryCatch(
    yaml::read_yaml(mapping_guidance),
    error = function(error) {
      stop(
        sprintf(
          "Mapping guidance YAML could not be parsed: %s",
          conditionMessage(error)
        ),
        call. = FALSE
      )
    }
  )
  if (!is.list(guidance_file) || !"domains" %in% names(guidance_file)) {
    stop("Mapping guidance YAML must contain a top-level 'domains' mapping.", call. = FALSE)
  }

  domains <- guidance_file$domains
  if (!is.list(domains)) {
    stop("Mapping guidance YAML 'domains' must be a named mapping.", call. = FALSE)
  }
  if (length(domains) > 0) {
    domain_names <- names(domains)
    if (is.null(domain_names) || any(!nzchar(domain_names))) {
      stop("Each mapping guidance domain entry must have a non-empty name.", call. = FALSE)
    }
    if (anyDuplicated(domain_names)) {
      duplicate_domains <- unique(domain_names[duplicated(domain_names)])
      stop(
        sprintf(
          "Mapping guidance YAML contains duplicate domain key(s): %s",
          paste(duplicate_domains, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    if (any(!grepl("^[A-Z][A-Z0-9]{1,7}$", domain_names))) {
      invalid_domains <- domain_names[
        !grepl("^[A-Z][A-Z0-9]{1,7}$", domain_names)
      ]
      stop(
        sprintf(
          "Mapping guidance YAML contains invalid domain name(s): %s",
          paste(invalid_domains, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  validated_domains <- list()
  for (domain_name in names(domains)) {
    domain_guidance <- domains[[domain_name]]
    if (!is.list(domain_guidance)) {
      stop(
        sprintf("Mapping guidance domain '%s' must be a named mapping.", domain_name),
        call. = FALSE
      )
    }
    if (length(domain_guidance) == 0) {
      validated_domains[[domain_name]] <- list()
      next
    }

    target_names <- names(domain_guidance)
    if (is.null(target_names) || any(!nzchar(target_names))) {
      stop(
        sprintf("Every guidance entry in domain '%s' must have a target-variable name.", domain_name),
        call. = FALSE
      )
    }
    if (anyDuplicated(target_names)) {
      duplicate_targets <- unique(target_names[duplicated(target_names)])
      stop(
        sprintf(
          "Mapping guidance domain '%s' contains duplicate target key(s): %s",
          domain_name,
          paste(duplicate_targets, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    invalid_target_names <- target_names[
      !grepl("^[A-Z][A-Z0-9_]{0,7}$", target_names)
    ]
    if (length(invalid_target_names) > 0) {
      stop(
        sprintf(
          "Mapping guidance domain '%s' contains invalid target-variable name(s): %s",
          domain_name,
          paste(invalid_target_names, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    validated_guidance <- list()
    for (target_name in target_names) {
      guidance_text <- domain_guidance[[target_name]]
      if (!is.character(guidance_text) || length(guidance_text) != 1 ||
          is.na(guidance_text) || !nzchar(trimws(guidance_text))) {
        stop(
          sprintf(
            "Mapping guidance for '%s.%s' must be non-empty text.",
            domain_name,
            target_name
          ),
          call. = FALSE
        )
      }
      validated_guidance[[target_name]] <- trimws(guidance_text)
    }
    validated_domains[[domain_name]] <- validated_guidance
  }

  if (!domain %in% names(validated_domains)) {
    return(list())
  }
  requested_guidance <- validated_domains[[domain]]
  if (!is.null(permitted_targets) && length(requested_guidance) > 0) {
    invalid_requested_targets <- setdiff(names(requested_guidance), permitted_targets)
    if (length(invalid_requested_targets) > 0) {
      stop(
        sprintf(
          paste0(
            "Mapping guidance target(s) for requested domain '%s' are not present ",
            "in the supplied SDTMIG metadata: %s"
          ),
          domain,
          paste(invalid_requested_targets, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
  requested_guidance
}
