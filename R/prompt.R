build_sdtm_mapping_prompt <- function(
    domain,
    study_inventory,
    domain_reference,
    study_context,
    reusable_guidance = list()) {
  domain <- normalize_sdtm_domain(domain)
  response_contract <- list(
    domain = domain,
    assessment = list(
      overall_confidence = "<HIGH, MEDIUM, or LOW>",
      summary = "<brief assessment>",
      review_notes = list("<review note>")
    ),
    mappings = list(list(
      target_variable = sprintf("<permitted %s target variable>", domain),
      sources = list(list(
        dataset = "<approved source dataset>",
        variable = "<exact column in that dataset>"
      )),
      mapping_type = "<DIRECT, ASSIGNED, DERIVED, or MULTI_SOURCE>",
      derivation_description = "<description or null>",
      controlled_terminology = FALSE,
      confidence = "<HIGH, MEDIUM, or LOW>",
      review_required = TRUE,
      rationale = "<reason for proposed mapping>"
    )),
    unresolved = list(list(
      target_variable = sprintf("<permitted %s target variable>", domain),
      reason = "<reason mapping is unresolved>",
      recommended_action = "Programmer review required."
    )),
    not_applicable = list(list(
      target_variable = sprintf("<permitted %s target variable>", domain),
      reason = "<why the variable does not apply to this study>",
      evidence = "<positive supplied context evidence supporting non-applicability>"
    ))
  )

  study_inventory_json <- jsonlite::toJSON(
    study_inventory,
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  )
  study_context_json <- jsonlite::toJSON(
    study_context,
    dataframe = "rows",
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    na = "null"
  )
  domain_sdtmig_json <- jsonlite::toJSON(
    list(dataset = domain_reference$dataset, variables = domain_reference$variables),
    dataframe = "rows",
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    na = "null"
  )
  permitted_targets_json <- jsonlite::toJSON(
    domain_reference$permitted_targets,
    auto_unbox = TRUE
  )
  response_contract_json <- jsonlite::toJSON(
    response_contract,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  guidance_section <- ""
  output_section_label <- "## E. Output contract\n"
  if (length(reusable_guidance) > 0) {
    guidance_json <- jsonlite::toJSON(
      reusable_guidance,
      auto_unbox = TRUE,
      pretty = TRUE,
      null = "null",
      na = "null"
    )
    guidance_section <- paste0(
      "\n\n## E. Reusable approved mapping guidance\n",
      "These are reusable mapping principles learned from previous programmer review.\n",
      "Apply them only where relevant to this project's approved source metadata and study context.\n",
      "Determine the actual current source datasets and exact source variables from the approved study metadata; the guidance does not supply or approve sources.\n",
      "Never invent a missing source dataset or variable to satisfy reusable guidance.\n",
      "Current SDTMIG requirements, approved metadata, protocol, annotated CRF, Treatment Arms context, and deterministic package safeguards remain authoritative.\n",
      "The proposal and review table must still report the actual project-specific sources, mapping type, derivation, rationale, confidence, and disposition.\n",
      guidance_json
    )
    output_section_label <- "## F. Output contract\n"
  }

  paste0(
    "## A. Task contract\n",
    sprintf("Propose a conservative draft %s SDTM mapping specification for programmer review.\n", domain),
    "This is an experimental proposal, not an approved specification.\n",
    "Do not return R code, SAS code, or executable transformation code.\n",
    "Use only the supplied source datasets, exact source column names, and supplied values.\n",
    "Do not invent source variables, collected values, or target variables.\n",
    sprintf("Multiple source datasets may contribute to %s; identify relevant sources from the approved metadata rather than dataset-name conventions.\n", domain),
    "Similar source and target names are clues only, not proof of a correct mapping.\n",
    "Variables ending in _STD may be useful evidence but are not automatically SDTM-compliant.\n",
    "Consider source meaning, datatype, context, distinct values, SDTMIG Core, and CDISC Notes.\n",
    "SDTMIG metadata defines the permitted target variables and their SDTM meaning.\n",
    "Approved source metadata defines the permitted raw source datasets, variables, and supplied distinct values.\n",
    "Protocol, annotated CRF, and Treatment Arms inputs are supplementary study context and should influence mappings only where relevant.\n",
    "The annotated CRF provides mapping evidence but is not an approved SDTM specification.\n",
    "Treatment-arm context is not required or relevant for every domain.\n",
    "If supplied evidence disagrees, lower confidence or leave the mapping unresolved and explain the conflict.\n",
    "Do not represent protocol text, CRF annotations, or Treatment Arms fields as raw-data source variables.\n",
    "Every sources entry must be an exact dataset/variable pair from approved source metadata.\n",
    "Explain contextual evidence in derivation_description and/or rationale.\n",
    "A target may be assigned or derived from study context with an empty sources array when defensible.\n",
    "Do not map a target solely because supplementary context contains a same-named field.\n",
    "Treatment-arm targets require a defensible relationship between an approved participant-level assignment source and reviewed arm definitions.\n",
    "Preserve uncertainty when participant-level assignment cannot be established.\n",
    "Reusable guidance and supplementary study context cannot manufacture missing source evidence.\n",
    "Place a variable in mappings only when a defensible mapping can be proposed.\n",
    "Place a variable in unresolved when it may apply but supplied evidence is insufficient.\n",
    "Place a variable in not_applicable only when supplied context positively establishes that it does not apply.\n",
    "Do not use not_applicable merely because no raw source variable was found.\n",
    "Return every permitted target exactly once across mappings, unresolved, and not_applicable.\n",
    "Do not return a target more than once or in more than one disposition.\n\n",
    "## B. Approved study metadata\n",
    study_inventory_json,
    sprintf("\n\n## C. Supplementary study context for %s\n", domain),
    study_context_json,
    sprintf("\n\n## D. %s SDTMIG metadata\n", domain),
    domain_sdtmig_json,
    guidance_section,
    "\n\n", output_section_label,
    "Return one JSON object only, without Markdown fences or surrounding prose.\n",
    "Every target_variable in mappings, unresolved, and not_applicable must exactly match one value in this permitted list:\n",
    permitted_targets_json,
    "\nUse sources as explicit dataset/variable pairs.\n",
    response_contract_json
  )
}

build_dm_prompt <- function(
    study_inventory,
    dm_reference,
    dm_study_context,
    reusable_guidance = list()) {
  build_sdtm_mapping_prompt(
    domain = "DM",
    study_inventory = study_inventory,
    domain_reference = dm_reference,
    study_context = dm_study_context,
    reusable_guidance = reusable_guidance
  )
}
