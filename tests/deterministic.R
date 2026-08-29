library(sdtmai)

expect_error_matching <- function(expression, pattern) {
  captured_error <- tryCatch(
    {
      force(expression)
      NULL
    },
    error = function(error) error
  )

  if (is.null(captured_error)) {
    stop(sprintf("Expected an error matching '%s', but no error occurred.", pattern))
  }
  if (!grepl(pattern, conditionMessage(captured_error), ignore.case = TRUE)) {
    stop(sprintf(
      "Expected an error matching '%s', received: %s",
      pattern,
      conditionMessage(captured_error)
    ))
  }
}

config <- sdtm_ai_config(model = "test-model")

project_test_root <- file.path(
  tempdir(),
  paste0("clinical-sdtm-project-", Sys.getpid())
)
network_input_dir <- "\\\\clinicalai_synthetic_test_datasets"
project_fixture <- sdtm_project_create(
  root = project_test_root,
  in_dir = network_input_dir
)
expected_project_fields <- c(
  "root", "in_dir", "metadata_dir", "mapping_dir", "code_dir", "out_dir"
)
stopifnot(
  identical(names(project_fixture), expected_project_fields),
  inherits(project_fixture, "clinical_sdtm_project"),
  identical(project_fixture$in_dir, network_input_dir),
  identical(basename(project_fixture$out_dir), "output"),
  all(dir.exists(unlist(project_fixture[c(
    "root", "metadata_dir", "mapping_dir", "code_dir", "out_dir"
  )]))),
  file.exists(file.path(project_fixture$root, ".clinicalai-sdtm-project"))
)

metadata_input_dir <- file.path(project_fixture$root, "external-fixture")
dir.create(metadata_input_dir)
metadata_csv <- file.path(metadata_input_dir, "source_fixture.CSV")
utils::write.csv(
  data.frame(
    SubjectId = c("S001", "S002", "S003"),
    Group = c("A", "B", "A"),
    Value = c(1, 2, 3),
    Notes = c(
      paste(rep("long free text", 20), collapse = " "),
      paste(rep("another narrative", 20), collapse = " "),
      paste(rep("third narrative", 20), collapse = " ")
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  ),
  metadata_csv,
  row.names = FALSE
)
metadata_project <- sdtm_project_create(
  root = project_fixture$root,
  in_dir = metadata_input_dir
)
generated_metadata <- sdtm_generate_metadata(project = metadata_project)
generated_metadata_file <- file.path(metadata_project$metadata_dir, "meta_data.json")
read_generated_metadata <- sdtmai:::read_approved_study_metadata(
  generated_metadata_file
)
stopifnot(
  file.exists(generated_metadata_file),
  length(generated_metadata) == 1,
  identical(generated_metadata[[1]]$dataset, "source_fixture"),
  identical(generated_metadata[[1]]$file_name, "source_fixture.CSV"),
  identical(generated_metadata[[1]]$columns, c("SubjectId", "Group", "Value", "Notes")),
  identical(generated_metadata[[1]]$row_count, 3L),
  identical(generated_metadata[[1]]$column_types$Group, "character"),
  identical(generated_metadata[[1]]$distinct_values$Group, c("A", "B")),
  is.null(generated_metadata[[1]]$distinct_values$SubjectId),
  is.null(generated_metadata[[1]]$distinct_values$Notes),
  identical(read_generated_metadata$dataset_names, "source_fixture"),
  identical(sdtmai:::resolve_source_reader("source.csv"), "readr::read_csv"),
  identical(sdtmai:::resolve_source_reader("source.SAS7BDAT"), "haven::read_sas")
)
expect_error_matching(
  sdtmai:::resolve_source_reader("source.xlsx"),
  "unsupported source file format.*source.xlsx"
)

txt_instructions_file <- file.path(project_fixture$root, "instructions.txt")
md_instructions_file <- file.path(project_fixture$root, "instructions.md")
writeLines("Add comments before each major programming step.", txt_instructions_file)
writeLines("- Keep joins explicit and readable.", md_instructions_file)
stopifnot(
  identical(
    sdtmai:::read_code_instructions(txt_instructions_file),
    "Add comments before each major programming step."
  ),
  identical(
    sdtmai:::read_code_instructions(md_instructions_file),
    "- Keep joins explicit and readable."
  )
)

valid_guidance_file <- file.path(project_fixture$root, "valid-guidance.yaml")
missing_domain_guidance_file <- file.path(
  project_fixture$root,
  "missing-domain-guidance.yaml"
)
malformed_guidance_file <- file.path(project_fixture$root, "malformed-guidance.yaml")
empty_guidance_file <- file.path(project_fixture$root, "empty-guidance.yaml")
writeLines(
  c(
    "domains:",
    "  DM:",
    "    RACE: >",
    "      Apply the reviewed multi-race handling principle using current sources.",
    "  AE:",
    "    AESEQ: >",
    "      Derive a stable sequence within subject."
  ),
  valid_guidance_file
)
writeLines(
  c(
    "domains:",
    "  AE:",
    "    AESEQ: Derive a stable sequence within subject."
  ),
  missing_domain_guidance_file
)
writeLines(
  c("domains:", "  DM:", "    RACE: [unterminated"),
  malformed_guidance_file
)
writeLines(
  c("domains:", "  DM:", "    RACE: '   '"),
  empty_guidance_file
)

loaded_dm_guidance <- sdtmai:::read_mapping_guidance(
  valid_guidance_file,
  domain = "DM",
  permitted_targets = c("RACE", "USUBJID")
)
stopifnot(
  identical(names(loaded_dm_guidance), "RACE"),
  grepl("reviewed multi-race", loaded_dm_guidance$RACE, fixed = TRUE),
  length(sdtmai:::read_mapping_guidance(
    missing_domain_guidance_file,
    domain = "DM",
    permitted_targets = c("RACE")
  )) == 0,
  length(sdtmai:::read_mapping_guidance(
    NULL,
    domain = "DM",
    permitted_targets = c("RACE")
  )) == 0
)
expect_error_matching(
  sdtmai:::read_mapping_guidance(
    malformed_guidance_file,
    domain = "DM"
  ),
  "could not be parsed"
)
expect_error_matching(
  sdtmai:::read_mapping_guidance(
    empty_guidance_file,
    domain = "DM"
  ),
  "DM.RACE.*non-empty text"
)

expect_error_matching(
  sdtm_generate_mapping(
    domain = "AE",
    metadata = "missing.json",
    sdtmig = "missing.xlsx",
    protocol = "missing.docx",
    acrf = "missing.docx",
    treatment_arms = "missing.xlsx",
    ai_config = config
  ),
  "supports only domain = 'DM'"
)

expect_error_matching(
  sdtm_generate_mapping(
    domain = "DM",
    metadata = "missing.json",
    sdtmig = "missing.xlsx",
    protocol = "missing.docx",
    acrf = "missing.docx",
    treatment_arms = "missing.xlsx",
    ai_config = config
  ),
  "study metadata file does not exist"
)

valid_mapping <- list(
  target_variable = "SEX",
  sources = list(list(dataset = "gl_dm", variable = "SEX_STD")),
  mapping_type = "DIRECT",
  derivation_description = NULL,
  controlled_terminology = TRUE,
  confidence = "HIGH",
  review_required = FALSE,
  rationale = "Synthetic validator fixture."
)

valid_proposal <- list(
  domain = "DM",
  assessment = list(overall_confidence = "MEDIUM", summary = "Fixture", review_notes = list()),
  mappings = list(valid_mapping),
  unresolved = list(),
  not_applicable = list()
)

duplicate_proposal <- valid_proposal
duplicate_proposal$mappings <- list(valid_mapping, valid_mapping)

expect_error_matching(
  sdtmai:::validate_mapping_response(
    duplicate_proposal,
    domain = "DM",
    permitted_targets = c("SEX"),
    study_dataset_names = c("gl_dm"),
    columns_by_dataset = list(gl_dm = c("SEX_STD"))
  ),
  "duplicate.*SEX"
)

invalid_source_proposal <- valid_proposal
invalid_source_proposal$mappings[[1]]$sources[[1]]$variable <- "SEX_UNKNOWN"

expect_error_matching(
  sdtmai:::validate_mapping_response(
    invalid_source_proposal,
    domain = "DM",
    permitted_targets = c("SEX"),
    study_dataset_names = c("gl_dm"),
    columns_by_dataset = list(gl_dm = c("SEX_STD"))
  ),
  "SEX_UNKNOWN.*not present"
)

study_fixture <- list(
  metadata = list(
    list(
      dataset = "gl_dm",
      file_name = "gl_dm.sas7bdat",
      file_path = "//archive.local/provenance/gl_dm.sas7bdat"
    ),
    list(
      dataset = "gl_enroll",
      file_name = "gl_enroll.csv",
      file_path = "//archive.local/provenance/gl_enroll.csv"
    )
  ),
  dataset_names = c("gl_dm", "gl_enroll"),
  columns_by_dataset = list(
    gl_dm = c("project", "SubjectId", "SiteNumber", "Folder", "SEX_STD", "AGE"),
    gl_enroll = c("project", "SubjectId", "SiteNumber", "Folder", "COUNTRY")
  )
)
dm_reference_fixture <- list(
  permitted_targets = c("SEX", "AGE", "COUNTRY"),
  variables = data.frame(
    "Variable Name" = c("SEX", "AGE", "COUNTRY"),
    "Variable Label" = c("Sex", "Age", "Country"),
    Core = c("Req", "Exp", "Req"),
    Role = c("Qualifier", "Qualifier", "Qualifier"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
)

prompt_dm_reference_fixture <- list(
  dataset = data.frame(
    "Dataset Name" = "DM",
    "Dataset Label" = "Demographics",
    Structure = "One record per subject",
    check.names = FALSE,
    stringsAsFactors = FALSE
  ),
  variables = data.frame(
    "Dataset Name" = c("DM", "DM"),
    "Variable Name" = c("RACE", "SEX"),
    "Variable Label" = c("Race", "Sex"),
    Core = c("Exp", "Req"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  ),
  permitted_targets = c("RACE", "SEX")
)
prompt_study_inventory_fixture <- list(list(
  dataset = "gl_dm",
  file_name = "gl_dm.csv",
  columns = list("WHITE_RAW", "ASIAN_RAW", "SEX_STD"),
  row_count = 3,
  column_types = list(
    WHITE_RAW = "numeric",
    ASIAN_RAW = "numeric",
    SEX_STD = "character"
  ),
  distinct_values = list(SEX_STD = c("F", "M"))
))
prompt_dm_context_fixture <- list(
  protocol_context = "Synthetic protocol context.",
  annotated_crf_context = "Synthetic annotated CRF context.",
  treatment_arms = data.frame(
    Cohort = "Cohort 1",
    ARMCD = "A1",
    ARM = "Arm 1",
    stringsAsFactors = FALSE
  )
)
prompt_without_guidance <- sdtmai:::build_dm_prompt(
  prompt_study_inventory_fixture,
  prompt_dm_reference_fixture,
  prompt_dm_context_fixture
)
prompt_with_empty_guidance <- sdtmai:::build_dm_prompt(
  prompt_study_inventory_fixture,
  prompt_dm_reference_fixture,
  prompt_dm_context_fixture,
  reusable_guidance = list()
)
prompt_with_guidance <- sdtmai:::build_dm_prompt(
  prompt_study_inventory_fixture,
  prompt_dm_reference_fixture,
  prompt_dm_context_fixture,
  reusable_guidance = loaded_dm_guidance
)
stopifnot(
  identical(prompt_without_guidance, prompt_with_empty_guidance),
  !grepl("Reusable approved mapping guidance", prompt_without_guidance, fixed = TRUE),
  grepl("## E. Reusable approved mapping guidance", prompt_with_guidance, fixed = TRUE),
  grepl("reviewed multi-race", prompt_with_guidance, fixed = TRUE),
  !grepl("AESEQ", prompt_with_guidance, fixed = TRUE),
  grepl("WHITE_RAW", prompt_with_guidance, fixed = TRUE),
  grepl("actual current source datasets and exact source variables", prompt_with_guidance, fixed = TRUE),
  grepl("Never invent a missing source dataset or variable", prompt_with_guidance, fixed = TRUE),
  grepl("## F. Output contract", prompt_with_guidance, fixed = TRUE)
)

review_row <- data.frame(
  domain = "DM",
  target_variable = "SEX",
  target_label = "Sex",
  core = "Req",
  role = "Qualifier",
  ai_disposition = "Mapped",
  ai_mapping_type = "DIRECT",
  ai_source_datasets = "gl_dm",
  ai_source_variables = "SEX_STD",
  ai_derivation = "",
  ai_confidence = "HIGH",
  ai_rationale = "Fixture",
  ai_reason = "",
  ai_evidence = "",
  programmer_decision = "Accept",
  approved_source_datasets = "",
  approved_source_variables = "",
  approved_mapping_type = "",
  approved_derivation = "",
  programmer_comment = "Reviewed",
  stringsAsFactors = FALSE
)

accepted_review <- sdtmai:::validate_programmer_review(
  review_row,
  study_fixture,
  dm_reference_fixture
)
stopifnot(
  accepted_review$approved_mapping$approved_mapping_type == "DIRECT",
  accepted_review$approved_mapping$approved_source_datasets == "gl_dm",
  accepted_review$approved_mapping$approved_source_variables == "SEX_STD"
)

invalid_accept <- review_row
invalid_accept$ai_disposition <- "Unresolved"
expect_error_matching(
  sdtmai:::validate_programmer_review(
    invalid_accept,
    study_fixture,
    dm_reference_fixture
  ),
  "Accept.*only valid.*Mapped"
)

resolved_row <- review_row
resolved_row$target_variable <- "AGE"
resolved_row$target_label <- "Age"
resolved_row$ai_disposition <- "Unresolved"
resolved_row$ai_mapping_type <- ""
resolved_row$ai_source_datasets <- ""
resolved_row$ai_source_variables <- ""
resolved_row$programmer_decision <- " Resolve "
resolved_row$approved_source_datasets <- " gl_dm "
resolved_row$approved_source_variables <- " AGE "
resolved_row$approved_mapping_type <- "DIRECT"

resolved_review <- sdtmai:::validate_programmer_review(
  resolved_row,
  study_fixture,
  dm_reference_fixture
)
stopifnot(
  resolved_review$approved_mapping$approved_disposition == "Mapped",
  resolved_review$approved_mapping$approved_source_datasets == "gl_dm",
  resolved_review$approved_mapping$approved_source_variables == "AGE"
)

invalid_source_review <- resolved_row
invalid_source_review$approved_source_variables <- "AGE_UNKNOWN"
expect_error_matching(
  sdtmai:::validate_programmer_review(
    invalid_source_review,
    study_fixture,
    dm_reference_fixture
  ),
  "AGE_UNKNOWN.*not present"
)

mismatched_sources <- resolved_row
mismatched_sources$approved_source_datasets <- "gl_dm;gl_enroll"
mismatched_sources$approved_source_variables <- "AGE"
expect_error_matching(
  sdtmai:::validate_programmer_review(
    mismatched_sources,
    study_fixture,
    dm_reference_fixture
  ),
  "counts differ.*AGE"
)

not_applicable_row <- review_row
not_applicable_row$programmer_decision <- "Not Applicable"
not_applicable_review <- sdtmai:::validate_programmer_review(
  not_applicable_row,
  study_fixture,
  dm_reference_fixture
)
stopifnot(
  not_applicable_review$approved_mapping$approved_disposition == "Not Applicable",
  not_applicable_review$approved_mapping$approved_mapping_type == "",
  not_applicable_review$approved_mapping$approved_source_datasets == ""
)

duplicate_review <- rbind(review_row, review_row)
expect_error_matching(
  sdtmai:::validate_programmer_review(
    duplicate_review,
    study_fixture,
    dm_reference_fixture
  ),
  "Duplicate target.*SEX"
)

invalid_decision <- review_row
invalid_decision$programmer_decision <- "Approved"
expect_error_matching(
  sdtmai:::validate_programmer_review(
    invalid_decision,
    study_fixture,
    dm_reference_fixture
  ),
  "Invalid.*programmer decision.*SEX"
)

derived_without_derivation <- resolved_row
derived_without_derivation$approved_mapping_type <- "DERIVED"
derived_without_derivation$approved_derivation <- ""
expect_error_matching(
  sdtmai:::validate_programmer_review(
    derived_without_derivation,
    study_fixture,
    dm_reference_fixture
  ),
  "DERIVED.*AGE.*derivation"
)

expect_error_matching(
  sdtm_generate_code(
    mapping = "missing-approved-mapping.csv",
    metadata = "missing.json",
    ai_config = config
  ),
  "approved mapping CSV file does not exist"
)

approved_code_row <- accepted_review$approved_mapping[, c(
  "domain", "target_variable", "approved_disposition",
  "approved_mapping_type", "approved_source_datasets",
  "approved_source_variables", "approved_derivation", "programmer_comment"
)]

unsupported_code_domain <- approved_code_row
unsupported_code_domain$domain <- "AE"
expect_error_matching(
  sdtmai:::validate_code_mapping(unsupported_code_domain, study_fixture),
  "supports only DM.*AE"
)

invalid_code_source <- approved_code_row
invalid_code_source$approved_source_variables <- "SEX_UNKNOWN"
expect_error_matching(
  sdtmai:::validate_code_mapping(invalid_code_source, study_fixture),
  "SEX_UNKNOWN.*not present"
)

unsupported_extension_study <- study_fixture
unsupported_extension_study$metadata[[1]]$file_name <- "gl_dm.xlsx"
expect_error_matching(
  sdtmai:::validate_code_mapping(
    approved_code_row,
    unsupported_extension_study
  ),
  "unsupported source file format.*gl_dm.xlsx"
)

duplicate_code_target <- rbind(approved_code_row, approved_code_row)
expect_error_matching(
  sdtmai:::validate_code_mapping(duplicate_code_target, study_fixture),
  "Duplicate mapped target.*SEX"
)

code_specification <- sdtmai:::validate_code_mapping(
  approved_code_row,
  study_fixture
)
approved_identifiers <- sdtmai:::validate_source_identifiers(
  code_specification,
  study_fixture,
  project_key = "project",
  subject_key = "SubjectId",
  site_key = "SiteNumber",
  visit_key = "Folder",
  record_key = ""
)
stopifnot(
  identical(approved_identifiers$subject_key, "SubjectId"),
  identical(approved_identifiers$record_key, "")
)

missing_subject_study <- study_fixture
missing_subject_study$columns_by_dataset$gl_dm <- setdiff(
  missing_subject_study$columns_by_dataset$gl_dm,
  "SubjectId"
)
expect_error_matching(
  sdtmai:::validate_source_identifiers(
    code_specification,
    missing_subject_study,
    project_key = "project",
    subject_key = "SubjectId",
    site_key = "SiteNumber",
    visit_key = "Folder",
    record_key = ""
  ),
  "gl_dm.*configured subject key.*SubjectId"
)

project_code_row <- approved_code_row
project_code_row$target_variable <- "STUDYID"
project_code_row$approved_source_variables <- "project"
project_code_specification <- sdtmai:::validate_code_mapping(
  project_code_row,
  study_fixture
)
expect_error_matching(
  sdtmai:::validate_source_identifiers(
    project_code_specification,
    study_fixture,
    project_key = "missing_project_key",
    subject_key = "SubjectId",
    site_key = "SiteNumber",
    visit_key = "Folder",
    record_key = ""
  ),
  "gl_dm.*configured project key.*missing_project_key"
)

code_prompt <- sdtmai:::build_dm_code_prompt(
  code_specification$mapped_rows,
  code_specification$relevant_metadata,
  approved_identifiers
)
dm_prompt_mentions <- gregexpr("object named exactly dm", code_prompt, fixed = TRUE)[[1]]
stopifnot(
  sum(dm_prompt_mentions > 0) >= 2,
  grepl("## Mandatory final reminder", code_prompt, fixed = TRUE),
  grepl("## Approved source identifiers", code_prompt, fixed = TRUE),
  grepl('"subject_key": "SubjectId"', code_prompt, fixed = TRUE),
  grepl('"record_key": ""', code_prompt, fixed = TRUE),
  grepl("approved subject-level join key", code_prompt, fixed = TRUE),
  grepl("approved identifiers for project/study, site, visit/folder, and record concepts", code_prompt, fixed = TRUE),
  grepl("Do not infer or substitute different identifier variables", code_prompt, fixed = TRUE),
  grepl("do not join datasets using guessed keys", code_prompt, fixed = TRUE)
)
stopifnot(
  identical(
    code_specification$relevant_metadata[[1]]$reader,
    "haven::read_sas"
  )
)

project_code_prompt <- sdtmai:::build_dm_code_prompt(
  code_specification$mapped_rows,
  code_specification$relevant_metadata,
  approved_identifiers,
  programmer_instructions = sdtmai:::read_code_instructions(
    txt_instructions_file
  )
)
expected_project_assignment <- paste0(
  "in_dir <- ",
  sdtmai:::format_r_string(project_fixture$in_dir)
)
project_source_read_block <- sdtmai:::build_source_read_block(
  project_fixture$in_dir,
  code_specification$relevant_metadata
)
csv_source_metadata <- list(
  dataset = "gl_enroll",
  object_name = "gl_enroll",
  file_name = "gl_enroll.csv",
  file_format = "CSV",
  reader = "readr::read_csv",
  approved_source_variables = "COUNTRY"
)
mixed_source_read_block <- sdtmai:::build_source_read_block(
  project_fixture$in_dir,
  c(code_specification$relevant_metadata, list(csv_source_metadata))
)
stopifnot(
  grepl(expected_project_assignment, project_source_read_block, fixed = TRUE),
  grepl('gl_dm <- haven::read_sas(', project_source_read_block, fixed = TRUE),
  grepl('file.path(in_dir, "gl_dm.sas7bdat")', project_source_read_block, fixed = TRUE),
  grepl('gl_enroll <- readr::read_csv(', mixed_source_read_block, fixed = TRUE),
  grepl('file.path(in_dir, "gl_enroll.csv")', mixed_source_read_block, fixed = TRUE),
  grepl('show_col_types = FALSE', mixed_source_read_block, fixed = TRUE),
  grepl("Generate only the R transformation block", project_code_prompt, fixed = TRUE),
  grepl("already created in_dir and loaded every approved required source dataset", project_code_prompt, fixed = TRUE),
  grepl("## Available preloaded source objects", project_code_prompt, fixed = TRUE),
  grepl('"object_name": "gl_dm"', project_code_prompt, fixed = TRUE),
  !grepl("gl_dm.sas7bdat", project_code_prompt, fixed = TRUE),
  !grepl("haven::read_sas", project_code_prompt, fixed = TRUE),
  !grepl("readr::read_csv", project_code_prompt, fixed = TRUE),
  !grepl("project$in_dir", project_code_prompt, fixed = TRUE),
  !grepl("//archive.local/provenance", project_code_prompt, fixed = TRUE),
  grepl("## Programmer code-generation instructions", project_code_prompt, fixed = TRUE),
  grepl("Add comments before each major programming step.", project_code_prompt, fixed = TRUE),
  grepl("1. Approved mapping and approved metadata", project_code_prompt, fixed = TRUE),
  grepl("3. Programmer code-generation instructions", project_code_prompt, fixed = TRUE),
  grepl("Ignore any instruction that conflicts", project_code_prompt, fixed = TRUE),
  grepl("Do not define or reference in_dir", project_code_prompt, fixed = TRUE),
  grepl("Do not generate source reads", project_code_prompt, fixed = TRUE)
)

valid_transformation_code <- 'dm <- gl_dm %>% dplyr::transmute(SEX = SEX_STD)'
stopifnot(sdtmai:::validate_generated_dm_transformation(
  valid_transformation_code,
  code_specification$relevant_metadata
))
expect_error_matching(
  sdtmai:::validate_generated_dm_transformation(
    paste(
      'gl_dm <- haven::read_sas(file.path(in_dir, "gl_dm.sas7bdat"))',
      valid_transformation_code,
      sep = "\n"
    ),
    code_specification$relevant_metadata
  ),
  "must not define input paths or read source datasets"
)
expect_error_matching(
  sdtmai:::validate_generated_dm_transformation(
    paste('gl_dm <- dplyr::filter(gl_dm, TRUE)', valid_transformation_code, sep = "\n"),
    code_specification$relevant_metadata
  ),
  "must not overwrite preloaded source object.*gl_dm"
)

default_source_read_block <- sdtmai:::build_source_read_block(
  "{{APPROVED_HOST_DATA_PATH}}",
  code_specification$relevant_metadata
)
valid_generated_code <- sdtmai:::combine_dm_code_blocks(
  default_source_read_block,
  valid_transformation_code
)
stopifnot(
  startsWith(valid_generated_code, default_source_read_block),
  endsWith(valid_generated_code, valid_transformation_code)
)
stopifnot(sdtmai:::validate_generated_dm_code(
  valid_generated_code,
  study_fixture,
  code_specification$relevant_metadata
))

historical_path_code <- paste(
  valid_generated_code,
  'historical_input <- "//archive.local/provenance/gl_dm.sas7bdat"',
  sep = "\n"
)
expect_error_matching(
  sdtmai:::validate_generated_dm_code(
    historical_path_code,
    study_fixture,
    code_specification$relevant_metadata
  ),
  "must not use metadata file_path"
)

wrong_reader_code <- sub(
  "haven::read_sas",
  "readr::read_csv",
  valid_generated_code,
  fixed = TRUE
)
expect_error_matching(
  sdtmai:::validate_generated_dm_code(
    wrong_reader_code,
    study_fixture,
    code_specification$relevant_metadata
  ),
  "must read approved source file.*haven::read_sas"
)

concrete_generated_code <- sdtmai:::combine_dm_code_blocks(
  project_source_read_block,
  valid_transformation_code
)
stopifnot(sdtmai:::validate_generated_dm_code(
  concrete_generated_code,
  study_fixture,
  code_specification$relevant_metadata,
  approved_input_dir = project_fixture$in_dir
))

expect_error_matching(
  sdtmai:::validate_generated_dm_code(
    paste0("```r\n", valid_generated_code, "\n```"),
    study_fixture,
    code_specification$relevant_metadata
  ),
  "Markdown code fences"
)

missing_dm_code <- sub("\ndm <-", "\ndm_result <-", valid_generated_code, fixed = TRUE)
expect_error_matching(
  sdtmai:::validate_generated_dm_code(
    missing_dm_code,
    study_fixture,
    code_specification$relevant_metadata
  ),
  "final object named dm"
)

diagnostic_test_key <- "sk-diagnostic-redaction-sentinel"
missing_dm_diagnostic <- tryCatch(
  {
    sdtmai:::validate_generated_dm_code(
      paste(missing_dm_code, diagnostic_test_key, sep = "\n"),
      study_fixture,
      code_specification$relevant_metadata,
      api_key = diagnostic_test_key
    )
    ""
  },
  error = function(error) conditionMessage(error)
)
stopifnot(
  grepl("Generated code excerpt", missing_dm_diagnostic, fixed = TRUE),
  grepl("dm_result <-", missing_dm_diagnostic, fixed = TRUE),
  grepl("[REDACTED]", missing_dm_diagnostic, fixed = TRUE),
  !grepl(diagnostic_test_key, missing_dm_diagnostic, fixed = TRUE)
)

setwd_code <- paste('setwd("C:/clinical-data")', valid_generated_code, sep = "\n")
expect_error_matching(
  sdtmai:::validate_generated_dm_code(
    setwd_code,
    study_fixture,
    code_specification$relevant_metadata
  ),
  "must not call setwd"
)

cat("Deterministic package tests passed.\n")
