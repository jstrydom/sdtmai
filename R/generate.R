#' Generate a draft SDTM mapping for programmer review
#'
#' Reads approved source metadata, local SDTMIG metadata, and optional study
#' context; requests one structured proposal from an OpenAI-compatible endpoint;
#' validates the response; and optionally writes a combined review CSV.
#'
#' @param domain SDTM domain present in the supplied SDTMIG workbook.
#' @param metadata Path to the approved study metadata JSON.
#' @param sdtmig Path to the local SDTMIG workbook.
#' @param protocol Optional path to the study protocol DOCX.
#' @param acrf Optional path to the annotated CRF DOCX.
#' @param treatment_arms Optional path to the Treatment Arms workbook.
#' @param ai_config Configuration created by [sdtm_ai_config()].
#' @param mapping_guidance Optional path to a reusable mapping-guidance YAML
#'   file. Guidance is intended only for non-trivial reusable logic; direct
#'   mappings should normally be omitted.
#' @param output_file Optional CSV path for the programmer review table. Use
#'   `NULL` to return the result without writing a file.
#'
#' @return A list with class `clinical_sdtm_mapping_result` containing the
#'   domain, assessment, mapped, unresolved, not-applicable, and review-table
#'   results. Expected programmer decisions are `Accept`, `Amend`,
#'   `Resolve`, and `Not Applicable`; [sdtm_validate_mapping()] enforces them.
#' @export
#'
#' @examples
#' \dontrun{
#' config <- sdtm_ai_config(model = "gpt-5.6")
#' result <- sdtm_generate_mapping(
#'   domain = "DM",
#'   metadata = "input/meta_data.json",
#'   sdtmig = "input/SDTMIG_v3.4.xlsx",
#'   protocol = "input/ABC-PHARM-01_Synthetic_Phase_I_Protocol.docx",
#'   acrf = "input/ABC-PHARM-01_Annotated_CRF.docx",
#'   treatment_arms = "input/ABC-PHARM-01_Treatment_Arms.xlsx",
#'   mapping_guidance = "mapping_library/sdtm_mapping_guidance.yaml",
#'   ai_config = config,
#'   output_file = "output/DM_mapping_review.csv"
#' )
#' }
sdtm_generate_mapping <- function(
    domain = "DM",
    metadata,
    sdtmig,
    protocol = NULL,
    acrf = NULL,
    treatment_arms = NULL,
    ai_config,
    mapping_guidance = NULL,
    output_file = NULL) {
  domain <- normalize_sdtm_domain(domain)

  if (!inherits(ai_config, "sdtm_ai_config")) {
    stop("ai_config must be created by sdtm_ai_config().", call. = FALSE)
  }

  input_paths <- list(
    "study metadata" = metadata,
    "SDTMIG workbook" = sdtmig
  )
  for (input_label in names(input_paths)) {
    assert_input_file(input_paths[[input_label]], input_label)
  }

  if (!is.null(output_file) &&
      (!is.character(output_file) || length(output_file) != 1 || !nzchar(output_file))) {
    stop("output_file must be NULL or one non-empty CSV path.", call. = FALSE)
  }

  study <- read_approved_study_metadata(metadata)
  domain_reference <- read_sdtmig_domain_reference(sdtmig, domain)
  study_context <- read_sdtm_study_context(protocol, acrf, treatment_arms)
  reusable_guidance <- read_mapping_guidance(
    mapping_guidance,
    domain = domain,
    permitted_targets = domain_reference$permitted_targets
  )
  prompt <- build_sdtm_mapping_prompt(
    domain = domain,
    study_inventory = study$inventory,
    domain_reference = domain_reference,
    study_context = study_context,
    reusable_guidance = reusable_guidance
  )

  api_key <- Sys.getenv(ai_config$api_key_env, unset = "")
  if (!nzchar(api_key)) {
    stop(
      sprintf("AI credential environment variable is absent or empty: %s", ai_config$api_key_env),
      call. = FALSE
    )
  }

  endpoint <- paste0(ai_config$base_url, "/chat/completions")
  request_body <- list(
    model = ai_config$model,
    messages = list(list(role = "user", content = prompt)),
    max_completion_tokens = ai_config$max_output_tokens,
    response_format = list(type = "json_object")
  )

  response_text <- perform_openai_chat_request(endpoint, api_key, request_body)

  proposal <- tryCatch(
    jsonlite::fromJSON(response_text, simplifyVector = FALSE),
    error = function(error) {
      stop(sprintf("Malformed AI response JSON: %s", conditionMessage(error)), call. = FALSE)
    }
  )

  validate_mapping_response(
    proposal = proposal,
    domain = domain,
    permitted_targets = domain_reference$permitted_targets,
    study_dataset_names = study$dataset_names,
    columns_by_dataset = study$columns_by_dataset
  )

  review_table <- build_mapping_review_table(proposal, domain_reference)

  result <- list(
    domain = domain,
    assessment = proposal$assessment,
    mapped = proposal$mappings,
    unresolved = proposal$unresolved,
    not_applicable = proposal$not_applicable,
    review_table = review_table
  )
  class(result) <- c("clinical_sdtm_mapping_result", "list")

  if (!is.null(output_file)) {
    output_directory <- dirname(output_file)
    if (!dir.exists(output_directory)) {
      dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
    }
    utils::write.csv(review_table, output_file, row.names = FALSE, na = "")
  }

  result
}
