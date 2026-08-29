#' Configure the SDTM AI proof of concept
#'
#' Creates a simple configuration for the currently supported OpenAI-compatible
#' provider. The API key itself is never accepted or stored in this object; it
#' is read from `api_key_env` only when a mapping request is made.
#'
#' @param provider Provider name. Only `"openai"` is supported.
#' @param model Model identifier sent to the provider.
#' @param api_key_env Environment-variable name containing the API key.
#' @param base_url OpenAI-compatible API base URL.
#' @param max_output_tokens Maximum response tokens requested from the model.
#'
#' @return A named list with class `sdtm_ai_config`.
#' @export
#'
#' @examples
#' config <- sdtm_ai_config(
#'   model = "gpt-5.6",
#'   api_key_env = "OPENAI_API_KEY"
#' )
sdtm_ai_config <- function(
    provider = "openai",
    model,
    api_key_env = "OPENAI_API_KEY",
    base_url = "https://api.openai.com/v1",
    max_output_tokens = 8000) {
  if (!identical(provider, "openai")) {
    stop("Unsupported AI provider. Version 0.1 supports only provider = 'openai'.", call. = FALSE)
  }

  if (!is.character(model) || length(model) != 1 || !nzchar(trimws(model))) {
    stop("model must be one non-empty character value.", call. = FALSE)
  }

  if (!is.character(api_key_env) || length(api_key_env) != 1 || !nzchar(trimws(api_key_env))) {
    stop("api_key_env must be one non-empty environment-variable name.", call. = FALSE)
  }

  if (!is.character(base_url) || length(base_url) != 1 || !nzchar(trimws(base_url))) {
    stop("base_url must be one non-empty URL.", call. = FALSE)
  }

  if (!is.numeric(max_output_tokens) || length(max_output_tokens) != 1 ||
      is.na(max_output_tokens) || max_output_tokens <= 0) {
    stop("max_output_tokens must be one positive number.", call. = FALSE)
  }

  config <- list(
    provider = provider,
    model = model,
    api_key_env = api_key_env,
    base_url = sub("/+$", "", base_url),
    max_output_tokens = as.integer(max_output_tokens)
  )

  class(config) <- c("sdtm_ai_config", "list")
  config
}
