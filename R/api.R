perform_openai_chat_request <- function(endpoint, api_key, request_body) {
  response <- httr2::request(endpoint) %>%
    httr2::req_headers(Authorization = paste("Bearer", api_key)) %>%
    httr2::req_body_json(request_body, auto_unbox = TRUE) %>%
    httr2::req_timeout(seconds = 180) %>%
    httr2::req_error(is_error = function(response) FALSE) %>%
    httr2::req_perform()

  http_status <- httr2::resp_status(response)
  response_body_text <- httr2::resp_body_string(response)
  response_body <- tryCatch(
    jsonlite::fromJSON(response_body_text, simplifyVector = FALSE),
    error = function(error) NULL
  )

  if (http_status < 200 || http_status >= 300) {
    error_detail <- NULL
    if (is.list(response_body) && is.list(response_body$error) &&
        is.character(response_body$error$message) &&
        length(response_body$error$message) == 1) {
      error_detail <- response_body$error$message
    } else if (is.list(response_body) && is.character(response_body$message) &&
               length(response_body$message) == 1) {
      error_detail <- response_body$message
    }
    if (is.null(error_detail) || !nzchar(trimws(error_detail))) {
      error_detail <- trimws(response_body_text)
    }
    if (!nzchar(error_detail)) {
      error_detail <- "<empty response body>"
    }
    error_detail <- gsub(api_key, "[REDACTED]", error_detail, fixed = TRUE)
    if (nchar(error_detail) > 2000) {
      error_detail <- paste0(substr(error_detail, 1, 2000), "...")
    }
    stop(
      sprintf(
        "OpenAI API request failed with HTTP %s %s: %s",
        http_status,
        httr2::resp_status_desc(response),
        error_detail
      ),
      call. = FALSE
    )
  }

  response_text <- NULL
  if (is.list(response_body) && is.list(response_body$choices) &&
      length(response_body$choices) > 0 &&
      is.list(response_body$choices[[1]]) &&
      is.list(response_body$choices[[1]]$message)) {
    response_text <- response_body$choices[[1]]$message$content
  }
  if (!is.character(response_text) || length(response_text) != 1 || !nzchar(response_text)) {
    if (is.null(response_body)) {
      response_structure <- sprintf(
        "non-JSON character body (%s bytes): %s",
        nchar(response_body_text, type = "bytes"),
        substr(response_body_text, 1, 1000)
      )
    } else {
      response_structure <- paste(
        utils::capture.output(utils::str(
          response_body,
          max.level = 4,
          vec.len = 3,
          give.attr = FALSE
        )),
        collapse = " "
      )
    }
    response_structure <- gsub(api_key, "[REDACTED]", response_structure, fixed = TRUE)
    if (nchar(response_structure) > 2000) {
      response_structure <- paste0(substr(response_structure, 1, 2000), "...")
    }
    stop(
      sprintf(
        paste0(
          "OpenAI API returned HTTP %s but choices[[1]]$message$content ",
          "was absent or empty. Response structure: %s"
        ),
        http_status,
        response_structure
      ),
      call. = FALSE
    )
  }

  response_text
}
