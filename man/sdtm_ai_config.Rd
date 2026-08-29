\name{sdtm_ai_config}
\alias{sdtm_ai_config}
\title{Configure the SDTM AI Proof of Concept}
\usage{
sdtm_ai_config(
  provider = "openai",
  model,
  api_key_env = "OPENAI_API_KEY",
  base_url = "https://api.openai.com/v1",
  max_output_tokens = 8000
)
}
\arguments{
  \item{provider}{Provider name. Only \code{"openai"} is supported.}
  \item{model}{Model identifier sent to the provider.}
  \item{api_key_env}{Environment-variable name containing the API key.}
  \item{base_url}{OpenAI-compatible API base URL.}
  \item{max_output_tokens}{Maximum response tokens requested from the model.}
}
\value{A named list with class \code{sdtm_ai_config}.}
\description{
Creates a simple live-provider configuration without accepting or storing an API key.
}
\examples{
config <- sdtm_ai_config(
  model = "gpt-5.6",
  api_key_env = "OPENAI_API_KEY"
)
}
