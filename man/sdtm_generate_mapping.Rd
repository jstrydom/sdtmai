\name{sdtm_generate_mapping}
\alias{sdtm_generate_mapping}
\title{Generate a Draft SDTM Mapping for Programmer Review}
\usage{
sdtm_generate_mapping(
  domain = "DM",
  metadata,
  sdtmig,
  protocol = NULL,
  acrf = NULL,
  treatment_arms = NULL,
  ai_config,
  mapping_guidance = NULL,
  output_file = NULL
)
}
\arguments{
  \item{domain}{SDTM domain present in the supplied SDTMIG workbook.}
  \item{metadata}{Path to the approved study metadata JSON.}
  \item{sdtmig}{Path to the local SDTMIG workbook.}
  \item{protocol}{Optional path to the study protocol DOCX.}
  \item{acrf}{Optional path to the annotated CRF DOCX.}
  \item{treatment_arms}{Optional path to the Treatment Arms workbook.}
  \item{ai_config}{Configuration created by \code{\link{sdtm_ai_config}}.}
  \item{mapping_guidance}{Optional path to a reusable mapping-guidance YAML
  file. Guidance is intended only for reusable non-trivial mapping logic;
  direct mappings should normally be omitted.}
  \item{output_file}{Optional CSV path for the programmer review table.}
}
\value{
A list with class \code{clinical_sdtm_mapping_result} containing the assessment,
mapped, unresolved, not-applicable, and combined review-table results.
}
\description{
Uses approved source metadata, the requested domain's local SDTMIG metadata,
and optional supplementary study context to request and validate a draft SDTM
mapping from an OpenAI-compatible endpoint. Supported domains are discovered
from the supplied SDTMIG workbook.
}
\details{
The generated review table contains empty programmer-review columns. Valid
values for \code{programmer_decision} are Accept, Amend, Resolve, and Not
Applicable; \code{\link{sdtm_validate_mapping}} enforces and processes them.

The guidance YAML is intended only for reusable non-trivial mapping guidance.
Direct mappings should normally be omitted. For example:

\preformatted{domains:
  DM:
    RACE: >
      Derive RACE from the available race indicator variables.
      Apply the reviewed multi-race handling rule.
    USUBJID: >
      Construct USUBJID consistently from the approved study and subject
      identifiers.
}
}
\examples{
\dontrun{
config <- sdtm_ai_config(model = "gpt-5.6")
result <- sdtm_generate_mapping(
  domain = "DM",
  metadata = "input/meta_data.json",
  sdtmig = "input/SDTMIG_v3.4.xlsx",
  protocol = "input/ABC-PHARM-01_Synthetic_Phase_I_Protocol.docx",
  acrf = "input/ABC-PHARM-01_Annotated_CRF.docx",
  treatment_arms = "input/ABC-PHARM-01_Treatment_Arms.xlsx",
  mapping_guidance = "mapping_library/sdtm_mapping_guidance.yaml",
  ai_config = config,
  output_file = "output/DM_mapping_review.csv"
)
ae_result <- sdtm_generate_mapping(domain = "AE", ...)
lb_result <- sdtm_generate_mapping(domain = "LB", ...)
}
}
