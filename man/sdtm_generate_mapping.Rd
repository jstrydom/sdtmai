\name{sdtm_generate_mapping}
\alias{sdtm_generate_mapping}
\title{Generate a Draft DM Mapping for Programmer Review}
\usage{
sdtm_generate_mapping(
  domain = "DM",
  metadata,
  sdtmig,
  protocol,
  acrf,
  treatment_arms,
  ai_config,
  mapping_guidance = NULL,
  output_file = NULL
)
}
\arguments{
  \item{domain}{SDTM domain. Version 0.1 supports only \code{"DM"}.}
  \item{metadata}{Path to the approved study metadata JSON.}
  \item{sdtmig}{Path to the local SDTMIG workbook.}
  \item{protocol}{Path to the study protocol DOCX.}
  \item{acrf}{Path to the annotated CRF DOCX.}
  \item{treatment_arms}{Path to the Treatment Arms workbook.}
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
Uses approved source metadata, local DM SDTMIG metadata, protocol and annotated
CRF evidence, and programmer-reviewable Treatment Arms context to request and
validate a draft DM mapping from an OpenAI-compatible endpoint.
}
\details{
The generated review table contains empty programmer-review columns. Expected
future values for \code{programmer_decision} are Accept, Amend, Resolve, and Not
Applicable; this version does not enforce or process those decisions.

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
}
}
