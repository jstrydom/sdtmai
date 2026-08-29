\name{sdtm_validate_mapping}
\alias{sdtm_validate_mapping}
\title{Validate a Programmer-Reviewed DM Mapping}
\usage{
sdtm_validate_mapping(mapping, metadata, sdtmig, output_file = NULL)
}
\arguments{
\item{mapping}{Path to the programmer-edited mapping review CSV.}

\item{metadata}{Path to the approved study metadata JSON.}

\item{sdtmig}{Path to the local SDTMIG workbook.}

\item{output_file}{Optional CSV path for the approved mapping table. Use
\code{NULL} to return the result without writing a file.}
}
\value{
A list with class \code{clinical_sdtm_mapping_review} containing the domain,
original review table, approved mapping, and unresolved review.
}
\description{
Reads a mapping review CSV produced by \code{\link[=sdtm_generate_mapping]{sdtm_generate_mapping()}},
validates the programmer's decisions against the approved source metadata and
local SDTMIG metadata, and returns a deterministic approved mapping table.
}
\examples{
\dontrun{
review <- sdtm_validate_mapping(
  mapping = "output/DM_mapping_review.csv",
  metadata = "input/meta_data.json",
  sdtmig = "input/SDTMIG_v3.4.xlsx",
  output_file = "output/DM_mapping_approved.csv"
)
review$approved_mapping
}
}
