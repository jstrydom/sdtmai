\name{sdtm_generate_code}
\alias{sdtm_generate_code}
\title{Generate Standalone R Code for an Approved DM Mapping}
\usage{
sdtm_generate_code(
  mapping,
  metadata,
  ai_config,
  project = NULL,
  input_dir = NULL,
  project_key = "project",
  subject_key = "SubjectId",
  site_key = "SiteNumber",
  visit_key = "Folder",
  record_key = "",
  code_instructions = NULL,
  output_file = NULL
)
}
\arguments{
\item{mapping}{Path to the approved DM mapping CSV produced by
\code{\link[=sdtm_validate_mapping]{sdtm_validate_mapping()}}.}

\item{metadata}{Path to the approved study metadata JSON.}

\item{ai_config}{Configuration created by \code{\link[=sdtm_ai_config]{sdtm_ai_config()}}.}

\item{project}{Optional project created by
\code{\link[=sdtm_project_create]{sdtm_project_create()}}. Its \code{in_dir} is
resolved into the standalone generated script.}

\item{input_dir}{Optional approved source-data directory when \code{project}
is not supplied. If both are omitted, the existing placeholder is used.}

\item{project_key}{Approved source project/study identifier.}

\item{subject_key}{Approved source subject identifier and subject-level join
key.}

\item{site_key}{Approved source site identifier.}

\item{visit_key}{Approved source visit/folder identifier.}

\item{record_key}{Optional approved source record identifier. An empty string
means that no record-level key is supplied.}

\item{code_instructions}{Optional path to a programmer-supplied \code{.txt} or
\code{.md} file containing style and implementation preferences.}

\item{output_file}{Optional path for the generated R file. Use \code{NULL} to
return the code without writing it.}
}
\value{
A list with class \code{clinical_sdtm_generated_code} containing the domain,
generated code, and output path.
}
\description{
Constructs the approved input directory and source reads deterministically,
then asks an OpenAI-compatible model only for the DM transformation block. The
blocks are combined into standalone R code, validated structurally, and never
executed.
}
\examples{
\dontrun{
config <- sdtm_ai_config(model = "gpt-5.6")
code <- sdtm_generate_code(
  mapping = "output/DM_mapping_approved.csv",
  metadata = "input/meta_data.json",
  project = project,
  project_key = "project",
  subject_key = "SubjectId",
  site_key = "SiteNumber",
  visit_key = "Folder",
  record_key = "",
  code_instructions = "code_instructions.md",
  ai_config = config,
  output_file = "output/dm.R"
)
code$code
}
}
