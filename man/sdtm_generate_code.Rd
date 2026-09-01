\name{sdtm_generate_code}
\alias{sdtm_generate_code}
\title{Generate Standalone R Code for an Approved SDTM Mapping}
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
\item{mapping}{Path to a single-domain approved mapping CSV produced by
\code{\link[=sdtm_validate_mapping]{sdtm_validate_mapping()}}.}

\item{metadata}{Path to the approved study metadata JSON.}

\item{ai_config}{Configuration created by \code{\link[=sdtm_ai_config]{sdtm_ai_config()}}.}

\item{project}{Optional project created by
\code{\link[=sdtm_project_create]{sdtm_project_create()}}. Its \code{in_dir} is
resolved into the standalone generated script.}

\item{input_dir}{Optional approved source-data directory when \code{project}
is not supplied. If both are omitted, the existing placeholder is used.}

\item{project_key}{Configured canonical source project/study identifier. It
is used only where applicable and is not required in every source dataset.}

\item{subject_key}{Configured canonical source subject identifier. It is
used only where applicable and is not required in every source dataset.}

\item{site_key}{Configured canonical source site identifier. It is used only
where applicable and is not required in every source dataset.}

\item{visit_key}{Configured canonical source visit/folder identifier. It is
used only where applicable and is not required in every source dataset.}

\item{record_key}{Optional configured canonical source record identifier. An
empty string means that no record-level key is supplied.}

\item{code_instructions}{Optional path to a programmer-supplied \code{.txt} or
\code{.md} file containing style and implementation preferences.}

\item{output_file}{Optional path for the generated R file. Use \code{NULL} to
return the code without writing it.}
}
\value{
A list with class \code{clinical_sdtm_generated_code} containing the inferred
domain, lowercase final-object name, generated code, and output path.
}
\description{
Constructs the approved input directory and source reads deterministically,
then asks an OpenAI-compatible model only for the approved domain's
transformation block. The mapping must contain exactly one non-empty domain.
The domain is inferred from the mapping, and the final R object is named using
the lowercase domain code. The blocks are combined into standalone R code,
validated structurally, and never executed.
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

# The same interface applies to other single-domain approved mappings:
# DM -> dm.R -> dm
# AE -> ae.R -> ae
# LB -> lb.R -> lb
}
}
