\name{sdtm_generate_metadata}
\alias{sdtm_generate_metadata}
\title{Generate Deterministic Source Metadata}
\usage{
sdtm_generate_metadata(project = NULL, input_dir = NULL, output_file = NULL)
}
\arguments{
\item{project}{Optional project created by
\code{\link[=sdtm_project_create]{sdtm_project_create()}}.}

\item{input_dir}{Source-data directory when \code{project} is not supplied.}

\item{output_file}{Optional JSON output path. With a project, defaults to
\code{file.path(project$metadata_dir, "meta_data.json")}.}
}
\value{
The generated metadata as a list, invisibly when written to a file.
}
\description{
Profiles CSV and SAS7BDAT files without using AI. Distinct values are retained
only for columns with at most 20 non-missing distinct values, excluding
identifier-like names, higher-cardinality columns, and practical free text.
}
\examples{
\dontrun{
project <- sdtm_project_create("study", "external/raw")
metadata <- sdtm_generate_metadata(project = project)
}
}
