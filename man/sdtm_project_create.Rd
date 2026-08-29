\name{sdtm_project_create}
\alias{sdtm_project_create}
\title{Create a Lightweight SDTM Project}
\usage{
sdtm_project_create(root, in_dir)
}
\arguments{
\item{root}{Project root directory.}

\item{in_dir}{Approved raw source-data directory. This may be an external or
network path and does not need to exist when the project is created.}
}
\value{
A \code{clinical_sdtm_project} list containing exactly \code{root},
\code{in_dir}, \code{metadata_dir}, \code{mapping_dir}, \code{code_dir}, and
\code{out_dir}.
}
\description{
Creates isolated metadata, mapping, code, and output directories beneath a
project root. The approved raw-data directory may remain outside the project.
}
\examples{
\dontrun{
project <- sdtm_project_create(
  root = "C:/ClinicalAI/ABC-PHARM-01",
  in_dir = "\\\\clinicalai_synthetic_test_datasets"
)
}
}
