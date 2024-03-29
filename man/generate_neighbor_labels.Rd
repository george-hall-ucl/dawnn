% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dawnn.R
\name{generate_neighbor_labels}
\alias{generate_neighbor_labels}
\title{Generate a matrix of the labels of the 1,000 nearest neighbors of each cell.}
\usage{
generate_neighbor_labels(cells, verbose, label_names, label_1)
}
\arguments{
\item{cells}{Seurat object containing the dataset.}

\item{verbose}{Boolean verbosity.}

\item{label_names}{String containing the name of the meta.data slot in
`cells' containing the labels of each cell.}

\item{label_1}{String containing the name of one of the labels.}
}
\value{
A data frame containing the labels of the 1000 nearest neighbors of
each cell.
}
\description{
Generate a matrix of the labels of the 1,000 nearest neighbors of each cell.
}
\examples{
\dontrun{
generate_neighbor_labels(cell_object, verbose = TRUE, label_names =
"sample_names", label_1 = "Condition1")
}
}
