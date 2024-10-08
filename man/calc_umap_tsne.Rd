% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_umap_tsne.R
\name{calc_umap_tsne}
\alias{calc_umap_tsne}
\title{Calculate UMAP and TSNE}
\usage{
calc_umap_tsne(spe, email_address, BPPARAM = NULL)
}
\arguments{
\item{spe}{spe object}

\item{email_address}{email address to send updates}

\item{BPPARAM}{parallelization param}
}
\value{
spe object
}
\description{
Calculate UMAP and TSNE
}
\examples{

calc_umap_tsne(spe, 't0jone12@louisville.edu')

}
