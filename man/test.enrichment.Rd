% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test.enrichment.R
\name{test.enrichment}
\alias{test.enrichment}
\title{test.enrichment}
\usage{
test.enrichment(
  interaction.data,
  feature.bed,
  significance.cutoff = 0.05,
  span = 0,
  n = 1000,
  remove.bait.to.bait = TRUE
)
}
\arguments{
\item{interaction.data}{Data table containing details on interactions}

\item{feature.bed}{BED file with regions of features}

\item{significance.cutoff}{q-value threshold for significant interactions}

\item{span}{Distance around target restriction fragment to consider. If set to zero (default),
only features that overlap with the restriction fragment itself are considered.}

\item{n}{Number of random samples to consider}

\item{remove.bait.to.bait}{Logical specifying whether to exclude bait-to-bait interactions}
}
\value{
list with elements
\item{observed}{observed overlap between significant interactions and features} 
\item{random}{vector of length \code{n} giving overlap between random samples and features}
}
\description{
test.enrichment
}
\examples{
\donttest{
if( bedtools.installed() ) {
  data(bre80);
  ctcf.bed <- system.file('extdata', 'T47D_chr2_CTCF.bed.gz', package = 'chicane');

  results <- chicane(interactions = bre80);
  test.enrichment(results, ctcf.bed, significance.cutoff = 0.25);
} 
}

}
\author{
Erle Holgersen <Erle.Holgersen@icr.ac.uk>
}
