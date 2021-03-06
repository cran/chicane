% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit.model.R
\name{fit.model}
\alias{fit.model}
\title{fit.model}
\usage{
fit.model(
  interaction.data,
  distance.bins = NULL,
  distribution = "negative-binomial",
  bait.filters = c(0, 1),
  target.filters = c(0, 1),
  adjustment.terms = NULL,
  maxit = 100,
  epsilon = 1e-08,
  cores = 1,
  trace = FALSE,
  verbose = FALSE,
  interim.data.dir = NULL
)
}
\arguments{
\item{interaction.data}{data.table object containing interaction counts. Must contain columns distance, count, and bait_trans_count.}

\item{distance.bins}{Number of bins to split distance into. Models are fit separately in each bin.}

\item{distribution}{Name of distribution of the counts. Options are 'negative-binomial', 
'poisson', 'truncated-poisson', and 'truncated-negative-binomial'}

\item{bait.filters}{Vector of length two, where the first element corresponds to the lower-end filter and the second to the upper-end filter.
When global multiple testing correction is performed, altering the bait filtering settings may affect the number of significant results.}

\item{target.filters}{Vector of length two, giving lower and higher filter, respectively. 
Changing this filtering setting may affect multiple testing correction by altering the number of tests performed.}

\item{adjustment.terms}{Character vector of extra terms to adjust for in the model fit.}

\item{maxit}{Maximum number of IWLS iterations for fitting the model (passed to \code{glm.control})}

\item{epsilon}{Positive convergence tolerance for Poisson and negative binomial models. Passed to \code{glm.control}}

\item{cores}{Integer value specifying how many cores to use to fit model for cis-interactions.}

\item{trace}{Logical indicating if output should be produced for each of model fitting procedure. Passed to \code{glm.control} or \code{gamlss.control}}

\item{verbose}{Logical indicating whether to print progress reports.}

\item{interim.data.dir}{Path to directory to store intermediate QC data and plots.}
}
\value{
Interactions data with expected number of interactions and p-values added.
}
\description{
Fit negative binomial model to obtain p-values for interactions.
}
\details{
Fit a negative binomial model for obtaining p-value for interactions. The data is first sorted by distance, and models
	are fit separately in each quantile of the distance-sorted data.
}
\examples{
\donttest{
	data(bre80);
fit.model(bre80);
}

}
