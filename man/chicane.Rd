% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chicane.R
\name{chicane}
\alias{chicane}
\title{chicane}
\usage{
chicane(
  bam = NULL,
  baits = NULL,
  fragments = NULL,
  interactions = NULL,
  replicate.merging.method = "sum",
  distribution = "negative-binomial",
  include.zeros = "none",
  bait.filters = c(0, 1),
  target.filters = c(0, 1),
  distance.bins = NULL,
  multiple.testing.correction = c("bait-level", "global"),
  adjustment.terms = NULL,
  remove.adjacent = FALSE,
  temp.directory = NULL,
  keep.files = FALSE,
  maxit = 100,
  epsilon = 1e-08,
  cores = 1,
  trace = FALSE,
  verbose = FALSE,
  interim.data.dir = NULL
)
}
\arguments{
\item{bam}{Path to a BAM file}

\item{baits}{Path to a BED file containing the baits}

\item{fragments}{Path to a BED file containing all restriction fragments in the genome}

\item{interactions}{Data table or path to a text file detailing fragment interactions, typically from \code{prepare.data}. 
Can be used instead of bam/baits/fragments specification if the text files have already been prepared.}

\item{replicate.merging.method}{Method that should be used for merging replicates, if applicable}

\item{distribution}{Name of distribution of the counts. Options are 'negative-binomial', 
'poisson', 'truncated-poisson', and 'truncated-negative-binomial'}

\item{include.zeros}{String specifying what zero counts to include. Options are none (default), cis, and all.}

\item{bait.filters}{Vector of length two, where the first element corresponds to the lower-end filter and the second to the upper-end filter.
When global multiple testing correction is performed, altering the bait filtering settings may affect the number of significant results.}

\item{target.filters}{Vector of length two, giving lower and higher filter, respectively. 
Changing this filtering setting may affect multiple testing correction by altering the number of tests performed.}

\item{distance.bins}{Number of bins to split distance into. Models are fit separately in each bin.}

\item{multiple.testing.correction}{String specifying how multiple testing correction should be performed, by bait or globally.}

\item{adjustment.terms}{Character vector of extra terms to adjust for in the model fit.}

\item{remove.adjacent}{Logical indicating whether to remove all reads mapping to adjacent restriction fragments.}

\item{temp.directory}{Directory where temporary files should be stored. Defaults to current directory.}

\item{keep.files}{Logical indicating whether to keep temporary files}

\item{maxit}{Maximum number of IWLS iterations for fitting the model (passed to \code{glm.control})}

\item{epsilon}{Positive convergence tolerance for Poisson and negative binomial models. Passed to \code{glm.control}}

\item{cores}{Integer value specifying how many cores to use to fit model for cis-interactions.}

\item{trace}{Logical indicating if output should be produced for each of model fitting procedure. Passed to \code{glm.control} or \code{gamlss.control}}

\item{verbose}{Logical indicating whether to print progress reports.}

\item{interim.data.dir}{Path to directory to store intermediate QC data and plots. NULL indicate skip intermediate results.}
}
\value{
Data table with columns
	\item{target.id}{String in chrN:start-end format identifying target fragment}
	\item{bait.id}{String in chrN:start-end format identifying bait fragment}
\item{target.chr}{Chromosome of target fragment}
	\item{target.start}{Start coordinate of target fragment (zero-based)}
\item{target.end}{End coordinate of target fragment}
\item{bait.chr}{Chromosome of bait fragment}
	\item{bait.start}{Start coordinate of bait fragment (zero-based)}
\item{bait.end}{End coordinate of bait fragment}
\item{bait.to.bait}{Boolean indicating if the interaction is bait-to-bait (i.e. the fragment listed as target is also a bait)}
	\item{bait.trans.count}{The number of reads linking the bait to fragments in trans (a measure of "interactibility")}
	\item{target.trans.count}{The number of reads linking the target to fragments in trans (a measure of "interactibility")}
	\item{distance}{Distance between the midpoints of the bait and target fragments (basepairs). NA for trans interactions}
	\item{count}{The number of reads linking the two fragments}
	\item{expected}{The expected number of reads linking the two fragments under the fitted model}
\item{p.value}{P-value for test of the observed number of reads significantly exceeding the expected count}
\item{q.value}{FDR-corrected p-value}
}
\description{
Run full method for detecting significant interactions in capture Hi-C experiments, starting 
 either from a BAM file or preprocessed data from \code{prepare.data}
}
\examples{
\donttest{
if( bedtools.installed() ) {
  # start from BAM file
  bam <- system.file('extdata', 'Bre80_2q35.bam', package = 'chicane');
  baits <- system.file('extdata', '2q35.bed', package = 'chicane');
  fragments <- system.file('extdata', 'GRCh38_HindIII_chr2.bed.gz', package = 'chicane');
  results <- chicane(
	bam = bam, 
	baits = baits, 
	fragments = fragments
	);
}

# start from pre-processed data
data(bre80); 
results <- chicane(interactions = bre80);
}

}
\author{
Erle Holgersen <Erle.Holgersen@icr.ac.uk>
}
