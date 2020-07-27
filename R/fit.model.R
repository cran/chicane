#' fit.model
#'
#' @description
#'  Fit negative binomial model to obtain p-values for interactions.
#'
#' @inheritParams chicane
#' @inheritParams fit.glm
#' @param interaction.data 
#'	data.table object containing interaction counts. Must contain columns distance, count, and bait_trans_count.
#' @param adjustment.terms 
#' 	Character vector of extra terms to adjust for in the model fit.
#' @param verbose
#'	Logical indicating whether to print progress reports. 	
#' @param cores
#'	Integer value specifying how many cores to use to fit model for cis-interactions.
#' @param interim.data.dir
#'  Path to directory to store intermediate QC data and plots.
#'
#' @return Interactions data with expected number of interactions and p-values added.
#'
#' @details
#' 	Fit a negative binomial model for obtaining p-value for interactions. The data is first sorted by distance, and models
#' 	are fit separately in each quantile of the distance-sorted data.
#'
#' @examples
#' \donttest{
#' 	data(bre80);
#'	fit.model(bre80);
#' }
#'
#' @export fit.model
fit.model <- function(
	interaction.data, 
	distance.bins = NULL,
	distribution = 'negative-binomial', 
	bait.filters = c(0, 1),
	target.filters = c(0, 1),
	adjustment.terms = NULL,
	maxit = 100,
	epsilon = 1e-8,
	cores = 1,
	trace = FALSE,
	verbose = FALSE,
	interim.data.dir = NULL
	) {
	
	### INPUT TESTS ###########################################################

	verify.interaction.data(interaction.data);

	### MAIN ##################################################################

	# filter out low and high-interacting fragments
	interaction.data <- filter.fragments(
		interaction.data,
		bait.filters = bait.filters,
		target.filters = target.filters,
		verbose = verbose
		);

	# split into bait to bait and other
	b2b.data <- interaction.data[ interaction.data$bait.to.bait ];
	non.b2b.data <- interaction.data[ !interaction.data$bait.to.bait ];

	# free up memory
	rm(interaction.data);
	gc();

	# fit models separately
	b2b.results <- NULL;
	non.b2b.results <- NULL;

	if( nrow(b2b.data) > 0 ) {
		b2b.results <- run.model.fitting(
			b2b.data, 
			distance.bins = distance.bins, 
			distribution = distribution, 
			verbose = verbose,
			bait.to.bait = TRUE,
			adjustment.terms = adjustment.terms,
			cores = cores,
			maxit = maxit,
			epsilon = epsilon,
			trace = trace,
			interim.data.dir = interim.data.dir
			);
	}

	if( nrow(non.b2b.data) > 0 ) {
		non.b2b.results <- 	run.model.fitting(
			non.b2b.data, 
			distance.bins = distance.bins, 
			distribution = distribution,
			verbose = verbose,
			bait.to.bait = FALSE,
			adjustment.terms = adjustment.terms,
			cores = cores,
			maxit = maxit, 
			epsilon = epsilon,
			trace = trace,
			interim.data.dir = interim.data.dir
			);
	}

	# combine results
	# hold off on sorting to optimize for speed
	#  - everything will get jumbled for the multiple testing correction anyways
	combined.data <- rbind(b2b.results, non.b2b.results);

	return(combined.data);
}
