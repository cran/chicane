#' add.covariates
#'
#' @description
#'	Add model covariates (trans counts and distance) to an interactions data table. 
#'
#' @param interaction.data 
#' 	data.table with interaction data. Must contain columns 
#' 	\code{bait.id}, \code{target.id}, \code{bait.chr}, \code{bait.start}, 
#' 	\code{bait.end}, \code{target.chr}, \code{target.start}, \code{target.end} and \code{count}.
#'
#' @return Updated data table with new columns
#'	\item{bait.trans.count}{number of trans interactions of bait fragment}
#' 	\item{target.trans.count}{number of trans interactions of target fragment}
#' 	\item{distance}{distance between bait and target fragment, or NA if trans}
#'
#' @author Erle Holgersen <Erle.Holgersen@icr.ac.uk>
#'
#' @examples
#' 	data(bre80);
#'	input.cols <- c('bait.id', 'target.id', 'bait.chr', 'bait.start', 
#'		'bait.end', 'target.chr', 'target.start', 'target.end', 'count');
#'	output <- add.covariates(bre80[, input.cols, with = FALSE]);
#'
#' @export add.covariates
add.covariates <- function( interaction.data ) {


	### INPUT TESTS ###########################################################

	if( !is.data.table(interaction.data) ) {
		stop('interaction.data must be a data.table');
	}

	required.columns <- c(
		'bait.chr', 'bait.start', 'bait.end', 
		'target.chr', 'target.start', 'target.end', 
		'count'
		);

	if( !all( required.columns %in% names(interaction.data) ) ) {
		error.message <- paste(
			'interaction.data must contain columns:\n', 
			paste(required.columns, collapse = ' ')
			);
		stop(error.message);
	}

	### MAIN ##################################################################

	# GET TRANS COUNT
	trans.counts <- get.trans.counts(interaction.data);

	# bait
	interaction.data <- merge(
		interaction.data,
		trans.counts, 
		by.x = 'bait.id',
		by.y = 'fragment.id',
		all.x = TRUE
		);
	setnames(interaction.data, 'trans.count', 'bait.trans.count');

	# target count
	interaction.data <- merge(
		interaction.data,
		trans.counts,
		by.x = 'target.id',
		by.y = 'fragment.id',
		all.x = TRUE
		);
	setnames(interaction.data, 'trans.count', 'target.trans.count');

	# CALCULATE DISTANCE
	interaction.data[, distance := get.distance(interaction.data)];

	return(interaction.data);
}