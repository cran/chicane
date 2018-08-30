#' get.trans.counts
#' 
#' @description 
#'	Calculate the number of trans-interactions per fragment, accounting for the fact that baits can be listed either as bait or target.
#'
#' @param interaction.data Data table containing interactions
#'
#' @return Data table with columns \code{fragment.id} and \code{trans.count}.
#'	\item{fragment.id}{ID of restriction fragment in chrN:start-end format}
#'	\item{trans.count}{Number of trans interactions involving the fragment}
#'
#' @examples
#' 	data(bre80);
#' 	get.trans.counts(bre80[, .(bait.chr, target.chr, bait.id, target.id, count)]);
#'
#' @export get.trans.counts
get.trans.counts <- function(interaction.data) {

	### INPUT TESTS ###########################################################

	if( !is.data.table(interaction.data) ) {
		stop('interaction.data must be a data table');
	}

	required.columns <- c('bait.chr', 'target.chr', 'bait.id', 'target.id', 'count');
	
	if( !all( required.columns %in% names(interaction.data) ) ) {
		error.message <- paste(
			'interaction.data must contain columns:', 
			paste(required.columns, collapse = ' ')
			);

		stop(error.message);
	}	

	if( !is.numeric( interaction.data$count) ) {
		stop('The column "count" in interaction.data must be numeric');
	}


	### MAIN ##################################################################

	trans <- interaction.data[ bait.chr != target.chr, list(target.id, bait.id, count) ];

	# split count column to have one entry per end of interaction
	melted.trans <- melt(trans, measure.vars = c('target.id', 'bait.id'));
	melted.trans <- melted.trans[, list(fragment.id = value, count)];

	# sum by fragment
	trans.counts <- melted.trans[, list(trans.count = sum(count)), by = fragment.id];


	# If a fragment has never occurred in a trans interaction, it will not be included in the 
	# data frame above. Fix this by adding them in manually
	
	# first make a data table of all unique fragment IDs
	# there might be quicker ways of doing this? 
	all.fragments <- data.table(
		fragment.id = c(interaction.data$target.id, interaction.data$bait.id)
		);

	all.fragments <- unique(all.fragments);

	# merge in with trans counts, and add trans count of zero as required
	trans.counts <- merge(
		all.fragments, 
		trans.counts,
		by = 'fragment.id',
		all.x = TRUE
		);

	trans.counts[ is.na(trans.count), trans.count := 0 ];

	return(trans.counts);
}