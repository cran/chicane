#' get.distance
#'
#' @description
#'	Calculate distance between bait and target region
#'
#' @param interaction.data 
#' 	data.table with interaction data. Must contain columns bait.chr, bait.start, bait.end, target.chr, target.start, target.end
#'
#' @return vector of absolute distances (NA for trans-interactions)
#'
#' @examples
#' 	data(bre80);
#'	input.cols <- c('bait.chr', 'bait.start', 'bait.end', 
#' 		'target.chr', 'target.start', 'target.end');
#' 	get.distance( bre80[, input.cols, with = FALSE]);
#'
#' @export get.distance
get.distance <- function(interaction.data) {

	### INPUT TESTS ###########################################################

	if( !is.data.table(interaction.data) ) {
		stop('interaction.data must be a data.table');
	}

	required.columns <- c('bait.chr', 'bait.start', 'bait.end', 'target.chr', 'target.start', 'target.end');
	if( !all( required.columns %in% names(interaction.data) ) ) {
		error.message <- paste(
			'interaction.data must contain columns:\n', 
			paste(required.columns, collapse = ' ')
			);
	}

	### MAIN ##################################################################

	bait.midpoint <- interaction.data$bait.start + (interaction.data$bait.end - interaction.data$bait.start)/2;
	target.midpoint <- interaction.data$target.start + (interaction.data$target.end - interaction.data$target.start)/2;

	distance <- ifelse(
		interaction.data$bait.chr == interaction.data$target.chr, 
		abs(bait.midpoint - target.midpoint),
		NA
		);

	return(distance); 
}