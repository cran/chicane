#' verify.interaction.data
#'
#' @description 
#'	Verify that interaction.data object is in expected format. Throws an error if object does not fit requirements.
#'
#' @param interaction.data Object to be verified.
#'
#' @return None
#'
verify.interaction.data <- function(interaction.data) {

	# data.table object
	if( !is.data.table(interaction.data) ) {
		stop('interaction.data must be a data.table object');
	}

	required.columns <- c('distance', 'count', 'bait.trans.count', 'target.trans.count');

	if( !all(required.columns %in% names(interaction.data)) ) {
		error.message <- paste(
			'interaction.data must contain the columns:\n',
			paste(required.columns, collapse = ' ')
			);
		stop(error.message);
	}


}