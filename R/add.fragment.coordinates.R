#' add.fragment.coordinates
#'
#' @description
#'	Expand target and bait IDs of the form chrN:start-end to separate coordinate columns in the data table
#'
#' @param id.data data table containing columns \code{target.id} and/or \code{bait.id} to be expanded
#'
#' @return Data table with added coordinate columns for target and bait (as applicable).
#'
#' @author Erle Holgersen <Erle.Holgersen@icr.ac.uk>
#'
#' @examples
#'	data(bre80);
#'	add.fragment.coordinates(bre80[, .(bait.id, target.id)]);
#'
#' @export add.fragment.coordinates
add.fragment.coordinates <- function(id.data) {

	### INPUT TESTS ###########################################################

	# don't actually need input to be a data table, data frame would do
	# data tables are also data frames, so this check will work
	if( !is.data.frame(id.data) ) {
		stop('id.data must be a data frame');
	}

	### MAIN ##################################################################

	expanded.data <- id.data;

	# process target if provided
	if( 'target.id' %in% names(id.data) ) {

		target.coordinates <- get.id.components(id.data$target.id);

		target.coordinates <- as.data.table( do.call(rbind, target.coordinates) );
		names(target.coordinates) <- paste0('target.', c('chr', 'start', 'end'));

		expanded.data <- cbind(expanded.data, target.coordinates);
	}

	# process bait if provided
	if( 'bait.id' %in% names(id.data) ) {

		bait.coordinates <- get.id.components(id.data$bait.id);

		bait.coordinates <- as.data.table( do.call(rbind, bait.coordinates) );
		names(bait.coordinates) <- paste0('bait.', c('chr', 'start', 'end'));


		expanded.data <- cbind(expanded.data, bait.coordinates);
	}

	# convert coordinates to numeric
	for(column in names(expanded.data)[ grepl('\\.(start|end)$', names(expanded.data) ) ] ) {
		set(expanded.data, j = column, value = as.numeric( expanded.data[[ column ]] ) );
	}

	return(expanded.data);
}
