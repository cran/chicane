#' convert.to.one.based
#'
#' @description 
#' 	Convert zero-based region in format chr:start-end to 1-based
#'
#' @param id string in format chr:start-end
#'
#' @return one-converted ID
#'
#' @export convert.to.one.based
convert.to.one.based <- function(id) {

	# split region and sanity check
	components <- get.components(id);
	if( components$start == components$end ) {
		stop('Start and end are equal. Are you sure the input is zero-based?');
	}

	# add 1 to start to make 1-based
	components$start <- components$start + 1;

	# re-assemble ID
	one.based.id <- paste0(components$chr, ':', components$start, '-', components$end);

	return(one.based.id);
}