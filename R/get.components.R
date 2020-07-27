#' get.components
#'
#' @description
#' Split a fragment in format chr:start-end to a list of corresponding elements
#'
#' @param id string in format chr:start-end
#'
#' @return list with entries 'chr', 'start', 'end'
#'
#' @export get.components
get.components <- function(id) {

	### INPUT TESTS ###########################################################

	if( length(id) > 1 ) {
		stop('Function not vectorized - use apply functions');
	}

	### MAIN ##################################################################

	components <- strsplit(id, split = ':|-')[[ 1 ]];

	component.list <- list(
		chr = components[1],
		start = as.numeric(components[2]),
		end = as.numeric(components[3])
		);

	return(component.list);

}