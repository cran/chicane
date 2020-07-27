#' get.interaction.id
#'
#' @description
#'	Generate a unique identifying ID for each interaction
#'
#' @param bait id of bait in format chr:start-end
#' @param other.end id of other end in format chr:start-end
#' @param bait.to.bait logical indicating whether both ends are baits
#' @param zero.based logical indicating if IDs are zero-based
#'
#' @return string identifying interaction
#'
#' @importFrom bedr bedr.sort.region
#' @export get.interaction.id
get.interaction.id <- function(bait, other.end, bait.to.bait, zero.based = FALSE) {

	if( length(bait) > 1 || length(other.end) > 1 ) {
		stop('Function is not serialized. Use apply functions');
	}

	if( zero.based ) {
		bait <- convert.to.one.based(bait);
		other.end <- convert.to.one.based(other.end);
	}

	elements <- c(bait, other.end);

	if( bait.to.bait ) {

		# can only sort if both regions are valid
		elements <- bedr::bedr.sort.region(elements, check.valid = FALSE, check.merge = FALSE);	
	} 

	id <- paste0(elements[1], '<->', elements[2]);

	return(id);
}
