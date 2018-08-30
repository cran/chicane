#' get.id.components
#'
#' @description
#' 	Split a segment ID in form \code{chrN:start-end} into its different components
#' 
#' @param id segment ID of form \code{chrN:start-end}
#'
#' @return 
#'	A character vector of length three, where the elements are chromosome, start, and end, respectively. 
#' 	If \code{id} is a vector, a list of the same length is returned
#' 
#' @examples
#' 	get.id.components('chrX:6-30');
#'	get.id.components(c('3:4-10', '22:1000-20000'))
#'
#' @export get.id.components
get.id.components <- function(id) {

	### INPUT TESTS ###########################################################


	### MAIN ##################################################################

	regex <- '(.*):(\\d+)-(\\d+)';

	chr <- gsub(regex, '\\1', id);
	start <-  gsub(regex, '\\2', id);
	end <-  gsub(regex, '\\3', id);

	if( 1 == length(id) ) {
		components <- c(chr, start, end);
	} else {
		components <- mapply(
			chr,
			start,
			end,
			FUN = function(x, y, z) c(x, y, z),
			SIMPLIFY = FALSE
			);

		# remove names
		names(components) <- NULL;

	}

	return(components);

}
