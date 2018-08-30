#' smart.split
#'
#' @description
#'	Split a data frame into a prespecified number of bins, using 
#'	\code{split} and \code{cut}. Unlike the default R functions, this does not
#'	fail when asked to split the data into a single bin.
#'
#' @param dat Data frame or data table to be split
#' @param bins Number of bins to split data into
#'
#' @return 
#'	List with \code{bins} elements. Each element corresponds to one portion 
#'	of the data  
#'
smart.split <- function(dat, bins) {

	### INPUT TESTS ###########################################################

	if( !is.data.frame(dat) ) {
		stop('dat must be a data frame');
	}

	if( !is.numeric(bins) || bins %% 1 != 0 || bins < 1 ) {
		stop('bins must be a positive integer');
	}

	### MAIN ##################################################################

	if( bins > 1 ) {
		split.data <- split(
			dat,
			cut( seq_len( nrow(dat) ), breaks = bins)
			);

	} else {
		# only a single bin - cut function will fail
		# simply return a list with one element containing all of the data
		split.data <- list( dat );
	}

	return(split.data);

}