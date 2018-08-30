#' check.split.data.numerical.fit
#'
#' @description
#'	Helper function to check if the chicane model can be fit on each element of a split data list.
#'
#' @param split.data List of data.table objects with fragment interaction data
#'
#' @return Logical indicating if the model can be fit
#'
check.split.data.numerical.fit <- function(split.data) {

	### INPUT TESTS ###########################################################

	if( !is.list(split.data) ) {
		stop('split.data must be a list');
	}

	element.is.data.table <- vapply(split.data, is.data.table, FUN.VALUE = FALSE);
	if( !all(element.is.data.table) ) {
		stop('Each element of split.data should be a data.table object');
	}

	### MAIN ##################################################################

	element.numerical.fit <- vapply(
		split.data,
		check.model.numerical.fit,
		FUN.VALUE = FALSE
		);

	return( all(element.numerical.fit) );
}