#' model.rows.sanity.check
#' 
#' @description
#' 	Check that the model fit contains the same number of rows as the data used to fit it, 
#'	and throw an error if not
#'
#' @param model.data Data used to fit model
#' @param model Resulting negative binomial model object
#'
#' @return None
#' 
model.rows.sanity.check <- function(model.data, model) {

	if( nrow(model.data) != length(model$fit) ) {

		error.message <- paste(
			'Internal bug - row number mismatch.\n', 
			'Data used to fit model contains', nrow(model.data), 'rows\n',
			'Model fit has length', length(model$fit)
			);

		stop(error.message);
	}

}