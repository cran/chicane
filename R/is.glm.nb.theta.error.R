#' check.glm.nb.theta.error
#'
#' @description
#' 	Check if an error matches the error raised by \code{glm.nb} due to an inflated theta estimate.
#'	This happens when the variance of the negative binomial does not exceed the mean (i.e. there is no overdispersion).
#'  In such cases, the Poisson distribution may be a suitable alternative.
#' 
#' @param e Error object
#' 
#' @return Boolean indicating if error matches
#'
is.glm.nb.theta.error <- function(e) {
	
	error.code <- deparse(e$call)[1]; # will be a character vector if it really is theta error
	error.message <- e$message;

	# run checks for non-compatibility with glm.nb error
	# testing for equality seems to be less robust than running 
	if( grepl('missing value where TRUE/FALSE needed', error.message, fixed = TRUE) ) {

		if( grepl('while ((it <- it + 1) < limit && abs(del) > eps)', error.code, fixed = TRUE) ) return(TRUE);

		if( grepl('while ((iter <- iter + 1) <= control$maxit && (abs(Lm0 - Lm)/d1', error.code, fixed = TRUE) ) return(TRUE);

		if( grepl('if (t0 < 0)', error.code, fixed = TRUE) ) return(TRUE);
	}

	return(FALSE);
}