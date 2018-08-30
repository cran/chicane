#' is.glm.nb.theta.warning
#'
#' @description
#' 	Check if a warning matches the square root warning raised by \code{glm.nb} due to an inflated theta estimate.
#'	This happens when the variance of the negative binomial does not exceed the mean (i.e. there is no overdispersion).
#'  In such cases, the Poisson distribution may be a suitable alternative.
#'
#' @param w Warning object
#'
#' @return Boolean indicating if warning matches
#' 
is.glm.nb.theta.warning <- function(w) {

	warning.code <- deparse(w$call)[1]

	if( 'sqrt(1/i)' == warning.code && 'NaNs produced' == w$message ) {
		return(TRUE);
    } else if( 'theta.ml(Y, mu, sum(w), w, limit = control$maxit, trace = control$trace > ' == warning.code && 'estimate truncated at zero' == w$message ) {
    	return( TRUE );
    } else {
    	return(FALSE);
    }
}