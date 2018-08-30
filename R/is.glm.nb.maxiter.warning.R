#' is.glm.nb.maxiter.warning
#'
#' @description
#'	Check if a warning object is an iteration limit reached warning from \code{glm.nb}
#' 
#' @param w Warning object
#'
#' @return Logical indicating if warning matches iteration limit reached warning
#'
is.glm.nb.maxiter.warning <- function(w) {

	if( 'iteration limit reached' != w$message ) {
		return(FALSE);
	} 

	if( 'theta.ml(Y, mu, sum(w), w, limit = control$maxit, trace = control$trace > ' != deparse(w$call)[1] ) {
		return(FALSE);
	}
	
	return(TRUE);
}