#' bedtools.installed
#'
#' @description
#'	Check if bedtools exists in PATH
#'
#' @return Logical indicating if bedtools was found in PATH
#'
#' @author Erle Holgersen <Erle.Holgersen@icr.ac.uk>
#'
#' @examples
#'	bedtools.installed();
#'
#' @export bedtools.installed
bedtools.installed <- function() {


	return( '' != Sys.which('bedtools') );
}