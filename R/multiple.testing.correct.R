#' multiple.testing.correct
#'
#' @description
#'	Perform multiple testing correction on p-values from interaction test.
#' 	By default, multiple testing correction is applied per bait. To change this
#' 	to a global multiple testing correction, set \code{bait.level = FALSE}.
#'
#' @param interaction.data 
#' 	Data table of interaction calls. Must contain columns p.value and bait.id.
#' @param bait.level 
#'	Logical indicating whether multiple testing correction should be performed per bait.
#' 
#' @return Original data table with new column
#' 	\item{q.value}{FDR-corrected p-value}
#'
#'
#' @examples
#' \dontrun{
#' 	data(bre80);
#'  results <- fit.model(bre80);	
#'  adjusted.results <- multiple.testing.correct(results);
#' }
#'
#' @export multiple.testing.correct
multiple.testing.correct <- function(
	interaction.data,
	bait.level = TRUE
	) {

	### INPUT TESTS ###########################################################

	verify.interaction.data(interaction.data);

	if( !all(c('bait.id', 'p.value') %in% names(interaction.data)) ) {
		stop('interaction.data must contain columns bait.id and p.value');
	}	

	### MAIN ##################################################################


	if( bait.level ) {
		# split input data by locus, and perform multiple testing correction for 
		# each locus separately
		locus.data <- split(
			interaction.data, 
			interaction.data$bait.id
			);

		q.value.data <- list();

		for(i in seq_along(locus.data) ) {

			temp.data <- locus.data[[i]];

			temp.data$q.value <- stats::p.adjust(
				temp.data$p.value,
				method = 'fdr'
				);

			q.value.data[[ i ]] <- temp.data;
		}

		q.value.data <- do.call(rbind, q.value.data);
	} else {
		# perform global FDR correction
		q.value.data <- interaction.data;
		q.value.data$q.value <- stats::p.adjust(
			q.value.data$p.value,
			method = 'fdr'
			);
	}


	return(q.value.data);
}

