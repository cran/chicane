#' check.model.identifiability
#'
#' @description
#' 	Check if chicane model can be fit on a given dataset. 
#'	\code{glm.nb} does not work when all responses are constant, or there are only two unique values and a covariate is a perfect predictor.
#'
#' @param interaction.data Data table of interaction data on which model is to be fit
#'
#' @return boolean indicating if model can be fit
#'
#' @export check.model.numerical.fit
check.model.numerical.fit <- function(interaction.data) {

	### INPUT TESTS ###########################################################

	if( !is.data.table(interaction.data) ) {
		stop('interaction.data must be a data.table object');
	}

	### MAIN ##################################################################

	is.trans <- all( is.na(interaction.data$distance ));
	is.b2b <- all( interaction.data$bait.to.bait );

	# figure out terms to include in model
	model.terms <- c('bait.trans.count');
	
	if( !is.trans ) {
		model.terms <- c( model.terms, 'distance' );
	}

	if( is.b2b ) {
		model.terms <- c( model.terms, 'target.trans.count' );
	}

	unique.counts <- length(unique( interaction.data$count) );

	if( 1 ==  unique.counts) {
		return(FALSE);
	}

	if( unique.counts < 3 ) {
		count.levels <- as.numeric( as.factor( interaction.data$count ) );
		
		for( model.term in model.terms ) {
			model.levels <- as.numeric( as.factor( interaction.data[[ model.term ]] ) );
			
			if( identical(count.levels, model.levels) ) {
				return(FALSE);
			}
		}
	}

	return(TRUE);
}