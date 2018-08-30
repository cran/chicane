#' distance.split
#'
#' @description
#'	Split interaction data into subsets that are large enough for the chicane model to be fit (see Details), 
#'	based on distance. This step allows the distance term in the model to be fit in a piecewise linear fashion. 
#'
#' @details
#'  Fitting \code{glm.nb} fails when there is a lack of overdispersion in the data. The chicane method
#' 	contains logic to catch these errors and instead fit a Poisson model. However, to avoid this happening
#' 	more than necessary, an attempt is made to avoid distance splits that will clearly result in numerical errors.
#' 	This includes bins of data where the count is the same for all rows, 
#'	or a covariate is a perfect predictor of count.  
#' 	
#' @param interaction.data 
#'	Data table of interaction data, typically from \code{prepare.data}
#' @param distance.bins 
#'	Number of distance bins desired. If NULL, a number is chosen to ensure that the negative binomial can be fit in all bins.
#' @param min.rows.bin
#' 	The minimum number of expected rows in a distance bin. Ignored if distance.bins is set
#' @param verbose 
#'	Logical indicating whether to print progress reports
#'
#' @return 
#'	List where each element corresponds to a specified distance bin, and the final one corresponding to trans-interactions (if present)
#'
#' @examples
#'	data(bre80);
#'	distance.split(bre80);
#'
#' @export distance.split
distance.split <- function(
	interaction.data, 
	distance.bins = NULL, 
	min.rows.bin = 50,
	verbose = FALSE
	) {

	### INPUT TESTS ###########################################################

	if( !is.data.table(interaction.data) ) {
		stop('interaction.data must be a data.table object');
	}

	if( !is.null(distance.bins) && !is.numeric(distance.bins) ) {
		stop('distance.bins must be a positive integer');
	} 

	if( !is.null(distance.bins) && ( 0 != distance.bins %% 1 || distance.bins < 1 ) ) {
		stop('distance.bins must be a positive integer');
	}

	### MAIN ##################################################################

	cis.data <- interaction.data[!is.na(distance)];
	trans.data <- interaction.data[is.na(distance)];

	cis.data <- cis.data[order(distance)];

	if( nrow(cis.data) < 50 || !check.model.numerical.fit(cis.data) ) {
		# if fewer than 50 rows in cis-data, or already too small for a decent numerical fit, keep as one item
		split.data <- list( cis.data );

	} else {
		
		if( is.null(distance.bins) ) {

			distance.bins <- min( round( nrow(cis.data)/min.rows.bin, 1), 100 );
			numerical.fit <- FALSE;

			if( verbose ) {
				cat('Splitting data for model fitting\n');
				cat('\tchecking distance.bins =', distance.bins, '\n');
			}
			

			# as long as model cannot be fit, try more distance.bins
			while( distance.bins >= 1 && !numerical.fit ) {

				distance.bins <- round(distance.bins/2);
				
				if( verbose ) cat('\tchecking distance.bins =', distance.bins, '\n');

				split.data <- smart.split(cis.data, bins = distance.bins);
			
				# get indicator of whether the model can be fit in each of the split data parts
				numerical.fit <- check.split.data.numerical.fit(split.data);
			}

		} else {
			# user has requested a specified number of distance bins
			# split into this number of groups, and throw an error if the model cannot be fit

			split.data <- smart.split(cis.data, bins = distance.bins);
	
			numerical.fit <- check.split.data.numerical.fit(split.data);

			if( !numerical.fit ) {
				stop('Model cannot be fit with the specified number of distance bins. Try using fewer bins.');
			}

		} # end distance bins if/else
	
	}

	# add data on trans interactions if it exists
	if( nrow(trans.data) > 0 ) {
		split.data[[ length(split.data) + 1 ]] <- trans.data;
	}
	
	return(split.data);

}
