#' distance.bin
#'
#' @description
#' 	Assign distances to a meaningful category
#'
#' @param distance 
#' Vector of distances that should be mapped to a distance bin
#'
#' @return vector of same length as \code{distance} containing assigned distance bins
#'
distance.bin <- function(distance) {

	### INPUT TESTS ###########################################################


	if( !is.numeric(distance) && !all(is.na(distance) ) ) {
		stop('distance must be a numeric vector');
	}


	### MAIN ##################################################################

	distance.bin <- rep(NA, length(distance));

	distance.bin[ is.na(distance) ] <- 'trans';
	distance.bin[ !is.na(distance) & distance < 1e5 ] <- '0-100Kb';
	distance.bin[ !is.na(distance) & distance >= 1e5 & distance < 1e6 ] <- '100Kb-1Mb';
	distance.bin[ !is.na(distance) & distance >= 1e6 & distance < 1e7 ] <- '1Mb-10Mb';
	distance.bin[ !is.na(distance) & distance >= 1e7 ] <- '10Mb+';

	if( any( is.na(distance.bin) ) ) {
		stop('Internal bug: NAs in distance.bin object');
	}

	# turn into factor
	distance.bin <- factor(
		distance.bin,
		levels = c('0-100Kb', '100Kb-1Mb', '1Mb-10Mb', '10Mb+')
		);

	return(distance.bin);
}