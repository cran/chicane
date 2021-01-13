#' get.combination.count
#' 
#' @description
#'	Calculate the number of possible combinations between baits and fragments, 
#' 	excluding self-ligations and only counting bait-to-bait interactions once (e.g. a-b, not b-a)
#'
#' @param baits vector of bait IDs in form chrN:start-end
#' @param fragments vector of fragment IDs in form chrN:start-end
#' @param cis.only logical indicating whether cis-interactions only should be considered
#'
#' @return total number of possible combinations
#' 
#' @export get.combination.count
get.combination.count <- function(baits, fragments, cis.only = FALSE) {

	### INPUT TESTS ###########################################################

	if( 1 == length(baits) && file.exists(baits) ) {
		stop('baits should be a vector of bait IDs');
	}

	if( 1 == length(fragments) && file.exists(fragments) ) {
		stop('fragments should be a vector of fragment IDs');
	}

	if( !all(baits %in% fragments) ) {
	    print(baits);
	    print(fragments);
		stop('All baits must be in fragments');
	}

	### MAIN ##################################################################

	if( cis.only ) {

		baits.chr <- gsub('(.*):(.*)', '\\1', baits);
		fragments.chr <- gsub('(.*):(.*)', '\\1', fragments);

		unique.chromosomes <- unique( c(baits.chr, fragments.chr) );

		# keep track of how many combinations per chromosome
		combinations.per.chromosome <- c();

		for( chr in unique.chromosomes ) {

			# call this function in trans mode to calculate possible combinations 
			# within this chromosome
			combinations.per.chromosome[ chr ] <- get.combination.count(
				baits = baits[ baits.chr == chr ],
				fragments = fragments[ fragments.chr == chr ],
				cis.only = FALSE
				);
		}

		possible.combinations <- sum( combinations.per.chromosome );

	} else {
		# convert to numeric in the process to avoid integer overflow
		n.baits <- as.numeric( length(baits) );
		n.fragments <- as.numeric( length(fragments) );

		# total number of combinations,
		# minus "reverse linked" bait-to-bait interactions and bait self-ligations
		possible.combinations <- n.baits*n.fragments - choose(n.baits, 2) - n.baits;

	}

	return(possible.combinations);
}