#' filter.fragments
#'
#' @description 
#'	Filter low and high-interacting restriction fragments based on the total number of trans counts
#'
#' @param interaction.data Data table containing interactions
#' @inheritParams chicane
#'
#' @return Data table containing fragments that passed all filters
#'
#' @author Erle Holgersen <Erle.Holgersen@icr.ac.uk>
#'
#' @examples 
#'	# filter out lowest 10% of baits
#' 	filter.fragments(bre80, bait.filters = c(0.1, 1))
#'
#' @export filter.fragments
filter.fragments <- function(
	interaction.data, 
	bait.filters = c(0, 1),
	target.filters = c(0, 1),
	verbose = FALSE
	) {

	# TO DO:
	# 	- benchmark speed (unique vs get trans counts)

	### INPUT TESTS ###########################################################

	verify.interaction.data( interaction.data );


	### MAIN ##################################################################	

	apply.target.filters <- !identical( c(0, 1), target.filters);
	apply.bait.filters <- !identical( c(0, 1), target.filters);

	if( verbose && (apply.bait.filters || apply.target.filters) ) {
		cat('Applying filters\n');
		cat('\tbaits:', paste0(round(100*bait.filters[1], 2), '% - ', round(100*bait.filters[2], 2), '%'), '\n' );
		cat('\ttargets:', paste0(round(100*bait.filters[1], 2), '% - ', round(100*bait.filters[2], 2), '%'), '\n' );
	}

	filtered.data <- interaction.data;

	# filter targets
	if( apply.target.filters ) {

		# get counts per fragment
		target.counts <- unique( interaction.data[, list(target.id, target.trans.count)] );
	
		# get absolute thresholds corresponding to percentiles
		target.lower.cutoff <- stats::quantile(
			target.counts$target.trans.count,
			prob = target.filters[1]
			);

		target.upper.cutoff <- stats::quantile(
			target.counts$target.trans.count,
			prob = target.filters[2]
			);

		filtered.data <- filtered.data[ target.trans.count >= target.lower.cutoff & target.trans.count <= target.upper.cutoff ];

	}

	# filter baits
	if( !apply.bait.filters ) {

		bait.counts <- unique( interaction.data[, list(bait.id, bait.trans.count)] );

		# get absolute thresholds corresponding to percentiles
		bait.lower.cutoff <- stats::quantile(
			bait.counts$bait.trans.count,
			prob = bait.filters[1]
			);

		bait.upper.cutoff <- stats::quantile(
			bait.counts$bait.trans.count,
			prob = bait.filters[2]
			);

		filtered.data <- filtered.data[ bait.trans.count >= bait.lower.cutoff & bait.trans.count <= bait.upper.cutoff ];
	}

	if( verbose && (apply.bait.filters || apply.target.filters) ) {
		cat('\ttargets:', nrow(filtered.data), 'interactions remain after filtering');
	}

	return(filtered.data);
}