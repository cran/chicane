#' stratified.enrichment.sample
#'
#' @description
#' Generate a stratified sample matching distance distribution of significant interactions.
#'
#' @param nonsignificant.results 
#' Data table containing non-significant interactions that should be sampled from
#' @param significant.results
#' Data table of significant results. Used to determine size of strata in stratified sampling procedure.
#'
stratified.enrichment.sample <- function(
	nonsignificant.results,
	significant.results 
	) {

	### INPUT TESTS ###########################################################

	if( !is.data.table(nonsignificant.results) ) {
		stop('nonsignificant.results should be a data.table');
	}

	if( !is.data.table(significant.results) ) {
		stop('significant.results should be a data.table');
	}

	if( 0 == nrow(significant.results) ) {
		stop('No significant results - cannot run sampling');
	}

	if( 0 == nrow(nonsignificant.results) ) {
		stop('No non-significant results - cannot run sampling');
	}

	### MAIN ##################################################################

	nonsig.distance.bins <- distance.bin( nonsignificant.results$distance );
	sig.distance.bins <- distance.bin( significant.results$distance );

	# get number of interactions to sample for each bin
	# distance.bin returns a factor, so these are guaranteed to be in the same order

	sig.bin.counts <- table(sig.distance.bins);
	nonsig.bin.counts <- table(nonsig.distance.bins);

	# if fewer than 1000 unique samples for any bin, skip to trans/cis split
	if( any(nonsig.bin.counts < sig.bin.counts & nonsig.bin.counts < 1000) ) {
		sig.distance.bins <- ifelse(
			is.na(significant.results$distance),
			'trans',
			'cis'
			);

		nonsig.distance.bins <- ifelse(
			is.na(nonsignificant.results$distance),
			'trans',
			'cis'
			);

		sig.bin.counts <- table(sig.distance.bins);
		nonsig.bin.counts <- table(nonsig.bin.counts);


		# TO DO: what if no non-significant trans counts?
	}

	stratified.sample <- list();

	for( bin in names(sig.bin.counts) ) {
		# number of interactions to sample
		n <- sig.bin.counts[ bin ];

		stratified.sample[[ bin ]] <- nonsignificant.results[ bin == nonsig.distance.bins ][ sample(.N, n, replace = TRUE) ];

	}

	stratified.sample <- do.call(rbind, stratified.sample);

	return(stratified.sample);
}

