#' test.enrichment
#'
#' @param interaction.data
#'  Data table containing details on interactions	
#' @param feature.bed
#' 	BED file with regions of features
#' @param significance.cutoff
#'	q-value threshold for significant interactions
#' @param span
#' 	Distance around target restriction fragment to consider. If set to zero (default),
#' 	only features that overlap with the restriction fragment itself are considered.	
#' @param n  
#' 	Number of random samples to consider
#' @param remove.bait.to.bait
#' 	Logical specifying whether to exclude bait-to-bait interactions
#'
#' @return list with elements
#' \item{observed}{observed overlap between significant interactions and features} 
#' \item{random}{vector of length \code{n} giving overlap between random samples and features}
#'
#' @author Erle Holgersen <Erle.Holgersen@icr.ac.uk>
#'
#' @examples
#' \donttest{
#' data(bre80);
#' ctcf.bed <- system.file('extdata', 'T47D_chr2_CTCF.bed.gz', package = 'chicane');
#'
#' results <- chicane(interactions = bre80);
#' test.enrichment(results, ctcf.bed, significance.cutoff = 0.25);
#' }
#'
#' @export test.enrichment
test.enrichment <- function(
	interaction.data, 
	feature.bed, 
	significance.cutoff = 0.05,
	span = 0,
	n = 1000,
	remove.bait.to.bait = TRUE
	) {

	# TO DO: 
	#  	- rename span argument ?
	#	- adopt CHiCAGO style list of BEDs ?

	### INPUT TESTS ###########################################################

	if( !bedtools.installed() ) stop('bedtools not found'); 
    
    # input should be a data table
    if( !is.data.table(interaction.data) ) {
        stop('interaction.data must be a data table');
    }
    
	# if no significant interactions, can't run test
	if( 0 == nrow(interaction.data[ q.value <= significance.cutoff ]) ) {
		stop('No significant interactions - cannot do enrichment analysis');
	}

	if( 0 == nrow(interaction.data[ q.value > significance.cutoff ]) ) {
		stop('All interactions are significant - cannot do enrichment analysis');
	}

	# need to check for existence of feature.bed file
	if( !is.character(feature.bed) || length(feature.bed) > 1 ) {
		stop('feature.bed should be a path to a BED file');
	}

	if( !file.exists(feature.bed) ) {
		stop( paste('File', feature.bed, 'does not exist') );
	}
    
	### MAIN ##################################################################

	# Get number of features in BED file
    # would like to avoid reading the file, but not sure there's a cross-platform 
    # way of doing it that supports potentially gzipped files
    features <- read.bed( feature.bed);
    n.features <- length(features);
    
    # remove features to free up memory
    rm( features );
    
	# no need to run bedtools and merging step on all data
	# we can first perform stratified sampling, then 
	
	# remove NA values 
	# these are rows where the model could not be fit, so we have no idea if they 
	# would have been significant or not
	interaction.data <- interaction.data[ !is.na(q.value) ];

	significant.results <- interaction.data[ q.value <= significance.cutoff ];
	nonsignificant.results <- interaction.data[ q.value > significance.cutoff ]; 

	if( remove.bait.to.bait ) {
		significant.results <- significant.results[ !significant.results$bait.to.bait ];
		nonsignificant.results <- nonsignificant.results[ !nonsignificant.results$bait.to.bait ];
	}

	stratified.samples <- lapply(
		1:n,
		function(i, nonsignificant.results, significant.results) {

			subsample <- stratified.enrichment.sample(
				nonsignificant.results, 
				significant.results
				);

			subsample[, group := i ];

			return(subsample);
			},
		nonsignificant.results = nonsignificant.results,
		significant.results = significant.results
		);


	stratified.sample <- do.call(rbind, stratified.samples);

	significant.results[, group := NA ];

	# overwrite interaction data with this reduced dataset - no need for the original any more
	interaction.data <- rbind(
		significant.results,
		stratified.sample
		);

	# get details on targets 
	targets <- interaction.data[, list(
		target.chr, 
		target.start = pmax(target.start - span, 0), 
		target.end = target.end + span, # no need to fix end, as it will just not overlap with any features
		target.id
		)];

	# only include each target once!
	targets <- unique(targets);

	targets.tempfile <- tempfile(fileext = '.txt');
	utils::write.table(
		targets,
		targets.tempfile,
		sep = '\t',
		quote = FALSE,
		row.names = FALSE,
		col.names = FALSE
		);

	bedtools.command <- paste(
		'bedtools intersect -a', targets.tempfile, 
		'-b', feature.bed, 
		'-wa'
		);

	# run bedtools command and read results to data.table
	feature.overlap <- fread( 
		bedtools.command, 
		col.names = c('chr', 'start', 'end', 'target.id')
		);

	enrichment.count <- feature.overlap[, list(feature.count = .N), by = 'target.id'];

	# add to original data
	interaction.data <- merge(
		interaction.data,
		enrichment.count,
		by = 'target.id',
		all.x = TRUE	
		);

	interaction.data[ is.na(feature.count), feature.count := 0 ];

	# run permutation test
	significant.results <- interaction.data[ q.value <= 0.05 ];
	nonsignificant.results <- interaction.data[ q.value > 0.05 ];

	# get feature count of significant interactions
	observed.feature.count <- sum( significant.results$feature.count );

	# run permutation study
	random.feature.counts <- vapply(
		split(nonsignificant.results, nonsignificant.results$group),
		function(x) sum( x$feature.count ),
		FUN.VALUE = 0
		);
	
	# combine results
	result.data <- data.table(
		observed = observed.feature.count,
		observed.prop = observed.feature.count/n.features,
		random.mean = mean(random.feature.counts),
		random.lower95 = stats::quantile(random.feature.counts, 0.025),
		random.upper95 = stats::quantile(random.feature.counts, 0.975),
		random.mean.prop = mean(random.feature.counts/n.features),
		random.lower95.prop = stats::quantile(random.feature.counts/n.features, 0.025),
		random.upper95.prop = stats::quantile(random.feature.counts/n.features, 0.975)
		);

	return(result.data);
}



