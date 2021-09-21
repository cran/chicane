#' combine.replicates
#'
#' @description
#' 	Merge biological replicates. 
#'
#' @param replicates 
#'	list of data table objects from \code{prepare.data}
#' @param method 
#'	string specifying the method for merging replicates. Options are 'sum' and 'weighted-sum'.
#'
#' @details 
#'  The parameter \code{method} determines which method is used for merging 
#' 	replicates. Available options are weighted-sum and sum.
#' 
#' 	'weighted-sum' implements the size factor scaling approach used in DEseq, 
#' 	rounded to the closest integer. See Anders and Huber 2010 for details.
#' 	
#' 	'sum' is the naive sum of counts across biological replicates. 
#'
#' @references
#' 	Anders, Simon, and Wolfgang Huber. "Differential expression analysis for sequence count data." \emph{Genome biology} 11.10 (2010): R106.
#' 
#' @return Data table object containing merged data, where counts are stored in colums
#'	\item{count.i}{count of interaction in ith replicate}
#'	\item{count}{count after merging replicates}
#'
#' @examples
#' \donttest{
#' if( bedtools.installed() ) {
#' 	  # preprocess data
#' 	  bam <- system.file('extdata', 'Bre80_2q35.bam', package = 'chicane');
#' 	  baits <- system.file('extdata', '2q35.bed', package = 'chicane');
#' 	  fragments <- system.file('extdata', 'GRCh38_HindIII_chr2.bed.gz', package = 'chicane');
#' 	  input.data <- prepare.data(
#'		  bam = bam, 
#'		  baits = baits, 
#'		  fragments = fragments
#'		  );
#'
#' 	  # combined two datasets into one
#'	  merged <- combine.replicates(list(input.data, input.data));
#'  }
#'	}
#'
#' @export combine.replicates
combine.replicates <- function(replicates, method = c('sum', 'weighted-sum') ) {

	# TO DO: 
	#	- figure out weighted sum count inflation problem
	
	### INPUT TESTS ###########################################################

	if( !is.list(replicates) ) {
		stop('replicates must be a list');
	}

	if( !all( vapply(replicates, is.data.table, FUN.VALUE = FALSE) ) ) {
		stop('all elements of replicates must be data.table objects');
	}

	### MAIN ##################################################################

	method <- match.arg(method);

	merged.data <- NULL;

	for(i in seq_along(replicates) ) {

		raw.data <- replicates[[ i ]];

		# restrict to columnns of interest
		# reconsider this if we figure out we don't need all of the columns
		raw.data <- raw.data[, 
			list(
				target.id, bait.id, target.chr, target.start, target.end, 
				bait.chr, bait.start, bait.end, bait.to.bait, count
				) 
			];

		# rename raw data to reflect count is one of many
		setnames(raw.data, 'count', paste0('count.', i));

		if( is.null(merged.data) ) {
			merged.data <- raw.data;
		} else {
			merged.data <- merge(
				merged.data, 
				raw.data,
				all = TRUE, 
				# need to explicitly pass in columns to merge by since data.table merges by keys by default
				by = intersect( names(merged.data), names(raw.data) )
				);
		}	
	}

	count.columns <- paste0('count.', seq_along(replicates) );

	# update NAs in counts to zeroes
	# this step might change if we incorporate zeroes to start with
	for( count in count.columns ) {
		merged.data[ is.na( get(count) ), (count) := 0 ];	
	}

	# merge replicates themselves
	if( 'weighted-sum' == method ) {
		# calculate size factors according to DEseq method
		# see Anders and Huber 2010 (and DEseq documentation) for details
		geometric.mean.rows <- apply(
			merged.data[, count.columns, with = FALSE], 
			1, 
			function(x) prod(x)^(1/length(x))
			);

		# divide each column by the geometric mean of the rows
		division.ratios <- merged.data[, count.columns, with = FALSE]/geometric.mean.rows;

		# leave out cases where geometric mean is zero
		division.ratios <- division.ratios[ 0 != geometric.mean.rows ];
		size.factors <- apply(division.ratios, 2, stats::median);


		# get final count as the weighted sum across replicates
		# same method as CHiCAGO
		counts <- apply(
			merged.data[, count.columns, with = FALSE],
			1,
			function(x, weights) round( sum(x*weights) ), 
			weights = size.factors
			);

	} else if( 'sum' == method ) {

		counts <- apply(
			merged.data[, count.columns, with = FALSE],
			1,
			sum
			);

	}

	merged.data[, count := counts];

	return(merged.data);
}