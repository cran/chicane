#' prepare.data
#' 
#' @description
#'	Prepare data for running interaction calling. Takes a BAM file and baits and restriction fragments as input, and returns a data table with data ready for analysis.
#'
#' @inheritParams chicane
#' @inheritParams combine.replicates
#' @param include.zeros
#' 	String specifying what zero counts to include. Options are none (default), cis, and all.
#' @param remove.adjacent
#' 	Logical indicating whether to remove all reads mapping to adjacent restriction fragments. 
#' @param temp.directory
#'	Directory where temporary files should be stored. Defaults to current directory. 
#' @param keep.files 
#' 	Logical indicating whether to keep temporary files
#'
#' @return Data table object with columns
#' 	\item{target.id}{String in chrN:start-end format identifying target fragment}
#' 	\item{bait.id}{String in chrN:start-end format identifying bait fragment}
#'	\item{target.chr}{Chromosome of target fragment}
#' 	\item{target.start}{Start coordinate of target fragment (zero-based)}
#'	\item{target.end}{End coordinate of target fragment}
#'	\item{bait.chr}{Chromosome of bait fragment}
#' 	\item{bait.start}{Start coordinate of bait fragment (zero-based)}
#'	\item{bait.end}{End coordinate of bait fragment}
#'	\item{bait.to.bait}{Boolean indicating if the interaction is bait-to-bait (i.e. the fragment listed as target is also a bait)}
#' 	\item{count}{The number of reads linking the two fragments}
#' 	\item{bait.trans.count}{The number of reads linking the bait to fragments in trans (a measure of "interactibility")}
#' 	\item{target.trans.count}{The number of reads linking the target to fragments in trans (a measure of "interactibility")}
#' 	\item{distance}{Distance between the midpoints of the bait and target fragments (basepairs). NA for trans interactions}
#' 
#' @examples
#' \donttest{
#' bam <- system.file('extdata', 'Bre80_2q35.bam', package = 'chicane');
#' baits <- system.file('extdata', '2q35.bed', package = 'chicane');
#' fragments <- system.file('extdata', 'GRCh38_HindIII_chr2.bed.gz', package = 'chicane');
#' input.data <- prepare.data(
#'		bam = bam, 
#'		baits = baits, 
#'		fragments = fragments
#'		);
#'	}
#'
#' @export prepare.data
prepare.data <- function(
	bam,
	baits,
	fragments,
	replicate.merging.method = 'sum',
	include.zeros = c('none', 'cis', 'all'),
	remove.adjacent = FALSE,
	temp.directory = NULL,
	keep.files = FALSE,
	verbose = FALSE
	) {
	
	include.zeros <- match.arg(include.zeros);

	### INPUT TESTS ###########################################################

	# check for too many combinations 
	# better to do this now to avoid having to run through all the counting steps first
	if( include.zeros %in% c('cis', 'all') ) {
		bait.ids <- read.bed(baits);
		fragment.ids <- read.bed(fragments);

		# check that there aren't too many combinations
		n.combinations <- get.combination.count(
			baits = bait.ids, 
			fragments = fragment.ids,
			cis.only = 'cis' == include.zeros
			);

		if( n.combinations > 10^9 ) {
			stop('Too many combinations, cannot include zeros');
		}
	}

	### MAIN ##################################################################

	# convert each replicate separately
	replicate.data <- lapply(
		bam,
		convert.bam,
		baits = baits,
		fragments = fragments,
		temp.directory = temp.directory,
		keep.files = keep.files
		);

	# merge replicates if required
	if( 1 == length(replicate.data) ) {
		interaction.data <- replicate.data[[ 1 ]];
	} else {
		interaction.data <- combine.replicates(
			replicate.data, 
			method = replicate.merging.method
			);
	}

	# add cis-interaction zeroes if requested
	if( 'cis' == include.zeros ) {

		# store for sanity check
		nonzero.row.count <- nrow(interaction.data);

		# get chromosome of each fragment
		fragment.chr <- gsub('(.*):(.*)', '\\1', fragment.ids);
		bait.chr <- gsub('(.*):(.*)', '\\1', bait.ids);

		filled.in.data <- list();

		# loop over each unique chromosome
		# okay to loop over , as the processing pipeline ensures all fragments are in fragment list 
		for( chr in unique( fragment.chr ) ) {

			if( verbose ) {
				cat('Filling in zero counts on', chr, '\n');
			}

			# subset out all interactions with bait on chromosome of interest
			chr.interaction.data <- interaction.data[ bait.chr == chr ];

			filled.in.data[[ chr ]] <- fill.in.zeros(
				chr.interaction.data, 
				baits = bait.ids[ chr == bait.chr ],
				fragments = fragment.ids[ chr == fragment.chr ]
				);
		}

		# coerce one data table object
		interaction.data <- do.call(rbind, filled.in.data);

		# sanity check
		if( nonzero.row.count != nrow( interaction.data[ count != 0 ]) ) {

			error.message <- paste(
				'Internal bug - mismatched non-zero row numbers when filling in zeros\n',
				'Before:', nonzero.row.count, 'non-zero rows\n',
				'After:', nrow(interaction.data[ count != 0 ]), 'non-zero rows\n'
				);

			stop(error.message);
		}

	} else if( 'all' == include.zeros ) {

		# no need to throw error for size, will be done in zero-filling function
		interaction.data <- fill.in.zeros(
			interaction.data, 
			baits = bait.ids, 
			fragments = fragment.ids
			);
	}

	# calculate trans counts and distance between fragments
	interaction.data <- add.covariates(interaction.data);

	if( remove.adjacent ) {
		# remove counts between adjacent fragments
		# if data has been processed with HiCUP, these are typically one end mapped 
		# to the opposite end of the adjacent restriction fragment,
		# as re-ligations are removed.
		interaction.data <- interaction.data[ !(bait.chr == target.chr & (bait.start == target.end | target.start == bait.end) ) ];
	}

	# want count to appear as the last column so it shows up next to expected
	new.column.order <- c( names(interaction.data)['count' != names(interaction.data)], 'count' );
	setcolorder(interaction.data, new.column.order);

	# sanity check that interaction data fits expected format
	# TO DO: figure out why this isn't working!
	# verify.interaction.data(interaction.data);

	return(interaction.data);
}