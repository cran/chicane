#' convert.bam
#'
#'
#' @description
#'	Convert a BAM file to a format that can be used for replicate merging. 
#'
#' 	\strong{Note:} This function does not process data enough to be used for interaction calling. Use \code{prepare.data} for full preprocessing.
#'
#' @inheritParams prepare.data
#'
#' @seealso \code{\link{prepare.data}}
#'
#' @author Erle Holgersen <Erle.Holgersen@icr.ac.uk>
#'
convert.bam <- function(
	bam,
	baits,
	fragments,
	temp.directory = NULL,
	keep.files = FALSE
	) {


	### INPUT TESTS ###########################################################

	# check for bedtools
	if( !bedtools.installed() ) stop('bedtools not found'); 

	if( !is.character(bam) && 1 == length(bam) ) {
		stop('bam must be the path to a BAM file.');
	}

	if( !is.character(baits) && 1 == length(baits) ) {
		stop('baits must be the path to a BED file.');
	}

	if( !is.character(fragments) || 1 != length(fragments) ) {
		stop('fragments must be the path to a BED file.');
	}

	if( !is.null(temp.directory) && !dir.exists(temp.directory) ) {
		error.message <- sprintf('Directory %s does not exist or is not a directory', temp.directory);
		stop( error.message );
	}

	if( is.null(temp.directory) ) temp.directory <- getwd();

	# make sure intermediate files don't exist
	sample.name <- gsub('\\.bam', '', basename(bam) );
	intermediate.files <- file.path(
		temp.directory,
		paste0(
			sample.name, 
			c('_b2b.bedpe', '_b2b_fragments.bedpe', '_nonb2b_baits_right.bedpe', '_nonb2b_fragments.bedpe')
			)
		);

	if( any( file.exists(intermediate.files) ) ) {
		existing.files <- intermediate.files[ file.exists(intermediate.files)];

		error.message <- paste(
			'The following intermediate files already exist:\n', 
			paste(existing.files, collapse = ' ')
			);	

		stop( error.message );
	}

	### MAIN ##################################################################

	# flag for keeping intermediary
	keep.files.flag <- ifelse(
		keep.files, 
		'-k',
		''
		);

	# need path to preprocessing script
	# 	-> make sure to change this!
	preprocessing.script <- system.file('bin', 'prepare_bam.sh', package = 'chicane');

	system.command <- paste(
		'sh', 
		preprocessing.script,
		'-b', bam,
		'-c', baits,
		'-f', fragments,
		'-d', temp.directory,
		keep.files.flag
		);

	# get all interacting fragments in a data table
	fragment.interactions <- data.table::fread(system.command);
	names(fragment.interactions) <- c('bait.chr', 'bait.start', 'bait.end', 'target.chr', 'target.start', 'target.end', 'bait.to.bait');

	# get counts of each interaction
	# bash script returns one row per read pair, want to aggregate across interactions
	interaction.data <- fragment.interactions[, list(count = .N), by = list(target.chr, target.start, target.end, bait.chr, bait.start, bait.end, bait.to.bait)];

	# add ID columns - needed for merging replicates
	interaction.data[, bait.id := paste0(bait.chr, ':', bait.start, '-', bait.end)];
	interaction.data[, target.id := paste0(target.chr, ':', target.start, '-', target.end)];

	return(interaction.data);
}