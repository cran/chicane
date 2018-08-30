#' read.bed
#'
#' @description 
#'	Read a BED file and return regions in chrN:start-end format
#'
#' @param bed.path Path to bed file
#' @param zero.based Whether to return ID in zero-based coordinates
#'	
#' @return vector of region IDs
#'
#' @examples
#'	bait.file <- system.file('extdata', '2q35.bed', package = 'chicane');
#'	baits <- read.bed(bait.file);
#'
#' @export read.bed
read.bed <- function(bed.path, zero.based = TRUE) {

	### INPUT TESTS ###########################################################

	if( !is.character(bed.path) || length(bed.path) != 1 ) {
		stop('bed.path must be a single character string');
	}

	if( !file.exists(bed.path) ) {
		error.message <- paste('File', bed.path, 'does not exist');
		stop(error.message);
	}

	### MAIN ##################################################################

	# need to read differently if file is gzipped, as fread does not support it
	# could use zcat for this - not sure which is better
	is.gzipped <- grepl( '\\.gz$', bed.path );

	if( is.gzipped ) {
		temp <- utils::read.table( bed.path, sep = '\t' );
		bed.contents <- as.data.table(temp);

	} else {
		bed.contents <- data.table::fread(bed.path);

	}

	if( zero.based ) {
		bed.ids <- paste0(bed.contents$V1, ':', bed.contents$V2, '-', bed.contents$V3);
	} else {
		bed.ids <- paste0(bed.contents$V1, ':', bed.contents$V2 + 1, '-', bed.contents$V3);
	}
	
	return(bed.ids);
}