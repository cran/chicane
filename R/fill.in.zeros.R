#' fill.in.zeros
#' 
#' @description 
#'	Add zero counts to interaction data
#'
#' @param interaction.data
#' 	Data table containing interaction data
#' @param baits
#'	Vector of bait IDs used in the experiment, in format chrN:start-end
#' @param fragments
#' 	Vector of potential fragments the baits can link up to, in format chrN:start-end
#'
#' @return Data table containing origiina
#'
#' @examples
#'	data(bre80);
#' 	bait.file <- system.file('extdata', '2q35.bed', package = 'chicane');
#' 	fragment.file <- system.file('extdata', 'GRCh38_HindIII_chr2.bed.gz', package = 'chicane');
#' 	results <- fill.in.zeros(
#'		bre80,
#'		baits = read.bed(bait.file), 
#'		fragments = read.bed(fragment.file)
#'		);
#'
#' @aliases fill.in.zeroes
#'
#' @export fill.in.zeros
fill.in.zeros <- function(interaction.data, baits, fragments) {


	### INPUT TESTS ###########################################################

	# check there aren't too many baits and fragments
	if( as.numeric(length(baits) )*as.numeric( length(fragments) ) > 10^9) {
		stop('Too many baits and/ or fragments');
	}

	### MAIN ##################################################################

	# if no baits, no possible combinations
	# return original input data
	if( 0 == length(baits) ) return(interaction.data);

	all.combinations <- CJ(fragments, baits);
	names(all.combinations) <- c('target.id', 'bait.id');

	# fill in coordinates
	expanded.combinations <- add.fragment.coordinates(all.combinations);

	# add in bait to bait indicator
	expanded.combinations[, bait.to.bait := (expanded.combinations$target.id %in% baits)];

	# remove "self-ligations"
	expanded.combinations <- expanded.combinations[ target.id != bait.id ];

	# remove bait-to-bait combinations where "target" comes before bait
	# easiest to do this by first splitting into bait-to-bait and non-bait-to-bait interactions
	#
	# Note: this method has the unfortunate consequence of not sorting into chromosome order
	# For example, chrM comes before chrX, chr20 before chr3
	#
	non.b2b.combinations <- expanded.combinations[ !expanded.combinations$bait.to.bait ];
	b2b.combinations <- expanded.combinations[ expanded.combinations$bait.to.bait ];

	b2b.combinations <- b2b.combinations[ b2b.combinations$bait.chr <= b2b.combinations$target.chr ]; # bait chromosome comes before target chromosome
	b2b.combinations <- b2b.combinations[ b2b.combinations$bait.chr != b2b.combinations$target.chr | b2b.combinations$bait.start < b2b.combinations$target.start]; # if same chromsome, bait start before target start

	# reassemble full dataset
	expanded.combinations <- rbind(non.b2b.combinations, b2b.combinations);


	# merge with original data
	filled.in.data <- merge(
		expanded.combinations, 
		interaction.data,
		by.x = names(expanded.combinations),
		by.y = names(expanded.combinations),
		all = TRUE
		);

	# set count := 0 for all count columns
	# there could be several replicates – want to fill in all of them
	for( count.column in names(filled.in.data)[ grepl('^count', names(filled.in.data)) ] ) {
		filled.in.data[[ count.column ]][ is.na(filled.in.data[[ count.column ]]) ] <- 0;
	}
	
	# remove duplicates
	filled.in.data <- unique(filled.in.data);

	# sanity check
	
	if( nrow(interaction.data) != nrow( filled.in.data[ count != 0 ]) ) {

		error.message <- paste(
			'Internal bug - mismatched non-zero row numbers when filling in zeros\n',
			'Before:', nrow(interaction.data), 'non-zero rows\n',
			'After:', nrow( filled.in.data[ count != 0 ]), 'non-zero rows\n'
			);

		stop(error.message);
	}

	return(filled.in.data);
}


#' @rdname fill.in.zeros
#'
#' @export fill.in.zeroes
fill.in.zeroes <- fill.in.zeros;