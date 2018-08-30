#' convert.hicup.digest.bed
#'
#' @description
#' 	Convert a HiCUP digest file to BED format.
#'
#' @param hicup.digest Path to HiCUP digest
#' @param file.name Path to output file. A blank string indicates output to the console.
#'
#' @examples
#'	hicup.digest <- system.file('extdata', 'HiCUP_digest_example.txt', package = 'chicane');
#' 	convert.hicup.digest.bed(hicup.digest);
#'
#' @export convert.hicup.digest.bed
convert.hicup.digest.bed <- function(hicup.digest, file.name = '') {

	hicup.data <- fread(hicup.digest, skip = 1, header = TRUE);

	# sanity check 
	if( !all(names(hicup.data)[1:3] == c('Chromosome', 'Fragment_Start_Position', 'Fragment_End_Position') )  ) {
		error.message <- paste(
			'HiCUP digest file does not match expected format.\n',
			'Expected first three columns to be Chromosome, Fragment_Start_Position, Fragment_End_Position'
			);

		stop(error.message);
	}

	# convert to bed format - zero based coordinates
	bed.data <- data.table(
		chr = hicup.data[[ 1 ]],
		start = hicup.data[[ 2 ]] - 1,
		end = hicup.data[[ 3 ]]
		);

	utils::write.table(
		bed.data,
		file = file.name,
		row.names = FALSE,
		col.names = FALSE,
		quote = FALSE,
		sep = '\t'
		);
}