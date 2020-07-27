#' convert.standard.format
#'
#' @description
#' 	Create a file in standard format for cross compatability including with 
#' WashU Epigenome Browser.
#'
#' @param chicane.results Path to CHiCANE interaction calls file
#' @param file.name Path to output file
#' 
#' @return TRUE if output files are created successfully
#'
#' @author Andrea Gillespie, Syed Haider 
#'
#' @examples
#'	chicane.results <- system.file(
#'    'extdata', 'T47D_2q35_filtered_chicane_calls.txt', 
#'    package = 'chicane'
#'    );
#'  output.file = file.path(tempdir(), 'temp_standard_format.txt');
#' 	convert.standard.format(chicane.results, file.name = output.file);
#'
#' @export convert.standard.format
convert.standard.format <- function(chicane.results, file.name = '') {

	calls <- fread(chicane.results, header = TRUE);

	# remove chrUn which will cause error in browser and only keep columns needed for long range format
	chrUn <- which(substr(calls$target.id, 1, 5) == 'chrUn');

	if (length(chrUn) > 0) { calls <- calls[-chrUn, ]; }

	score <- -log10(calls$q.value);
	score[ is.infinite(score) ] <- max( score[ is.finite(score) ] );

	standard.format <- data.table(
		id = mapply(
			FUN = get.interaction.id,
			bait = calls$bait.id,
			other.end = calls$target.id,
			bait.to.bait = calls$bait.to.bait,
			MoreArgs = list(
				zero.based = TRUE
				)
			),
		trans = calls$bait.chr != calls$target.chr,
		b2b = calls$bait.to.bait,
		distance = calls$distance,
		count = calls$count,
		score = score
		);

	# sort
	standard.format <- standard.format[ order(score, decreasing = TRUE) ];

	utils::write.table(
		standard.format, 
		file = file.name,
		sep = '\t',
		row.names = FALSE
		);

	return (TRUE);
}
