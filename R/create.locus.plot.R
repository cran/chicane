#' create.locus.plot
#'
#' @description
#' 	Create a file compatible with WashU Epigenome Browser from CHiCANE interaction calls.
#'
#' @param genome Name of genome build (e.g. 'hg38' or 'hg37')
#' @param chr Chromosome number for desired locus including 'chr' (e.g. 'chr1')
#' @param start Start coordinate of desired locus
#' @param end End coordinate of desired locus
#' @param gene.data Path to chosen genome annotation file in .gtf format
#' @param genomic.features Path to BED file with coordinates of desired feature track
#' @param feature.name Title to appear above genomic features
#' @param fdr.filter Q-value filter threshold for interaction calls to be included
#' @param interaction.data Path to unfiltered CHiCANE calls output
#' @param file.name Path to output file
#' @param height Height in inches for desired plot
#' @param width Width in inches of desired plot
#' @param track.heights Vector of length 6 indicating desired height of individual tracks
#' @param \dots Any additional parameters to \code{Gviz::plotTracks}
#'
#' @return TRUE if plot was successfully created
#'
#' @author Andrea Gillespie, Syed Haider 
#'
#' @importFrom grDevices pdf
#'
#' @examples
#' # In order to conserve memory only significant interactions are included in example 
#' # interaction.data file. However, in order to show raw counts, unfiltered calls should be 
#' # included and only significant interactions (as set by fdr.filter) wil be displayed
#' 
#' gene.data <- system.file('extdata', 'gencode_2q35.gtf', package = 'chicane');
#' genomic.features <- system.file('extdata', '2q35.bed', package = 'chicane');
#' interaction.data <- system.file(
#'   'extdata', 'T47D_2q35_filtered_chicane_calls.txt', 
#'   package = 'chicane'
#'   );
#' file.name <- file.path(tempdir(), "chr2_interactions.pdf");
#'
#' \donttest{
#' create.locus.plot(
#'    genome = 'hg38', 
#'    chr = 'chr2', 
#'    start = 216600000,
#'    end = 217200000,
#'    gene.data = gene.data,
#'    genomic.features = genomic.features,
#'    feature.name = 'baits',
#'    interaction.data = interaction.data,
#'    file.name = file.name,
#'    collapseTranscripts = TRUE,
#'    shape = "arrow"
#'    );
#'  }
#'
#' @export create.locus.plot
create.locus.plot <- function(
	genome = 'hg38',
	chr = NULL,
	start = NULL,
	end = NULL,
	gene.data = NULL,
	genomic.features = NULL,
	feature.name = NULL, 
	fdr.filter = 0.05,
	interaction.data = NULL,
	file.name = NULL,
	height = 5.5,
	width = 8.5,
	track.heights = c(0.2, 0.5, 0.8, 0.5, 1.5, 2),
	...
	) {

	options(ucscChromosomeNames=FALSE);

	### MAIN ######################################################################

	ideogram.track <- Gviz::IdeogramTrack(genome = genome, chromosome = chr);
	genome.axis.track <- Gviz::GenomeAxisTrack();

	genes.track <- Gviz::GeneRegionTrack(
		gene.data,
		chromosome = chr, 
		start = start, 
		end = end,
		stacking = 'squish',
		stackHeight = 0.5,
		name = 'Genes'
		);

	Gviz::displayPars(genes.track)$cex.title <- 0.8;
	Gviz::displayPars(genes.track)$lex <- 1.5;
	Gviz::displayPars(genes.track)$cex <- 1.2;

	genomic.features <- fread(genomic.features);
	# limit features coordinates to locus
	feat.coords <- genomic.features[
		which(genomic.features$V1 == chr & genomic.features$V2 >= start & genomic.features$V3 <= end), 
		];
	feat.widths <- feat.coords$V3 - feat.coords$V2;

	annotation.track <- Gviz::AnnotationTrack(
		start = feat.coords$V2,
		width = feat.widths,
		chromosome = chr,
		group = rep(feature.name, nrow(feat.coords)),
		name = 'ROIs'
		);
	Gviz::feature(annotation.track) <- rep('features', nrow(feat.coords));
	Gviz::displayPars(annotation.track)$cex.title <- 0.8;	
	Gviz::displayPars(annotation.track)$lwd <- 0;	

	# limit reads to desired chromosome
	interactions <- fread( paste( 'grep', paste0(chr, ':'), interaction.data ) );
	names(interactions) <- colnames(fread(interaction.data, nrows = 1));

	fragments <- c(interactions$bait.id, interactions$target.id);
	counts.track <- Gviz::DataTrack(
		range = GenomicRanges::GRanges(fragments),
		data = rep(interactions$count, 2),
		name = 'Counts'
		);
	Gviz::displayPars(counts.track)$cex.title <- 0.85;

	interaction.tracks <- list();

	# restrict to q-value
	calls <- interactions[ q.value < fdr.filter ];

	interaction.object <- GenomicInteractions::GenomicInteractions(
		anchor1 = GenomicRanges::GRanges( calls$bait.id ),
		anchor2 = GenomicRanges::GRanges( calls$target.id ),
		counts = calls$q.value
		);

	interaction.track <- GenomicInteractions::InteractionTrack(
		interaction.object,
		name = 'Interactions'
		);

	Gviz::displayPars(interaction.track)$col.outside <- 'black';
	Gviz::displayPars(interaction.track)$col.interactions <- 'black';
	Gviz::displayPars(interaction.track)$col.anchors.line <- '#585858';
	Gviz::displayPars(interaction.track)$col.anchors.fill <- 'gainsboro';
	Gviz::displayPars(interaction.track)$cex.title <- 0.9;
	Gviz::displayPars(interaction.track)$plot.outside <- FALSE;
	Gviz::displayPars(interaction.track)$background.title <- 'limegreen';
	Gviz::displayPars(interaction.track)$frame <- TRUE;

	track.list <- list(
		ideogram.track, 
		genome.axis.track, 
		genes.track, 
		annotation.track, 
		counts.track, 
		interaction.track
		);

	pdf(
		file = file.name,
		height = height,
		width = width
		);
	Gviz::plotTracks(
		track.list,
		sizes = track.heights,
		transcriptAnnotation = 'symbol',
		# shape = "arrow",
		features = 'tomato',
		groupAnnotation = 'group',
		just.group = 'above',
		col.main = 'black',
		cex.main = 2,
		col.title = 'black',
		from = start,
		to = end,
		chromsome = chr,
		type = 'histogram',
		...
		);

	dev.off();

	return (TRUE);

}