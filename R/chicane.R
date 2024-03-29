#' chicane
#'
#' @description
#'	Run full method for detecting significant interactions in capture Hi-C experiments, starting 
#'  either from a BAM file or preprocessed data from \code{prepare.data}
#'
#' @param bam 
#' 	Path to a BAM file
#' @param baits 
#'	Path to a BED file containing the baits
#' @param fragments 
#'	Path to a BED file containing all restriction fragments in the genome
#' @param interactions 
#'	Data table or path to a text file detailing fragment interactions, typically from \code{prepare.data}. 
#'	Can be used instead of bam/baits/fragments specification if the text files have already been prepared.
#' @param replicate.merging.method
#' 	Method that should be used for merging replicates, if applicable
#' @param bait.filters 
#'	Vector of length two, where the first element corresponds to the lower-end filter and the second to the upper-end filter.
#' 	When global multiple testing correction is performed, altering the bait filtering settings may affect the number of significant results.
#' @param target.filters
#' 	Vector of length two, giving lower and higher filter, respectively. 
#'	Changing this filtering setting may affect multiple testing correction by altering the number of tests performed.
#' @param distance.bins
#' 	Number of bins to split distance into. Models are fit separately in each bin.
#' @param multiple.testing.correction
#'	String specifying how multiple testing correction should be performed, by bait or globally.
#' @param verbose
#' 	Logical indicating whether to print progress reports.
#' @param interim.data.dir
#'  Path to directory to store intermediate QC data and plots. NULL indicate skip intermediate results.
#' @inheritParams prepare.data 
#' @inheritParams combine.replicates
#' @inheritParams fit.model
#' @inheritParams fit.glm
#' 
#' @return Data table with columns
#' 	\item{target.id}{String in chrN:start-end format identifying target fragment}
#' 	\item{bait.id}{String in chrN:start-end format identifying bait fragment}
#'	\item{target.chr}{Chromosome of target fragment}
#' 	\item{target.start}{Start coordinate of target fragment (zero-based)}
#'	\item{target.end}{End coordinate of target fragment}
#'	\item{bait.chr}{Chromosome of bait fragment}
#' 	\item{bait.start}{Start coordinate of bait fragment (zero-based)}
#'	\item{bait.end}{End coordinate of bait fragment}
#'	\item{bait.to.bait}{Boolean indicating if the interaction is bait-to-bait (i.e. the fragment listed as target is also a bait)}
#' 	\item{bait.trans.count}{The number of reads linking the bait to fragments in trans (a measure of "interactibility")}
#' 	\item{target.trans.count}{The number of reads linking the target to fragments in trans (a measure of "interactibility")}
#' 	\item{distance}{Distance between the midpoints of the bait and target fragments (basepairs). NA for trans interactions}
#' 	\item{count}{The number of reads linking the two fragments}
#' 	\item{expected}{The expected number of reads linking the two fragments under the fitted model}
#'	\item{p.value}{P-value for test of the observed number of reads significantly exceeding the expected count}
#'	\item{q.value}{FDR-corrected p-value}
#'
#' @import data.table
#'
#' @examples
#' \donttest{
#' if( bedtools.installed() ) {
#'   # start from BAM file
#'   bam <- system.file('extdata', 'Bre80_2q35.bam', package = 'chicane');
#'   baits <- system.file('extdata', '2q35.bed', package = 'chicane');
#'   fragments <- system.file('extdata', 'GRCh38_HindIII_chr2.bed.gz', package = 'chicane');
#'   results <- chicane(
#'		bam = bam, 
#'		baits = baits, 
#'		fragments = fragments
#'		);
#' }
#'
#' # start from pre-processed data
#' data(bre80); 
#' results <- chicane(interactions = bre80);
#' }
#'
#' @author Erle Holgersen <Erle.Holgersen@icr.ac.uk>
#'
#' @export chicane
chicane <- function(
	bam = NULL,
	baits = NULL,
	fragments = NULL,
	interactions = NULL, 
	replicate.merging.method = 'sum',
	distribution = 'negative-binomial',
	include.zeros = 'none',
	bait.filters = c(0, 1),
	target.filters = c(0, 1),
	distance.bins = NULL,
	multiple.testing.correction = c('bait-level', 'global'),
	adjustment.terms = NULL,
	remove.adjacent = FALSE,
	temp.directory = NULL,
	keep.files = FALSE,
	maxit = 100,
	epsilon = 1e-8,
	cores = 1,
	trace = FALSE,
	verbose = FALSE,
	interim.data.dir = NULL
	) {

	# TO DO:
	#	- check format of interactions data if passed directly

	### INPUT TESTS ######################################################

	if( is.null(interactions) && (is.null(bam) || is.null(baits) || is.null(fragments)) ) {
		stop('Must provide either interactions or bam/baits/fragments.');
	}

	if( !is.null(interactions) &&  !(is.null(bam) || is.null(baits) || is.null(fragments)) ) {
		stop('Cannot deal with both interactions and bam/baits/fragments. Please provide one or the other.');
	}

	if( !is.null(interactions) && !is.character(interactions) && !is.data.table(interactions) ) {
		stop('interactions should be either a data.table object or the path to a text file generated by prepare.data');
	}

	if( is.null(interactions) ) {
	    
	    input.files <- list(bam, baits, fragments);
	    
	    # all should be character strings
	    object.is.character <- vapply(
	        input.files,
	        FUN = function(x) all( is.character(x) ),
	        FUN.VALUE = FALSE 
	        );
	    
	    if( !all(object.is.character) ) {
		    stop('bam, baits, and fragments should be character strings');
	    }
	    
	    # baits and fragments should have length 1
	    # bam could have longer length if replicates should be combined
	    if( !all( 1 == vapply(input.files[-1], length, FUN.VALUE = 0) ) ) {
	        stop('bam, baits, and fragments should have length 1');
	    }
    
	   # all files should exist
	   for( input.file in unlist(input.files) ) {
	       if( !file.exists(input.file) ) {
	           error.message <- paste('File', input.file, 'does not exist');
	           stop(error.message);
	       }
	   }
	    
	}
    
	# adding in zeros happens at the data preparation step
	# throw an error if user wants it added to pre-prepared data
	if( !is.null(interactions) && 'none' != include.zeros ) {
		stop('Zeros are added in the prepare.data step. Please provide bam/baits/fragments');
	}

	### MAIN #############################################################
	
	replicate.merging.method <- match.arg(replicate.merging.method);
	multiple.testing.correction <- match.arg(multiple.testing.correction);


	# prepare data
	if( is.null(interactions) ) {

		if( verbose ) {
			cat('PREPARING DATA\n');
		}

		interaction.data <- prepare.data(
			bam, 
			baits, 
			fragments, 
			replicate.merging.method = replicate.merging.method,
			include.zeros = include.zeros,
			remove.adjacent = remove.adjacent,
			temp.directory = temp.directory,
			keep.files = keep.files,
			verbose = verbose
			);
		
	} else if( is.character(interactions) ) { 

		if( !file.exists(interactions) ) {
			error.message <- paste('File', interactions, 'does not exist');
			stop(error.message);
		}

		if( verbose ) {
			cat('READING DATA FROM FILE\n');
		}
		
		interaction.data <- data.table::fread(interactions);

	} else if( is.data.table(interactions) ) {
		interaction.data <- interactions;

		# free up memory
		rm(interactions);
	} 

	if( verbose ) {
		cat('FITTING MODEL\n');
	}
	
	chicane.results <- fit.model(
		interaction.data, 
		distance.bins = distance.bins, 
		distribution = distribution,
		bait.filters = bait.filters,
		target.filters = target.filters,
		adjustment.terms = adjustment.terms,
		verbose = verbose,
		cores = cores,
		maxit = maxit,
		epsilon = epsilon,
		trace = trace,
		interim.data.dir = interim.data.dir
		);

	chicane.results <- multiple.testing.correct(
		chicane.results,
		bait.level = 'bait-level' == multiple.testing.correction
		);

	# sort by q-value
	chicane.results <- chicane.results[ order(q.value, p.value) ];

	return(chicane.results);
}