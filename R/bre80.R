#' Bre80 Cell Line
#'
#' @description
#' 	A dataset containing processed data from a capture Hi-C experiment in the Bre80 
#'	normal epithelial breast tissue cell line. The experiment targeted several breast 
#'	cancer risk loci, and reads that mapped to the 2q35 SNPs rs13387042 and rs16857609
#' 	are included in the dataset. 
#'
#' 	Data was prepared using the \code{prepare.data} function. Coordinates are GRCh38.
#'
#' @format 
#' A data table object with 47,766 rows and 13 columns.
#'
#' The variables are as follows:
#' \itemize{
#' 	\item target.id String in chrN:start-end format identifying target fragment
#' 	\item bait.id String in chrN:start-end format identifying bait fragment
#'	\item target.chr Chromosome of target fragment
#' 	\item target.start Start coordinate of target fragment (zero-based)
#'	\item target.end End coordinate of target fragment
#'	\item bait.chr Chromosome of bait fragment
#' 	\item bait.start Start coordinate of bait fragment (zero-based)
#'	\item bait.end End coordinate of bait fragment
#'	\item bait.to.bait Boolean indicating if the interaction is bait-to-bait (i.e. the fragment listed as target is also a bait)
#' 	\item bait.trans.count The number of reads linking the bait to fragments in trans (a measure of "interactibility")
#' 	\item target.trans.count The number of reads linking the target to fragments in trans (a measure of "interactibility")
#' 	\item distance Distance between the midpoints of the bait and target fragments (basepairs). NA for trans interactions
#' 	\item count The number of reads linking the two fragments
#' }
#'
#' @references 
#' 	Baxter, Joseph S., et al. "Capture Hi-C identifies putative target genes at 33 breast cancer risk loci." Nature Communications 9.1 (2018): 1028.
#'
#' @docType data
#' @keywords datasets
#' @name bre80
#' @usage data(bre80)
#'
NULL