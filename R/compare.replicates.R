#' compare.replicates
#'
#' @description
#' 	Compare replicates in a pairwise manner and further stratified by distance
#'
#' @param interaction.data A named vector specifying paths to files created using \{prepare.data()\}
#' @param output.directory Path to the output directory where pairwise plots are generated
#'
#' @return TRUE if pairwise plots were successfully created
#'
#' @author Syed Haider 
#'
#' @examples
#' # TODO
#'
#' @importFrom stats cor.test
#'
#' @export compare.replicates
compare.replicates <- function(interaction.data = NULL, output.directory = "./") { 

	if (is.null(interaction.data) || length(interaction.data) < 2) {
		stop("\ncannot proceed, not enough (< 2) replicate files provided!");
		}

	if (is.null(names(interaction.data))) {
		names(interaction.data) <- paste0("R", 1:length(interaction.data));
		}

	int.data <- list();
	distance.bins <- list(
		'0-10kbp' = c(0, 1e4),
		'10-100kbp' = c(1e4, 1e5),
		'100kbp-1Mbp' = c(1e5, 1e6),
		'1-10Mbp' = c(1e6, 1e7),
		'10-100Mbp' = c(1e7, 1e8),
		'100-1000Mbp' = c(1e8, 1e9)
		);

	for (sample.i in names(interaction.data)) {

		int.data[[sample.i]] <- fread(interaction.data[sample.i]);
		int.data[[sample.i]][, 'id' := paste(int.data[[sample.i]]$bait.id, int.data[[sample.i]]$target.id, sep = '.')];
		}

	# go through pairs of replicates and assess concordance
	for (i in 1:(length(int.data)-1)) { 
		for (j in (i+1):length(int.data)) { 

			# extract replicate names
			rep.i <- names(int.data)[i];
			rep.j <- names(int.data)[j];

			# combine the replicates into one matrix with counts
			unique.int <- union(int.data[[i]]$id, int.data[[j]]$id);

			# create temp matrix for pairwise comparison
			x <- matrix(
				data = 0,
				nrow = length(unique.int),
				ncol = 3,
				dimnames = list(
					unique.int,
					c('distance', 'R1', 'R2')
					)
				);

			# fill in relevant counts
			x[int.data[[i]]$id, 'R1'] <- log2(int.data[[i]]$count + 1);
			x[int.data[[j]]$id, 'R2'] <- log2(int.data[[j]]$count + 1);

			# fill in distance
			x[int.data[[i]]$id, 'distance'] <- int.data[[i]]$distance;
			x[int.data[[j]]$id, 'distance'] <- int.data[[j]]$distance;

			# data shown as intensity
			for (dist.i in names(distance.bins)) {

				# get distance range
				lower.dist <- distance.bins[[dist.i]][1];
				upper.dist <- distance.bins[[dist.i]][2];

				# extract counts where both samples have > 1 read count
				scp.x <- x[which(x[, 'distance'] >= lower.dist & x[, 'distance'] < upper.dist & x[, 'R1'] > 0 & x[, 'R2'] > 0), 'R1'];
				scp.y <- x[which(x[, 'distance'] >= lower.dist & x[, 'distance'] < upper.dist & x[, 'R1'] > 0 & x[, 'R2'] > 0), 'R2'];

				# skip bins without data as will cause error message
				if(length(table(scp.x)) == 0) next;

				# make a scatter plot with density
				cor.obj <- cor.test(scp.x, scp.y, method = "spearman");
				pdf(
					file = file.path(output.directory, paste0('counts_cis__', rep.i, '_', rep.j, '__', dist.i, '.pdf')),
					height = 5,
					width = 5
					);
				par(
					tcl = -0.2,
					las = 1,
					font.lab = 2,
					cex.lab = 1,
					font.axis = 2,
					cex.axis = 1,
					mar = c(2.7, 3.5, 0.5, 0.2), 
					mgp = c(1.66, 0.25, 0),
					oma = c(0, 0, 0, 0)
					);
				plot(
					scp.x, 
					scp.y,
					xaxs = "r",
					xlab = paste(rep.i, ' (log2 no. of reads)'),
					ylab = paste(rep.j, ' (log2 no. of reads)'),
					#xlim = ,
					#ylim = ,
					pch = 20,
					col = "black",
					cex = 1
					);
				mtext(
					las = 0,
					adj = 0.062,
					padj = 2,
					text = paste0("rho: ", round(cor.obj$estimate, 3), ", P: ", format.pval(cor.obj$p.value, digits = 3, eps = .Machine$double.xmin)),
					side = 3,
					font = 1,
					cex = 1,
					font.axis = 2
					);
				dev.off();

				}
			}
		}

	return (TRUE);
}
