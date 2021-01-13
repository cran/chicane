#' create.modelfit.plot
#'
#' @description
#' 	create a plot representing model's fit
#'
#' @param model An object of fitted model
#' @param file.name A string specifying plotting file name
#' @param resolution A numeric specifying plot's resolution
#'
#' @return TRUE if plot was successfully created
#'
#' @author Syed Haider
#'
#' @importFrom grDevices dev.off png
#' @importFrom graphics mtext par plot title
#' @importFrom stats logLik
#'
#' @export create.modelfit.plot

create.modelfit.plot <- function(model, file.name = NULL, resolution = 300) {

	cat("\nstarting model fit plot for: ", file.name);

	# initialise cosmetics
	margins = c(2.66, 3.5, 0.72, 0.2);
	height = 3.2; 
	width = 5;
	mar.bottom <- margins[1];
	mar.left <- margins[2];
	mar.top <- margins[3];
	mar.right <- margins[4];
	mgp.lab <- 1.66;

	current.type <- getOption("bitmapType");
	if (capabilities("cairo")) { options(bitmapType = "cairo"); } 

	png(
		filename = file.name,
		height = height,
		width = width,
		units = "in",
		res = resolution
	);   

	par(
		tcl = -0.2,
		las = 1,
		font.lab = 2,
		cex.lab = 1.2,
		font.axis = 2,
		cex.axis = 1.2,
		mar = c(mar.bottom, mar.left, mar.top, mar.right),
		mgp = c(mgp.lab, 0.25, 0),
		oma = c(0, 0, 0, 0),
		family = "sans"
		);

	plot(
		countreg::rootogram(model, style = "hanging", plot = TRUE, main = ""), 
		xaxs = "r",
		xlab = "Count",
		ylab = ""
		);

	# yaxis label
	mtext(
		las = 0,
		padj = -1.72,
		text = expression(sqrt("Frequency")),
		side = 2,
		font = 2,
		cex = 1.2,
		font.axis = 2
		);

	title(paste("log-likelihood:", round(logLik(model), 3)), font.main = 2);

	dev.off();
	options(bitmapType = current.type);

	cat("\nfinished creating model fit plot");

	return( TRUE );

}
