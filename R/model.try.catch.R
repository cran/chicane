#' model.try.catch
#'
#' @description
#' 	Internal function for fitting model within a tryCatch loop, handling numerical errors gracefully.
#'
#' @inheritParams run.model.fitting
#' @param model.formula formula
#' @param data model data
#' @param init.theta Initial value of theta in negative binomial model
#' @param start starting values of coefficients in linear predictor
#' 
#' @return List with elements
#'  \item{model}{model object. Set to NULL if no model could be fit.}
#' 	\item{expected.values}{vector of expected values for each element in original data, or vector of NAs if no model could be fit}
#' 	\item{p.values}{vector of p-values for test of significantly higher response than expected, or vector of NAs if no model could be fit}
#'
model.try.catch <- function(
	model.formula, 
	data,
	distribution = 'negative-binomial',
	maxit = 100,
	epsilon = 1e-8,
	init.theta = NULL, 
	start = NULL,
	trace = FALSE,
	verbose = FALSE
	) {
	
	# Note: 
	#	this function probably does not need to be exported, but to avoid problems with
	#	foreach I have added a chicane:: prefix to the function call in run.model.fitting
	#	As a consequence, I had to export this function.

	# Negative binomial GLM only supports overdispersion. 
	# When the variance does not exceed the mean, this causes problems with model fitting/
	# 
	# These problems manifest in one of two ways:
	#	1) An error in while ((it <- it + 1) < limit && abs(del) > eps)
	#	2) A warning about NaNs produced in sqrt(1/i)
	#
	# Handle these cases by trying to fit a Poisson distribution instead.
	model <- tryCatch({

		# try fitting distribution as requested by user
		fit.glm( 
			model.formula, 
			data,
			distribution = distribution,
			maxit = maxit,
			epsilon = epsilon,
			trace = trace,
			init.theta = init.theta,
			start = start
 			); 

		}, error = function(e) {

			# if problem was negative binomial, try Poisson
			if( 'negative-binomial' == distribution && is.glm.nb.theta.error(e) ) {

				if(verbose) cat('\nDispersion error - running Poisson\n');
				
				temp.model <- fit.glm( 
					model.formula, 
					data,
					distribution = 'poisson',
					maxit = maxit,
					epsilon = epsilon,
					trace = trace
 					); 

				return(temp.model);
				
			} else if ( grepl('no valid set of coefficients has been found: please supply starting values', e$message, fixed = TRUE) ) {
				# enter error as couldn't fit model	

				if(verbose) cat('\nNo valid coefficients error - skipping ahead\n');

				model.data <- list(
					model = NULL,
					expected.values = rep(NA, nrow(data)),
					p.values = rep(NA, nrow(data))
					);

				return( model.data );

			} else if( grepl("NA/NaN/Inf in 'x'", e$message, fixed = TRUE ) ) {

				if(verbose) cat('\nNA/NaN/Inf in x error - skipping ahead\n');

				model.data <- list(
					model = NULL,
					expected.values = rep(NA, nrow(data)),
					p.values = rep(NA, nrow(data))
					);

				return( model.data );

			} else {
				if(verbose) {
					cat('\nUnknown error - skipping ahead\n');
					cat(e$message, '\n');
				}

				model.data <- list(
					model = NULL,
					expected.values = rep(NA, nrow(data)),
					p.values = rep(NA, nrow(data))
					);
			}

		}, warning = function(w) {

			dispersion.problem <- FALSE;
				
			if( 'negative-binomial' == distribution && ( is.glm.nb.maxiter.warning(w) || is.glm.nb.theta.warning(w) ) ) {
				
				if(verbose) cat('Caught a warning - checking for dispersion problems\n');

				# See if problem is lack of overdispersion
				# Get estimate of theta after a low number of iterations
				# 	=> if no evidence for overdispersion, fit Poisson

				negbin.fit <- suppressWarnings(
					fit.glm( 
						model.formula, 
						data,
						distribution = 'negative-binomial',
						maxit = 25,
						epsilon = epsilon,
						trace = trace,
						init.theta = init.theta,
						start = start
 						)
					); 

				max.mu <- max(negbin.fit$model$fitted.values);
				var.at.max.mu <- max.mu + (max.mu^2)/negbin.fit$model$theta;

				# assess difference between NB variance and Poisson one
				if( (var.at.max.mu - max.mu)/max.mu < 0.001 ) {
					dispersion.problem <- TRUE;
				}

			} 

			# if there is a dispersion problem, try Poisson
			# if not, raise original warning and fit original distribution
			if( dispersion.problem ) {

				if(verbose) cat('Dispersion problem detected - fitting Poisson\n');

				distribution <- 'poisson';
			} else {
				warning(w);
			}

			temp.model <- tryCatch({
				fit.glm( 
					model.formula, 
					data,
					distribution = distribution,
					maxit = maxit,
					epsilon = epsilon,
					trace = trace
 					); 
				}, error = function(e) {

					if(verbose) cat('\nUnknown error - skipping ahead\n');
					list(
						model = NULL,
						expected.values = rep(NA, nrow(data)),
						p.values = rep(NA, nrow(data))
						);
				});

			return(temp.model);

		});


	return(model);

}