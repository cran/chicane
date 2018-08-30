#' fit.glm
#'
#' @description
#'	Fit GLM according to a specified distribution. This needs to be done separately from \code{glm}
#' 	in order to include negative binomial and truncated distributions as options.
#'
#' @param formula 
#' 	Formula specifying model of interest
#' @param data 
#'	Data frame containing variables specified in formula
#' @param distribution 
#' 	Name of distribution of the counts. Options are 'negative-binomial', 
#'	'poisson', 'truncated-poisson', and 'truncated-negative-binomial'
#' @param start 
#' 	Starting values for model coefficients
#' @param init.theta
#' 	Initial value of theta if fitting the negative binomial distribution
#' @param maxit 
#'	Maximum number of IWLS iterations for fitting the model (passed to \code{glm.control})
#' @param epsilon
#'	Positive convergence tolerance for Poisson and negative binomial models. Passed to \code{glm.control}
#' @param trace
#' 	Logical indicating if output should be produced for each of model fitting procedure. Passed to \code{glm.control} or \code{gamlss.control}
#'
#' @import gamlss gamlss.tr
#'
#' @return List with elements
#'  \item{model}{model object}
#' 	\item{expected.values}{vector of expected values for each element in original data}
#' 	\item{p.values}{vector of p-values for test of significantly higher response than expected}
#' 
fit.glm <- function(
	formula, 
	data, 
	distribution = c('negative-binomial', 'poisson', 'truncated-poisson', 'truncated-negative-binomial'),
	start = NULL,
	init.theta = NULL,
	maxit = 100,
	epsilon = 1e-8,
	trace = FALSE
	) {

	distribution <- match.arg(distribution);

	### INPUT TESTS ###########################################################



	### MAIN ##################################################################

	# get observed counts
	# might want to account for case where data is missing? 
	observed.count <- data[[ all.vars(formula)[1] ]];

	# negative binomial can be tricky to fit
	# increase the max number of iterations to help
	glm.control <- stats::glm.control(
		maxit = maxit, 
		epsilon = epsilon, 
		trace = trace
		);

	if( 'negative-binomial' == distribution ) {

		# unfortunately init.theta uses a missing() construct
		# need a different function call if init.theta = NULL
		
		if( is.null(init.theta) ) {
			model <- MASS::glm.nb(
				formula, 
				data,
				control = glm.control,
				start = start
				);

		} else {
			model <- MASS::glm.nb(
				formula, 
				data,
				control = glm.control,
				init.theta = init.theta,
				start = start
				);
		}


		# sanity check that no rows were lost due to missing data
		model.rows.sanity.check(data, model);

		expected.values <- model$fitted.values;
		p.values <- stats::pnbinom(
			observed.count - 1, # probability of an observation at least this large
			mu = expected.values,
			size = model$theta,
			lower = FALSE
			);

	} else if( 'poisson' == distribution ) {

		model <- stats::glm(
			formula, 
			data, 
			family = 'poisson',
			control = glm.control,
			start = start
			);

		# Make sure no rows have been lost 
		# (causes problems when adding to the data frame)
		model.rows.sanity.check(data, model);

		expected.values <- model$fitted.values;
		p.values <- stats::ppois(
			observed.count - 1, # probability of an observation at least this large
			lambda = expected.values,
			lower = FALSE
			);

	} else {

		# gamlss.installed <- requireNamespace('gamlss.tr', quietly = TRUE);

		# if( !gamlss.installed ) {
		# 	stop('Truncated distributions depend on the GAMLSS package - please install it and try again');
		# }

		gamlss.control <-  gamlss::gamlss.control(
			trace = trace,
			c.crit = 0.1 # see if this speeds up model fitting
			);

		# make a temporary data frame containing only columns needed for model we want to specify
		# GAMLSS throws an error when there are ANY NAs in the data.. even if we don't use that column
		temp.data <- stats::get_all_vars(formula, data = data);

		if( 'truncated-poisson' == distribution ) {


			gamlss.tr::gen.trun( 0, family = 'PO' );

			# TO DO: 
			#	- extend sanity check to accommodate S4 objects

			model <- gamlss::gamlss(
				formula,
				data = temp.data,
				family = POtr(), 
				control = gamlss.control
				);

			# mean of truncated Poisson is no longer equal to lambda
			# see: https://en.wikipedia.org/wiki/Zero-truncated_Poisson_distribution
			expected.values <- ( model$mu.fv*exp(model$mu.fv) )/(exp(model$mu.fv) - 1);
			p.values <- mapply(	
				FUN = function(observed, mu) {
					# GAMLSS distribution functions do not accept arguments out of range of the distribution
					# Solution: set to 1 if observed count is lowest it can be
					p.value <- ifelse(
						observed >= 2,
						pPOtr(observed - 1, mu = mu, lower.tail = FALSE),
						1
						);

					return(p.value);
					},
				observed.count,
				model$mu.fv
				);

		} else if( 'truncated-negative-binomial' == distribution ) {
			

			gamlss.tr::gen.trun( 0, family = 'NBI' );

			model <- gamlss::gamlss(
				formula,
				data = temp.data,
				family = NBItr(), 
				control = gamlss.control
				);

			# calculate mean of a zero-truncated negative binomial with parameters mu, sigma
			expected.values <- model$mu.fv/( 1 - (1 + model$sigma.fv*model$mu.fv)^(-1/model$sigma.fv) );
			p.values <- mapply(	
				FUN = function(observed, mu, sigma) {
					# GAMLSS distribution functions do not accept arguments out of range of the distribution
					# Solution: set to 1 if observed count is lowest it can be
					p.value <- ifelse(
						observed >= 2,
						pNBItr(
							observed - 1, 
							mu = mu, 
							sigma = sigma,
							lower.tail = FALSE
							),
						1
						);

					return(p.value);
					},
				observed.count,
				model$mu.fv,
				model$sigma.fv
				);	
		} 
	}

	model.data <- list(
		model = model,
		expected.values = expected.values,
		p.values = p.values
		);

	return(model.data);
}
