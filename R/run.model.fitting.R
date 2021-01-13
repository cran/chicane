#' run.model.fitting
#'
#' @description
#' 	Run model fitting procedure for either bait-to-bait or other interactions.
#'	Meant for internal use only. 
#'
#' @inheritParams fit.model
#' @inheritParams fit.glm
#' @param bait.to.bait Logical indicating if model should be fit as bait-to-bait
#' @param adjustment.terms Characted vector of extra terms to adjust for in the model fit
#'
#'
#' @return Interactions data with expeceted number of interactions and p-values added.
#'
#' @importFrom foreach %dopar%
#' @importFrom iterators icount
#' @importFrom stats logLik
run.model.fitting <- function(
	interaction.data,
	distance.bins = NULL, 
	distribution = 'negative-binomial',
	bait.to.bait = FALSE,
	adjustment.terms = NULL,
	maxit = 100,
	epsilon = 1e-8,
	cores = 1,
	trace = FALSE,
	verbose = FALSE,
	interim.data.dir = NULL
	) {

	# TO DO:
	# 	- see if you can avoid cis/ trans repetitiveness
	
	### INPUT TESTS ###########################################################

	# be extremely strict about these things to avoid bugs
	if( bait.to.bait && !all( interaction.data$bait.to.bait ) ) {
		stop('Cannot fit bait-to-bait model when not all interactions are bait-to-bait');
	}

	if( !bait.to.bait && any(interaction.data$bait.to.bait) ) {
		stop('Cannot fit non-bait-to-bait model on bait-to-bait interactions');
	}

	if( !is.null(adjustment.terms) && !is.character(adjustment.terms) ) {
		stop('adjustment.terms must be a character vector');
	}

	### MAIN ##################################################################

	# figure out formula to use based on whether it's bait-to-bait or not
	# need separate cis and trans formulas because of distance adjustment
	if( bait.to.bait ) {
		cis.formula <- stats::as.formula(count ~ log(distance) + log(bait.trans.count + 1)*log(target.trans.count + 1));
		trans.formula <- stats::as.formula(count ~ log(bait.trans.count + 1)*log(target.trans.count + 1));
	} else {
		cis.formula <- stats::as.formula(count ~ log(distance) + log(bait.trans.count + 1) );
		trans.formula <- stats::as.formula(count ~ log(bait.trans.count + 1) )
	}

	# if requested, update model with user-requested terms
	if( !is.null(adjustment.terms) ) {
		adjustment.string <- paste(adjustment.terms, collapse = ' + ');

		cis.formula <- stats::update.formula(cis.formula, paste0('~ . + ', adjustment.string) );
		trans.formula <- stats::update.formula(trans.formula, paste0('~ . + ', adjustment.string) );

		# graceful error handling – make sure all variables are in the input data
		# do this here in case user specifies something like log(x) in adjustment.terms
		formula.vars <- unique( c(all.vars(cis.formula), all.vars(trans.formula)) );
		if( !all(formula.vars %in% names(interaction.data)) ) {
			error.message <- paste(
				'The following variables were not found in the data:', 
				paste(formula.vars[ !(formula.vars %in% names(interaction.data) ) ], collapse = ' ')
				);
			stop(error.message);
		}
	}

	trans.data <- interaction.data[ is.na(distance) ];

	# Fit models separately in each quantile of distance
	cis.data <- interaction.data[ !is.na(distance) ];
	cis.data <- cis.data[ order(distance) ];

	# free up memory
	rm(interaction.data);
	gc();

	# list of data.tables, where each element corresponds to 
	# a specific distance
	distance.binned.data <- distance.split(
		cis.data, 
		distance.bins = distance.bins, 
		verbose = verbose
		);

	# store interaction data after fitting models
	p.value.data <- list();

	# speed up model fitting by passing starting values and theta between iterations 
	# hopefully this will also help with stability ?
	init.theta <- NULL;
	start <- NULL;

	if( cores > 1 ) {
		computing.cluster <- parallel::makeCluster( cores );
		doParallel::registerDoParallel( computing.cluster );
	} else {
		foreach::registerDoSEQ();
	}

	iter.i <- NULL;
	p.value.data <- foreach::foreach(
		temp.data = distance.binned.data,
		iter.i = icount(),
		.packages = c('MASS', 'data.table', 'gamlss', 'gamlss.tr')
		) %dopar% {
		
		# progress meter
		if(verbose) cat('*');

		# fit model through helper function that gracefully handles numerical errors
		model <- model.try.catch(
			cis.formula, 
			temp.data,
			distribution = distribution,
			maxit = maxit,
			epsilon = epsilon,
			trace = trace,
			init.theta = init.theta,
			start = start
			);

		temp.data[, expected := model$expected.values ];
		temp.data[, p.value := model$p.values ];

		# clear memory
		for (gc.i in 1:5) { gc(); }

		# plot model's fit
		if (!is.null(interim.data.dir) && !is.null(model$model) && bait.to.bait == FALSE) {

			# store model fits to a file:
			sink(file = file.path(interim.data.dir, paste0('model_fit_distance_adjusted_nonb2b_', iter.i, '.txt')), type = c('output', 'message'));
			print(summary(model$model));
			print(logLik(model$model));
			sink(NULL)
			if (distribution %in% c('negative-binomial', 'poisson')) {
				create.modelfit.plot(
					model$model, 
					file.name = file.path(interim.data.dir, paste0('model_fit_distance_adjusted_nonb2b_', iter.i, '.png'))
					);
				}
			else {
				if (verbose) cat('\nskipping model fit rootogram as countreg::rootogram does not support: ', distribution);
				}
			}

		# clear memory
		for (gc.i in 1:5) { gc(); }

		return(temp.data);
	}

	# fit trans-interactions
	# (same model, but no distance correction)
	if( nrow(trans.data) > 0  ) {

		if(verbose) {
			cat('\n\ttrans interactions\n');
		}

		trans.model <- model.try.catch(
			trans.formula, 
			trans.data,
			distribution = distribution,
			maxit = maxit,
			epsilon = epsilon,
			trace = trace,
			init.theta = init.theta,
			start = start
			);

		trans.data[, expected := trans.model$expected.values ];
		trans.data[, p.value := trans.model$p.values ];

		# add to p-value data frame
		p.value.data[[ length(p.value.data) + 1 ]] <- trans.data; 

	}

	# clean up parallel computing
	if ( cores > 1 ) {
		foreach::registerDoSEQ();
		parallel::stopCluster(computing.cluster);
		remove(computing.cluster);
	}

	p.value.data <- do.call(rbind, p.value.data);

	return(p.value.data);

}
