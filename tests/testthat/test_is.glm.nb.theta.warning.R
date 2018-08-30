context('is.glm.nb.theta.warning');


test_that('Warning is recognized', {

	# Don't have an easy minimal example of warning through glm.nb – try direct sqrt instead
	
	i <- -10;

	tryCatch(
		sqrt(1/i), 
		warning = function(w) {

			expect_equal(
				is.glm.nb.theta.warning(w),
				TRUE
				);
		});

	});

test_that('Other warnings are not recognized', {


	expect_equal(
		is.glm.nb.theta.error( simpleWarning('a different warning') ),
		FALSE
		);

	tryCatch(
		log(-10), 
		warning = function(w) {

			expect_equal(
				is.glm.nb.theta.warning(w),
				FALSE
				);
		});

	});