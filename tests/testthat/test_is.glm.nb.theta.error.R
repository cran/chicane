context('is.glm.nb.theta.error');


test_that('Error is recognized', {

	set.seed(7);

	tryCatch(
		MASS::glm.nb(rep(1, 100) ~ 1), 
		error = function(e) {

			expect_equal(
				is.glm.nb.theta.error(e),
				TRUE
				);
		});

	});

test_that('Other errors are not recognized', {


	expect_equal(
		is.glm.nb.theta.error( simpleError('a different error') ),
		FALSE
		);

	tryCatch(
		not.a.variable, 
		error = function(e) {

			expect_equal(
				is.glm.nb.theta.error(e),
				FALSE
				);
		});

	});