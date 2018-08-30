library(testthat);
library(data.table);

context('verify.interacation.data');

test_that('must be data table object', {

	expect_error( verify.interacation.data(NULL) );


	test.data <- data.frame(
		A = rnorm(100), 
		B = rpois(100, lambda = 20)
		);

	expect_error( verify.interacation.data(test.data) );

	expect_error( verify.interacation.data( 'test' ) );

	});

test_that('must contain required columns', {

	test.data <- data.table(
		count = rnorm(100), 
		bait.trans.count = rpois(100, lambda = 20),
		target.trans.count = rpois(100, lambda = 20)
		);

	expect_error( verify.interaction.data(test.data) );

	test.data[, distance := bait.trans.count];
	test.data[, bait.trans.count := NULL];

	expect_error( verify.interaction.data(test.data) );

	test.data[, bait.trans.count := target.trans.count];
	test.data[, target.trans.count := NULL];

	expect_error( verify.interaction.data(test.data) );


	test.data[, target.trans.count := count];
	test.data[, count := NULL];

	expect_error( verify.interaction.data(test.data) );

	});