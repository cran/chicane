library(testthat);

context('check.model.numerical.fit');

test_that('fails when count is constant', {

	set.seed(7);

	test.data <- data.table(
		count = rep(1, 4),
		bait.trans.count = 1:4,
		target.trans.count = 1:4,
		distance = rep(NA, 4)
		);

	expect_equal(
		check.model.numerical.fit(test.data),
		FALSE
		);


	test.data$count <- 10000;

	expect_equal(
		check.model.numerical.fit(test.data),
		FALSE
		);

	});

test_that('fails when count only has two distinct values perfectly predicted by a covariate', {

	set.seed(7);

	test.data <- data.table(
		count = c(1, 1, 2, 2),
		bait.trans.count = c(3, 3, 4, 4),
		target.trans.count = 1:4,
		distance = rep(NA, 4)
		);

	expect_equal(
		check.model.numerical.fit(test.data),
		FALSE
		);


	n <- 100;
	test.data <- data.table(
		count = c( rep(1, n/2), rep(2, n/2) ),
		bait.trans.count = rpois(n, lambda = 20),
		target.trans.count = c( rep(122, n/2), rep(155, n/2) ),
		distance = rep(NA, n)
		);

	expect_equal(
		check.model.numerical.fit(test.data),
		FALSE
		);


	});