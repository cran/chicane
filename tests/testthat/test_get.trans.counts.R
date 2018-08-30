context('get.trans.counts');

test_that('Input meets minimum requirements', {

	# data table only as input
	expect_error( get.trans.counts(2) );
	expect_error( get.trans.counts( 'hello' ) );
	expect_error( get.trans.counts( data.frame() ) );

	# required columns
	expect_error( get.trans.counts( data.table('A' = 2 )) );

	# count should be numeric
	test.data <- data.table(
		bait.chr = 'chr1',
		target.chr = 'chr2',
		bait.id = 'A',
		target.id = 'B',
		count = 'error'
		);

	expect_error( get.trans.counts(test.data) );

	});

test_that('Empty data returns empty data table', {

	# empty data
	test.data <- data.table(
		bait.chr = character(),
		target.chr = character(),
		bait.id = character(),
		target.id = character(),
		count = numeric()
		);

	test.solution <- data.table(
		fragment.id = character(),
		trans.count = numeric()
		);

	expect_equal( get.trans.counts(test.data), test.solution);

	});

test_that('A single interaction works as expected', {

	test.data <- data.table(
		bait.id = 'A',
		target.id = 'B',
		bait.chr = 'chr2',
		target.chr = 'chr3',
		count = 1
		);

	test.solution <- data.frame(
		fragment.id = c('A', 'B'),
		trans.count = c(1, 1),
		stringsAsFactors = FALSE
		);

	expect_equal(
		as.data.frame( get.trans.counts(test.data) ),
		test.solution
		);

	test.data$count <- 100;
	test.solution$trans.count <- 100;

	expect_equal(
		as.data.frame( get.trans.counts(test.data) ),
		test.solution
		);

	});


test_that('Two entries works as expected', {

	test.data <- data.table(
		bait.id = c('A', 'A'),
		target.id = c('B', 'C'),
		bait.chr = c('chr2', 'chr2'),
		target.chr = c('chr3', 'chr4'),
		count = c(1, 2)
		);

	test.solution <- data.frame(
		fragment.id = c('A', 'B', 'C'),
		trans.count = c(3, 1, 2),
		stringsAsFactors = FALSE
		);

	expect_equal(
		as.data.frame( get.trans.counts(test.data) ),
		test.solution
		);

	# include a fragment without trans interactions
	test.data <- data.table(
		bait.id = c('A', 'A'),
		target.id = c('B', 'C'),
		bait.chr = c('chr2', 'chr2'),
		target.chr = c('chr3', 'chr2'),
		count = c(1, 2)
		);

	test.solution <- data.frame(
		fragment.id = c('A', 'B', 'C'),
		trans.count = c(1, 1, 0),
		stringsAsFactors = FALSE
		);

	expect_equal(
		as.data.frame( get.trans.counts(test.data) ),
		test.solution
		);

	});

test_that('Three entries works as expected', {

	test.data <- data.table(
		bait.id = c('A', 'A', 'B'),
		target.id = c('B', 'C', 'D'),
		bait.chr = c('chr2', 'chr2', 'chr4'),
		target.chr = c('chr4', 'chr2', 'chr5'),
		count = c(1, 4, 1)
		);

	test.solution <- data.frame(
		fragment.id = c('A', 'B', 'C', 'D'),
		trans.count = c(1, 2, 0, 1),
		stringsAsFactors = FALSE
		);

	expect_equal(
		as.data.frame( get.trans.counts(test.data) ),
		test.solution
		);

	});




