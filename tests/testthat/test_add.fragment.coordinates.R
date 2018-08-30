context('add.fragment.coordinates');

test_that('Throws error on bad input', {

	# not data frame input
	expect_error( add.fragment.coordinates( 'hello' ) );
	expect_error( add.fragment.coordinates( 5 ) );
	expect_error( add.fragment.coordinates( list() ) );

});


test_that('Returns same object if no columns to expand', {

	input <- data.table(
		x = rnorm(10),
		y = rnorm(10)
		);

	expect_equal(
		add.fragment.coordinates(input),
		input
		);

	# empty data frame works
	expect_equal(
		add.fragment.coordinates( data.table() ),
		data.table()
		);

});


test_that('Expands bait.id correctly', {

	input <- data.table(
		x = rnorm(2),
		bait.id = c('chr1:0-100', 'chr10:100-200')
		);

	expected.output <- cbind(
		input,	
		data.table(
			bait.chr = c('chr1', 'chr10'),
			bait.start = c(0, 100),
			bait.end = c(100, 200)
			)
		);	

	expect_equal(
		add.fragment.coordinates(input),
		expected.output
		);

});



test_that('Expands target.id correctly', {

	input <- data.table(
		x = rnorm(2),
		target.id = c('HPV-2:10-110', '10:100-200')
		);

	expected.output <- cbind(
		input,	
		data.table(
			target.chr = c('HPV-2', '10'),
			target.start = c(10, 100),
			target.end = c(110, 200)
			)
		);	

	expect_equal(
		add.fragment.coordinates(input),
		expected.output
		);
});

test_that('Expands both bait.id and target.id correctly', {

	input <- data.table(
		x = rnorm(2),
		bait.id = c('chr1:0-100', 'chr10:100-200'),
		target.id = c('HPV-2:10-110', 'chr7:100-200')
		);

	expected.output <- cbind(
		input,	
		data.table(
			target.chr = c('HPV-2', 'chr7'),
			target.start = c(10, 100),
			target.end = c(110, 200),
			bait.chr = c('chr1', 'chr10'),
			bait.start = c(0, 100),
			bait.end = c(100, 200)
			)
		);	

	expect_equal(
		add.fragment.coordinates(input),
		expected.output
		);
});

