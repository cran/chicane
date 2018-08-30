context('add.covariates');

test_that('Throws error on bad input', {

	# correctly formatted, but not a data table
	input.data.frame <- data.frame(
		bait.chr = 'chr5',
		bait.start = 0,
		bait.end = 10,
		bait.id = 'chr5:0-10',
		target.chr = 'chr2',
		target.start = 100,
		target.end = 110,
		target.id = 'chr2:100-110',
		count = 10,
		stringsAsFactors = FALSE
		);

	# not data table input
	expect_error( add.covariates( 'hello' ) );
	expect_error( add.covariates( 5 ) );
	expect_error( add.covariates( input.data.frame ) );


	# missing column
	input.data.table <- as.data.table(input.data.frame);

	for( i in seq_along(input.data.table) ) {
		expect_error( 
			add.covariates( input.data.table[, -i, with = FALSE] ) 
			);
	}

	# sanity check that full example works
	expect_silent( add.covariates(input.data.table) );
});


test_that('Countable examples work as expected', {

	# single row – trans
	input <- data.table(
		target.id = 'A',
		bait.id = 'B',
		bait.chr = 'chr5',
		bait.start = 0,
		bait.end = 10,
		target.chr = 'chr2',
		target.start = 100,
		target.end = 110,
		count = 10
		);

	expected.output <- cbind(
		input,
		data.table(
			bait.trans.count = 10,
			target.trans.count = 10,
			distance = NA
			)
		);

	setkey(expected.output, 'target.id');

	expect_equal(
		add.covariates(input),
		expected.output
		);

	# single row – cis
	input <- data.table(
		target.id = 'C',
		bait.id = 'D',
		bait.chr = 'chr10',
		bait.start = 5,
		bait.end = 15,
		target.chr = 'chr10',
		target.start = 105,
		target.end = 115,
		count = 1
		);

	expected.output <- cbind(
		input,
		data.table(
			bait.trans.count = 0,
			target.trans.count = 0,
			distance = 100
			)
		);
	setkey(expected.output, 'target.id');

	expect_equal(
		add.covariates(input),
		expected.output
		);


	# two rows without overlap
	input <- data.table(
		target.id = c('A', 'C'),
		bait.id = c('B', 'D'),
		bait.chr = c('chr5', 'chr10'),
		bait.start = c(0, 5),
		bait.end = c(10, 15),
		target.chr = c('chr2', 'chr10'),
		target.start = c(100, 105),
		target.end = c(110, 115),
		count = c(10, 1)
		);

	expected.output <- cbind(
		input,
		data.table(
			bait.trans.count = c(10, 0),
			target.trans.count = c(10, 0),
			distance = c(NA, 100)
			)
		);
	setkey(expected.output, 'target.id');

	expect_equal(
		add.covariates(input),
		expected.output
		);

	# two rows with overlap
	input <- data.table(
		target.id = c('A', 'B'),
		bait.id = c('B', 'D'),
		bait.chr = c('chr5', 'chr5'),
		bait.start = c(0, 5),
		bait.end = c(10, 15),
		target.chr = c('chr2', 'chr5'),
		target.start = c(100, 0),
		target.end = c(110, 10),
		count = c(10, 1)
		);

	expected.output <- cbind(
		input,
		data.table(
			bait.trans.count = c(10, 0),
			target.trans.count = c(10, 10),
			distance = c(NA, 5)
			)
		);

	setkey(expected.output, 'target.id');

	expect_equal(
		add.covariates(input),
		expected.output
		);

});