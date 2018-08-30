context('get.combination.count');


test_that('Countable trans examples work as expected', {

	expect_equal(
		get.combination.count(
			baits = c('A', 'B'), 
			fragments = c('A', 'B', 'C')
			),
		3
		);

	expect_equal(
		get.combination.count(
			baits = c('A'), 
			fragments = c('A', 'B', 'C')
			),
		2
		);

	expect_equal(
		get.combination.count(
			baits = 'A', 
			fragments = 'A'
			),
		0
		);

	});

test_that('Countable cis examples work as expected', {

	expect_equal(
		get.combination.count(
			baits = c('chr1:A', 'chr2:A'), 
			fragments = c('chr1:A', 'chr2:A', 'chr2:B'),
			cis.only = TRUE
			),
		1
		);

	expect_equal(
		get.combination.count(
			baits = c('chr1:A', 'chr2:A'), 
			fragments = c('chr1:A', 'chr2:A', 'chr3:B'),
			cis.only = TRUE
			),
		0
		);


	expect_equal(
		get.combination.count(
			baits = c('chr1:A', 'chr2:A', 'chr3:A'), 
			fragments = c('chr1:A', 'chr2:A', 'chr3:A', 'chr3:B'),
			cis.only = TRUE
			),
		1
		);

	expect_equal(
		get.combination.count(
			baits = c('chr1:A', 'chr1:B', 'chr3:A'), 
			fragments = c('chr1:A', 'chr1:B', 'chr1:C', 'chr3:A', 'chr3:B'),
			cis.only = TRUE
			),
		4
		);



	});