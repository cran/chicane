context('get.id.components');

test_that('Single ID works as expected', {

	expect_equal( get.id.components('chr1:1-2'), c('chr1', '1', '2'));
	expect_equal( get.id.components('chr15:1545-23435'), c('chr15', '1545', '23435') );
	expect_equal( get.id.components('15:1545-23435'), c('15', '1545', '23435') );

	expect_equal(get.id.components('HPV-2:1-2'), c('HPV-2', '1', '2') );

	});

test_that('Multiple IDs work as expected', {

	expect_equal(
		get.id.components( c('chr1:1-2', 'chr15:1545-23435', '15:1545-23435') ), 
		list(
			c('chr1', '1', '2'),
			c('chr15', '1545', '23435'),
			c('15', '1545', '23435')
			)
		);

	});
