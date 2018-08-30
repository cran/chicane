context('smart.split');

test.data <- data.frame(
	x = c( rep('A', 5), rep('B', 5) ),
	y = rnorm(10)
	);

test_that('Input must be data frame and positive integer', {

	expect_error( smart.split(dat = test.data, bins = 0) );
	expect_error( smart.split(dat = test.data, bins = 423.3244242) );
	expect_error( smart.split(dat = 'hello', bins = 4) );

});


test_that('Basic cases run', {

	expect_equal( 
		smart.split(dat = test.data, bins = 1),
		list( test.data )
		);

	expect_equal({ 
		# need to remove names of list to get test case to work
		temp <- smart.split(dat = test.data, bins = 2)
		names(temp) <- NULL;

		return(temp); 
		},
		list( test.data[1:5, ], test.data[6:10, ] )
		);
});