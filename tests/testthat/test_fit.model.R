context('fit.model');

test_that('Parameterized distributions work', {
   
    #data(bre80);
    #bre80 <- bre80[1:(nrow(bre80)/10)];
    #for( distribution in c('poisson', 'truncated-poisson', 'truncated-negative-binomial') ) {
        # might get warnings due to these distributions being wonky
        # test for no errors instead
        
    #    expect_error(
    #        fit.model(
    #           bre80,
    #            distribution = distribution
    #            ),
    #        NA
    #        );
    #}
});


test_that('Can specify extra terms', {
   
    #data(bre80);
    #bre80 <- bre80[1:(nrow(bre80)/3)];
    #expect_warning(
    #   fit.model(
    #    	bre80, 
    #    	adjustment.terms = 'target.trans.count'
    #    	),
    #    NA
    #    );

    # get an error if term doesn't exist
    #expect_error(
    #	fit.model(
    #		bre80, 
    #		adjustment.terms = 'DOESNOTEXIST'
    #		)
    #	);
});