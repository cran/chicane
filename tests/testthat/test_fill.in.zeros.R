context('fill.in.zeros');

test_that('Upper limit on number of baits/ fragments', {
    
    data(bre80);
    
    expect_error(
        fill.in.zeros(
            interaction.data = bre80,
            baits = rnorm(10^4),
            fragments = rnorm(10^6)
            )
        );
        
});

