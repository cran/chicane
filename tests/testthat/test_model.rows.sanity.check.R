context('model.rows.sanity.check');

test_that('Works as it should', {
    
    data('bre80');
    
    # run a quick linear regression to prove the point
    mini.lm <- lm(
        count ~ bait.trans.count + target.trans.count, 
        bre80[1:1000]
        );
    
    expect_error( model.rows.sanity.check( bre80, mini.lm ) );
    expect_error( model.rows.sanity.check( bre80[1:999], mini.lm ) );
    expect_error( model.rows.sanity.check( bre80[1:1001], mini.lm ) );
    
    expect_warning( model.rows.sanity.check(bre80[1:1000], mini.lm), NA);
     
});
