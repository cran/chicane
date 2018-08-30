context('test.enrichment');

# Can only run these tests if bedtools is available
skip.if.no.bedtools <- function() {
    
    if( !bedtools.installed() ) {
        skip('bedtools not available');
    }
}

## PREPARE DATA FOR TESTING

ctcf.bed <- system.file('extdata', 'T47D_chr2_CTCF.bed.gz', package = 'chicane');

# use bre80 data so we won't need bedtools
data(bre80);
chicane.results <- chicane(interactions = bre80 );

## RUN TESTS

test_that('Input must be valid', {
    
    # interaction data must be a data table
    expect_error(
        test.enrichment(
            interaction.data = 'hello',
            feature.bed = ctcf.bed
            )
        ); 
    
    expect_error(
        test.enrichment(
            interaction.data = 5,
            feature.bed = ctcf.bed
            )
        ); 
    
    expect_error(
        test.enrichment(
            interaction.data = as.data.frame(chicane.results),
            feature.bed = ctcf.bed
            )
        ); 
    
    # feature BED must exist
    expect_error(
        test.enrichment(
            interaction.data = chicane.results,
            feature.bed = tempfile(fileext = '.bed')
            )
        );
    
    # feature BED must be a single file
    expect_error(
        test.enrichment(
            interaction.data = chicane.results,
            feature.bed = c(ctcf.bed, ctcf.bed)
            )
        );
    
    # need both significant and non-significant results
    for(q.value in c(0.0001, 1) ) {
        chicane.results$q.value <- q.value;
        expect_error(
            test.enrichment(
                interaction.data = chicane.results,
                feature.bed = ctcf.bed
                )
            );
    } # end of q-value loop
    
});


test_that('Runs without warnings', {
    
    skip.if.no.bedtools();

    # make sure we have significant results for testing
    chicane.results[ 1:100, q.value := 0.0005 ];
    
    # interaction data must be a data table
    expect_warning(
        test.enrichment(
            interaction.data = chicane.results,
            feature.bed = ctcf.bed
        ),
        NA
    ); 
    
});

test_that('Output format is as expected', {
    skip.if.no.bedtools();
    
    # make sure we have significant results for testing
    chicane.results[ 1:100, q.value := 0.0005 ];
    
    # interaction data must be a data table
    results <- test.enrichment(
        interaction.data = chicane.results,
        feature.bed = ctcf.bed
        ); 
    
    expect_true( is.data.table(results) );
    
});