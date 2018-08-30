context('read.bed');

baits <- system.file('extdata', '2q35.bed', package = 'chicane');

test_that('Throws error on bad input', {
    
    # file doesn't exist
    expect_error( read.bed( tempfile() ) );
    
    # not a single character string
    expect_error( read.bed( as.factor(baits) ) );
    expect_error( read.bed( c(baits, baits) ) );
    
});

test_that('Zero-based works as expected', {
    
    zero.based.ids <- read.bed(baits);
    one.based.ids <- read.bed(baits, zero.based = FALSE);
    
    zero.based.starts <- as.numeric(
        gsub('(.*):(.*)-(.*)', '\\2', zero.based.ids)
        );
    one.based.starts <- as.numeric(
        gsub('(.*):(.*)-(.*)', '\\2', one.based.ids)
        );
    
    expect_equal(zero.based.starts + 1, one.based.starts);
});

test_that('Can read gzipped files', {
    
    fragments <- system.file('extdata', 'GRCh38_HindIII_chr2.bed.gz', package = 'chicane');     
    
    expect_silent( read.bed(fragments) );
    });

