context('prepare.data');

# Can only run these tests if bedtools is available
skip.if.no.bedtools <- function() {

	if( !bedtools.installed() ) {
		skip('bedtools not available');
	}
}

test_that('Runs without warnings', {

	skip.if.no.bedtools();

	# tiny BAM file for testing read counts
	bam <- system.file('extdata', 'read_count_test_cis.bam', package = 'chicane');
	baits <- system.file('extdata', '2q35.bed', package = 'chicane');
    fragments <- system.file('extdata', 'GRCh38_HindIII_chr2.bed.gz', package = 'chicane');


	# store temp files in unique directory
	temp.directory <- tempfile();
	dir.create(temp.directory);

	expect_warning( 
		prepare.data(
			bam, 
			baits, 
			fragments,
			temp.directory = temp.directory
			),
		NA
		);

	});


test_that('Output is what we expect', {

	skip.if.no.bedtools();

    # store temp files in unique directory
    temp.directory <- tempfile();
    dir.create(temp.directory);
    
	# tiny BAM file for testing read counts
	bam <- system.file('extdata', 'read_count_test_cis.bam', package = 'chicane');
	baits <- system.file('extdata', '2q35.bed', package = 'chicane');
	fragments <- system.file('extdata', 'GRCh38_HindIII_chr2.bed.gz', package = 'chicane');

	test.data <- prepare.data(
		bam, 
		baits, 
		fragments,
		temp.directory = temp.directory
		);

	# hard-coded values based on inspecting BAM file
	expect_equal( 
		test.data$count,
		rep(1, 3)
		);

	expect_equal( 
		test.data$bait.to.bait,
		rep(FALSE, 3)
		);

	expect_equal( 
		test.data$bait.trans.count,
		rep(0, 3)
		);

	expect_equal( 
		test.data$target.trans.count,
		rep(0, 3)
		);

	# check bait ID and target ID work
	expect_equal( 
		test.data$bait.id,
		paste0(test.data$bait.chr, ':', test.data$bait.start, '-', test.data$bait.end)
		);

	expect_equal( 
		test.data$target.id,
		paste0(test.data$target.chr, ':', test.data$target.start, '-', test.data$target.end)
		);

	});

test_that('Can fill in zeros', {

    skip.if.no.bedtools();
    
    # store temp files in unique directory
    temp.directory <- tempfile();
    dir.create(temp.directory);
    
    bam <- system.file('extdata', 'read_count_test.bam', package = 'chicane');
    baits <- system.file('extdata', '2q35.bed', package = 'chicane');
    
    # use limited set of fragments to speed things up
    fragments <- c(
        'chr14:100227468-100228720',
        'chr14:104340446-104341196',
        'chr14:106284868-106286345',
        'chr15:100180015-100183303',
        'chr15:100180015-100183303',
        'chr15:101298105-101298372',
        'chr18:44605537-44609142',
        'chr18:45418602-45422767',
        'chr18:52015540-52016129',
        'chr18:74763083-74763343',
        'chr2:217430817-217437011',
        'chr2:217035649-217042355', # other bait from baits bedfile
        'chr2:0-100' # dummy fragment to illustrate zero count concept
        );
    
    fragments.data <- data.frame(
        chr = gsub('(.*):(.*)-(.*)', '\\1', fragments),
        start = as.numeric(  gsub('(.*):(.*)-(.*)', '\\2', fragments) ),
        end = as.numeric(  gsub('(.*):(.*)-(.*)', '\\3', fragments) )
        );
        
    fragments.file <- tempfile(fileext = '.bed')
    write.table(
        fragments.data, 
        fragments.file,
        sep = '\t',
        quote = FALSE,
        row.names = FALSE,
        col.names = FALSE
        );
    
    # cis
    expect_warning(
        prepare.data(
            bam,
            baits,
            fragments = fragments.file,
            temp.directory = temp.directory,
            include.zeros = 'cis'
            ),
        NA
        );
    
    # trans - need new temp directory to avoid errors
    temp.directory <- tempfile();
    dir.create(temp.directory);

    expect_warning(
        prepare.data(
            bam,
            baits,
            fragments = fragments.file,
            temp.directory = temp.directory,
            include.zeros = 'all'
        ),
        NA
        );
    
    });

test_that('Can combine replicates', {
    
    skip.if.no.bedtools();
    
    # tiny BAM file for testing read counts
    bam <- system.file('extdata', 'read_count_test_cis.bam', package = 'chicane');
    baits <- system.file('extdata', '2q35.bed', package = 'chicane');
    fragments <- system.file('extdata', 'GRCh38_HindIII_chr2.bed.gz', package = 'chicane');

    # store temp files in unique directory
    temp.directory <- tempfile();
    dir.create(temp.directory);
    
    # test weighted sum here, naive sum below
    expect_warning( 
        prepare.data(
            list(bam, bam), 
            baits, 
            fragments,
            temp.directory = temp.directory,
            replicate.merging.method = 'weighted-sum'
            ),
        NA
        );

    # output is what we expect
    output <- prepare.data(
        list(bam, bam), 
        baits, 
        fragments,
        temp.directory = temp.directory,
        replicate.merging.method = 'sum'
        );
    
    expect_equal(
        output$count,
        output$count.1 + output$count.2
        );
        
});



