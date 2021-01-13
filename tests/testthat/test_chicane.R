context('chicane');

test_that('Runs without warnings', {

	data(bre80);
	bre80 <- bre80[1:(nrow(bre80)/3)];
	expect_warning( 
		chicane( interactions = bre80 ),
		NA
		);
	});

test_that('Integrated processing and model fitting works', {

	#if( !bedtools.installed() ) {
	#	skip('bedtools not available');
	#}

	# tiny BAM file for testing read counts
	#bam <- system.file('extdata', 'Bre80_2q35.bam', package = 'chicane');
	#baits <- system.file('extdata', '2q35.bed', package = 'chicane');
	#fragments <- system.file('extdata', 'GRCh38_HindIII_chr2.bed.gz', package = 'chicane');

	#expect_warning( 
	#	chicane(
	#		bam, 
	#		baits, 
	#		fragments,
	#		temp.directory = tempdir()
	#		),
	#	NA
	#	);
	
	# can fill in zeros 
	# this might result in fitted rates of zero warnings, which is correct
	# thus, run expect_no_error instead
	#bam <- system.file('extdata', 'read_count_test_cis.bam', package = 'chicane');
	
	#temp.directory <- tempfile();
	#dir.create(temp.directory);

	#expect_error( 
	#    chicane(
	#        bam, 
	#        baits, 
	#        fragments,
	#        temp.directory = temp.directory,
	#        include.zeros = 'cis'
	#        ),
	#    NA
	#    );

	});


test_that('Input is valid', {

	data(bre80);

	bam <- system.file('extdata', 'Bre80_2q35.bam', package = 'chicane');
	baits <- system.file('extdata', '2q35.bed', package = 'chicane');
	fragments <- system.file('extdata', 'GRCh38_HindIII_chr2.bed.gz', package = 'chicane');

	# need to provide all three bam/ baits/ fragments
	expect_error( chicane(bam = bam, baits = baits) );
	expect_error( 
		chicane(
			baits = baits, 
			fragments = fragments
			)
		);
	expect_error( 
		chicane(
			bam = bam, 
			fragments = fragments
			)
		);
	expect_error( 
		chicane(
			bam = bam, 
			baits = baits
			)
		);
	
	# baits/fragments need to be single character strings
	expect_error( 
	    chicane(
	        bam = bam, 
	        baits = c(baits, baits),
	        fragments = fragments
	        )
    	);
	
	# files need to exist
	expect_error( 
	    chicane(
	        bam = bam, 
	        baits = tempfile(), 
	        fragments = fragments
	        )
	    );
	
	expect_error( 
	    chicane(
	        bam = tempfile(), 
	        baits = baits, 
	        fragments = fragments
	        )
	    );
	
	expect_error( 
	    chicane(
	        bam = bam, 
	        baits = baits, 
	        fragments = tempfile()
	        )
	    );
	
	# can't ask to expand zeros if interactions are provided
	expect_error( 
		chicane(
			interactions = bre80,
			include.zeros = 'cis'
			)
		);

	});
