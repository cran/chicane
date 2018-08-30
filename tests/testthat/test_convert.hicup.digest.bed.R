context('convert.hicup.digest.bed');

test_that('Simple example works', {

	input.string <- paste(
		'Genome:GRCh37_Ensembl_75	Restriction_Enzyme1:HindIII [A^AGCTT]	Restriction_Enzyme2:None	Hicup digester version 0.5.9',
		"Chromosome	Fragment_Start_Position	Fragment_End_Position	Fragment_Number	RE1_Fragment_Number	5'_Restriction_Site	3'_Restriction_Site",
		'1	1	16007	1	1	None	Re1',
		'1	16008	24571	2	2	Re1	Re1',
		'1	24572	27981	3	3	Re1	Re1',
		sep = '\n'
		);

	expected.output <- paste(
		'1	0	16007',
		'1	16007	24571',
		'1	24571	27981',
		sep = '\n'
		);

	input.tempfile <- tempfile(fileext = '.txt');

	write(
		input.string,
		input.tempfile
		);

	# output matches
	expect_output(
		convert.hicup.digest.bed(input.tempfile), 
		expected.output
		);

	});

test_that('Throws error if format is weird', {

	input.strings <- list(
		# missing first line
		paste(
			"Chromosome	Fragment_Start_Position	Fragment_End_Position	Fragment_Number	RE1_Fragment_Number	5'_Restriction_Site	3'_Restriction_Site",
			'1	1	16007	1	1	None	Re1',
			'1	16008	24571	2	2	Re1	Re1',
			sep = '\n'
			),
		# header mismatch
		paste(
			'Genome:GRCh37_Ensembl_75	Restriction_Enzyme1:HindIII [A^AGCTT]	Restriction_Enzyme2:None	Hicup digester version 0.5.9',
			"V1	Fragment_Start_Position	Fragment_End_Position	Fragment_Number	RE1_Fragment_Number	5'_Restriction_Site	3'_Restriction_Site",
			'1	1	16007	1	1	None	Re1',
			sep = '\n'
			)
		);

	for( input.string in input.strings ) {

		input.tempfile <- tempfile(fileext = '.txt');
		write(
			input.string,
			input.tempfile
			);

		# expect an error
		expect_error( convert.hicup.digest.bed(input.tempfile) );
	}

	});