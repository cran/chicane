#! /bin/bash

### DESCRIPTION ##############################################################
# Preprocess a HiCUP output BAM file before running chicane method

### FUNCTIONS #################################################################

usage() {
	echo "Preprocess BAM for running chicane";
	echo "Usage: $(basename $0) -b bam -c baits -f fragments [-d temp_directory] [-k]";
	echo " -b: Path to BAM file to be processed";
	echo " -c: Path to BED file containing baits";
	echo " -f: Path to BED file containing restriction enzyme fragments of entire genome";
	echo " -d: Directory where temporary files should be stored. Defaults to current directory";
	echo " -k: flag for keeping temporary files";
}


### COMMAND LINE ARGUMENTS ####################################################

while getopts 'b:c:f:d:k' OPTION; do
	case "$OPTION" in 
		# BAM file
		b)
			bam=$OPTARG;
			;;
		# Capture baits
		c) 
			baits=$OPTARG;
			;;
		# all restriction fragments in BED format
		f)
			fragments=$OPTARG;
			;;
		# output directory – optional argument
		d)
			temp_directory=$OPTARG;
			;;
		# flag for keeping intermediate files
		k)
			keep=1;
			;;
		# undefined option – print help
		?) 
			usage;
			exit 1;
	esac
done

if [ -z $bam ] || [ -z $baits ] || [ -z $fragments ]; then
	usage;
	exit 1;
fi

### MAIN ######################################################################


# Check that bedtools is installed. This is also done in R script, but do 
# it here too in case user executes script directly
bedtools_path=`command -v bedtools`;
if [ -z $bedtools_path ]; then
	echo "bedtools not found – please install it and try again";
	exit 1;
fi

# minimum overlap fraction – must be higher than 0.5 for code to work
f=0.501;

sample_name=`basename ${bam} | sed 's/\\.bam//'`;

# move to directory where temporary files should be stored
if [ $temp_directory ]; then
	cd $temp_directory;
fi

## MAKE SURE FILES WON'T BE OVERWRITTEN

if [ -f ${sample_name}_b2b.bedpe ]; then
	echo "${sample_name}_b2b.bedpe already exists";
	exit 1;
fi

if [ -f ${sample_name}_b2b_fragments.bedpe ]; then
	echo "${sample_name}_b2b_fragments.bedpe already exists";
	exit 1;
fi

if [ -f ${sample_name}_nonb2b_baits_right.bedpe ]; then
	echo "${sample_name}_nonb2b_baits_right.bedpe already exists";
	exit 1;
fi

if [ -f ${sample_name}_nonb2b_fragments.bedpe ]; then
	echo "${sample_name}_nonb2b_fragments.bedpe already exists";
	exit 1;
fi


# bait-to-bait interactions are entered twice
# treat them separately from the rest and merge files afterwards
# 

# remove duplicate entries of b2b 
# need over 50% overlap, so each read can overlap at most one -> each b2b read covers two lines
# swap order of fragments so we can use bedtools intersect to get other end
bedtools pairtobed -abam $bam \
-b $baits -bedpe -type both -f 0.51 | awk 'BEGIN{ OFS="\t"; } 
	NR % 2 == 1 {
		# skip ahead if no overlap
		if($14==0) {
			next;
		}
		
		print $4, $5, $6, $11, $12, $13;
 	}' > ${sample_name}_b2b.bedpe;

# not guaranteed to have both b2b and non-b2b interactions – include a test
# figure out how this performance compares to wc -l file | cut
b2b_count=`cat ${sample_name}_b2b.bedpe | wc -l`;


if [ $b2b_count -gt 0 ]; then
	# re-run bedtools intersect and remove exact coordinates
	bedtools intersect -a ${sample_name}_b2b.bedpe \
	-b $baits -wao -f $f | awk 'BEGIN{ OFS="\t"; }
		{
			print $4, $5, $6, $7, $8, $9;
		}' >  ${sample_name}_b2b_fragments.bedpe;
fi

## Non bait-to-bait interactions

# start by moving baits to the right. We can then use intersect to get fragment corresponding to other end
bedtools pairtobed -abam $bam \
-b $baits -bedpe -type xor -f $f | awk 'BEGIN{ OFS="\t"; }
	{
		# check if the first read is bait by evaluating whether it overlaps
		# for read to overlap bait, need to both end after the bait fragment has started AND 
		# start before the bait fragment has ended

		# BED files are zero-indexed, so do not consider "or equal" – strict inequalities only
		end_after_bait_start = $3>$12 ? 1 : 0;
		start_before_bait_end = $2<$13 ? 1 : 0;

		chromosomes_match = $1==$11;
		overlap = end_after_bait_start && start_before_bait_end;	

		if(chromosomes_match &&  overlap) {
			print $4, $5, $6, $1, $2, $3, $7, $8, $9, $10, $11, $12, $13;
		} else {
			print $0;
		}
	}' >  ${sample_name}_nonb2b_baits_right.bedpe;

nonb2b_count=`cat ${sample_name}_nonb2b_baits_right.bedpe | wc -l`;


# use bedtools intersect to obtain details of which restriction fragment the non-bait end corresponds to
if [ $nonb2b_count -gt 0 ]; then
	bedtools intersect -a ${sample_name}_nonb2b_baits_right.bedpe \
	-b $fragments -wao -f $f | awk 'BEGIN{ OFS="\t"; } 
		{
			# if no overlap, skip ahead
			if($17==0) {
				next;
			}

			# move baits back to the left
			print $11, $12, $13, $14, $15, $16;
		}' > ${sample_name}_nonb2b_fragments.bedpe;
fi

# reverse order if necessary and print
if [ $b2b_count -gt 0 ]; then
	awk 'BEGIN{ OFS="\t"; } 
		{
			chromosomes_match = $1==$4;
			starts_match = $2==$5;
			ends_match = $3==$6;

			same_fragment = chromosomes_match && starts_match && ends_match;

			if(same_fragment) {
				# skip over reads that map to the same fragment
				next;
			} else {
				# bait-to-bait

				# sort positions to make sure the same bait always ends up on the left
				# sort first by chromosome, then by start position
				if($1<$4) {
					# first chromosome smaller than second chromosome
					#	=> keep in current order
					print $1, $2, $3, $4, $5, $6, "TRUE";
				} else if($4<$1) {
					# second chromosome smaller than first chromosome
					#	=> reverse order
					print $4, $5, $6, $1, $2, $3, "TRUE";
				} else {
					# same chromosome, compare start position

					if($2<$5) {
						# first start position smaller than second one
						#  	=> keep in current order
						print $1, $2, $3, $4, $5, $6, "TRUE";
					} else {
						# second start position smaller than first one
						#  	=> reverse order
						print $4, $5, $6, $1, $2, $3, "TRUE";
					}
				}
			}
		}' ${sample_name}_b2b_fragments.bedpe;
fi

if [ $nonb2b_count -gt 0 ]; then
	awk 'BEGIN{ OFS="\t"; } { print $0, "FALSE"; }' ${sample_name}_nonb2b_fragments.bedpe;
fi

# clean up – delete files unless user wants to keep them
if [ -z $keep ]; then

	# throws an error if files didn't exist - use if statement
	# somehow this fails to delete _b2b.bedpe, so no if statement for that one
	rm ${sample_name}_b2b.bedpe 
	
	if [ -f ${sample_name}_b2b_fragments.bedpe ]; then
		rm ${sample_name}_b2b_fragments.bedpe;
	fi

	if [ $nonb2b_count -gt 0 ]; then
		rm ${sample_name}_nonb2b_baits_right.bedpe ${sample_name}_nonb2b_fragments.bedpe;
	fi

fi


