## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = TRUE,
  eval = TRUE
  );


## ---- warning = FALSE---------------------------------------------------------
library(chicane);

# example BAM file, baits, and restriction fragments
bam <- system.file('extdata', 'Bre80_2q35.bam', package = 'chicane');
baits <- system.file('extdata', '2q35.bed', package = 'chicane');
fragments <- system.file('extdata', 'GRCh38_HindIII_chr2.bed.gz', package = 'chicane'); # HindIII fragments on chromosome 2

if( bedtools.installed() ) {
  chicane.results <- chicane(
    bam = bam,
    baits = baits,
    fragments = fragments
    );

    print( chicane.results[ 1:10 ] );
}


## ---- warning = FALSE---------------------------------------------------------
if( bedtools.installed() ) {
  interaction.data <- prepare.data(
    bam = bam,
    baits = baits,
    fragments = fragments
    );
}

## -----------------------------------------------------------------------------
if( bedtools.installed() ) print(interaction.data);

## ---- warning = FALSE, run = FALSE--------------------------------------------
if( bedtools.installed() ) {
  file.name <- tempfile('interaction_data');
  write.table(interaction.data, file.name, row.names = FALSE);
  chicane.results <- chicane(interactions = file.name); 
}

## -----------------------------------------------------------------------------
data(bre80);
adjusted.results <- chicane(
  interactions = bre80, 
  adjustment.terms = 'target.chr'
  );

print( adjusted.results[ 1:5 ] );

## ---- warning = FALSE, run = FALSE, eval = FALSE------------------------------
#  if( bedtools.installed() ) {
#    create.locus.plot(
#      genome = 'hg38',
#      chr = 'chr2', start = 111100000, end = 112200000,
#      gene.data = '/path/to/gene_data.gtf',
#      genomic.features = '/path/to/baitmap.bed',
#      feature.name = 'baits',
#      interaction.data = '/path/to/interaction_calls.txt',
#      file.name = '/path/to/locus_plot.pdf'
#      );
#  }

## ---- warning = FALSE, run = FALSE, eval = FALSE------------------------------
#  if( bedtools.installed() ) {
#  
#    # initial prep of interactions data table
#    interaction.data <- prepare.data(
#      bam = list.files('data/bams/T47D', pattern = '.bam', full.names = TRUE),
#      baits = 'data/captured_fragments/T47D/Baxter_captured_fragments.bed',
#      fragments = 'data/GRCh38_HindIII_fragments.bed'
#      );
#  
#    # save interactions data table
#    file.name <- tempfile('interaction_data');
#    write.table(interaction.data, file.name, row.names = FALSE);
#  
#    # run interaction calling and save model fit plots and statistics
#    chicane.results <- chicane(
#      interactions = file.name,
#      interim.data.dir = 'graphs/T47D/model_fits'
#      );
#  
#    # save full interaction calls table
#    write.table(
#      chicane.results,
#      file = 'data/chicane/T47D/interaction_calls.txt',
#      row.names = FALSE,
#      quote = FALSE,
#      sep = '\t'
#      );
#  
#    # filter for significant only and save these results separately
#    significant.results <- chicane.results[q.value < 0.05]
#  
#    write.table(
#      significant.results,
#      file = 'data/chicane/T47D/interaction_calls_significant.txt',
#      row.names = FALSE,
#      quote = FALSE,
#      sep = '\t'
#      );
#  
#  }

## ---- warning = FALSE, run = FALSE, eval = FALSE------------------------------
#  if( bedtools.installed() ) {
#  
#    # calculate proportions by interaction type
#    total <- nrow(significant.results);
#    trans.prop <- sum(is.na(significant.results$distance))/total;
#    cis.prop <- sum(!is.na(significant.results$distance))/total;
#    b2b.prop <- sum(significant.results$bait.to.bait)/total;
#    int.data <- c('trans' = trans.prop, 'cis' = cis.prop, 'bait.to.bait' = b2b.prop);
#    print(int.data);
#  
#    # get number of interactions by distance bins
#    binned.data <- NULL;
#  
#    distance.bins <- list(
#      "0-10kbp" = c(0, 1e4),
#      "10-100kbp" = c(1e4, 1e5),
#      "100kbp-1Mbp" = c(1e5, 1e6),
#      "1-10Mbp" = c(1e6, 1e7),
#      "10-100Mbp" = c(1e7, 1e8),
#      "100-1000Mbp" = c(1e8, 1e9)
#      );
#  
#    for(dist.i in names(distance.bins)){
#      bin.sum <- length(which(significant.results$distance >= distance.bins[[dist.i]][1] & significant.results$distance < distance.bins[[dist.i]][2]));
#      binned.data <- cbind(binned.data, bin.sum);
#      }
#  
#    colnames(binned.data) <- names(distance.bins);
#    print(binned.data);
#  
#  }

## ---- warning = FALSE, run = FALSE, eval = FALSE------------------------------
#  if( bedtools.installed() ) {
#  
#    # make a browser compatible file from significant interactions
#    browser.file <- tempfile('significant_calls_standard.txt')
#    convert.standard.format(
#      chicane.results = 'data/chicane/T47D/interaction_calls_significant.txt',
#      file.name = browser.file
#      );
#  
#  }

## ---- warning = FALSE, run = FALSE, eval = FALSE------------------------------
#  if( bedtools.installed() ) {
#  
#    # set genome annotation file
#    gene.ann <- system.file('extdata', 'gencode_2q35.gtf', package = 'chicane');
#  
#    # generate 2q35 locus plot
#    create.locus.plot(
#      genome = 'hg38',
#      chr = 'chr2',
#      start = 216600000,
#      end = 217140000,
#      gene.data = gene.ann,
#      genomic.features = 'data/captured_fragments/T47D/Baxter_captured_fragments.bed',
#      feature.name = 'baits',
#      interaction.data = 'data/chicane/T47D/interaction_calls.txt',
#      file.name = 'graphs/T47D/2q35.pdf'
#      );
#  
#  }

## -----------------------------------------------------------------------------
sessionInfo();

