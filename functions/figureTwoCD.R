## figureTwoCD.R

## Erich S. Huang
## Sage Bionetworks
## erich.huang@sagebase.org

figureTwoCD <- function(){
  
  ## REQUIRE
  require(synapseClient)
  require(gtools)
  require(ggplot2)
  
  ## READ IN THE LEADERBOARD DATA
  cat('Loading the necessary objects from Synapse\n')
  fifteenOctLBEnt <- loadEntity('syn1744693')
  pre15OctLBEnt <- loadEntity('syn1745570')
  finalLBEnt <- loadEntity('syn1739275')
  
  oct15LB <- fifteenOctLBEnt$objects$object
  preOct15LB <- pre15OctLBEnt$objects$object
  finalLB <- finalLBEnt$objects$object
  
  rownames(finalLB) <- finalLB$SYNID
  
  ## ESTABLISH THE QUANTILES FOR THE PRE-15 OCT SCORES
  # first remove the NA rows
  sPreOct15LB <- preOct15LB[!is.na(preOct15LB$ConcordanceIndex), ]
  
  preOct15Quants <- quantcut(sPreOct15LB$ConcordanceIndex)
  names(preOct15Quants) <- sPreOct15LB[ , 1]
  
  ## GENERATE THE PLOT FOR PRE-15 OCT VERSUS OSLOVAL FINAL SCORES
  cat('Intersecting model IDs and plotting a scatterplot\n')
  intIds <- intersect(finalLB$SYNID, sPreOct15LB$SynapseID)
  preFinalDF <- data.frame('metabricPrelim' = sPreOct15LB[intIds, 'ConcordanceIndex'],
                       'osloVal' = finalLB[intIds, 'CCI'],
                       'preOct15Quantile' = as.factor(as.numeric(preOct15Quants[intIds])))
  
  preFinalPearson <- cor.test(preFinalDF[ , 1], preFinalDF[ , 2], method = 'pearson')
  preFinalSpearman <- cor.test(preFinalDF[ , 1], preFinalDF[ , 2], method = 'spearman')
  
  preFinalPlot <- ggplot(preFinalDF, aes(metabricPrelim, osloVal)) + 
    scale_size(range = c(1, 7)) +
    geom_point(aes(colour = preOct15Quantile), size = 5) +
    ggtitle('Scatterplot of Final OsloVal vs. Pre-Oct 15th Model Scores\n') +
    xlab('\nMETABRIC (pre-October 15th) Concordance Indices') +
    ylab('OsloVal Final Concordance Indices\n') +
    xlim(0.5, 1) +
    ylim(0.5, 1) +
    annotate('text', x = 0.57, y = 1, 
             label = paste('Pearson Corr = ', round(preFinalPearson$estimate, 2), sep = '')) +
    annotate('text', x = 0.575, y = 0.98, 
             label = paste('Spearman Corr = ', round(preFinalSpearman$estimate, 2), sep = '')) +
    geom_abline(slope = 1, colour = 'red', linetype = 'dotted')
  
  ## GENERATE THE PLOT FOR THE PRE-15 OCT VERSUS THE 15 OCT SCORES
  rownames(oct15LB) <- oct15LB$SynID
  
  oct15Scores <- oct15LB$Final.Test.Score
  names(oct15Scores) <- oct15LB$SynID
  
  octInt <- intersect(oct15LB$SynID, sPreOct15LB$SynapseID)
  
  oct15Scores <- oct15Scores[octInt]
  preOctScores <- sPreOct15LB$ConcordanceIndex
  names(preOctScores) <- rownames(sPreOct15LB)
  preOctScores <- preOctScores[octInt]
  preOctQuants <- as.factor(as.numeric(preOct15Quants[octInt]))
  
  octDF <- data.frame('preOct' = preOctScores, 'oct15' = oct15Scores, 'quants' = preOctQuants)
  
  octoberPearson <- cor.test(octDF[ , 1], octDF[ , 2], method = 'pearson')
  octoberSpearman <- cor.test(octDF[ , 1], octDF[ , 2], method = 'spearman')
  
  preOctPlot <- ggplot(octDF, aes(preOct, oct15)) + geom_point(aes(colour = quants), size = 5) +
    ggtitle('Scatterplot of Oct 15th vs. Pre-Oct 15th Model Scores\n') +
    xlab('\nMETABRIC (pre-October 15th Concordance Indices') +
    ylab('METABRIC October 15th Concordance Indices\n') +
    xlim(0.5, 1) +
    ylim(0.5, 1) +
    annotate('text', x = 0.57, y = 1, 
             label = paste('Pearson Corr = ', round(octoberPearson$estimate, 2), sep = '')) +
    annotate('text', x = 0.575, y = 0.98, 
             label = paste('Spearman Corr = ', round(octoberSpearman$estimate, 2), sep = '')) +
    geom_abline(slope = 1, colour = 'red', linetype = 'dotted')
  
  show(preFinalPlot)
  show(preOctPlot)
  
  returnList <- list('scatterPreOctober' = preOctPlot,
                     'scatterPreFinal' = preFinalPlot)
  
  return(returnList)
}
