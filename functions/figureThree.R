## figureeThree.R

## Erich S. Huang
## Sage Bionetworks
## erich.huang@sagebase.org

figureThree <- function(pseudoRndSeed = NULL){
  
  ## REQUIRE
  require(synapseClient)
  require(ggplot2)
  require(reshape)
  require(doMC)
  require(survcomp)
  
  registerDoMC()
  
  ## LOAD NECESSARY OBJECTS
  cat('Loading necessary Synapse entities\n')
  osloEnt <- loadEntity('syn1725898') # matrix of predictions 
  osloScores <- osloEnt$objects$osloPredictions
  
  osloSurvEnt <- loadEntity('syn1710257') # survival object
  osloSurv <- osloSurvEnt$objects$oslovalSurvData
  rownames(osloSurv) <- rownames(osloScores)
  
  orderedIDs <- colnames(osloScores)
  
  ## FOR USING THE MCLAPPLY FUNCTION, CONVERT TO LIST
  osloPredList <- mclapply(seq_len(ncol(osloScores)), function(i){osloScores[ , i]})
  names(osloPredList) <- colnames(osloScores)
  
  ## SAMPLE 80% OF THE PREDICTIONS & CALCULATE SCORES 100 TIMES
  cat('Performing subsampling evaluation on scorable models\n')
  
  if(is.null(pseudoRndSeed)){
  set.seed(130409)} else {set.seed(pseudoRndSeed)}
  
  samplePerfMatrix <- matrix(NA, 83, 100)
  
  for(i in 1:100){
    cat(sprintf('Scoring subsampling iteration %s/100\n', i))
    iSamplePerfList <- mclapply(osloPredList, function(modelPredVec){
      sampleVec <- sample(1:184, 0.8*nrow(osloScores))
      modelSamp <- modelPredVec[sampleVec]
      trueTimeSamp <- osloSurv[sampleVec, 1]
      trueEventSamp <- osloSurv[sampleVec, 2]
      sampleCi <- concordance.index(modelSamp, trueTimeSamp, trueEventSamp)
      sampleCi$c.index
    })
    iSamplePerfVec <- do.call('rbind', iSamplePerfList)
    samplePerfMatrix[ , i] <- iSamplePerfVec
  }
  
  rownames(samplePerfMatrix) <- colnames(osloScores)
  mSampPerfMat <- melt(samplePerfMatrix)
  
  sampleRankMatrix <- apply(samplePerfMatrix, 2, function(x){rank(-x)})
  mSampRankMat <- melt(sampleRankMatrix)
  
  cat('Generating plots\n')
  ## GENERATE PLOT BY CONCORDANCE INDEX
  ciSampleBoxPlot <- ggplot(mSampPerfMat, aes(X1, value)) + 
    geom_boxplot() + 
    geom_jitter(alpha = 0.1) +
    scale_x_discrete(limits = orderedIDs) +
    theme(axis.text.x = element_text(angle = -90, hjust = 0)) +
    ggtitle('Random Subsampling Testing of Score Stability\n') +
    xlab('\nModels') + ylab('Concordance Index\n')
  
  
  ## GENERATE PLOT BY RANK
  rankSampleBoxPlot <- ggplot(mSampRankMat, aes(X1, value)) +
    geom_boxplot() +
    geom_jitter(alpha = 0.1) +
    scale_x_discrete(limits = orderedIDs) +
    scale_y_reverse() +
    theme(axis.text.x = element_text(angle = -90, hjust = 0)) +
    ggtitle('Random Subsampling Testing of Rank Stability\n') +
    xlab('\nRank') + ylab('Concordance Index\n')
  
  ## GENERATE DENSITY PLOT FOR CCI (FOR FUN)
  ciSampleDensityPlot <- ggplot(mSampPerfMat, aes(value, fill = factor(X1))) + 
    geom_density(alpha = 0.2) + 
    theme(legend.position = 'none') +
    xlab('\nConcordance Index') + ylab('Probability Density Function\n') +
    ggtitle('Random Subsampling Testing: Model Density Functions by Score\n')
  
  ## GENERATE DENSITY PLOT FOR RANK (FOR FUN)
  rankSampleDensityPlot <- ggplot(mSampRankMat, aes(value, fill = factor(X1))) + 
    geom_density(alpha = 0.2) + 
    theme(legend.position = 'none') + 
    xlab('\nRank') + ylab('Probability Density Function\n') +
    ggtitle('Random Subsampling Testing: Model Density Functions by Rank\n')


  returnList <- list('ciSampleBoxPlot' = ciSampleBoxPlot,
                     'rankSampleBoxPlot' = rankSampleBoxPlot,
                     'ciSampleDensityPlot' = ciSampleDensityPlot,
                     'rankSampleDensityPlot' = rankSampleDensityPlot)
  
  return(returnList)
  
}

