## figureTwoB.R

## Erich S. Huang
## Sage Bionetworks
## erich.huang@sagebase.org

figureTwoB <- function(){
  ## REQUIRE
  require(synapseClient)
  require(plyr)
  require(reshape)
  require(ggplot2)
  require(survcomp)
  
  ## READ IN THE LEADERBOARD DATA
  fifteenOctLBEnt <- loadEntity('syn1744693')
  pre15OctLBEnt <- loadEntity('syn1745570')
  finalLBEnt <- loadEntity('syn1739275')
  
  oct15LB <- fifteenOctLBEnt$objects$object
  preOct15LB <- pre15OctLBEnt$objects$object
  finalLB <- finalLBEnt$objects$object
  
  # Take advantage of R's rownames functionality
  rownames(finalLB) <- finalLB$SYNID
  rownames(oct15LB) <- oct15LB$SynID
  rownames(preOct15LB) <- preOct15LB$SynapseID
  
  ## CREATE SCORE VECTORS
  finalScores <- finalLB$CCI
  names(finalScores) <- finalLB$SYNID
  
  oct15Scores <- oct15LB$Final.Test.Score
  names(oct15Scores) <- oct15LB$SynID
  
  preOct15Scores <- preOct15LB$ConcordanceIndex
  names(preOct15Scores) <- preOct15LB$SynapseID
  
  ## FIND THE FULL INTERSECT ACROSS ALL THREE PHASES
  fullIntersect <- Reduce(intersect, list(finalLB$SYNID, oct15LB$SynID, preOct15LB$SynapseID))
  intScores <- data.frame('final' = finalScores[fullIntersect], 
                          'oct15' = oct15Scores[fullIntersect], 
                          'preOct15' = preOct15Scores[fullIntersect])
  
  ## CREATE PERMUTED CONCORDANCE INDEX DATA FOR A NULL DISTRIBUTION
  ## Load in the true osloval survival data
  testSurvEnt <- loadEntity('syn1710257')
  testSurvObj <- testSurvEnt$objects$oslovalSurvData
  
  mockData <- matrix(NA, nrow = 184, ncol = length(fullIntersect))
  
  set.seed(1211021520)
  for(i in 1:length(fullIntersect)){
    newCol <- sample(1:184, 184)
    mockData[ , i] <- newCol
  }
  
  resultVec <- matrix(NA, nrow = 3, ncol = length(fullIntersect))
  for(i in 1:length(fullIntersect)){
    iConcIndex <- concordance.index(mockData[ , i], testSurvObj[ , 1], testSurvObj[ , 2])
    resultVec[1, i] <- iConcIndex$c.index
    resultVec[2, i] <- iConcIndex$lower
    resultVec[3, i] <- iConcIndex$upper
  }
  
  nullVec <- resultVec[1, ]
  #####
  intScores <- data.frame(intScores, 'null' = nullVec)
  
  meltIntScores <- melt(intScores)
  colnames(meltIntScores) <- c('phase', 'cci')
  
  ## PLOT
  densPlot <- ggplot(meltIntScores, aes(cci, fill = factor(phase))) +
    geom_density(alpha = 0.3) +
    ggtitle('Density Plot of Model Scores by Challenge Phase\n') +
    ylab('Probability Density Function') +
    xlab('Concordance Index')
  
  return(densPlot)
}