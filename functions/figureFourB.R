## figureFourB.R

## Erich S. Huang
## Sage Bionetworks
## erich.huang@sagebase.org

## Adapted from Erhan Bilal's Figure 4B work on
## analyzing the results of the BCC challenge.

figureFourB <- function(){
  ## REQUIRE
  require(synapseClient)
  require(ggplot2)
  require(reshape)
  require(gtools)
  require(survcomp)
  
  ## LOAD NECESSARY DATA OBJECTS
  osloEnt <- loadEntity('syn1725898') # matrix of predictions 
  
  osloSurvEnt <- loadEntity('syn1710257') # survival object
  
  osloLbEnt <- loadEntity('syn1739275') # Final osloval leadeboard
  
  osloPredMat <- osloEnt$objects$osloPredictions
  osloPredMat <- data.frame('patientID' = 1:184, osloPredMat)
  osloPredMat <- apply(osloPredMat, 2, rank)
  
  osloSurv <- osloSurvEnt$objects$oslovalSurvData
  
  osloLb <- osloLbEnt$objects$object
  osloLb <- osloLb[1:83, ]
  
  ## GENERATE OSLOVAL 'META-' OR COMMUNITY SCORES
  indivScore <- NULL
  metaScore <- NULL
  for (i in 1:83) {
    indivScore <- c(indivScore, osloLb[i,'CCI'])
    
    if (i == 1) {
      metaPrediction <- osloPredMat[ , 2]
    }
    else {    
      metaPrediction <- rowMeans(osloPredMat[ , 2:(i+1)])
    }
    performance <- concordance.index(as.numeric(metaPrediction), osloSurv[ , 1], osloSurv[ , 2])
    metaScore <- c(metaScore, performance$c.index)
  }
  
  osloDF <- data.frame('osloIndivScore' = indivScore, 'osloMetaScore' = metaScore)
  mOsloDF <- melt(osloDF)
  mOsloDF <- data.frame(mOsloDF, rep(1:83, 2))
  colnames(mOsloDF) <- c('scoreType', 'cci', 'rank')
  
  osloPlot <- ggplot(mOsloDF, aes(rank, cci, colour = scoreType)) +
    geom_line(size = 4) +
    ggtitle('OsloVal: Individual Scores & "MetaScores"\n') +
    ylab('Concordance Index\n') +
    xlab('\nLeaderboard Rank')
  
  returnList <- list('osloMetaPlot' = osloPlot,
                   'oslovalDataFrame' = osloDF)
  
  return(returnList)
  
}