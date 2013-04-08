## figureFour.R

## Erich S. Huang
## Sage Bionetworks
## erich.huang@sagebase.org

## Adapted from Erhan Biilal's Figure 4 work on
## analyzing the results of the BCC challenge.

figureFour <- function(){
  ## REQUIRE
  require(synapseClient)
  require(ggplot2)
  require(reshape)
  require(gtools)
  require(survcomp)
  
  ## LOAD NECESSARY DATA OBJECTS
  metbEnt <- loadEntity('syn1738796') # matrix of predictions
  osloEnt <- loadEntity('syn1725898') # matrix of predictions 
  
  metbSurvEnt <- loadEntity('syn1125632') # survival object
  osloSurvEnt <- loadEntity('syn1710257') # survival object
  
  metbLbEnt <- loadEntity('syn1744693') # 15 October leaderboard
  osloLbEnt <- loadEntity('syn1739275') # Final osloval leadeboard
  
  metbPredMat <- metbEnt$objects$object
  osloPredMat <- osloEnt$objects$osloPredictions
  osloPredMat <- data.frame('patientID' = 1:184, osloPredMat)
  
  metbSurv <- metbSurvEnt$objects$clinicalSurvData
  osloSurv <- osloSurvEnt$objects$oslovalSurvData
  
  metbLb <- metbLbEnt$objects$object
  osloLb <- osloLbEnt$objects$object
  osloLb <- osloLb[1:83, ]
  
  ## GENERATE METABRIC 'META'- OR COMMUNITY SCORES
  indivScore <- NULL
  metaScore <- NULL
  for (i in 1:nrow(metbLb)) {
    indivScore <- c(indivScore, metbLb[i,'Final.Test.Score'])
    
    if (i == 1) {
      metaPrediction <- metbPredMat[ , 2]
    }
    else {    
      metaPrediction <- rowMeans(metbPredMat[ , 2:(i+1)])
    }
    performance <- concordance.index(as.numeric(metaPrediction), metbSurv[ , 1], metbSurv[ , 2])
    metaScore <- c(metaScore, performance$c.index)
  }
  
  metabricDF <- data.frame('mbricIndivScore' = indivScore, 'mbricMetaScore' = metaScore)
  mMbricDF <- melt(metabricDF)
  mMbricDF <- data.frame(mMbricDF, 'rank' = rep(1:154, 2))
  colnames(mMbricDF) <- c('scoreType', 'cci', 'rank')
  
  mbricPlot <- ggplot(mMbricDF, aes(rank, cci, colour = scoreType)) +
    geom_line(size = 4) + 
    ggtitle('METABRIC: Individual Scores & "MetaScores"\n') +
    ylab('Concordance Index\n') +
    xlab('\nLeaderboard Rank')
  
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
  
  plotList <- list('mbricMetaPlot' = mbricPlot,
                   'oslovalMetaPlot' = osloPlot)
  
  return(plotList)
  
}
