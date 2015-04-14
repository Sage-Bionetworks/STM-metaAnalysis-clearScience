## figureFourA.R

## Erich S. Huang
## Sage Bionetworks
## erich.huang@sagebase.org

## Adapted from Erhan Bilal's Figure 4A work on
## analyzing the results of the BCC challenge.

figureFourA <- function(... = NULL){
  ## REQUIRE
  require(synapseClient)
  require(ggplot2)
  require(reshape)
  require(gtools)
  require(survcomp)
  
  ## LOAD NECESSARY DATA OBJECTS
  metbEnt <- synGet('syn1738796', load=TRUE) # matrix of predictions
  
  metbSurvEnt <- synGet('syn1125632', load=TRUE) # survival object
  
  metbLbEnt <- synGet('syn1744693', load=TRUE) # 15 October leaderboard
  
  metbPredMat <- metbEnt@objects$object
  metbPredMat <- apply(metbPredMat, 2, rank)
  
  metbSurv <- metbSurvEnt@objects$clinicalSurvData
  
  metbLb <- metbLbEnt@objects$object

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
  
  returnList <- list('mbricMetaPlot' = mbricPlot,
                   'metabricDataFrame' = metabricDF)
  
  return(returnList)
  
}