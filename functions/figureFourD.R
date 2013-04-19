## figureFourC.R

## Erich S. Huang
## Sage Bionetworks
## erich.huang@sagebase.org

## Adapted from Erhan Bilal's Figure 4C work on
## analyzing the results of the BCC challenge.

figureFourD <- function(... = NULL){
  
  ## REQUIRE
  require(synapseClient)
  require(ggplot2)
  require(reshape)
  require(gtools)
  require(survcomp)
  
  ## LOAD NECESSARY OBJECTS
  osloEnt <- loadEntity('syn1725898') # matrix of predictions 
  osloSurvEnt <- loadEntity('syn1710257') # survival object
  osloLbEnt <- loadEntity('syn1739275') # Final osloval leadeboard
  
  osloScores <- osloEnt$objects$osloPredictions
  osloScores <- data.frame('patientID' = 1:184, osloScores)
  osloScores <- apply(osloScores, 2, rank)
  
  osloSurv <- osloSurvEnt$objects$oslovalSurvData
  
  osloLeaderboard <- osloLbEnt$objects$object
  osloLeaderboard <- osloLeaderboard[1:83, ]
  
  # Plots boxplots for metapredictions of 1,5,10,20,50 random participants
  oMeta1Score <- osloLeaderboard[ , 'CCI']
  oMeta5Score <- rep(0, 100)
  oMeta10Score <- rep(0, 100)
  oMeta20Score <- rep(0, 100)
  oMeta50Score <- rep(0, 100)
  
  cIndex <- function(predVec){
    ciListObj <- concordance.index(predVec, osloSurv[ , 1], osloSurv[ , 2])
    score <- ciListObj$c.index
  }
  
  for (i in 1:100) {    
    oMeta5Score[i] <- cIndex(rowMeans(osloScores[, (1 + sample.int(length(oMeta1Score), 5))]))
    oMeta10Score[i] <- cIndex(rowMeans(osloScores[, (1 + sample.int(length(oMeta1Score), 10))]))
    oMeta20Score[i] <- cIndex(rowMeans(osloScores[, (1 + sample.int(length(oMeta1Score), 20))]))
    oMeta50Score[i] <- cIndex(rowMeans(osloScores[, (1 + sample.int(length(oMeta1Score), 50))]))
  }
  
  oMetaDF <- data.frame('meta5' = oMeta5Score,
                        'meta10' = oMeta10Score,
                        'meta20' = oMeta20Score,
                        'meta50' = oMeta50Score)
  
  meltMMetaDF <- melt(oMetaDF)
  colnames(meltMMetaDF) <- c('scoretype', 'metascore')
  
  meta1DF <- data.frame('scoretype' = rep('meta1', 83), 'metascore' = oMeta1Score)
  
  meltOMetaDF <- rbind(meta1DF, meltMMetaDF)
  
  oMetaBoxPlot <- ggplot(meltOMetaDF, aes(scoretype, metascore)) + geom_boxplot() +
    geom_jitter(aes(colour = factor(scoretype)), size = 4, alpha = 0.6) +
    theme(axis.text.y=element_text(size = 20)) +
    theme(axis.text.x=element_text(size = 15)) +
    ggtitle('OsloVal community scores by community size\n') +
    ylab('Concordance Index\n') + xlab('\nCommunity Sizes')
  
  returnList <- list('osloCommunityBoxPlot' = oMetaBoxPlot,
                     'osloCommunityDataFrame' = meltOMetaDF)
  
  return(returnList)
}
