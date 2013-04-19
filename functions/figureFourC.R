## figureFourC.R

## Erich S. Huang
## Sage Bionetworks
## erich.huang@sagebase.org

## Adapted from Erhan Bilal's Figure 4C work on
## analyzing the results of the BCC challenge.

figureFourC <- function(... = NULL){
  
  ## REQUIRE
  require(synapseClient)
  require(ggplot2)
  require(reshape)
  require(gtools)
  require(survcomp)
  
  ## LOAD NECESSARY OBJECTS
  metbLbEnt <- loadEntity('syn1744693') # 15 October leaderboard
  metbEnt <- loadEntity('syn1738796') # matrix of predictions
  metbSurvEnt <- loadEntity('syn1125632') # survival object 
  
  metbLeaderboard <- metbLbEnt$objects$object
  metbScores <- metbEnt$objects$object
  metbSurv <- metbSurvEnt$objects$clinicalSurvData
  
  # Plots boxplots for metapredictions of 1,5,10,20,50 random participants
  mMeta1Score <- metbLeaderboard[ ,'Final.Test.Score']
  mMeta5Score <- rep(0, 100)
  mMeta10Score <- rep(0, 100)
  mMeta20Score <- rep(0, 100)
  mMeta50Score <- rep(0, 100)
  
  cIndex <- function(predVec){
    ciListObj <- concordance.index(predVec, metbSurv[ , 1], metbSurv[ , 2])
    score <- ciListObj$c.index
  }
  
  for (i in 1:100) {    
    mMeta5Score[i] <- cIndex(rowMeans(metbScores[ , (1 + sample.int(length(mMeta1Score), 5))]))
    mMeta10Score[i] <- cIndex(rowMeans(metbScores[, (1 + sample.int(length(mMeta1Score), 10))]))
    mMeta20Score[i] <- cIndex(rowMeans(metbScores[, (1 + sample.int(length(mMeta1Score), 20))]))
    mMeta50Score[i] <- cIndex(rowMeans(metbScores[, (1 + sample.int(length(mMeta1Score), 50))]))
  }
  
  mMetaDF <- data.frame('meta5' = mMeta5Score,
                        'meta10' = mMeta10Score,
                        'meta20' = mMeta20Score,
                        'meta50' = mMeta50Score)
  
  meltMMetaDF <- melt(mMetaDF)
  colnames(meltMMetaDF) <- c('scoretype', 'metascore')
  
  meta1DF <- data.frame('scoretype' = rep('meta1', 154), 'metascore' = mMeta1Score)
  
  meltMMetaDF <- rbind(meta1DF, meltMMetaDF)
  
  mbMetaBoxPlot <- ggplot(meltMMetaDF, aes(scoretype, metascore)) + geom_boxplot() +
    geom_jitter(aes(colour = factor(scoretype)), size = 4, alpha = 0.6) +
    theme(axis.text.y=element_text(size = 20)) +
    theme(axis.text.x=element_text(size = 15)) +
    ggtitle('METABRIC community scores by community size\n') +
    ylab('Concordance Index\n') + xlab('\nCommunity Sizes')
  
  returnList <- list('mbricCommunityBoxPlot' = mbMetaBoxPlot,
                     'mbricCommunityDataFrame' = meltMMetaDF)
  
  return(returnList)
}
