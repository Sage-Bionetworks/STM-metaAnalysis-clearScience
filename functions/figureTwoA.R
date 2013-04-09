## figureTwoA.R

## Erich S. Huang
## Sage Bionetworks
## erich.huang@sagebase.org

figureTwoA <- function(){
  
  ## REQUIRE
  require(synapseClient)
  require(doMC)
  require(plyr)
  require(ggplot2)
  
  registerDoMC()
  
  ## LOAD NECESSARY OBJECTS
  metbLbEnt <- loadEntity('syn1745570') # 15 October leaderboard
  preOctLeaderboard <- metbLbEnt$objects$object
  
  modelIDs <- preOctLeaderboard$SynapseID
  dateVec <- mclapply(modelIDs, function(synId){
    queryString <- sprintf("select createdOn from entity where entity.id == '%s'", synId)
    queryResult <- synapseQuery(queryString)
    createdOnA <- as.numeric(queryResult[1, 1])/1000
    createdOnB <- as.POSIXct(createdOnA, origin = '1970-01-01')
    modelDate <- unlist(strsplit(as.character(createdOnB), split = ' '))[1]
  })
  
  names(dateVec) <- modelIDs
  dateFrame <- data.frame(t(data.frame(t(sapply(dateVec, c)))))
  
  dateScoreFrame <- data.frame(preOctLeaderboard$ConcordanceIndex, dateFrame)
  colnames(dateScoreFrame) <- c('score', 'modelDate')
  
  posixDate <- rep(NA, 1414)
  for(i in 1:1414){
    iDate <- (dateScoreFrame[i, 'modelDate'])[[1]]
    if(is.null(iDate) == 'TRUE'){
      posixDate[i] <- NA
    } else {
      iPosix <- as.numeric(as.POSIXct(iDate))
      posixDate[i] <- iPosix
    }
  }
  
  dateScoreFrame <- data.frame(dateScoreFrame, 'posixDate' = posixDate)
  dateScoreFrame <- arrange(dateScoreFrame, posixDate)
  
  # dateClass <- as.factor(dateScoreFrame$posixDate)
  # dateLevels <- levels(dateClass)
  
  newFrame <- data.frame(dateScoreFrame$posixDate, dateScoreFrame$score)
  colnames(newFrame) <- c('posixDate', 'modelScore')
  
  newFrame <- arrange(newFrame, posixDate, modelScore)
  
  # dfEnt <- loadEntity('syn1681429')
  # dfEnt <- addObject(dfEnt, dateScoreFrame)
  # dfEnt <- storeEntity(dfEnt)
  
  dateNA <- which(is.na(newFrame$posixDate))
  
  newFrame <- newFrame[-dateNA, ]
  
  scoreNA <- which(is.na(newFrame$modelScore))
  
  newFrame <- newFrame[-scoreNA, ]
  
  # dateLevels <- levels(newFrame$posixDate)
  uniqueDates <- unique(newFrame$posixDate)
  
  dateMaxScore <- rep(NA, length(uniqueDates))
  # for(i in 1:length(dateLevels)){
  #   iDate <- dateLevels[i]
  #   iDateScores <- newFrame[newFrame$posixDate == iDate, 'modelScore']
  #   iDateMaxScore <- max(iDateScores)
  #   dateMaxScore[i] <- iDateMaxScore
  # }
  # names(dateMaxScore) <- dateLevels
  
  for(i in 1:length(uniqueDates)){
    iDate <- uniqueDates[i]
    iDateScores <- newFrame[newFrame$posixDate <= iDate, 'modelScore']
    iDateMaxScore <- max(iDateScores)
    dateMaxScore[i] <- iDateMaxScore
  }
  names(dateMaxScore) <- uniqueDates
  
  ## DEFINE A COLOR PALETTE FOR THIS FIGURE
  cbbPalette <- c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")
  
  evolDF <- data.frame('maxScores' = dateMaxScore, 'dateLevels' = uniqueDates, 'xLevel' = 1:22)
  evolPlot <- ggplot(evolDF, aes(xLevel, maxScores)) +
    geom_point() +
    geom_segment(aes(x = 1, xend = 2, y = evolDF[1, 1], yend = evolDF[2, 1], colour = cbbPalette[1]), size = 5) +
    geom_segment(aes(x = 2, xend = 3, y = evolDF[2, 1], yend = evolDF[3, 1], colour = cbbPalette[2]), size = 5) +
    geom_segment(aes(x = 3, xend = 4, y = evolDF[3, 1], yend = evolDF[4, 1], colour = cbbPalette[3]), size = 5) +
    geom_segment(aes(x = 4, xend = 5, y = evolDF[4, 1], yend = evolDF[5, 1], colour = cbbPalette[4]), size = 5) +
    geom_segment(aes(x = 5, xend = 6, y = evolDF[5, 1], yend = evolDF[6, 1], colour = cbbPalette[5]), size = 5) +
    geom_segment(aes(x = 6, xend = 7, y = evolDF[6, 1], yend = evolDF[7, 1], colour = cbbPalette[6]), size = 5) +
    geom_segment(aes(x = 7, xend = 8, y = evolDF[7, 1], yend = evolDF[8, 1], colour = cbbPalette[7]), size = 5) +
    geom_segment(aes(x = 8, xend = 9, y = evolDF[8, 1], yend = evolDF[9, 1], colour = cbbPalette[8]), size = 5) +
    geom_segment(aes(x = 9, xend = 10, y = evolDF[9, 1], yend = evolDF[10, 1], colour = cbbPalette[1]), size = 5) +
    geom_segment(aes(x = 10, xend = 11, y = evolDF[10, 1], yend = evolDF[11, 1], colour = cbbPalette[2]), size = 5) +
    geom_segment(aes(x = 11, xend = 12, y = evolDF[11, 1], yend = evolDF[12, 1], colour = cbbPalette[3]), size = 5) +
    geom_segment(aes(x = 12, xend = 13, y = evolDF[12, 1], yend = evolDF[13, 1], colour = cbbPalette[4]), size = 5) +
    geom_segment(aes(x = 13, xend = 14, y = evolDF[13, 1], yend = evolDF[14, 1], colour = cbbPalette[5]), size = 5) +
    geom_segment(aes(x = 14, xend = 15, y = evolDF[14, 1], yend = evolDF[15, 1], colour = cbbPalette[6]), size = 5) +
    geom_segment(aes(x = 15, xend = 16, y = evolDF[15, 1], yend = evolDF[16, 1], colour = cbbPalette[7]), size = 5) +
    geom_segment(aes(x = 16, xend = 17, y = evolDF[16, 1], yend = evolDF[17, 1], colour = cbbPalette[8]), size = 5) +
    geom_segment(aes(x = 17, xend = 18, y = evolDF[17, 1], yend = evolDF[18, 1], colour = cbbPalette[1]), size = 5) +
    geom_segment(aes(x = 18, xend = 19, y = evolDF[18, 1], yend = evolDF[19, 1], colour = cbbPalette[2]), size = 5) +
    geom_segment(aes(x = 19, xend = 20, y = evolDF[19, 1], yend = evolDF[20, 1], colour = cbbPalette[3]), size = 5) +
    geom_segment(aes(x = 20, xend = 21, y = evolDF[20, 1], yend = evolDF[21, 1], colour = cbbPalette[4]), size = 5) +
    geom_segment(aes(x = 21, xend = 22, y = evolDF[21, 1], yend = evolDF[22, 1], colour = cbbPalette[5]), size = 5) +
    theme(legend.position="none") 
  
  modelDates <- unlist(dateScoreFrame$modelDate)
  modelDates <- as.factor(modelDates)
  xLabels <- levels(modelDates)
  
  evolPlotDate <- evolPlot + scale_x_discrete(breaks = 1:22, labels = xLabels) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab('\nDate') + ylab('Max. CCI\n') +
    ggtitle('Evolution of Model Scores\n')
  
  # plotEnt <- loadEntity('syn1688393')
  # plotEnt <- addObject(plotEnt, evolPlotDate)  
  # plotEnt <- storeEntity(plotEnt)  
  
}

