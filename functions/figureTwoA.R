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
  cat('Loading necessary objects from Synapse\n')
  metbLbEnt <- loadEntity('syn1745570') # Pre-15 October leaderboard
  preOctLeaderboard <- metbLbEnt$objects$object
  
  cat('Querying Synapse for model "created on" dates\n')
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
  
  dateScoreFrame <- data.frame('score' = preOctLeaderboard$ConcordanceIndex, 'modelDate' = dateFrame)
  rownames(dateScoreFrame) <- modelIDs
  colnames(dateScoreFrame) <- c('score', 'modelDate')
  
  cat('Converting dates to POSIX\n')
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
  
  cat('Eliminating models with NA scores or dates\n')
  dateScoreFrame <- data.frame(dateScoreFrame, 'posixDate' = posixDate)
  dateScoreFrame <- name_rows(dateScoreFrame)
  dateScoreFrame <- arrange(dateScoreFrame, posixDate)
  
  newFrame <- data.frame('posixDate' = dateScoreFrame$posixDate, 
                         'modelScore' = dateScoreFrame$score,
                         '.rownames' = dateScoreFrame$.rownames,
                         stringsAsFactors = FALSE)
  
  newFrame <- arrange(newFrame, posixDate, modelScore)
  
  dateNA <- which(is.na(newFrame$posixDate))
  newFrame <- newFrame[-dateNA, ]
  
  scoreNA <- which(is.na(newFrame$modelScore))
  newFrame <- newFrame[-scoreNA, ]
  
  uniqueDates <- unique(newFrame$posixDate)
  
  cat('Extracting the top scoring model for each date period\n')
  dateMaxScore <- as.data.frame(matrix(NA, length(uniqueDates), 2))
  for(i in 1:length(uniqueDates)){
    iDate <- uniqueDates[i]
    iDateInd <- which(newFrame$posixDate <= iDate)
    iDateScores <- newFrame[iDateInd, 'modelScore']
    iDateScores <- data.frame(iDateScores, 
                              newFrame$.rownames[iDateInd],
                              stringsAsFactors = FALSE)
    iDateMaxWhich <- which.max(iDateScores[ , 1])
    iDateMaxScore <- iDateScores[iDateMaxWhich, 1]
    iDateMaxID <- iDateScores[iDateMaxWhich, 2]
    dateMaxScore[i, 1] <- iDateMaxScore
    dateMaxScore[i, 2] <- iDateMaxID
  }
  colnames(dateMaxScore) <- c('score', 'id')
  
  ## DEFINE A COLOR PALETTE FOR THIS FIGURE
  cat('Plotting the figure\n')
  cbbPalette <- c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")
    
  evolDF <- data.frame('modelID' = dateMaxScore$id,
                       'maxScores' = dateMaxScore$score, 
                       'dateLevels' = uniqueDates, 
                       'xLevel' = 1:22)
  
  .e <- environment()
  
  evolPlot <- ggplot(evolDF, aes(xLevel, maxScores), environment = .e) +
    geom_point(size = 8, alpha = 0.2) +
    geom_segment(aes(x = 1, xend = 2, y = evolDF[1, 2], yend = evolDF[2, 2], colour = cbbPalette[1]), size = 5) +
    geom_segment(aes(x = 2, xend = 3, y = evolDF[2, 2], yend = evolDF[3, 2], colour = cbbPalette[2]), size = 5) +
    geom_segment(aes(x = 3, xend = 4, y = evolDF[3, 2], yend = evolDF[4, 2], colour = cbbPalette[3]), size = 5) +
    geom_segment(aes(x = 4, xend = 5, y = evolDF[4, 2], yend = evolDF[5, 2], colour = cbbPalette[4]), size = 5) +
    geom_segment(aes(x = 5, xend = 6, y = evolDF[5, 2], yend = evolDF[6, 2], colour = cbbPalette[5]), size = 5) +
    geom_segment(aes(x = 6, xend = 7, y = evolDF[6, 2], yend = evolDF[7, 2], colour = cbbPalette[6]), size = 5) +
    geom_segment(aes(x = 7, xend = 8, y = evolDF[7, 2], yend = evolDF[8, 2], colour = cbbPalette[7]), size = 5) +
    geom_segment(aes(x = 8, xend = 9, y = evolDF[8, 2], yend = evolDF[9, 2], colour = cbbPalette[8]), size = 5) +
    geom_segment(aes(x = 9, xend = 10, y = evolDF[9, 2], yend = evolDF[10, 2], colour = cbbPalette[1]), size = 5) +
    geom_segment(aes(x = 10, xend = 11, y = evolDF[10, 2], yend = evolDF[11, 2], colour = cbbPalette[2]), size = 5) +
    geom_segment(aes(x = 11, xend = 12, y = evolDF[11, 2], yend = evolDF[12, 2], colour = cbbPalette[3]), size = 5) +
    geom_segment(aes(x = 12, xend = 13, y = evolDF[12, 2], yend = evolDF[13, 2], colour = cbbPalette[4]), size = 5) +
    geom_segment(aes(x = 13, xend = 14, y = evolDF[13, 2], yend = evolDF[14, 2], colour = cbbPalette[5]), size = 5) +
    geom_segment(aes(x = 14, xend = 15, y = evolDF[14, 2], yend = evolDF[15, 2], colour = cbbPalette[6]), size = 5) +
    geom_segment(aes(x = 15, xend = 16, y = evolDF[15, 2], yend = evolDF[16, 2], colour = cbbPalette[7]), size = 5) +
    geom_segment(aes(x = 16, xend = 17, y = evolDF[16, 2], yend = evolDF[17, 2], colour = cbbPalette[8]), size = 5) +
    geom_segment(aes(x = 17, xend = 18, y = evolDF[17, 2], yend = evolDF[18, 2], colour = cbbPalette[1]), size = 5) +
    geom_segment(aes(x = 18, xend = 19, y = evolDF[18, 2], yend = evolDF[19, 2], colour = cbbPalette[2]), size = 5) +
    geom_segment(aes(x = 19, xend = 20, y = evolDF[19, 2], yend = evolDF[20, 2], colour = cbbPalette[3]), size = 5) +
    geom_segment(aes(x = 20, xend = 21, y = evolDF[20, 2], yend = evolDF[21, 2], colour = cbbPalette[4]), size = 5) +
    geom_segment(aes(x = 21, xend = 22, y = evolDF[21, 2], yend = evolDF[22, 2], colour = cbbPalette[5]), size = 5) +
    theme(legend.position="none") 
  
  modelDates <- unlist(dateScoreFrame$modelDate)
  modelDates <- as.factor(modelDates)
  xLabels <- levels(modelDates)
  
  evolPlotDate <- evolPlot + scale_x_discrete(breaks = 1:22, labels = xLabels) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab('\nDate') + ylab('Max. CCI\n') +
    ggtitle('Evolution of Model Scores\n')
  
  show(evolPlotDate)
  
  # plotEnt <- loadEntity('syn1688393')
  # plotEnt <- addObject(plotEnt, evolPlotDate)  
  # plotEnt <- storeEntity(plotEnt)  
  
}

