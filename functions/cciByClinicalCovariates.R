## cciByClinicalCovariates.R

## Erich S. Huang
## Sage Bionetworks
## erich.huang@sagebase.org


### FIRST FUNCTION
cciBoxPlotDataPrep <- function(){
  ## REQUIRE
  require(synapseClient)
  require(ggplot2)
  require(survcomp)
  require(reshape)
  require(gtools)
  
  ## LOAD DATA OBJECTS
  clinEnt <- loadEntity('syn1710251')
  osloVecEnt <- loadEntity('syn1725898')
  survEnt <- loadEntity('syn1710257')
  
  xIntClinDat <- clinEnt$objects$oslovalClinicalTable
  survObj <- survEnt$objects$oslovalSurvData
  rownames(survObj) <- rownames(xIntClinDat)
  
  ## CATEGORIZE SURVIVAL DATA
  yTime <- survEnt$objects$oslovalSurvData[ , 1]/356
  timeCat <- rep(NA, length(yTime))
  timeCat[which(yTime <= 5)] <- 1
  timeCat[which(yTime > 5 & yTime <= 10)] <- 2
  timeCat[which(yTime > 10)] <- 3

  ## IDENTIFY CLINICAL SUBCATEGORIES
  osloPredMat <- osloVecEnt$objects$osloPredictions
  clinLogicalMat <- as.data.frame(matrix(0, nrow = nrow(osloPredMat), ncol = 16))
  rownames(clinLogicalMat) <- rownames(osloPredMat)
  colnames(clinLogicalMat) <- c('grade1', 
                                'grade2', 
                                'grade3', 
                                'lnNeg', 
                                'ln1to3', 
                                'ln4to9', 
                                'ln10plus', 
                                'time0to5', 
                                'time5to10', 
                                'time10plus',
                                'preMeno',
                                'postMeno',
                                'erPos',
                                'erNeg',
                                'her2Pos',
                                'her2Neg')
  
  xIntClinDat <- data.frame(as.character(rownames(xIntClinDat)), 
                            xIntClinDat, stringsAsFactors = FALSE)
  colnames(xIntClinDat)[1] <- 'patientSamps'
  
  grade1Samps <- xIntClinDat[which(xIntClinDat$grade == 1), 1]
  clinLogicalMat[grade1Samps, 'grade1'] <- 1 
  
  grade2Samps <- xIntClinDat[which(xIntClinDat$grade == 2), 1]
  clinLogicalMat[grade2Samps, 'grade2'] <- 1
  
  grade3Samps <- xIntClinDat[which(xIntClinDat$grade == 3), 1]
  clinLogicalMat[grade3Samps, 'grade3'] <- 1
  
  lnNegSamps <- xIntClinDat[which(xIntClinDat$lymph_nodes_positive == 0), 1]
  clinLogicalMat[lnNegSamps, 'lnNeg'] <- 1
  
  ln1to3Samps <- xIntClinDat[which(xIntClinDat$lymph_nodes_positive > 0 &
                                     xIntClinDat$lymph_nodes_positive < 4), 1]
  clinLogicalMat[ln1to3Samps, 'ln1to3'] <- 1
  
  ln4to9Samps <- xIntClinDat[which(xIntClinDat$lymph_nodes_positive >= 4 &
                                     xIntClinDat$lymph_nodes_positive <= 9), 1]
  clinLogicalMat[ln4to9Samps, 'ln4to9'] <- 1
  
  ln10plusSamps <- xIntClinDat[which(xIntClinDat$lymph_nodes_positive >= 10), 1]
  clinLogicalMat[ln10plusSamps, 'ln10plus'] <- 1
  
  time0to5Samps <- xIntClinDat[which(timeCat == 1), 1]
  clinLogicalMat[time0to5Samps, 'time0to5'] <- 1
  
  time5to10Samps <- xIntClinDat[which(timeCat == 2), 1]
  clinLogicalMat[time5to10Samps, 'time5to10'] <- 1
  
  time10plusSamps <- xIntClinDat[which(timeCat == 3), 1]
  clinLogicalMat[time10plusSamps, 'time10plus'] <- 1
  
  preMenoSamps <- xIntClinDat[which(xIntClinDat$age_at_diagnosis <= 50), 1]
  clinLogicalMat[preMenoSamps, 'preMeno'] <- 1
  
  postMenoSamps <- xIntClinDat[which(xIntClinDat$age_at_diagnosis > 50), 1]
  clinLogicalMat[postMenoSamps, 'postMeno'] <- 1
  
  erPosSamps <- xIntClinDat[which(xIntClinDat$ER.Expr == '+'), 1]
  clinLogicalMat[erPosSamps, 'erPos'] <- 1
  
  erNegSamps <- xIntClinDat[which(xIntClinDat$ER.Expr == '-'), 1]
  clinLogicalMat[erNegSamps, 'erNeg'] <- 1
  
  her2PosSamps <- xIntClinDat[which(xIntClinDat$HER2_SNP6_state == 'GAIN'), 1]
  clinLogicalMat[her2PosSamps, 'her2Pos'] <- 1
  
  her2NegSamps <- xIntClinDat[which(xIntClinDat$HER2_SNP6_state == 'NEUT' |
                                      xIntClinDat$HER2_SNP6_state == 'LOSS'), 1]
  clinLogicalMat[her2NegSamps, 'her2Neg'] <- 1
  
  ## NOW THAT A LOGICAL MATRIX HAS BEEN GENERATED, CREATE SUBMATRICES OF 
  ## MODEL PREDICTIONS FOR EACH PATIENT BELONGING TO A SUBGROUP
  groupPredMatList <- apply(clinLogicalMat, 2, function(x){osloPredMat[x == 1, ]})
  names(groupPredMatList) <- colnames(clinLogicalMat)
  
  ## GENERATE CONCORDANCE INDEX SCORES FOR EACH MODEL BASED ON THE SUBGROUPS
  groupCciList <- lapply(groupPredMatList, function(groupPredMat){
    trueTime <- survObj[rownames(groupPredMat), 1]
    trueEvent <- survObj[rownames(groupPredMat), 2]
    cciVec <- apply(groupPredMat, 2, function(modelVec){
      cciResult <- concordance.index(modelVec, trueTime, trueEvent)
      cciResult$c.index
    })
  })
  
  ## RETURN OBJECTS
  returnObj <- list('clinicalGroupLogicalMatrix' = clinLogicalMat,
                    'clinicalGroupCciList' = groupCciList)
  return(returnObj)
}

### SECOND FUNCTION
cciBoxPlots <- function(returnObj){
  ## REQUIRE
  require(ggplot2)
  require(reshape)
  
  ## MELT THE DATAFRAME
  cciDF <- as.data.frame(returnObj$clinicalGroupCciList)
  mCciDF <- melt(cciDF)
  colnames(mCciDF) <- c('clinical', 'cci')
  
  ## DEFINE A SUBFUNCTION
  makeBoxPlot <- function(clinDF){
    boxPlot <- ggplot(clinDF, aes(factor(clinical), cci)) +
      geom_boxplot() +
      geom_jitter(aes(colour = factor(clinical)), size = 5)
  }
  
  ## FIRST BOXPLOT: BY GRADE
  gradeDF <- mCciDF[grep('grade', mCciDF$clinical), ]
  gradeBoxPlot <- ggplot(gradeDF, aes(factor(clinical), cci)) + 
    geom_boxplot() +
    geom_jitter(aes(colour = factor(clinical)), size = 5) +
    ggtitle('Model Performance & Histological Grade\n') +
    xlab('\nGrade') + ylab('Concordance Index\n')
  
  ## SECOND BOXPLOT: BY LYMPH NODE CATEGORY
  lymphNodeDF <- mCciDF[grep('ln', mCciDF$clinical), ]
  lymphNodeBoxPlot <- ggplot(lymphNodeDF, aes(factor))
}









