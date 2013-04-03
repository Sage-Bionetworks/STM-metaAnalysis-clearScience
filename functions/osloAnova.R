## osloAnova.R

## Adapted from Erhan Bilal's code
## Erich S. Huang
## Sage Bionetworks
## erich.huang@sagebase.org

osloAnovaAnalysis <- function(){
  ## REQUIRE
  require(synapseClient)
  
  ## LOAD NECESSARY DATA OBJECTS
  osloPredEnt <- loadEntity('syn1725898')
  osloScores <- osloPredEnt$objects$osloPredictions
  clinEnt <- loadEntity('syn1449480')
  clinicData <- clinEnt$objects$xIntClinDat
  survEnt <- loadEntity('syn1449477')
  clinicalSurvData <- survEnt$objects$xIntSurvObj
  
  ## OBTAIN VECTORS FOR EACH CLINICAL CHARACTERISTIC
  age <- clinicData[ ,'age_at_diagnosis']
  lymphnodes <- clinicData[ ,'lymph_nodes_positive']
  grade <- as.factor(clinicData[ ,'grade'])
  tumorsize <- clinicData[ ,'size']
  her2 <- as.factor(clinicData[ ,'HER2_SNP6_state'])
  er <- as.factor(clinicData[ ,'ER.Expr'])
  pr <- as.factor(clinicData[ ,'PR.Expr'])
  os <- clinicalSurvData[ , 1]
  ev <- clinicalSurvData[ , 2]
  
  ## DISCRETIZE CLINICAL CHARACTERISTICS
  ## lymph node status
  for (i in 1:length(lymphnodes)) {
    if (is.na(lymphnodes[i])) {}
    else if (lymphnodes[i] >=1 & lymphnodes[i] <=3) {
      lymphnodes[i] <- 1
    }
    else if (lymphnodes[i] >= 4 & lymphnodes[i] <= 9) {
      lymphnodes[i] <- 2
    } 
    else if (lymphnodes[i] > 9) {
      lymphnodes[i] <- 3
    }
  }
  lymphnodes <- as.factor(lymphnodes)
  
  ## age
  age <- as.factor(ifelse(age <= 50, 0, 1))
  
  ## size
  tumorsize <- as.factor(ifelse(tumorsize <= 2, 0, 1))
  
  ## followup time
  for (i in 1:length(os)) {
    if (is.na(os[i])) {}
    else if (os[i] <= 1825 & ev[i] == 1) {
      os[i] <- 0
    }
    else if (os[i] > 1825 & os[i] <= 3650 & ev[i] == 1) {
      os[i] <- 1
    } 
    else if (os[i] > 3650) {
      os[i] <- 2
    }
  }
  os <- as.factor(os)
  
  ## HER2 status
  diHer2 <- rep(NA, length(her2))
  diHer2[her2 == 'GAIN'] <- 'pos'
  diHer2[her2 != 'GAIN'] <- 'neg'
  
  
  
  

}