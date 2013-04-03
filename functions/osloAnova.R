## osloAnova.R

## Adapted from Erhan Bilal's code
## Erich S. Huang
## Sage Bionetworks
## erich.huang@sagebase.org

osloAnovaAnalysis <- function(){
  ## REQUIRE
  require(synapseClient)
  require(survcomp)
  
  ## LOAD NECESSARY DATA OBJECTS
  osloPredEnt <- loadEntity('syn1725898')
  osloScores <- osloPredEnt$objects$osloPredictions
  N <- ncol(osloScores)
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
  
  ## AGE ANOVA
  y <- array(0, c(2*N, 1))
  x <- factor(c(array(0, c(N, 1)), array(1, c(N, 1))), labels = c(0,1))            
  for (i in 1:N) {
    iCciResultA <- concordance.index(osloScores[age == 0, i], clinicalSurvData[age == 0, 1], clinicalSurvData[age == 0, 2])
    y[i] <- iCciResultA$c.index
    iCciResultB <- concordance.index(osloScores[age == 1, i], clinicalSurvData[age == 1, 1], clinicalSurvData[age == 1, 2])
    y[N+i] <- iCciResultB$c.index
  }
  a <- anova(lm(y ~ x))
  age_pvar <- 100 * a[1,2] / sum(a[,2])
  message("% variance age: ", age_pvar)
  message("p-value: ", a[1,5])
  


}



#########
# varLevels <- levels(age)
# nVarLevels <- length(levels(age))
# yMat <- matrix(data = NA, nrow = , ncol = nVarLevels)
# 
# y1 <- rep(NA, ncol(osloScores))
# y2 <- rep(NA, ncol(osloScores))
# x <- as.factor(c(rep(0, ncol(osloScores)), rep(1, ncol(osloScores))))
# 
# varANOVA <- function(clinicalVec){
#   varLevels <- levels(clinicalVec)
#   foo <- lapply(varLevels, function(varLevel){
#     cciResult <- concordance.index(osloScores[clinicalVec == 0, ])
#   })
# }

foo <- apply(osloScores, 2, function(x){
  
})


