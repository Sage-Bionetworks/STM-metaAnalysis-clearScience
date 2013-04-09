require(doMC)
registerDoMC()

## LOAD NECESSARY OBJECTS
osloEnt <- loadEntity('syn1725898') # matrix of predictions 
osloScores <- osloEnt$objects$osloPredictions

osloSurvEnt <- loadEntity('syn1710257') # survival object
osloSurv <- osloSurvEnt$objects$oslovalSurvData
rownames(osloSurv) <- rownames(osloScores)

## FOR USING THE MCLAPPLY FUNCTION, CONVERT TO LIST
osloPredList <- mclapply(seq_len(ncol(osloScores)), function(i){osloScores[ , i]})
names(osloPredList) <- colnames(osloScores)

## CREATE A SAMPLING MATRIX
set.seed(130409)
samplePerfMatrix <- matrix(NA, 83, 100)
for(i in 1:5){
  iSamplePerfList <- mclapply(osloPredList, function(modelPredVec){
    sampleVec <- sample(1:184, 0.8*nrow(osloScores))
    modelSamp <- modelPredVec[sampleVec]
    trueTimeSamp <- osloSurv[sampleVec, 1]
    trueEventSamp <- osloSurv[sampleVec, 2]
    sampleCi <- concordance.index(modelSamp, trueTimeSamp, trueEventSamp)
    sampleCi$c.index
  })
  iSamplePerfVec <- do.call('rbind', iSamplePerfList)
  samplePerfMatrix[ , i] <- iSamplePerfVec
}






# samplingList <- vector('list', 100)
# samplingList <- mclapply(samplingList, function(x){rbinom(0.8*nrow(osloScores), 1, 0.5)})
# 
# 
# 
# foo <- mclapply(sampleMat, function(sampleVec))