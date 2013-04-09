require(doMC)
registerDoMC()

## LOAD NECESSARY OBJECTS
osloEnt <- loadEntity('syn1725898') # matrix of predictions 
osloScores <- osloEnt$objects$osloPredictions

osloSurvEnt <- loadEntity('syn1710257') # survival object
osloSurv <- osloSurvEnt$objects$oslovalSurvData
rownames(osloSurv) <- rownames(osloScores)

orderedIDs <- colnames(osloScores)

## FOR USING THE MCLAPPLY FUNCTION, CONVERT TO LIST
osloPredList <- mclapply(seq_len(ncol(osloScores)), function(i){osloScores[ , i]})
names(osloPredList) <- colnames(osloScores)

## SAMPLE 80% OF THE PREDICTIONS & CALCULATE SCORES 100 TIMES
set.seed(130409)
samplePerfMatrix <- matrix(NA, 83, 100)

for(i in 1:100){
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

rownames(samplePerfMatrix) <- colnames(osloScores)
mSampPerfMat <- melt(samplePerfMatrix)

sampleRankMatrix <- apply(samplePerfMatrix, 2, function(x){rank(-x)})
mSampRankMat <- melt(sampleRankMatrix)

## GENERATE PLOT BY CONCORDANCE INDEX
ciSampleBoxPlot <- ggplot(mSampPerfMat, aes(X1, value)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.1) +
  scale_x_discrete(limits = orderedIDs)

## GENERATE PLOT BY RANK
rankSampleBoxPlot <- ggplot(mSampRankMat, aes(X1, value)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.1) +
  scale_x_discrete(limits = orderedIDs) +
  scale_y_reverse()
  

