## provFigureThree.R

## Erich S. Huang
## Sage Bionetworks
## erich.huang@sagebase.org

## REQUIRE 
require(synapseClient)
require(rGithubClient)

## SOURCE CONVENIENCE FUNCTIONS
sourceRepoFile('erichhuang/rStartup', 'startupFunctions.R')

## GET CLEARSCIENCE REPO 
clearSciRepo <- getRepo('sage-bionetworks/STM-metaAnalysis-clearScience/')
sourceRepoFile(clearSciRepo, 'functions/figureThree.R')

## GET INPUT OBJECTS
osloEnt <- getEntity('syn1725898') # matrix of predictions 
osloSurvEnt <- getEntity('syn1710257') # survival object

## RUN THE FUNCTION
figOut <- figureThree()

fig3Ent <- synPut(figOut$rankSampleBoxPlot, parentId ='syn1763719')
fig3PngEnt <- File('figureBinaries/figureThree.png', parentId = 'syn1763719')
fig3PngEnt <- createEntity(fig3PngEnt)

fig3supA <- synPut(figOut$ciSampleBoxPlot, parentId = 'syn1763719')
fig3SupPngEnt <- File('figureBinaries/figureThreeSuppA.png', parentId = 'syn1763719')
fig3SupPngEnt <- createEntity(fig3SupPngEnt)

fig3supBEnt <- synPut(figOut$ciSampleDensityPlot, parentId = 'syn1763719')
fig3SupBPngEnt <- File('figureBinaries/figureThreeSuppB.png', parentId = 'syn1763719')
fig3SupBPngEnt <- createEntity(fig3SupBPngEnt)

fig3SupCEnt <- synPut(figOut$rankSampleDensityPlot, parentId = 'syn1763719')
fig3SupCPngEnt <- File('figureBinaries/figureThreeSuppC.png', parentId = 'syn1763719')
fig3SupCPngEnt <- createEntity(fig3SupCPngEnt)

intAEnt <- synPut(figOut$sampleRankMatrix, parentId = 'syn1738742')
intBEnt <- synPut(figOut$samplePerformanceMatrix, parentId = 'syn1738742')

## CREATE INTERMEDIATE ACTIVITY
figThreeLink <- getPermlink(clearSciRepo, 'functions/figureThree.R')
intAct <- Activity(name = "Create Model Concordance Index and Rank Subsampling Matrices",
                   used = list(
                     list(url = figThreeLink, name = basename(figThreeLink), wasExecuted = T),
                     list(entity = osloEnt, wasExecuted = F),
                     list(entity = osloSurvEnt, wasExecuted = F)
                   ))

intAct <- createEntity(intAct)

intList <- list(intAEnt, intBEnt)

generatedByList(intList, intAct)

## CREATE TERMINAL ACTIVITY
finalAct <- Activity(name = "Create Figure Three and Supplemental Figures",
                     used = list(
                       list(url = figThreeLink, name = basename(figThreeLink), wasExecuted = T),
                       list(entity = intAEnt, wasExecuted = F),
                       list(entity = intBEnt, wasExecuted = F)
                     ))

finalAct <- createEntity(finalAct)

resultList <- list(fig3Ent, fig3PngEnt, fig3supA, fig3SupPngEnt, fig3supBEnt, fig3SupBPngEnt, fig3SupCEnt, fig3SupCPngEnt)

foo <- generatedByList(resultList, finalAct)