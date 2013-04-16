## provFigureFourC.R

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
sourceRepoFile(clearSciRepo, 'functions/figureFourC.R')

## INPUT OBJECTS
metbLbEnt <- getEntity('syn1744693') # 15 October leaderboard
metbEnt <- getEntity('syn1738796') # matrix of predictions
metbSurvEnt <- getEntity('syn1125632') # survival object 

## RUN THE FUNCTION
figOut <- figureFourC()

intEnt <- synPut(figOut$mbricCommunityDataFrame, 'syn1738742')

figPngEnt <- File('figureBinaries/figureFourC.png', parentId = 'syn1763978')
figPngEnt <- createEntity(figPngEnt)
figObjEnt <- synPut(figOut$mbricCommunityBoxPlot, 'syn1763978')

## INTERMEDIATE ACTIVITY
figFourLink <- getPermlink(clearSciRepo, 'functions/figureFourC.R')
intAct <- Activity(name = "Create METABRIC Community Size Dataframe",
                   used = list(
                     list(url = figFourLink, name = basename(figFourLink), wasExecuted = T),
                     list(entity = metbEnt, wasExecuted = F),
                     list(entity = metbSurvEnt, wasExecuted = F),
                     list(entity = metbLbEnt, wasExecuted = F)
                   ))

intAct <- createEntity(intAct)

generatedBy(intEnt) <- intAct
intEnt <- storeEntity(intEnt)

## TERMINAL ACTIVITY
finalAct <- Activity(name = "Create METABRIC Community Size Boxplot",
                     used = list(
                       list(url = figFourLink, name = basename(figFourLink), wasExecuted = T),
                       list(entity = intEnt, wasExecuted = F)
                     ))

finalAct <- createEntity(finalAct)

finalList <- list(figPngEnt, figObjEnt)
foo <- generatedByList(finalList, finalAct)