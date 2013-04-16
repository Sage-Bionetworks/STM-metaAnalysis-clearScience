## provFigureFourA.R

## REQUIRE 
require(synapseClient)
require(rGithubClient)

## SOURCE CONVENIENCE FUNCTIONS
sourceRepoFile('erichhuang/rStartup', 'startupFunctions.R')

## GET CLEARSCIENCE REPO 
clearSciRepo <- getRepo('sage-bionetworks/STM-metaAnalysis-clearScience/')
sourceRepoFile(clearSciRepo, 'functions/figureFourA.R')

## INPUT OBJECTS
metbEnt <- getEntity('syn1738796') # matrix of predictions
metbSurvEnt <- getEntity('syn1125632') # survival object
metbLbEnt <- getEntity('syn1744693') # 15 October leaderboard

## RUN THE CODE
figOut <- figureFourA()
intEnt <- synPut(figOut, 'syn1738742')
figPngEnt <- File('figureBinaries/figureFourA.png', parentId = 'syn1763978')
figPngEnt <- createEntity(figPngEnt)
figObjEnt <- synPut(figOut$mbricMetaPlot, 'syn1763978')

## INTERMEDIATE ACTIVITY
figFourLink <- getPermlink(clearSciRepo, 'functions/figureFourA.R')
intAct <- Activity(name = "Create METABRIC Individual and Community Score Dataframe",
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
finalAct <- Activity(name = "Create METABRIC Individual and Community Score Plot",
                   used = list(
                     list(url = figFourLink, name = basename(figFourLink), wasExecuted = T),
                     list(entity = intEnt, wasExecuted = F)
                   ))

finalAct <- createEntity(finalAct)

finalList <- list(figPngEnt, figObjEnt)
foo <- generatedByList(finalList, finalAct)