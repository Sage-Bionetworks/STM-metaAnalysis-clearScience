## provFigureFourD.R

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
sourceRepoFile(clearSciRepo, 'functions/figureFourD.R')

## INPUT OBJECTS
osloEnt <- getEntity('syn1725898') # matrix of predictions 
osloSurvEnt <- getEntity('syn1710257') # survival object
osloLbEnt <- getEntity('syn1739275') # Final osloval leadeboard

## RUN THE FUNCTION
figOut <- figureFourD()

figObjEnt <- synPut(figOut$osloCommunityBoxPlot, 'syn1763978')
figPngEnt <- File('figureBinaries/figureFourD.png', parentId = 'syn1763978')
figPngEnt <- createEntity(figPngEnt)

intEnt <- synPut(figOut$osloCommunityDataFrame, 'syn1738742')

## INTERMEDIATE ACTIVITY
figFourLink <- getPermlink(clearSciRepo, 'functions/figureFourD.R')
intAct <- Activity(name = "Create OsloVal Community Size Dataframe",
                   used = list(
                     list(url = figFourLink, name = basename(figFourLink), wasExecuted = T),
                     list(entity = osloEnt, wasExecuted = F),
                     list(entity = osloSurvEnt, wasExecuted = F),
                     list(entity = osloLbEnt, wasExecuted = F)
                   ))

intAct <- createEntity(intAct)

generatedBy(intEnt) <- intAct
intEnt <- storeEntity(intEnt)

## TERMINAL ACTIVITY
finalAct <- Activity(name = "Create OsloVal Community Size Boxplot",
                     used = list(
                       list(url = figFourLink, name = basename(figFourLink), wasExecuted = T),
                       list(entity = intEnt, wasExecuted = F)
                     ))

finalAct <- createEntity(finalAct)

finalList <- list(figPngEnt, figObjEnt)
foo <- generatedByList(finalList, finalAct)

