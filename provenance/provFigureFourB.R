## provFigureFourB.R

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
sourceRepoFile(clearSciRepo, 'functions/figureFourB.R')

## INPUT OBJECTS
osloEnt <- getEntity('syn1725898') # matrix of predictions 
osloSurvEnt <- getEntity('syn1710257') # survival object
osloLbEnt <- getEntity('syn1739275') # Final osloval leadeboard

## RUN THE CODE
figOut <- figureFourB()

intEnt <- synPut(figOut$oslovalDataFrame, parentId = 'syn1738742')
figPngEnt <- File('figureBinaries/figureFourB.png', parentId = 'syn1763978')
figPngEnt <- createEntity(figPngEnt)
figObjEnt <- synPut(figOut$osloMetaPlot, parentId = 'syn1763978')

## INTERMEDIATE ACTIVITY
figFourLink <- getPermlink(clearSciRepo, 'functions/figureFourB.R')
intAct <- Activity(name = "Create OsloVal Individual and Community Score Dataframe",
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
finalAct <- Activity(name = "Create OsloVal Individual and Community Score Plot",
                     used = list(
                       list(url = figFourLink, name = basename(figFourLink), wasExecuted = T),
                       list(entity = intEnt, wasExecuted = F)
                     ))

finalAct <- createEntity(finalAct)

finalList <- list(figPngEnt, figObjEnt)
foo <- generatedByList(finalList, finalAct)
