## provFigureTwoB.R

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
sourceRepoFile(clearSciRepo, 'functions/figureTwoB.R')

## INPUT OBJECTS
fifteenOctLBEnt <- getEntity('syn1744693')
pre15OctLBEnt <- getEntity('syn1745570')
finalLBEnt <- getEntity('syn1739275')

## RUN THE FUNCTION
figOut <- figureTwoB()
figPngEnt <- File('figureBinaries/figureTwoB.png', name = 'figureTwoB.png', parentId = 'syn1750004')
figObjEnt <- synPut(figOut, 'syn1750004')

## DEFINE THE ACTIVITY
figTwoBLink <- getPermlink(clearSciRepo, 'functions/figureTwoB.R')
figAct <- Activity(name = "Create Density Plot of Model Performance by Challenge Phase",
                   used = list(
                     list(url = figTwoBLink, name = basename(figTwoBLink), wasExecuted = T),
                     list(entity = pre15OctLBEnt, wasExecuted = F),
                     list(entity = fifteenOctLBEnt, wasExecuted = F),
                     list(entity = finalLBEnt, wasExecuted = F)
                   ))
figAct <- createEntity(figAct)

outputList <- list(figPngEnt, figObjEnt)
foo <- generatedByList(outputList, figAct)
