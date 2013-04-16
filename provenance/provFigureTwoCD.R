## provFigureTwoCD.R

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
sourceRepoFile(clearSciRepo, 'functions/figureTwoCD.R')

## INPUT OBJECTS
fifteenOctLBEnt <- getEntity('syn1744693')
pre15OctLBEnt <- getEntity('syn1745570')
finalLBEnt <- getEntity('syn1739275')

## RUN THE FUNCTION
figOut <- figureTwoCD()

figDPngEnt <- File('figureBinaries/figureTwoD.png', parentId = 'syn1750004')
figDPngEnt <- createEntity(figDPngEnt)
figDObjEnt <- synPut(figOut$scatterPreOctober, 'syn1750004')

figCPngEnt <- File('figureBinaries/figureTwoC.png', parentId = 'syn1750004')
figCPngEnt <- createEntity(figCPngEnt)
figCObjEnt <- synPut(figOut$scatterPreFinal, 'syn1750004')

## DEFINE THE ACTIVITY
figTwoCDLink <- getPermlink(clearSciRepo, 'functions/figureTwoCD.R')
figAct <- Activity(name = "Create Scatterplots of Challenge Phases",
                   used = list(
                     list(url = figTwoCDLink, name = basename(figTwoCDLink), wasExecuted = T),
                     list(entity = pre15OctLBEnt, wasExecuted = F),
                     list(entity = fifteenOctLBEnt, wasExecuted = F),
                     list(entity = finalLBEnt, wasExecuted = F)
                   ))
figAct <- createEntity(figAct)

outputList <- list(figPngEnt, figObjEnt)
foo <- generatedByList(outputList, figAct)