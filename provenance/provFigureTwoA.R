## provFigureTwoA.R

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
sourceRepoFile(clearSciRepo, 'functions/figureTwoA.R')

## INPUT FILES
pre15OctLbEnt <- getEntity('syn1745570')

## GENERATE CODE OUTPUT
figTwoAOutput <- figureTwoA()
intEnt <- synPut(figTwoAOutput$dateVector, 'syn1738742')

## DEFINE ACTIVITY FOR INTERMEDIATE DATA
figTwoALink <- getPermlink(clearSciRepo, 'functions/figureTwoA.R')
intAct <- Activity(name = "Create Intermediate Date Vector",
                         used = list(
                           list(url = figTwoALink, name = basename(figTwoALink), wasExecuted = T),
                           list(entity = pre15OctLbEnt, wasExecuted = F)
                         ))
intAct <- createEntity(intAct)

generatedBy(intEnt) <- intAct
intEnt <- storeEntity(intEnt)

## DEFINE FIGURE OUTPUT PROV
figPngEnt <- File('figureBinaries/figureTwoA.png', name = 'figureTwoA.png', parentId = 'syn1750004')
figPngEnt <- storeEntity(figPngEnt)

figObjEnt <- synPut(figTwoAOutput$evolutionPlot, 'syn1750004')

finalAct <- Activity(name = "Generate Figure",
                     used = list(
                       list(url = figTwoALink, name = basename(figTwoALink), wasExecuted = T),
                       list(entity = intEnt, wasExecuted = F)))
finalAct <- storeEntity(finalAct)

outputList <- list(figPngEnt, figObjEnt)
foo <- generatedByList(outputList, finalAct)