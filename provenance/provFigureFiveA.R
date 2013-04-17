## provFigureFiveA.R

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
sourceRepoFile(clearSciRepo, 'functions/figureFiveA.R')

## INPUT OBJECTS
clinGroupCciEnt <- getEntity('syn1738278')

## RUN FUNCTION
figOut <- figureFiveA()

intEnt <- synPut(figOut$anovaDF, parentId = 'syn1738742')
figObjEnt <- synPut(figOut$anovaBarPlot, parentId = 'syn1766378')
figPngEnt <- File('figureBinaries/figureFiveA.png', parentId = 'syn1766378')
figPngEnt <- createEntity(figPngEnt)

## INTERMEDIATE ACTIVITY
figFiveLink <- getPermlink(clearSciRepo, 'functions/figureFiveA.R')
intAct <- Activity(name = "Perform One Way ANOVA of Model Performance by Clinical Characteristics",
                   used = list(
                     list(url = figFiveLink, name = basename(figFiveLink), wasExecuted = T),
                     list(entity = clinGroupCciEnt, wasExecuted = F)
                   ))

intAct <- createEntity(intAct)

generatedBy(intEnt) <- intAct
intEnt <- updateEntity(intEnt)

## TERMINAL ACTIVITY
finalAct <- Activity(name = "Create ANOVA Of Clinical Characteristics Boxplot",
                     used = list(
                       list(url = figFiveLink, name = basename(figFiveLink), wasExecuted = T),
                       list(entity = intEnt, wasExecuted = F)
                     ))

finalAct <- createEntity(finalAct)

finalList <- list(figPngEnt, figObjEnt)
foo <- generatedByList(finalList, finalAct)