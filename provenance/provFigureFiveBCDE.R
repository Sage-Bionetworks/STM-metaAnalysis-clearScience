## provFigureFiveBCDE.R

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
sourceRepoFile(clearSciRepo, 'functions/figureFiveBCDE.R')

## INPUT OBJECTS
clinEnt <- getEntity('syn1710251')
osloVecEnt <- getEntity('syn1725898')
survEnt <- getEntity('syn1710257')

## INTERMEDIATE OBJECTS (ALREADY CREATED)
clinLogicalEnt <- getEntity('syn1738276')
clinSubEnt <- getEntity('syn1738278')

## RUN FUNCTION
figOut <- figureFiveBCDE()

figBPngEnt <- File('figureBinaries/figureFiveB.png', parentId = 'syn1766378')
figBPngEnt <- createEntity(figBPngEnt)
figBObjEnt <- synPut(figOut$gradeBoxPlot, parentId = 'syn1766378')

figCPngEnt <- File('figureBinaries/figureFiveC.png', parentId = 'syn1766378')
figCPngEnt <- createEntity(figCPngEnt)
figCObjEnt <- synPut(figOut$lymphNodeBoxPlot, parentId = 'syn1766378')

figDPngEnt <- File('figureBinaries/figureFiveD.png', parentId = 'syn1766378')
figDPngEnt <- createEntity(figDPngEnt)
figDObjEnt <- synPut(figOut$timeBoxPlot, parentId = 'syn1766378')

figEPngEnt <- File('figureBinaries/figureFiveE.png', parentId = 'syn1766378')
figEPngEnt <- createEntity(figEPngEnt)
figEObjEnt <- synPut(figOut$clincalBoxPlot, parentId = 'syn1766378')

## INTERMEDIATE ACTIVITY
figFiveLink <- getPermlink(clearSciRepo, 'functions/figureFiveBCDE.R')
intAct <- Activity(name = "Data Preparation for Model Performance by Clinical Characteristic",
                   used = list(
                     list(url = figFiveLink, name = basename(figFiveLink), wasExecuted = T),
                     list(entity = clinEnt, wasExecuted = F),
                     list(entity = osloVecEnt, wasExecuted = F),
                     list(entity = survEnt, wasExecuted = F)
                   ))

intAct <- createEntity(intAct)

intList <- list(clinLogicalEnt, clinSubEnt)
generatedByList(intList, intAct)

## TERMINAL ACTIVITY
finalAct <- Activity(name = "Create Model Performance by Clinical Characteristic Boxplots",
                     used = list(
                       list(url = figFiveLink, name = basename(figFiveLink), wasExecuted = T),
                       list(entity = clinLogicalEnt, wasExecuted = F),
                       list(entity = clinSubEnt, wasExecuted = F)
                     ))

finalAct <- createEntity(finalAct)

finalList <- list(figBObjEnt, figBPngEnt, figCObjEnt, figCPngEnt,
                  figDObjEnt, figDPngEnt, figEObjEnt, figEPngEnt)
foo <- generatedByList(finalList, finalAct)