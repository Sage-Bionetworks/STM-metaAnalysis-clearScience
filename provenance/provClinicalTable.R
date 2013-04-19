## provClinicalTable.R

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
sourceRepoFile(clearSciRepo, 'functions/clinicalTable.R')

## INPUT OBJECTS
mbricEnt <- getEntity('syn1710260')
osloClinEnt <- getEntity('syn1710251')
osloSurvEnt <- getEntity('syn1710257')

## OUTPUT OBJECT
tableEnt <- getEntity('syn1768409')

## ACTIVITY
codeLink <- getPermlink(clearSciRepo, 'functions/clinicalTable.R')
tableAct <- Activity(name = 'Generate Clinical Table',
                     used = list(
                       list(url = codeLink, name = basename(codeLink), wasExecuted = T),
                       list(entity = mbricEnt, wasExecuted = F),
                       list(entity = osloClinEnt, wasExecuted = F),
                       list(entity = osloSurvEnt, wasExecuted = F)
                       ))

tableAct <- createEntity(tableAct)
generatedBy(tableEnt) <- tableAct
tableEnt <- storeEntity(tableEnt)