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


