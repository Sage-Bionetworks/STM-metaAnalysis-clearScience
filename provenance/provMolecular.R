## provMolecular.R

## Erich S. Huang
## Sage Bionetworks
## erich.huang@sagebase.org

## The QC and Normalization of the METABRIC and OsloVal molecular data was done prior
## to much of the clearScience infrastucture. For now we will provenance them as
## "Supervised SNM Quality Control and Normalization: Brigham Mecham"

## REQUIRE 
require(synapseClient)
require(rGithubClient)

## SOURCE CONVENIENCE FUNCTIONS
sourceRepoFile('erichhuang/rStartup', 'startupFunctions.R')

## PULL DOWN THE DATA OBJECTS VIA SYNAPSE QUERY
projTable <- folderContents('syn1710250')

#              entity.name  entity.id
# 1  metabricClinicalTable syn1710260
# 2        metabricCnaData syn1710262
# 3    metabricDssSurvData syn1730400
# 4       metabricExprData syn1710275
# 5       metabricSurvData syn1710277
# 6   metabricVal2SurvData syn1744695
# 7   oslovalClinicalTable syn1710251
# 8         oslovalCnaData syn1710253
# 9        oslovalExprData syn1710255
# 10       oslovalSurvData syn1710257
# 11       stmMetaAnalysis syn1725922

projIdVector <- projTable$entity.id[1:10]

projIdEntList <- lapply(projIdVector, getEntity)

## CREATE THE ACTIVITY
qcActivity <- Activity(list(name = "SNM Supervised Normalization and QC Activities"))
qcActivity <- createEntity(qcActivity)

generatedByList(projIdEntList, qcActivity)
qcActivity <- getActivity('1768396')
qcActivity@properties$description <- 'Actors: Brigham Mecham, Adam Margolin, Erich S. Huang, Erhan Bilal'
qcActivity <- updateEntity(qcActivity)
