## provLeaderboardObjects.R

## Erich S. Huang
## Sage Bionetworks
## erich.huang@sagebase.org

## Manually defining that these data objects were generated from the leaderboard infrastructure prior
## to the provenance services API.

## REQUIRE 
require(synapseClient)
require(rGithubClient)

## A CONVENIENCE FUNCTION
sourceRepoFile('erichhuang/rStartup', 'startupFunctions.R')

## 
infrastructureActivity <- Activity(list(name = "Pre-15 October Leaderboard Infrastructure"))
infrastructureActivity <- createEntity(infrastructureActivity) # 1761417

preLBEnt <- synGet('syn1745570', load=TRUE)
generatedBy(preLBEnt) <- infrastructureActivity
preLBEnt <- storeEntity(preLBEnt)

infraActivity <- Activity(list(name = "Leaderboard Infrastructure"))
infraActivity <- createEntity(infraActivity) # 1761430
oct15LBEnt <- synGet('syn1744693', load=TRUE)
finalLBEnt <- synGet('syn1739275', load=TRUE)
lbList <- list(oct15LBEnt, finalLBEnt)

generatedByList(lbList, infraActivity)

