## provFigureOne.R

## Erich S. Huang
## Sage Bionetworks
## erich.huang@sagebase.org

## Defining provenance of Figure One

## REQUIRE
require(synapseClient)

fig1Act <- Activity(list(name = 'Manual Figure Generation'))
fig1Act <- createEntity(fig1Act)

fig1Ent <- getEntity('syn1759452')
generatedBy(fig1Ent) <- fig1Act
fig1Ent <- storeEntity(fig1Ent)