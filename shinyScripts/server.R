## server.R

## Erich S. Huang
## Sage Bionetworks
## Seattle, Washington
## erich.huang@sagebase.org

library(shiny)
library(synapseClient)
library(ggplot2)
library(plyr)

## DATA LOAD FROM SYNAPSE
oslovalLbEnt <- loadEntity('syn1739275')
cciEnt <- loadEntity('syn1738278')

oslovalLB <- oslovalLbEnt$objects$object
rownames(oslovalLB) <- oslovalLB$SYNID
cciGroupList <- cciEnt$objects$object

# pre-sort (descending) the cciGroup vectors
sGroupList <- lapply(cciGroupList, sort, decreasing = TRUE)

subgroupLbGenerator <- function(subgroupVector){
  orderedIDs <- names(subgroupVector)
  subgroupLB <- data.frame('ORIGINALRANK' = oslovalLB[orderedIDs, 'RANK'],
                           'CCI' = subgroupVector,
                           'MODEL' = oslovalLB[orderedIDs, 'MODEL'],
                           'CREATOR' = oslovalLB[orderedIDs, 'CREATOR'])
}

# pre-define sub-grouped leaderboards
erPosLB <- subgroupLbGenerator(sGroupList$erPos)
erNegLB <- subgroupLbGenerator(sGroupList$erNeg)
her2PosLB <- subgroupLbGenerator(sGroupList$her2Pos)
her2NegLB <- subgroupLbGenerator(sGroupList$her2Neg)
prPosLB <- subgroupLbGenerator(sGroupList$prPos)
prNegLB <- subgroupLbGenerator(sGroupList$prNeg)
grade1LB <- subgroupLbGenerator(sGroupList$grade1)
grade2LB <- subgroupLbGenerator(sGroupList$grade2)
grade3LB <- subgroupLbGenerator(sGroupList$grade3)
lnNegLB <- subgroupLbGenerator(sGroupList$lnNeg)
lnN1LB <- subgroupLbGenerator(sGroupList$ln1to3)
lnN2LB <- subgroupLbGenerator(sGroupList$ln4to9)
lnN3LB <- subgroupLbGenerator(sGroupList$ln10plus)
timeALB <- subgroupLbGenerator(sGroupList$time0to5)
timeBLB <- subgroupLbGenerator(sGroupList$time5to10)
timeCLB <- subgroupLbGenerator(sGroupList$time10plus)
ageALB <- subgroupLbGenerator(sGroupList$preMeno)
ageBLB <- subgroupLbGenerator(sGroupList$postMeno)
sizeALB <- subgroupLbGenerator(sGroupList$size0to2)
sizeBLB <- subgroupLbGenerator(sGroupList$size2plus)

# set up a dataframe for boxplots
boxplotDF <- data.frame('category' = rep('allPatients', nrow(oslovalLB)), 
                        'scores' = oslovalLB$CCI)

## SHINY SERVER LOGIC ########################################################
shinyServer(function(input, output) {
  
  # User inputs the requested clinical subgroup
  datasetInput <- reactive({
    switch(input$clinCovar,
           "All Patients" = oslovalLB,
           "ER Positive" = erPosLB,
           "ER Negative" = erNegLB,
           'HER2 Positive' = her2PosLB,
           'HER2 Negative' = her2NegLB,
           'PR Positive' = prPosLB,
           'PR Negative' = prNegLB,
           'Histologic Grade 1' = grade1LB,
           'Histologic Grade 2' = grade2LB,
           'Histologic Grade 3' = grade3LB,
           'Lymph Node Negative' = lnNegLB,
           'Lymph Node 1-3' = lnN1LB,
           'Lymph Node 4-9' = lnN2LB,
           'Lymph Node 10+' = lnN3LB,
           'Followup Time 0-5 Years' = timeALB,
           'Followup Time 5-10 Years' = timeBLB,
           'Followup Time 10+ Years' = timeCLB,
           'Age ≤ 50 Years' = ageALB,
           'Age 50+ Years' = ageBLB,
           'Tumor Size 0-2 CM' = sizeALB,
           'Tumor Size 2+ CM' = sizeBLB)
  })
  
  # Server outputs
  output$tableView <- renderTable({
    datasetInput()
  })
  
  output$graphics1 <- renderPlot({
    lbTable <- datasetInput()
    subgroupDF <- data.frame('category' = rep('clinicalSubgroup', nrow(lbTable)), 
                             'scores' = lbTable$CCI)
    boxplotDF <- rbind(boxplotDF, subgroupDF)
    
    class(boxplotDF$scores) <- 'numeric'
    
    lbBoxplot <- ggplot(boxplotDF, aes(x = factor(category), y = scores)) + 
      geom_boxplot() + 
      geom_jitter(aes(colour = factor(category)), size = 4) +
      ggtitle('All Patients versus Clinical Subgroup')
    show(lbBoxplot)
  })
  
})