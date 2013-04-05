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

# set up a dataframe for boxplots
boxplotDF <- data.frame('category' = rep('allPatients', nrow(oslovalLB)), 
                        'scores' = oslovalLB$CCI)

## SHINY SERVER LOGIC ########################################################
shinyServer(function(input, output) {
  
  # User inputs the requested clinical subgroup
  datasetInput <- reactive(function() {
    switch(input$clinCovar,
           "All Patients" = oslovalLB,
           "ER Positive" = oslovalLB[names(sGroupList$erPos), ],
           "ER Negative" = oslovalLB[names(sGroupList$erNeg), ],
           'HER2 Positive' = oslovalLB[names(sGroupList$her2Pos), ],
           'HER2 Negative' = oslovalLB[names(sGroupList$her2Neg), ],
           'PR Positive' = oslovalLB[names(sGroupList$prPos), ],
           'PR Negative' = oslovalLB[names(sGroupList$prNeg), ],
           'Histologic Grade 1' = oslovalLB[names(sGroupList$grade1), ],
           'Histologic Grade 2' = oslovalLB[names(sGroupList$grade2), ],
           'Histologic Grade 3' = oslovalLB[names(sGroupList$grade3), ],
           'Lymph Node Negative' = oslovalLB[names(sGroupList$lnNeg), ],
           'Lymph Node 1-3' = oslovalLB[names(sGroupList$ln1to3), ],
           'Lymph Node 4-9' = oslovalLB[names(sGroupList$ln4to9), ],
           'Lymph Node 10+' = oslovalLB[names(sGroupList$ln10plus), ],
           'Followup Time 0-5 Years' = oslovalLB[names(sGroupList$time0to5), ],
           'Followup Time 5-10 Years' = oslovalLB[names(sGroupList$time5to10), ],
           'Followup Time 10+ Years' = oslovalLB[names(sGroupList$time10plus), ],
           'Age ≤ 50 Years' = oslovalLB[names(sGroupList$preMeno), ],
           'Age 50+ Years' = oslovalLB[names(sGroupList$postMeno), ],
           'Tumor Size 0-2 CM' = oslovalLB[names(sGroupList$size0to2), ],
           'Tumor Size 2+ CM' = oslovalLB[names(sGroupList$size2plus), ]
           )
  })
  
  # Server outputs
  lbTable <- datasetInput()
  
  output$tableView <- renderTable({
    lbTable
  })
  
  output$graphics1 <- renderPlot({
    subgroupDF <- data.frame('category' = rep('clinicalSubgroup', 83), 
                             'scores' = lbTable$CCI)
    boxplotDF <- rbind(boxplotDF, subgroupDF)
    lbBoxplot <- ggplot(boxplotDF, aes(x = factor(category), y = scores)) + 
      geom_boxplot() + 
      geom_jitter(aes(colour = factor(category)), size = 4) +
      ggtitle('Boxplot of Model Scores by Clinical Subgroup versus All Patients')
    show(lbBoxplot)
  })
  
})