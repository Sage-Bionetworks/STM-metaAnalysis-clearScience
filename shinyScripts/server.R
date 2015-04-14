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
oslovalLbEnt <- synGet('syn1739275', load=TRUE)
cciEnt <- synGet('syn1738278', load=TRUE)

oslovalLB <- oslovalLbEnt@objects$object

oslovalLB$MODEL <- strtrim(oslovalLB$MODEL, 30)

rownames(oslovalLB) <- oslovalLB$SYNID
cciGroupList <- cciEnt@objects$object

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
subgroupDFList <- lapply(sGroupList, subgroupLbGenerator)

# set up a dataframe for boxplots
boxplotDF <- data.frame('category' = rep('allPatients', nrow(oslovalLB)), 
                        'scores' = oslovalLB$CCI)

## SHINY SERVER LOGIC ########################################################
shinyServer(function(input, output) {
  
  # User selects (inputs) the requested clinical subgroup
  datasetInput <- reactive({
    switch(input$clinCovar,
           "All Patients" = oslovalLB,
           "ER Positive" = subgroupDFList$erPos,
           "ER Negative" = subgroupDFList$erNeg,
           'HER2 Positive' = subgroupDFList$her2Pos,
           'HER2 Negative' = subgroupDFList$her2Neg,
           'PR Positive' = subgroupDFList$prPos,
           'PR Negative' = subgroupDFList$prNeg,
           'Histologic Grade 1' = subgroupDFList$grade1,
           'Histologic Grade 2' = subgroupDFList$grade2,
           'Histologic Grade 3' = subgroupDFList$grade3,
           'Lymph Node Negative' = subgroupDFList$lnNeg,
           'Lymph Node 1-3' = subgroupDFList$ln1to3,
           'Lymph Node 4-9' = subgroupDFList$ln4to9,
           'Lymph Node 10+' = subgroupDFList$ln10plus,
           'Followup Time 0-5 Years' = subgroupDFList$time0to5,
           'Followup Time 5-10 Years' = subgroupDFList$time5to10,
           'Followup Time 10+ Years' = subgroupDFList$time10plus,
           'Age ≤ 50 Years' = subgroupDFList$preMeno,
           'Age 50+ Years' = subgroupDFList$postMeno,
           'Tumor Size 0-2 CM' = subgroupDFList$size0to2,
           'Tumor Size 2+ CM' = subgroupDFList$size2plus)
  })
  
  # Server outputs
  output$tableView <- renderTable({
    datasetInput()[1:10, 1:4]
  })
  
  output$graphics1 <- renderPlot({
    lbTable <- datasetInput()
    subgroupDF <- data.frame('category' = rep('clinicalSubgroup', nrow(lbTable)), 
                             'scores' = lbTable$CCI)
    boxplotDF <- rbind(boxplotDF, subgroupDF)
    
    class(boxplotDF$scores) <- 'numeric'
    
    lbBoxplot <- ggplot(boxplotDF, aes(x = factor(category), y = scores)) + 
      geom_boxplot() + 
      geom_jitter(aes(colour = factor(category)), size = 7, alpha = 0.7) +
      ggtitle(paste('All Patients (Red) versus ', 
                    toupper(input$clinCovar),
                    ' (Blue)\n', sep = '')) +
      ylab('Concordance Index\n') + xlab('\nCategory') +
      theme(legend.position = 'none')
    show(lbBoxplot)
  })
  
  output$graphics2 <- renderPlot({
    lbTable <- datasetInput()
    subgroupDF <- data.frame('category' = rep('clinicalSubgroup', nrow(lbTable)), 
                             'scores' = lbTable$CCI)
    boxplotDF <- rbind(boxplotDF, subgroupDF)
    
    class(boxplotDF$scores) <- 'numeric'
    
    lbDenseplot <- ggplot(boxplotDF, aes(scores, fill = factor(category))) + 
      geom_density(alpha = 0.3) +
      ggtitle(paste('All Patients (Red) versus ', 
                    toupper(input$clinCovar), 
                    ' (Blue)\n', sep = '')) +
      ylab('Concordance Index\n') + xlab('\nCategory') +
      theme(legend.position = 'none')
    show(lbDenseplot)
  })
  
})

# 
# 
# output$graphics3 <- renderPlot({
#   lbDensplot <- ggplot(sortedLeaderboard[1:input$obs, ], aes(score, fill = factor(round))) + 
#     geom_density(alpha = 0.3) + 
#     ggtitle('Density Plot of Windowed Submission Scores\n')
#   show(lbDensplot)
# })