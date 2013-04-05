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

## SHINY SERVER LOGIC ########################################################
shinyServer(function(input, output) {
  
  # Return the requested clinical subgroup
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
           'Lymph Node 1-3', = 
           'Lymph Node 4-9',
           'Lymph Node 10+',
           'Followup Time 0-5 Years',
           'Followup Time 5-10 Years',
           'Followup Time 10+ Years',
           'Age ≤ 50 Years',
           'Age 50+ Years',
           'Tumor Size 0-2 CM',
           'Tumor Size 2+ CM'
           )
  })
  
  # Show the first "n" observations
  output$view <- renderTable({
    head(sortedLeaderboard, n = input$obs)
  })
  output$graphics1 <- renderPlot({
    lbHist <- ggplot(sortedLeaderboard[1:input$obs, ], aes(x = score)) + geom_histogram(alpha = 0.3) +
      ggtitle('Histogram of Windowed Submission Scores')
    show(lbHist)
  })
  output$graphics2 <- renderPlot({
    lbBoxplot <- ggplot(sortedLeaderboard[1:input$obs, ], aes(x = factor(round), y = score)) + 
      geom_boxplot() + 
      geom_jitter(aes(colour = factor(round)), size = 4) +
      ggtitle('Boxplot of Windowed Submission Scores')
    show(lbBoxplot)
  })
  output$graphics3 <- renderPlot({
    lbDensplot <- ggplot(sortedLeaderboard[1:input$obs, ], aes(score, fill = factor(round))) + 
      geom_density(alpha = 0.3) + 
      ggtitle('Density Plot of Windowed Submission Scores')
    show(lbDensplot)
  })
  
})