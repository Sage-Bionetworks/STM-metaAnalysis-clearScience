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

## SHINY SERVER LOGIC
shinyServer(function(input, output) {
  
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