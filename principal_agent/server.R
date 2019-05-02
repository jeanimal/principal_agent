#
# This is the server logic of a Shiny web application.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("functions.R")

shinyServer(function(input, output) {
  output$utilityPlot <- renderPlot({
    q <- seq(0, 30, by=1)
    createUtilityPlot(q, input$alpha, input$theta1, input$theta2)
  })
  output$solutionTable <- renderTable({
    createSolutionDataFrame(input$alpha, input$theta1, input$theta2)
  })
  output$iutilityPlot <- renderPlot({
    q <- seq(0, 30, by=5)
    icreateUtilityContourPlotWithMaxLabel(q, input$ialpha, input$itheta1,
                                          input$itheta2, input$iproportion,
                                          xLab="q1", yLab="q2")
    #icreateUtilityPlot(q, input$ialpha, input$itheta1, input$itheta2,
    #                   input$iproportion)
  })
  output$isolutionTable <- renderTable({
    icreateSolutionDataFrame(input$ialpha, input$itheta1, input$itheta2,
                             input$iproportion)
  })
  output$solutionTableSyncToI <- renderTable({
    # Use the non-i data frame with i inputs.
    createSolutionDataFrame(input$ialpha, input$itheta1, input$itheta2)
  })
})