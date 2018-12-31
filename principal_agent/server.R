#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(ggplot2)
#library(Cairo)   # For nicer ggplot2 output when deployed on Linux
library(shiny)

# Treat q as a column vector of quantities, then calculate a column vector
# of transfer and utility.
calculateUtility <- function(q, alpha, theta) {
  df <- data.frame(q, sapply(q, function(x) theta * x))
  names(df) <- c('quantity','transfer')
  df['utility'] <- (1-exp(-alpha*df$q))/alpha - df$transfer
  df
}

shinyServer(function(input, output) {
  output$utilityPlot <- renderPlot({
    q <- seq(1, 30, by=1)
    df <- calculateUtility(q, input$alpha, input$theta)
    ggplot(df, aes(quantity, utility)) + geom_path()
  })
  
})
