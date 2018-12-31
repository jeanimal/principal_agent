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

# DEPRECATED.
# Treat q as a column vector of quantities, then calculate a column vector
# of transfer and utility.
calculateUtility <- function(q, alpha, theta) {
  df <- data.frame(q, sapply(q, function(x) theta * x))
  names(df) <- c('quantity','transfer')
  df['utility'] <- (1-exp(-alpha*df$q))/alpha - df$transfer
  df
}

# Treat q as a column vector of quantities, then calculate a column vector
# of raw utility. Then for each theta in the thetaList, generate a transfer
# and net utility column.
calcMultipleUtility <- function(q, alpha, thetaList) {
  df <- data.frame(q, sapply(q, function(x) (1-exp(-alpha*x))/alpha))
  names(df) <- c('quantity','utility')
  for (i in seq(length(thetaList))) {
    df[paste0('transfer', i)] <- thetaList[i] * df['quantity']
    df[paste0('utility', i)] <- df['utility'] - df[paste0('transfer', i)]
  }
  df
}

shinyServer(function(input, output) {
  output$utilityPlot <- renderPlot({
    q <- seq(1, 30, by=1)
    df <- calcMultipleUtility(q, input$alpha, c(input$theta))
    ggplot(df, aes(quantity, utility1)) + geom_path()
  })
  
})
