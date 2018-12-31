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
library(reshape2)
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

# The raw utility of q widgets for a principal with an exponential utility
# and alpha preference.
expUtility <- function(alpha, q) {
  (1-exp(-alpha*q))/alpha
}

# Solves the optimal quantity under full information.
solveQ <- function(alpha, theta) {
  -log(theta) / alpha
}

# Use qVec as quantities, then calculate a column vector
# of raw utility. Then for each theta in the thetaList, generate a transfer
# and net utility column.
calcMultipleUtility <- function(qVec, alpha, thetaVec) {
  df <- data.frame(qVec, sapply(qVec, function(x) expUtility(alpha, x)))
  names(df) <- c('quantity','utility')
  for (i in seq(length(thetaVec))) {
    df[paste0('transfer', i)] <- thetaVec[i] * df['quantity']
    df[paste0('net_utility_with_agent', i)] <- df['utility'] - df[paste0('transfer', i)]
  }
  df
}

shinyServer(function(input, output) {
  output$utilityPlot <- renderPlot({
    q <- seq(1, 30, by=1)
    dfWide <- calcMultipleUtility(q, input$alpha, c(input$theta1, input$theta2))
    dfLong <- melt(dfWide, id=c("quantity"),
                   measure=c("utility", "net_utility_with_agent1", "net_utility_with_agent2"))
    colnames(dfLong) <-c("quantity", "type", "utility")
    p <- ggplot(dfLong, aes(x=quantity, y=utility, colour=type)) + geom_path()
    q1 <- solveQ(input$alpha, input$theta1)
    u1 <- expUtility(input$alpha, q1) - q1 * input$theta1
    p <- p + geom_label(label="max", aes(x=q1, y=u1), colour="green")
    q2 <- solveQ(input$alpha, input$theta2)
    u2 <- expUtility(input$alpha, q2) - q2 * input$theta2
    p <- p + geom_label(label="max", aes(x=q2, y=u2), colour="blue")
    p
  })
})
