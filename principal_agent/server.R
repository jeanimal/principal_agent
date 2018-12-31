#
# This is the server logic of a Shiny web application.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(ggplot2)
# library(Cairo)   # For nicer ggplot2 output when deployed on Linux
library(reshape2)
library(shiny)

# The raw utility of q widgets for a principal with an exponential utility
# and alpha preference.
expUtility <- function(alpha, q) {
  (1-exp(-alpha*q))/alpha
}

# Solves the optimal quantity under full information.
# Derivative of utility with respect to q: alpha * exp(-alpha * q) / alpha
# Derivative of cost with respect to q: theta
# Set these equal and solve for q.
# exp(-alpha * q) = theta
# -alpha * q = log(theta)
# q = - log(theta) / alpha
# Note that in R, "log" is the natural log.
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

# Example: createUtilityPlot(seq(0, 30, by=1), 0.1, 0.1, 0.2)
createUtilityPlot <- function(qVec, alpha, theta1, theta2) {
  dfWide <- calcMultipleUtility(qVec, alpha, c(theta1, theta2))
  # colnames(dfWide)[which(names(dfWide) == "net_utility_with_agent1")] <- "net_utility_with_efficient_agent"
  # colnames(dfWide)[which(names(dfWide) == "net_utility_with_agent2")] <- "net_utility_with_inefficient_agent"
  dfLong <- melt(dfWide, id=c("quantity"),
                 measure=c("utility", "net_utility_with_agent1", "net_utility_with_agent2"))
  colnames(dfLong) <-c("quantity", "type", "utility")
  p <- ggplot(dfLong, aes(x=quantity, y=utility, colour=type)) + geom_path()
  q1 <- solveQ(alpha, theta1)
  u1 <- expUtility(alpha, q1) - q1 * theta1
  p <- p + geom_label(label="max", aes(x=q1, y=u1), colour="green")
  q2 <- solveQ(alpha, theta2)
  u2 <- expUtility(alpha, q2) - q2 * theta2
  p <- p + geom_label(label="max", aes(x=q2, y=u2), colour="blue")
  p
}

createSolutionDataFrame <- function(alpha, theta1, theta2) {
  q1 <- solveQ(alpha, theta1)
  u1 <- expUtility(alpha, q1) - q1 * theta1
  q2 <- solveQ(alpha, theta2)
  u2 <- expUtility(alpha, q2) - q2 * theta2
  data.frame(agent=c("agent1", "agent2"), theta=c(theta1, theta2),
             quantity=c(q1, q2),
             payment=c(q1*theta1, q2*theta2), principalUtility=c(u1, u2))
}

shinyServer(function(input, output) {
  output$utilityPlot <- renderPlot({
    q <- seq(0, 30, by=1)
    createUtilityPlot(q, input$alpha, input$theta1, input$theta2)
  })
  output$solutionTable <- renderTable({
    createSolutionDataFrame(input$alpha, input$theta1, input$theta2)
  })
})