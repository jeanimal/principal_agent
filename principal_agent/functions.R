# Functions that create data tables and ggplot2 plots of principal-agent
# models.  These are adverse selection models in Laffont and Mortimort,
# 2012, The Theory of Incentives.
#
# Example usage:
#
# source("principal_agent/functions.R")
#
# # Complete information
# createSolutionDataFrame(0.1, 0.1, 0.2)
# createUtilityPlot(seq(0, 30, by=1), 0.1, 0.1, 0.2)
#
# # Incomplete information
# icreateSolutionDataFrame(0.1, 0.1, 0.2, 0.5)
# icreateUtilityPlot(seq(0, 30, by=5), 0.1, 0.1, 0.2, 0.5)

library(ggplot2)
# library(Cairo)   # For nicer ggplot2 output when deployed on Linux
library(reshape2)

#
# Function names beginning with "i" are specifically for the incomplete
# information case.
#

# The raw utility of q widgets for a principal with an exponential utility
# and alpha preference.
expUtility <- function(alpha, q) {
  (1-exp(-alpha*q))/alpha
}

# Solves the optimal quantity under full information.
# Derivative of utility with respect to q: 
#  alpha * exp(-alpha * q) / alpha = exp(-alpha * q)
# Derivative of cost with respect to q: theta
# Set these equal and solve for q.
# exp(-alpha * q) = theta
# -alpha * q = log(theta)
# q = - log(theta) / alpha
# Note that in R, "log" is the natural log.
solveQ <- function(alpha, theta) {
  -log(theta) / alpha
}

# Solves the quantity that achieve zero utility.
# Set (1-exp(-alpha*q))/alpha - theta*q = 0 and solve for q.
# But the solution involves Lambert's W.
# How to solve (using x in place of q):
# https://www.wolframalpha.com/input/?i=solve+for+x+(1-exp(-alpha*x))%2Falpha-theta*x%3D0
solveBreakeven <- function(alpha, theta) {
  # I don't want to make everyone install gsl just for this one function.?
  require('gsl')
  innerValue <- -exp(-1/theta)/theta
  (theta * lambert_W0(innerValue) + 1) / (alpha * theta)
}

isolveAdjustment <- function(thetaEfficient, thetaInefficient, propEfficient) {
  thetaDiff = thetaInefficient - thetaEfficient
  propEfficient/(1-propEfficient) * thetaDiff
}

# Solves for q of the inefficient agent under incomplete information, where
# two possible thetas are known and the proportion of efficient agents.
# propEfficient must be in [0, 1).
# thetaInefficient > thetaEfficient
# TEST: isolveQInefficient(0.1, 0.1, 0.2, 0.5) == 12.04 (roughly)
# # where q-SB = -ln( v/(1-v) (theta- -theta_) + theta- ) / alpha
# (sing eq. 2.29 and substituting that S' = exp(alpha * q-SB) and solving for q-SB)
isolveQInefficient <- function(alpha, thetaEfficient, thetaInefficient,
                               propEfficient) {
  adj <- isolveAdjustment(thetaEfficient, thetaInefficient, propEfficient)
  -log(adj + thetaInefficient) / alpha
}

isolveTEfficient <- function(alpha, thetaEfficient, thetaInefficient,
                             propEfficient) {
  qInefficient <- isolveQInefficient(alpha, thetaEfficient, thetaInefficient,
                                     propEfficient)
  thetaDiff = thetaInefficient - thetaEfficient
  thetaEfficient * solveQ(alpha, thetaEfficient) + thetaDiff * qInefficient
}

# Use qVec as quantities, then calculate a column vector
# of raw utility. Then for each theta in the thetaList, generate a transfer
# and net utility column.
calcMultipleUtility <- function(qVec, alpha, thetaVec, prefix='net_utility_with_agent') {
  df <- data.frame(qVec, sapply(qVec, function(x) expUtility(alpha, x)))
  names(df) <- c('quantity','utility')
  for (i in seq(length(thetaVec))) {
    df[paste0(prefix, i)] <- df['utility'] - thetaVec[i] * df['quantity']
  }
  df
}

# The principal's utility.
# See the reduced program P' on p. 42 of Laffont and Martimort 2002.
icalcUtility <- function(alpha, q1, q2, thetaEfficient, thetaInefficient,
                         propEfficient) {
  propEfficient*(expUtility(alpha, q1) - q1*thetaEfficient) +
    (1-propEfficient)*(expUtility(alpha, q2) - q2*thetaInefficient) -
    propEfficient * (thetaInefficient - thetaEfficient) * q2
}

# Example: icalcMultipleUtility(seq(0, 30, by=1), 0.1, 0.1, 0.2, 0.5)
icalcMultipleUtility <- function(qVec, alpha, thetaEfficient, thetaInefficient,
                                 propEfficient, principalKnowsType=FALSE) {
  # TODO(jean): The transfer to agent 1 (efficient)
  # is greater than in the calculation below (greater than q1 * theta1).
  df1 <- calcMultipleUtility(qVec, alpha, c(thetaEfficient))
  names(df1) <- c('q1', 'raw_u1', 'utility1')
  df2 <- calcMultipleUtility(qVec, alpha, c(thetaInefficient))
  names(df2) <- c('q2', 'raw_u2', 'utility2')
  dfc <- merge(df1, df2)
  if (principalKnowsType) {
    dfc['adj'] <- 0
  } else {
    # Incentive compatibility:
    # The adjustment is the extra wage to agent1 to prevent him from taking easy contract.
    # What he earns with industrious > what he earns with easy
    # transfer1 - theta1 * q1 > transfer2 - theta1 * q2 # not theta 2!
    # transfer1 - theta1 * q1 > theta2 * q2 - theta1 * q2
    # transfer1 > (theta2 = theta1) * q2 + theta1 * q1
    # Before, he got paid theta1 * q1, so the adjustment is (theta2 = theta1) * q2
    # This enters the princpal's profit as a cost adjustment.
    dfc['adj'] <- propEfficient * (thetaInefficient - thetaEfficient) * dfc['q2']
  }
  dfc['net_utility'] <- propEfficient*(dfc['utility1']) + 
    (1-propEfficient)*dfc['utility2'] - dfc['adj']
  dfc
}

# Example: createUtilityPlot(seq(0, 30, by=1), 0.1, 0.1, 0.2)
createUtilityPlot <- function(qVec, alpha, theta1, theta2, principalKnowsType=FALSE) {
  dfWide <- calcMultipleUtility(qVec, alpha, c(theta1, theta2), prefix="p")
  # colnames(dfWide)[which(names(dfWide) == "p1")] <- "net_utility_with_efficient_agent"
  # colnames(dfWide)[which(names(dfWide) == "p2")] <- "net_utility_with_inefficient_agent"
  dfLong <- melt(dfWide, id=c("quantity"), measure=c("p1", "p2"))
  colnames(dfLong) <-c("quantity", "type", "principal_utility")
  p <- ggplot(dfLong, aes(x=quantity, y=principal_utility, colour=type)) + geom_path()
  q1 <- solveQ(alpha, theta1)
  u1 <- expUtility(alpha, q1) - q1 * theta1
  # TODO(jean): Dynamically find the color ggplot used.
  p <- p + geom_label(label="max with agent 1", x=q1, y=u1, colour="red")
  q2 <- solveQ(alpha, theta2)
  u2 <- expUtility(alpha, q2) - q2 * theta2
  p <- p + geom_label(label="max with agent 2", x=q2, y=u2, colour="blue")
  p + theme(legend.position="none")
}

#
# Pretending the utility function = sales for the next few functions.
#

# Price = sales / quantity.  Not the same as slope.
pricePerQ <- function(alpha, q) {
  expUtility(alpha, q)/q
}

# Example:
# profit(0.1, 0.5, 7, currencyScale=10)
# [1] 15.34147
profit <- function(alpha, theta, q, currencyScale=1) {
  (expUtility(alpha, q) - theta * q) * currencyScale
}

# Example: createSalesCostProfitPlot(seq(0, 30, by=1), 0.1, 0.2)
# Pretending the utility function = sales,
# Outputs plot of sales (no cost), cost with wage theta, and profit = sales + cost
# (where cost is a negative number).  The max is labelled on the profit plot.
# Extended example:
# createSalesCostProfitPlot(seq(0, 30, by=1), 0.1, 0.5, currencyScale=10, includeBreakeven=TRUE)
# You must have the gsl library installed to use includeBreakeven=TRUE.
createSalesCostProfitPlot <- function(qVec, alpha, theta2, currencyScale=1,
                                  includeBreakeven=FALSE) {
  theta1 <- 0
  dfWide <- calcMultipleUtility(qVec, alpha, c(theta1, theta2))
  colnames(dfWide)[which(names(dfWide) == "net_utility_with_agent1")] <- "sales"
  colnames(dfWide)[which(names(dfWide) == "net_utility_with_agent2")] <- "profits"
  dfLong <- melt(dfWide, id=c("quantity"), measure=c("sales", "profits"))
  colnames(dfLong) <-c("quantity", "type", "money")
  # Order of factor levels determines legend order.  I want sales, profits, costs.
  dfLong["type"] = factor(dfLong[["type"]], levels=c("sales","profits", "costs"))
  # Add two rows-- first and last point-- to define cost line.  Assumes qVec ordered.
  dfLine <- data.frame(quantity=c(qVec[1], qVec[length(qVec)]), type=c("costs", "costs"))
  dfLine["money"] <- -theta2 * dfLine["quantity"]
  dfLong <- rbind(dfLong, dfLine)
  p <- ggplot(dfLong, aes(x=quantity, y=currencyScale*money, colour=type)) + geom_path()
  p <- p + ylab("money")
  p <- p + scale_color_manual(values=c("blue", "purple", "red"))
  q2 <- solveQ(alpha, theta2)
  u2 <- currencyScale*(expUtility(alpha, q2) - q2 * theta2)
  p <- p + geom_label(label="max", x=q2, y=u2, colour="purple")
  if (includeBreakeven) {
    qBreakEven <- solveBreakeven(alpha, theta2)
    p <- p + geom_label(label="breakeven", x=qBreakEven, y=0, colour="purple")
  }
  p
}

# Example: createTwoAgentProfitPlot(seq(0, 30, by=1), 0.1, 0.1, 0.2)
# Pretending the utility function = sales....
# Outputs plots of principal profit using agent1 with wage theta1 and agent2 with
# wage theta2.  The max is labelled for each.
# Extended example:
# createTwoAgentProfitPlot(seq(0, 30, by=1), 0.1, 0.1, 0.2, currencyScale=10)
# createTwoAgentProfitPlot(seq(0, 30, by=1), 0.1, 0.1, 0.2, currencyScale=10) + ylim(-25, 75)
createTwoAgentProfitPlot <- function(qVec, alpha, theta1, theta2, currencyScale=1) {
  theta3 <- 0.5
  dfWide <- calcMultipleUtility(qVec, alpha, c(theta1, theta2, theta3),  prefix="p")
  dfLong <- melt(dfWide, id=c("quantity"), measure=c("p1", "p2", "p3"))
  colnames(dfLong) <-c("quantity", "type", "profit")
  
  p <- ggplot(dfLong, aes(x=quantity, y=currencyScale*profit, colour=type)) + geom_path()
  p <- p + ylab("profit")
  colors <- c("brown", "dark green", "purple")
  p <- p + scale_color_manual(values=colors)

  # Below assumes theta1 < theta2, so agent 1 is more efficient.
  q1 <- solveQ(alpha, theta1)
  u1 <- currencyScale*(expUtility(alpha, q1) - q1 * theta1)
  p <- p + geom_label(label="cost=1", x=q1, y=u1, colour=colors[1])

  q2 <- solveQ(alpha, theta2)
  u2 <- currencyScale*(expUtility(alpha, q2) - q2 * theta2)
  p <- p + geom_label(label="cost=2", x=q2, y=u2, colour=colors[2])

  q3 <- solveQ(alpha, theta3)
  u3 <- currencyScale*(expUtility(alpha, q3) - q3 * theta3)
  p <- p + geom_label(label="cost=5", x=q3, y=u3, colour=colors[3])

  p + theme(legend.position="none")
}

#
# Back to utility.
# Functions are preceded by "i" to indicate incomplete information.
#

# p <- icreateUtilityContourPlot(seq(0, 30, by=5), 0.1, 0.1, 0.2, 0.5)
# p + geom_contour(aes(colour = stat(level)))
# Or
# icreateUtilityContourPlot(seq(0, 30, by=2), 0.1, 0.1, 0.2, 0.5, principalKnowsType=TRUE)
icreateUtilityContourPlot <- function(qVec, alpha, theta1, theta2, propEfficient,
                                      principalKnowsType=FALSE) {
  dfc <- icalcMultipleUtility(qVec, alpha, theta1, theta2, propEfficient, principalKnowsType)
  dfcLong <- melt(dfc, id=c("q1", "q2"), measure=c("net_utility"))
  colnames(dfcLong) <-c("q1", "q2", "ignore", "wgt_avg_principal_utility")
  
  # Create the base graph with a curve for each level of q2.
  p <- ggplot(dfcLong, aes(x=q1, y=q2, z=wgt_avg_principal_utility)) + geom_contour(aes(colour = stat(level)))
  p <- p + xlab("qEfficient") + ylab("qInefficient")
  p
}

# Example: icreateUtilityPlot(seq(0, 30, by=5), 0.1, 0.1, 0.2, 0.5)
# Test: icreateUtilityPlot(seq(0, 30, by=2), 0.1, 0.1, 0.2, 0.5)
# This can be slow to render, soon't give it too many q's.
icreateUtilityPlot <- function(qVec, alpha, theta1, theta2, propEfficient) {
  dfc <- icalcMultipleUtility(qVec, alpha, theta1, theta2, propEfficient)
  dfcLong <- melt(dfc, id=c("q1", "q2"), measure=c("net_utility"))
  colnames(dfcLong) <-c("q1", "q2", "ignore", "wgt_avg_principal_utility")
  
  # Create the base graph with a curve for each level of q2.
  p <- ggplot(dfcLong, aes(x=q1, y=wgt_avg_principal_utility, colour=q2, group=q2)) + geom_line()
  utilFunc <- function(q1, q2) {icalcUtility(alpha, q1, q2, theta1, theta2,
                                             propEfficient)}
  
  # Add the curve for inputs that maximize the principal's utility.
  qVecRep <- rep(qVec, length(qVec)) # hack to make data same length as original.
  q2Max <- isolveQInefficient(alpha, theta1, theta2, propEfficient)
  p <- p + geom_line(aes(x=qVecRep, y=utilFunc(qVecRep, q2Max)), colour="red")
  
  # Create a label directly on each base line.  It's easier than using the
  # legend and trying to distinguish different shades of blue.
  for (qVal in qVec) {
    q2Label <- paste0("q2=", format(round(qVal, 0), nsmall = 0))
    p <- p + geom_label(label=q2Label, x=qVal, y=utilFunc(qVal, qVal))
  }
  
  # Add the label for inputs that maximize the principal's utility.
  # q2Max was calculated above.
  q1Max <- solveQ(alpha, theta1)
  uMax <- utilFunc(q1Max, q2Max)
  labelMax <- paste0("max (q2=", format(round(q2Max, 2), nsmall = 2), ")")
  p <- p + geom_label(label=labelMax, x=q1Max, y=uMax, colour="red")
  p
}

# Example: createSolutionDataFrame(0.1, 0.1, 0.2)
createSolutionDataFrame <- function(alpha, theta1, theta2) {
  q1 <- solveQ(alpha, theta1)
  t1 <- q1 * theta1
  u1 <- expUtility(alpha, q1) - t1
  q2 <- solveQ(alpha, theta2)
  t2 <- q2 * theta2
  u2 <- expUtility(alpha, q2) - t2
  # Agent new utility is payment - effort, but in this case both agents
  # are paid exactly their effort amount, so zero utility.
  data.frame(agent=c("agent1", "agent2"), theta=c(theta1, theta2),
             quantity=c(q1, q2), payment=c(t1, t2),
             agentUtility=c(0, 0), principalUtility=c(u1, u2),
             totalUtility=c(u1, u2))
}

# Example: icreateSolutionDataFrame(0.1, 0.1, 0.2, 0.5)
icreateSolutionDataFrame <- function(alpha, theta1, theta2, propEfficient) {
  q1 <- solveQ(alpha, theta1)
  t1 <- isolveTEfficient(alpha, theta1, theta2, propEfficient)
  u1 <- expUtility(alpha, q1) - t1
  q2 <- isolveQInefficient(alpha, theta1, theta2,
                           propEfficient)
  t2 <- q2 * theta2
  u2 <- expUtility(alpha, q2) - t2
  # Agent net utility is payment - effort, which is non-zero for agent 1.
  au1 <- t1 - theta1*q1
  au2 <- t2 - theta2*q2
  weightedAvg <- function(v1, v2) {propEfficient*v1 + (1-propEfficient)*v2}
  data.frame(contract=c("high_effort (q1)", "low_effort (q2)", "WEIGHTED AVG"),
             quantity=c(q1, q2, weightedAvg(q1, q2)),
             payment=c(t1, t2, weightedAvg(t1, t2)),
             agentUtility=c(au1, au2, weightedAvg(au1, au2)),
             principalUtility=c(u1, u2, weightedAvg(u1, u2)),
             totalUtility=c(u1 + au1, u2 + au2,
                            weightedAvg(u1 + au1, u2 + au2)))
}