# In "moral hazard" the principal's worry is that an agent may not exert as
# much effort as they could, since effort is not directly observable.
# p1: probability of success from high effort (effort=1)
# p2: probability of success from low effort, where p2 < p1 (effort=0)

# sHigh: principal utility from success  (fixed)
# sLow: principal utility from failure (fixed, possibly zero)
# p: probability of success when high effort level.  Should be > 0.5.
# tHigh: transfer payment to agent upon success (TBD)
# tLow: transfer payment to agent upon failure (TBD)
# EXPECTED principal utility.
# Since everything is linear, a plot relative to (tHigh, tLow) is a plane.
# That means that the max is always determined by the constraints.
principalUtility <- function(sHigh, sLow, p, tHigh, tLow) {
  p * (sHigh - tHigh) + (1-p) * (sLow - tLow)
}

# Example: m1calcMultipleUtility(seq(0, 1, by=0.2), 0.8, 0.2, 0.5)
# ggplot(out, aes(x=t1, y=t2, z=net_utility)) + geom_contour(aes(colour = stat(level)))
m1calcMultipleUtility <- function(tVec, sHigh, sLow, p) {
  df1 <- data.frame(tVec, sapply(tVec, function(t) { sHigh-t }))
  names(df1) <- c('t1','utility1')
  df2 <- data.frame(tVec, sapply(tVec, function(t) { (sLow-t) }))
  names(df2) <- c('t2','utility2')
  dfc <- merge(df1, df2)
  dfc['net_utility'] <- p*(dfc['utility1']) + (1-p)*dfc['utility2']
  dfc
}

# Not for all combos of tLow and tHigh, only those that have agent utility
# at the bare minimum of zero (participation constraint).
# psi = agent disutility of work.
# Example: m1calcMultipleConstrainedUtility(seq(0, 3, 0.2), 6, 3, 0.6, 0.1, 3)
m1calcMultipleConstrainedUtility <- function(tLow, sHigh, sLow, p, agentAlpha, psi) {
  d <- data.frame(tLow, sapply(tLow, function(x) {agentUtilityConstraint(agentAlpha, p, x, psi)}))
  names(d) <- c('tLow', 'tHigh')
  d['principalUtility'] <- principalUtility(sHigh, sLow, p, d['tLow'], d['tHigh'])
  d
}

# u is the agent's money utility function.  h is its inverse.

# This disutility is based on Cobb-Douglas.
# It returns zero for zero effort.
agentDisutilityOfEffort <- function(agentAlpha, bExponent, effort) {
  agentAlpha * effort^bExponent
}

expUtility <- function(alpha, val) {
  (1-exp(-alpha*val))/alpha
}

# alpha: agent utility curve parameter
# p: probability of success when high effort level
# tHigh: transfer payment to agent upon success (TBD)
# tLow: transfer payment to agent upon failure (TBD)
# Ignoring psi for now.  psi = disutility of effort 1 (rather than 0).
# EXPECTED agent utility.
agentUtility <- function(agentAlpha, p, tHigh, tLow) {
  p * expUtility(agentAlpha, tHigh) + (1-p) * expUtility(agentAlpha, tLow)
}


# What should tHigh be, given tLow?
# (p * (1-exp(-alpha*tHigh)) + (1-p) * (1-exp(-alpha*tLow)))/alpha = psi
# Solve above for tHigh where agentUtility = psi, i.e. utility - effort = 0.
# https://www.wolframalpha.com/input/?i=solve+(p+*+(1-exp(-alpha*x))+%2B+(1-p)+*+(1-exp(-alpha*y))+%3D+Z+alpha)+for+x
# should be symmetric at p=0.5
# psi is the disutility of effort level 1 (vs. 0.)
agentUtilityConstraint <- function(alpha, p, tLow, psi) {
  # Below is equivalent to:
  # log(-(exp(alpha*tLow)* p)/(1 - p + exp(alpha*tLow)*(-1 + alpha* psi)))/alpha
  log((exp(alpha*tLow)* p)/(p -1 + exp(alpha*tLow)*(1 - alpha* psi)))/alpha
}
