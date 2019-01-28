# Principal Agent

Functions to generate data tables and plots to demonstrate simple principal-agent
models.  These are wrapped in Shiny for a nice UI but can also be called locally.

I manually deploy the Shiny app to:
https://jeanimal.shinyapps.io/principal_agent/
(So the repo can get out of sync with the deployed app.)

# Using Locally

## Data and plots for complete information model

```r
source("principal_agent/functions.R")

alpha <- 0.1; theta1 <- 0.1; theta2 <- 0.2
createSolutionDataFrame(alpha, theta1, theta2)
#>    agent theta quantity  payment agentUtility principalUtility totalUtility
#> 1 agent1   0.1 23.02585 2.302585            0         6.697415     6.697415
#> 2 agent2   0.2 16.09438 3.218876            0         4.781124     4.781124

qVec <- seq(0, 30, by=1)
createUtilityPlot(qVec, alpha, theta1, theta2)
```

![CreateUtilityPlot example output](https://github.com/jeanimal/principal_agent/blob/master/img/createUtilityPlotExampleOutput.jpeg)

## Data and plots for incomplete information model

Use the same functions but prefixed with an "i" and add the proportionEfficient.

The output plot should be a 3D plot that is a function of q1 and q2, so I put
q1 on the x-axis and represented different values of q2 with different lines.

```r
source("principal_agent/functions.R")

alpha <- 0.1; theta1 <- 0.1; theta2 <- 0.2; proportionEfficient <- 0.5
icreateSolutionDataFrame(alpha, theta1, theta2, proportionEfficient)
#>           contract quantity  payment agentUtility principalUtility totalUtility
#> 1 high_effort (q1) 23.02585 3.506558    1.2039728         5.493442     6.697415
#> 2  low_effort (q2) 12.03973 2.407946    0.0000000         4.592054     4.592054
#> 3     WEIGHTED AVG 17.53279 2.957252    0.6019864         5.042748     5.644735

# Use fewer q's for speed and legibility.
qVec <- seq(0, 30, by=5)
icreateUtilityPlot(qVec, alpha, theta1, theta2, proportionEfficient)
```

![CreateUtilityPlot example output](https://github.com/jeanimal/principal_agent/blob/master/img/icreateUtilityPlotExampleOutput.jpeg)

# Dev Tools

The app was created with RStudio, so just load the .Rproj file for easy development and running locally.

# Principal-Agent Models

The models are based on the summaries in Laffont and Martimort's (2001) _The Theory of Incentives_.

# Authors

* Jean Czerlinski Whitmore

# License

This project is licensed under the MIT License-- see the [LICENSE](LICENSE) file.
