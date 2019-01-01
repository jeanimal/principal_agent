# Principal Agent

Functions to generate data tables and plots to demonstrate simple principal-agent
models.  These are wrapped in Shiny for a nice UI but can also be called locally.

I manually deploy the Shiny app to:
https://jeanimal.shinyapps.io/principal_agent/
(So the repo can get out of sync with the deployed app.)

# Using Locally

```r
source("functions.R")

# Complete information

alpha <- 0.1; theta1 <- 0.1; theta2 <- 0.2
createSolutionDataFrame(alpha, theta1, theta2)
#>    agent theta quantity  payment net_utility
#> 1 agent1   0.1 23.02585 2.302585    6.697415
#> 2 agent2   0.2 16.09438 3.218876    4.781124

qVec <- seq(0, 30, by=1)
createUtilityPlot(qVec, alpha, theta1, theta2)
```

![CreateUtilityPlot example output](github.com/jeanimal/principal_agent/img/CreateUtilityPlotExampleOutput.jpg)

# Dev Tools

The app was created with RStudio, so just load the .Rproj file for easy development and running locally.

# Principal-Agent Models

The models are based on the summaries in Laffont and Maritmort's 2001 _The Theory of Incentives_.

# Authors

* Jean Czerlinski Whitmore

# License

This project is licensed under the MIT License-- see the [LICENSE](LICENSE) file.
