#
# This is the user-interface definition of a Shiny web application.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for principal-agent application.
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Principal's utility by quantity"),
  helpText("Lesson: The efficient agent (lower theta, less effort ",
           "per widget, by default agent1) gets a contract to produce ",
           "more widgets for a lower payment than the inefficient agent ",
           "(by default agent2)."),
  helpText("(This is in a one-shot game where the principal proposes a contract",
           "and the agent can take it or leave it.  The principal knows the ",
           "agent's theta before proposing the contract.)"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       numericInput("alpha",
                   "Principal's alpha (in utility):",
                   min = 0,
                   max = 1.0,
                   step = 0.1,
                   value = 0.1),
       numericInput("theta1",
                   "Agent 1 theta (effort per widget):",
                   min = 0,
                   max = 1.0,
                   step = 0.1,
                   value = 0.1),
       numericInput("theta2",
                    "Agent 2 theta (effort per widget):",
                    min = 0,
                    max = 1.0,
                    step = 0.1,
                    value = 0.2),
       helpText("This plots the principal's net utility:"),
       withMathJax(),
       helpText("$$\\frac{1-e^{{-\\alpha}{q}}}{\\alpha} - {\\theta}{q}$$")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("utilityPlot"),
       tableOutput('solutionTable')
    )
  ),
  hr(),
  print("Source code: "),
  tags$a(href="https://github.com/jeanimal/principal_agent",
         "https://github.com/jeanimal/principal_agent")
  
))