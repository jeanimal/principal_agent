#
# This is the user-interface definition of a Shiny web application.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for principal-agent application.
shinyUI(
  navbarPage("Principal-Agent",
             tabPanel("Intro",
                      fluidPage(
  helpText("This app demonstrates principal-agent models, which calculate ",
           "how principals (employers) should compensate agents (employees) ",
           "to maximize the principal's utility.  The models are from chapter ",
           "2 of Laffont and Mortimort, 2002, The Theory of Incentives.")
                      )),
             tabPanel("Full Info",
                      fluidPage(
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
    
    mainPanel(
       plotOutput("utilityPlot"),
       tableOutput('solutionTable')
    )
  )
)),
tabPanel("Incomplete Info",
         fluidPage(
           helpText("Lesson: When you do not know ahead of time which agents are ",
                    "efficient, you discourage pretending to be inefficient ",
                    "by offering more pay for more work (quantity q)."),
           helpText("The principal's average utility (averaged across types of agents) ",
                    "is a function of both q1 and q2, which would require a 3D plot.  ",
                    "Instead, q1 is on the x-axis and q2 is ",
                    "shown with a line for each of several selected values."),
           helpText("The principal does not know each agent's type ",
                    "but does know the proportion of agents of each type.",
                    "The principal has to offer two contracts and let agents pick ",
                    "one or do nothing (leave). With good contracts, ",
                    "the efficient agent will choose the high-effort ",
                    "contract and the inefficient agent will choose the  ",
                    "low-effort contract.  The principal wants to propose ",
                    "contracts that maximize her utility."),
           sidebarLayout(
             sidebarPanel(
               numericInput("ialpha",
                            "Principal's alpha (in utility):",
                            min = 0,
                            max = 1.0,
                            step = 0.1,
                            value = 0.1),
               numericInput("itheta1",
                            "Agent 1 theta (effort per widget):",
                            min = 0,
                            max = 1.0,
                            step = 0.1,
                            value = 0.1),
               numericInput("itheta2",
                            "Agent 2 theta (effort per widget):",
                            min = 0,
                            max = 1.0,
                            step = 0.1,
                            value = 0.2),
               numericInput("iproportion",
                            "Proportion of agents like agent 1:",
                            min = 0,
                            max = 1.0,
                            step = 0.1,
                            value = 0.5)
             ),
             mainPanel(
               plotOutput("iutilityPlot"),
               tableOutput('isolutionTable')
             )
           )
)),
footer = fluidPage(
  hr(),
  tags$a(href="https://github.com/jeanimal/principal_agent",
         "https://github.com/jeanimal/principal_agent")
)
))