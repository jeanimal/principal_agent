#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Principal's utility by quantity"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("alpha",
                   "Principal's alpha (exponential factor in widget utility):",
                   min = 0,
                   max = 1.0,
                   value = 0.1),
       sliderInput("theta",
                   "Agent's theta (wage per widget):",
                   min = 0,
                   max = 1.0,
                   value = 0.1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("utilityPlot")
    )
  ),
  hr(),
  print("Source code: "),
  tags$a(href="https://github.com/jeanimal/principal_agent",
         "https://github.com/jeanimal/principal_agent")
  
))