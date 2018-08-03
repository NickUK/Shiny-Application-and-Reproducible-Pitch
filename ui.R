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
  titlePanel("Titanic Survival Predictor"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       helpText("This application predicts titanic survival for a passanger"),
       selectInput("gender", label = h4("Gender"), 
                   choices = list("Male" = "male", "Female" = "female")),
       selectInput("class", label = h4("Passenger Class"), 
                   choices = list("1st" = "1st", "2nd" = "2nd", "3rd" = "3rd")),
       numericInput("age", label = h4("Age"), step = 1, value = 20, min=0, max=80)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot"),
       p("Predicted survival chance: ", textOutput("result", inline=TRUE), "%")
    )
  )
))
