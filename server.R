#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(carData) # Contains our data set (TitanicSurvival)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    subset <- TitanicSurvival[TitanicSurvival$survived == "yes",]
    
    ggplot(data=subset, aes(x=sex, fill=passengerClass)) + geom_bar() + labs(y="Survived", x="Gender", fill="Class")
  })
  output$result <- renderText({
    tm <- TitanicSurvival
    tm$survived <- as.numeric(tm$survived) - 1
    fit <- lm(survived ~ ., data=tm)
    
    prediction <- predict(fit, newdata = data.frame(sex=input$gender, age=input$age, passengerClass=input$class))
    prediction <- max(min(round(prediction * 100, digits=2), 100), 0)
    prediction
  })
})
