#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

shinyUI(pageWithSidebar(

    # Application Title
    headerPanel("MPG Prediction"),
    
    sidebarPanel(
        sliderInput("hp", "HP", 170,
                     min = 50, max = 350, step = 5)
        # submitButton("Submit")
    ),
    
    mainPanel(
        h3("Results of prediction"),
        h4("You entered"),
        verbatimTextOutput("inputValue"),
        h4("Which resulted in a prediction of "),
        verbatimTextOutput("prediction"),
        plotOutput("scatterPlot"),
        leafletOutput("mymap"),
        actionButton("recalc", "New Points")
    )
))
