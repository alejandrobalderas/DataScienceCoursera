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

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Find a city or country in a map"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       textInput("country2Search", "Enter a Country/City here","Munich Germany"),
       sliderInput("zoomSlider","Standard Zoom",5,
                   min = 1, max = 14, step = 1),
       submitButton("Submit"),
       h6("Created 27.02.2018")),
    
    
    # Show a plot of the generated distribution
    mainPanel(
       #leafletOutput("myLeaflet")
        h4("You entered"),
        verbatimTextOutput("text"),
        h4("The coordinates of this place are:"),
        verbatimTextOutput("coords"),
        leafletOutput("mymap")
    )
  )
))
