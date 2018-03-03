#

library(shiny)
library(leaflet)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Find a city or country in a map"),
  
  # Sidebar with the inputs for the app
  sidebarLayout(
    sidebarPanel(
        # Uses the text input and stores it in the variable country2Search to do a query further on
       textInput("country2Search", "Enter a Country/City here","Munich Germany"),
       sliderInput("zoomSlider","Standard Zoom",5,
                   min = 1, max = 14, step = 1),
       submitButton("Submit"),
       h6("Created 27.02.2018"),
       a(h5("Documentation"),href="http://rpubs.com/alejandrobalderas/365773",target="_blank"),
       a(h5("Pitch"),href="http://rpubs.com/alejandrobalderas/365993",target="_blank"),
       a(h5("Code"),href="https://github.com/alejandrobalderas/DataScienceCoursera/tree/master/09_DevelopingDataProducts/shinyLeafletMap",target="_blank")
       ),
    
    
    
    # Show the place, coordinates and map using leaflet
    mainPanel(
        h4("You entered"),
        verbatimTextOutput("text"),
        h4("The coordinates of this place are:"),
        verbatimTextOutput("coords"),
        leafletOutput("mymap")
    )
  )
))
