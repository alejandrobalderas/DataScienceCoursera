#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(ggmap)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    point2Locate <- reactive(input$country2Search)
    coords <- reactive(geocode(point2Locate(), source = "dsk"))
    output$text <- renderText(point2Locate())
    lon <- reactive(coords()$lon)
    lat <- reactive(coords()$lat)
    coords_text <- reactive({paste("Lon: ",lon(), " Lat: ", lat())})
    output$coords <- renderText(coords_text())
    output$mymap <- renderLeaflet({
        leaflet()%>%
            addTiles()%>%
            addMarkers(data = coords()) %>%
            setView(lon(), lat(), zoom = input$zoomSlider)
    })
    
})
