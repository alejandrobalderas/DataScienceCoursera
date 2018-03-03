
# Load the following libraries
library(shiny)
library(leaflet)
library(ggmap)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # Get the information of countr2Search and store it in place2Locate
    place2Locate <- reactive(input$country2Search)
    # Uses geocode and the dsk database to search for the coordinates
    coords <- reactive(geocode(place2Locate(), source = "dsk"))
    output$text <- renderText(place2Locate())
    lon <- reactive(coords()$lon)
    lat <- reactive(coords()$lat)
    # Store the longitude and latitude for display
    coords_text <- reactive({paste("Lon: ",lon(), " Lat: ", lat())})
    output$coords <- renderText(coords_text())
    # Create the leaflet app
    output$mymap <- renderLeaflet({
        leaflet()%>%
            addTiles()%>%
            addMarkers(data = coords()) %>%
            setView(lon(), lat(), zoom = input$zoomSlider)
    })
    
})
