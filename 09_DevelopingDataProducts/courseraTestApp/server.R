#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(leaflet)

data("mtcars")

shinyServer(function(input, output) {
    

    
    fit.LM <- lm(mpg ~ hp, data = mtcars)
    pred <- reactive({
        predict(fit.LM, newdata = data.frame(hp = input$hp))
        })
    output$inputValue <- renderText(input$hp)
    output$prediction <- renderText(pred())
    #predPoint <- data.frame(hp = input$hp, mpg = pred())

    output$scatterPlot <- renderPlot({
        ggplot(data = mtcars, aes(x = hp, y = mpg)) + 
            geom_point(color = "blue", size = 2) +
            geom_smooth(method = "lm",color = "red", se = F) +
            geom_point(data = data.frame(hp = input$hp, mpg = pred()), size = 5)
    })
    
    points <- eventReactive(input$recalc,{
        cbind(rnorm(40)*2 + 13, rnorm(40) + 48)
    }, ignoreNULL = F)
    
    output$mymap <- renderLeaflet({
        leaflet()%>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = T)
                             )%>%
            addMarkers(data = points())
    })
})
