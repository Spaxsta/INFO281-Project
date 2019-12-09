library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)

wvs <- read.csv("LocationData.csv")

Lat <- wvs$Latitude

Lon <- wvs$Longitude

schoolNames <- wvs$Org.Name

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
    leafletOutput("mymap"),
    p()
)

server <- function(input, output, session) {
    
    points <- eventReactive(input$recalc, {
        cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)

    output$mymap <- renderLeaflet({
        leaflet() %>%
            setView(lng = 175, lat = -41, zoom = 7) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addCircleMarkers(data = wvs, lng = ~Lon, lat = ~Lat, popup = ~schoolNames, stroke = FALSE, fillOpacity = 0.5, label =  ~schoolNames, 
                             labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))
    })
}

shinyApp(ui, server)