library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)

SchoolData <- read.csv("LocationData.csv")

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
            addCircleMarkers(data = SchoolData, 
                             lng = SchoolData$Longitude, 
                             lat = SchoolData$Latitude, 
                             stroke = FALSE, 
                             fillOpacity = 0.5,        
                             popup = paste(SchoolData$schoolName, "<b>",SchoolData$Org.Name,"</b>", "<br>",
                                                           "Decile Rating: ", SchoolData$Decile, "<br>",
                                                           "CoEd Status: ", SchoolData$CoEd.Status, "<br>",
                                                           "School Type: ", SchoolData$Org.Type, "<br>",
                                                           "Enrolment Number: ", SchoolData$Total, "<br>",
                                                           "Number of European Pupils: ", SchoolData$European, "<br>",
                                                           "Number of Māori Pupils: ", SchoolData$Maori, "<br>",
                                                           "Number of Pacific Pupils: ", SchoolData$Pacific, "<br>",
                                                           "Number of Asian Pupils: ", SchoolData$Asian, "<br>",
                                                           "Number of MELAA Pupils: ", SchoolData$MELAA, "<br>",
                                                           "Number of Other Pupils: ", SchoolData$Other, "<br>",
                                                           "Number of International Pupils: ", SchoolData$International, "<br>"))
                                                                                                                                                
        #, label =  ~Decile, 
        #labelOptions = labelOptions(noHide = T, direction = 'Top', textOnly = T) This is too slow currently need to figure out why
    })
}

shinyApp(ui, server)