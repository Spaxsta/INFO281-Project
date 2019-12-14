library(shiny)
library(leaflet)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)

SchoolData <- read.csv("LocationData.csv")
DecileUE <- read.csv("Decile_UE_Gained.csv")

currentYear <- as.numeric(sub("%", "",DecileUE$Current.Year.Attainment.Rate,fixed=TRUE))

dat <- data.frame(decile=c("1-3", "4-7", "8-10"),
                  passRate=currentYear)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

mapColours <- c(rgb(1,0,0), rgb(1,0,0), rgb(1,0,0),rgb(0,1,0),rgb(0,1,0),rgb(0,1,0),rgb(0,1,0),rgb(0,0,1),rgb(0,0,1),rgb(0,0,1))

ui <- fluidPage(
    leafletOutput("mymap"),
    p(),
    plotOutput("barplot")
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
                             color = mapColours[SchoolData$Decile],
                             label =  SchoolData$Decile, 
                             labelOptions = labelOptions(direction = 'Top', textOnly = TRUE),
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
    
    output$barplot <-renderPlot({
        ggplot(data = dat, aes(y=passRate, x=decile, fill=decile))+
            geom_bar(stat="identity", width=0.7)+
            geom_text(aes(label=passRate), vjust=5) +
            ggtitle("University Entrance achieved by school decile")+
            labs(x = NULL, y = "Pass Rate")+
            theme_classic()+
            theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
    })
}

shinyApp(ui, server)