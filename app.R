library(shiny)
library(leaflet)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)

SchoolData <- read.csv("LocationData.csv")
DecileUE <- read.csv("Decile_UE_Gained.csv")
EthnicityUE <- read.csv("Ethnicity_UE_Gained.csv")
Level1Ethnicity <- read.csv("Level1_Ethnicity.csv")
Level2_Ethnicity <- read.csv("Level2_Ethnicity.csv")
Level3_Ethnicity <- read.csv("Level3_Ethnicity.csv")


currentYearEthnicityUE <- as.numeric(sub("%", "",EthnicityUE$Current.Year.Attainment.Rate,fixed=TRUE))
currentYearEthnicityL1 <- as.numeric(sub("%", "",Level1Ethnicity$Current.Year.Attainment.Rate,fixed=TRUE))
currentYearEthnicityL2 <- as.numeric(sub("%", "",Level2_Ethnicity$Current.Year.Attainment.Rate,fixed=TRUE))
currentYearEthnicityL3 <- as.numeric(sub("%", "",Level3_Ethnicity$Current.Year.Attainment.Rate,fixed=TRUE))

currentYearDecileUE <- as.numeric(sub("%", "",DecileUE$Current.Year.Attainment.Rate,fixed=TRUE))

Ethnicitydat <- data.frame(ethnicity=c("Maori", "European", "Pacifica", "Asian", "Middle Eastern"),
                           University_Enterance=currentYearEthnicityUE, NCEA_Level_1=currentYearEthnicityL1,
                           NCEA_Level_2=currentYearEthnicityL2, NCEA_Level_3=currentYearEthnicityL3)

Deciledat <- data.frame(decile=c("1-3", "4-7", "8-10"),
                  passRate=currentYearDecileUE)

deciles = c("1-3", "4-7", "8-10")

r_colors <- rgb(t(col2rgb(colors()) / 255))

names(r_colors) <- colors()

mapColours <- c(rgb(1,0,0), rgb(1,0,0), rgb(1,0,0),rgb(0,1,0),rgb(0,1,0),rgb(0,1,0),rgb(0,1,0),rgb(0,0,1),rgb(0,0,1),rgb(0,0,1))

ui <- fluidPage(
    column(6,
           leafletOutput("mymap", width = 400, height = 500)
             
    ),
    column(6,
           selectInput(inputId = 'Ethnicity',
                       label='Select ethnicity NCEA level pass rate:',
                       choices=c('University_Enterance','NCEA_Level_1','NCEA_Level_2','NCEA_Level_3'),
                       selected='passRateUE'),
           plotOutput("barplot"),
           p(),
           plotOutput("barplot2")
    )

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
        ggplot(data = Ethnicitydat, aes_string(y=input$Ethnicity, x=Ethnicitydat$ethnicity, fill=Ethnicitydat$ethnicity))+
            geom_bar(stat="identity", width=0.7)+
            geom_text(aes(label=paste(Ethnicitydat$input$Ethnicity, "%", sep = "")), vjust=5) +
            ggtitle(paste(input$Ethnicity, "achieved \nby school ethnicity 2018"))+
            labs(x = NULL, y = "Pass Rate")+
            ylim(0,100)+
            theme_classic()+
            theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
    })
    
    output$barplot2 <-renderPlot({
        ggplot(data = Deciledat, aes(y=passRate, x=decile, fill=decile))+
            geom_bar(stat="identity", width=0.7)+
            geom_text(aes(label=paste(passRate, "%", sep = "")), vjust=5) +
            ggtitle("UE achieved by school decile 2018")+
            labs(x = NULL, y = "Pass Rate")+
            theme_classic()+
            theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
    })
}

shinyApp(ui, server)