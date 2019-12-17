library(shiny)
library(leaflet)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)

SchoolData <- read.csv("data/LocationData.csv")

DecileUE <- read.csv("data/Decile_UE_Gained.csv")
Level1_Decile <- read.csv("data/Level1_Decile.csv")
Level2_Decile <- read.csv("data/Level2_Decile.csv")
Level3_Decile <- read.csv("data/Level3_Decile.csv")

EthnicityUE <- read.csv("data/Ethnicity_UE_Gained.csv")
Level1Ethnicity <- read.csv("data/Level1_Ethnicity.csv")
Level2_Ethnicity <- read.csv("data/Level2_Ethnicity.csv")
Level3_Ethnicity <- read.csv("data/Level3_Ethnicity.csv")


currentYearEthnicityUE <- as.numeric(sub("%", "",EthnicityUE$Current.Year.Attainment.Rate,fixed=TRUE))
currentYearEthnicityL1 <- as.numeric(sub("%", "",Level1Ethnicity$Current.Year.Attainment.Rate,fixed=TRUE))
currentYearEthnicityL2 <- as.numeric(sub("%", "",Level2_Ethnicity$Current.Year.Attainment.Rate,fixed=TRUE))
currentYearEthnicityL3 <- as.numeric(sub("%", "",Level3_Ethnicity$Current.Year.Attainment.Rate,fixed=TRUE))

currentYearDecileUE <- as.numeric(sub("%", "",DecileUE$Current.Year.Attainment.Rate,fixed=TRUE))
currentYearDecileL1 <- as.numeric(sub("%", "",Level1_Decile$Current.Year.Attainment.Rate,fixed=TRUE))
currentYearDecileL2 <- as.numeric(sub("%", "",Level2_Decile$Current.Year.Attainment.Rate,fixed=TRUE))
currentYearDecileL3 <- as.numeric(sub("%", "",Level3_Decile$Current.Year.Attainment.Rate,fixed=TRUE))

Ethnicitydat <- data.frame(ethnicity=c("Maori", "European", "Pacifica", "Asian", "Middle Eastern"),
                           University_Enterance=currentYearEthnicityUE, NCEA_Level_1=currentYearEthnicityL1,
                           NCEA_Level_2=currentYearEthnicityL2, NCEA_Level_3=currentYearEthnicityL3)

Deciledat <- data.frame(decile=c("1-3", "4-7", "8-10"),
                        University_Enterance=currentYearDecileUE, NCEA_Level_1=currentYearDecileL1,
                        NCEA_Level_2=currentYearDecileL2, NCEA_Level_3=currentYearDecileL3)

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
                       selected='University_Enterance'),
           plotOutput("barplot"),
           p(),
           selectInput(inputId = 'Decile',
                       label='Select decile NCEA level pass rate:',
                       choices=c('University_Enterance','NCEA_Level_1','NCEA_Level_2','NCEA_Level_3'),
                       selected='University_Enterance'),
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
            labs(fill='Ethnicity')+
            ggtitle(paste(input$Ethnicity, "achieved \nby school ethnicity 2018"))+
            labs(x = NULL, y = "Pass Rate")+
            ylim(0,100)+
            theme_classic()+
            theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
    })
    
    output$barplot2 <-renderPlot({
        ggplot(data = Deciledat, aes_string(y=input$Decile, x=Deciledat$decile, fill=Deciledat$decile))+
            geom_bar(stat="identity", width=0.7)+
            geom_text(aes(label=paste( "%", sep = "")), vjust=5) +
            labs(fill='Decile')+
            ggtitle(paste(input$Decile, "achieved \nby school decile 2018"))+
            labs(x = NULL, y = "Pass Rate")+
            ylim(0,100)+
            theme_classic()+
            theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
    })
}

shinyApp(ui, server)