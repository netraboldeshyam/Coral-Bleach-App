#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggplot2)
library(shiny)
library(datasets)
require(datasets)
require(tidyr)
require(tidyverse)
library(scales)
library(plyr)
library(httpuv)


# Define UI for application that draws a histogram
ui <- fluidPage(titlePanel("Coral Bleach App"),
  sidebarLayout(headerPanel(""),
    sidebarPanel(selectInput("variable", "Coral Type:",
    c("Blue Corals" = "blue corals",
      "Hard Corals" = "hard corals",
      "Sea Fans" = "sea fans",
      "Sea Pens" = "sea pens",
      "Soft Corals" = "soft corals")),
        selectInput("method", "Smoother:",
           c("Linear Regression" = "lm","LOESS" = "loess")))), 
            leafletOutput("mymap"),      
            mainPanel(h3(textOutput("caption")),
              plotOutput("coralplot", width = "1200px", height = "600px")))

setwd("C:/Users/ntrbl/OneDrive/Desktop/Monash/Study/R/CoralBleaching")
df <- read.csv("assignment-02-data-formated-1.csv")
str(df)

df$value <- as.numeric(sub("%","",df$value))

# Define server logic required to leaflet
server <- function(input, output) {
  output$caption <- reactiveText(function(){
    paste("Output for", input$variable)
  })
  output$mymap <- renderLeaflet({leaflet(data = df) %>%
      addTiles() %>% addMarkers(~longitude, ~latitude, popup = as.character(location), label = as.character(location), labelOptions(noHide = T, labelOptions = TRUE, textsize = "15px"))
  }) 
  
  output$coralplot <- reactivePlot(function()  
  {
    if (input$variable == "blue corals") 
    { 
      blue_corals <- subset(df, coralType == "blue corals")
      blue_corals
      
      icon.blueicon <- makeAwesomeIcon(icon = "flag", markerColor = 'blue', iconColor =  'black')
      
      output$mymap <- renderLeaflet({
        leaflet(data = blue_corals) %>% addTiles() %>%
          addAwesomeMarkers(~longitude, ~latitude, popup = ~as.character(location),
                            label = ~as.character(location), labelOptions = labelOptions(noHide = T), icon = icon.blueicon)
      })
      gplot <- ggplot(data = blue_corals, aes( x = year, y = as.numeric(sub("%","",value/100)))) + geom_point(color = "blue", size= 2.5) + scale_y_continuous("% of Bleaching", labels = scales::percent) + facet_grid(latitude+location~coralType) + geom_smooth(method = input$method, color = "lightgrey")
      print(gplot)
    }
    if (input$variable == "hard corals") 
    {
      hard_corals <- subset(df, coralType == "hard corals")
      hard_corals
      
      icon.greenicon <- makeAwesomeIcon(icon = "flag", markerColor = 'green', iconColor =  'black')
      
      output$mymap <- renderLeaflet({
        leaflet(data = hard_corals) %>% addTiles() %>%
          addAwesomeMarkers(~longitude, ~latitude, popup = ~as.character(location),
                            label = ~as.character(location), labelOptions = labelOptions(noHide = T), icon = icon.greenicon)
      })
      gplot <- ggplot(data = hard_corals, aes( x = year, y = as.numeric(sub("%","",value/100)))) + geom_point(color = "green", size= 2.5) + scale_y_continuous("% of Bleaching", labels = scales::percent) + facet_grid(latitude+location~coralType) + geom_smooth(method = input$method, color = "lightgrey")
      print(gplot)
    }
    if (input$variable == "sea pens") 
    {
      sea_pens <- subset(df, coralType == "sea pens")
      sea_pens
      
      icon.redicon <- makeAwesomeIcon(icon = "flag", markerColor = 'red', iconColor =  'black')
      
      output$mymap <- renderLeaflet({
        leaflet(data = sea_pens) %>% addTiles() %>%
          addAwesomeMarkers(~longitude, ~latitude, popup = ~as.character(location),
                            label = ~as.character(location), labelOptions = labelOptions(noHide = T), icon = icon.redicon)
      })
      gplot <- ggplot(data = sea_pens, aes( x = year, y = as.numeric(sub("%","",value/100)))) + geom_point(color = "red", size= 2.5) + scale_y_continuous("% of Bleaching", labels = scales::percent) + facet_grid(latitude+location~coralType) + geom_smooth(method = input$method, color = "lightgrey")
      print(gplot)
    }
    if (input$variable == "soft corals") 
    {
      soft_corals <- subset(df, coralType == "soft corals")
      soft_corals
      
      icon.orangeicon <- makeAwesomeIcon(icon = "flag", markerColor = 'orange',iconColor =  'black')
      
      output$mymap <- renderLeaflet({
        leaflet(data = soft_corals) %>% addTiles() %>%
          addAwesomeMarkers(~longitude, ~latitude, popup = ~as.character(location),
                            label = ~as.character(location), labelOptions = labelOptions(noHide = T), icon = icon.orangeicon)
      })
      gplot <- ggplot(data = soft_corals, aes( x = year, y = as.numeric(sub("%","",value/100)))) + geom_point(color = "orange", size= 2.5) + scale_y_continuous("% of Bleaching", labels = scales::percent) + facet_grid(latitude+location~coralType) + geom_smooth(method = input$method, color = "lightgrey") + theme()
      print(gplot)
    }
    if (input$variable == "sea fans") 
    {
      sea_fans <- subset(df, coralType == "sea fans")
      sea_fans
      
      icon.purpleicon <- makeAwesomeIcon(icon = "flag", markerColor = 'purple', iconColor =  'black')
      
      output$mymap <- renderLeaflet({
        leaflet(data = sea_fans) %>% addTiles() %>%
          addAwesomeMarkers(~longitude, ~latitude, popup = ~as.character(location),
                            label = ~as.character(location), labelOptions = labelOptions(noHide = T), icon = icon.purpleicon)
      })
      gplot <- ggplot(data = sea_fans, aes( x = year, y = as.numeric(sub("%","",value/100)))) + geom_point(color = "purple", size= 2.5) + scale_y_continuous("% of Bleaching", labels = scales::percent) + facet_grid(latitude+location~coralType) + geom_smooth(method = input$method, color = "lightgrey")
      print(gplot)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

