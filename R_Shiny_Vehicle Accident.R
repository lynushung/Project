#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Let people can install all the packages we need
list.of.packages <- c("shiny","dplyr","DT","readr","readxl","tidyr","leaflet", "data.table", "lattice", "hms")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(data.table)
library(shiny)
library(leaflet)
library(lattice)
library(dplyr)
library(hms)
library(DT)

#Setting work directory

vehicleaccident.df <- read.csv("Vehicle_Accident_Data.csv", stringsAsFactors = F, header = TRUE, sep = ',')

#Splitting Timestamp into Date and Time
  #sapply: seperate and "[" take small part out -1(first part-date// something in string(character)

vehicleaccident.df$Crash_Date <- sapply(strsplit(as.character(vehicleaccident.df$Crash.Date.Time), " "), "[", 1)
vehicleaccident.df$Time <- sapply(strsplit(as.character(vehicleaccident.df$Crash.Date.Time), " "), "[", 2)
vehicleaccident.df$Location <- gsub("\\(", '', vehicleaccident.df$Location)
vehicleaccident.df$Location <- gsub("\\)", '', vehicleaccident.df$Location)
vehicleaccident.df$Latitude <- sapply(strsplit(as.character(vehicleaccident.df$Location), ", "), "[", 1)
vehicleaccident.df$Longitude <- sapply(strsplit(as.character(vehicleaccident.df$Location), ", "), "[", 2)

vehicleaccident.df$Hit_Run <- as.character(vehicleaccident.df$Hit.And.Run)

#Organizing and Renaming Dataset

focus <- vehicleaccident.df
colnames(focus)[colnames(focus) == 'Report.Number'] <- 'ReportNo'

#Formatting Data Structure

str(focus)
summary(focus)

focus$Fatality <- as.character(focus$Fatality)
focus$Crash_Date <- as.Date.character(focus$Crash_Date, '%m/%d/%Y')
focus$Latitude <- as.numeric(focus$Latitude)
focus$Longitude <- as.numeric(focus$Longitude)

#Dropping 0 from Latitude & Longitude
  #take >0

focus <- focus[(focus$Latitude > 0),]

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Vehicle Accident"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout( position = 'right',
      absolutePanel(style = "opacity: 0.70",
        
        id = 'controls', class = 'panel panel-default', fixed = TRUE,
        draggable = TRUE, top = 60, left = 'auto', right = 25, bottom = 'auto',
        width = 'auto', height = 'auto',
        
        dateRangeInput(inputId = 'DateRange', label = 'Dates',
                       start = as.Date('2018-10-01'),
                       end = as.Date('2018-11-01')),
        
        selectInput(inputId = 'HitAndRun', label = 'Hit And Run',
                    choices = c('TRUE', 'FALSE'),
                    selected = 'TRUE'),
        
        selectInput(inputId = 'Fatal', label = 'Fatality',
                    choices = c('TRUE', 'FALSE'),
                    selected = 'FALSE')
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput('Mymap', width = 1200, height = 430),
         dataTableOutput('Finaltable')
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Filtering dataset based on chosen inputs
  
  filtered <- reactive({
    out <- focus
    out <- out %>%
      filter(Crash_Date >= input$DateRange[1] & Crash_Date <= input$DateRange[2])
    out <- out %>%
      filter(Hit_Run %in% input$HitAndRun)
    out <- out %>%
      filter(Fatality %in% input$Fatal)
  })
  
  # Render the Map

  output$Mymap <- renderLeaflet({
    df <- filtered()
    m <- leaflet(data = df) %>%
      addTiles(
        urlTemplate = "http://{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href = "http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -97.32, lat = 32.756, zoom = 15) %>%
      addMarkers(lng = ~Longitude,
                 lat = ~Latitude,
                 popup = paste("Report Number:", df$ReportNo))
    
    m
    
  })
  
    output$Finaltable <- DT::renderDataTable({
    DT::datatable(data = filtered())
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

