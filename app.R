# SETUP ---------------------------------------------------------------

setwd("~/Documents/Github/polMonitor")

# pkgs
library(shiny)
library(shinythemes)
library(tidyverse)
library(sf)
library(leaflet)
library(tidycensus)
library(RColorBrewer)
library(forcats)
library(viridis)
library(mapview)

# load data
mpv_data <- read_csv("geocodedMPVDataset.csv")

# to remove
mpv_data$`Victim's age band` <- cut(as.numeric(mpv_data$`Victim's age`),
                               breaks = c(0, 15, 34, 54, 74, Inf),
                               labels = c("0-15", "16-34", "35-54", "55-74", "75+"))

# make factor vars
mpv_data$`Victim's gender` <- as.factor(mpv_data$`Victim's gender`)
mpv_data$`Victim's race` <- as.factor(mpv_data$`Victim's race`)

# df of missing geocodes
missing_geocode <- filter(mpv_data, is.na(lon) | is.na(lat))

# remove missing geocodes from original df
mpv_data <- filter(mpv_data, !is.na(lon) & !is.na(lat))


## UI ------------------------------------------------------------------

# Choices for drop-downs
vars <- c(
  "none" = "none",
  "race (victim)" = "Victim's race",
  "age (victim)" = "Victim's age band",
  "gender (victim)" = "Victim's gender"
)

# make a navigation bar, set params
ui <- navbarPage("polMonitor", theme = shinytheme("cosmo"),
                 tabPanel("dot map",
                          div(class="outer",
                          tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css")
                          ),
                          # output area for leaflet map
                          leafletOutput("dot_map", width = "100%", height = "100%"),
                          # inset panel for map output options
                          absolutePanel(id="controls", class = "panel panel-default",
                                        top = 100, right = 20, left = "auto", bottom = "auto",
                                        draggable = TRUE, width = "auto", height = "auto",
                                        # map's date range 
                                        dateRangeInput(inputId="dates", label="occurred between:",
                                                       start = min(as.Date(mpv_data$`Date of injury resulting in death (month/day/year)`)),
                                                       end = max(as.Date(mpv_data$`Date of injury resulting in death (month/day/year)`)),
                                                       format = "d M yyyy", startview = "year"),
                                        selectInput(inputId="colour", label="colour", choices=vars,
                                                    selected=NULL, multiple=FALSE)
                          )
                          )
                 )
                 )


# SERVER ----------------------------------------------------------

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what dates the user selected
  filtered_data <- reactive({
    mpv_data[mpv_data$`Date of injury resulting in death (month/day/year)` >= input$dates[1] & 
               mpv_data$`Date of injury resulting in death (month/day/year)` <= input$dates[2],]
  })
   
  # create map output, define params that won't change dynamically 
  output$dot_map <- renderLeaflet({
    
    leaflet(mpv_data) %>% addProviderTiles("CartoDB.DarkMatter")
  })
  
  # observer to update map options selected
  observe({
    
    colourBy <- input$colour
    
    popup <- paste(sep = "<br/>", 
                   paste0("<img src='", mpv_data$`URL of image of victim`, "'  />"), 
                   paste0("<b>name: </b>", mpv_data$`Victim's name`))
    
    if (input$colour == "none") {
    
    leafletProxy("dot_map", data = filtered_data()) %>%
      clearShapes() %>%
      addCircles(~lon, ~lat, radius = 0.2, fillOpacity = 0.3, color="#DC143C", fillColor = "#DC143C",
                 layerId=~mpv_data, popup= ~popup)
      
    } else {
      
      colourData <- mpv_data[[colourBy]]
      pal <- colorFactor("viridis", colourData)
      
      leafletProxy("dot_map", data = filtered_data()) %>%
        clearShapes() %>%
        addCircles(~lon, ~lat, radius = 0.2, fillOpacity = 0.3, color=pal(colourData), 
                   fillColor = pal(colourData), popup= ~popup, layerId=~mpv_data) %>%
        addLegend("bottomleft", pal=pal, values=colourData, title=colourBy,
                  layerId="colourLegend")
      
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

