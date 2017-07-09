# SETUP ---------------------------------------------------------------

setwd("~/Documents/Github/polMonitor")

# pkgs
library(shiny)
library(shinythemes)
library(tidyverse)
library(sf)
library(leaflet)
library(RColorBrewer)
library(viridis)
library(tigris)
library(tidycensus)
library(lubridate)


# LOAD ----------------------------------------------------------------

# load census data
censusData <- read_csv("censusData.csv")

#load state shapefile
states <- st_read("tl_2015_us_state")

# load deaths data
mpv_data <- read_csv("geocodedMPVDataset.csv") %>%
  mutate(month=month(`Date of injury resulting in death (month/day/year)`, label=TRUE))


# CLEAN ----------------------------------------------------------------

# df of missing geocodes
missing_geocode <- filter(mpv_data, is.na(lon) | is.na(lat))

# remove missing geocodes from original df
mpv_data <- filter(mpv_data, !is.na(lon) & !is.na(lat))

# factor vars
censusData$gender <- as.factor(censusData$gender)
censusData$age_band <- as.factor(censusData$age_band)
censusData$race <- as.factor(censusData$race)
mpv_data$`Victim's gender` <- as.factor(mpv_data$`Victim's gender`)
mpv_data$`Victim's age band` <- as.factor(mpv_data$`Victim's age band`)
mpv_data$`Victim's race` <- as.factor(mpv_data$`Victim's race`)


## UI ------------------------------------------------------------------

# Choices for drop-downs
vars <- c(
  "none" = "none", "race (victim)" = "Victim's race", "age (victim)" = "Victim's age band",
  "gender (victim)" = "Victim's gender"
)

# make a navigation bar, set params
ui <- navbarPage(title="polMonitor", theme = shinytheme("cosmo"),
                 
                 tabPanel(shinyjs::useShinyjs(), title="map",
                          div(class="outer",
                          tags$head(
                            # Include custom CSS
                            includeCSS("styles.css")
                          ),
                          # output area for leaflet map
                          leafletOutput("map", width = "100%", height = "100%"),
                          # inset panel for map output options
                          absolutePanel(id="controls", class = "panel panel-default",
                                        top = 100, right = 100, left = "auto", bottom = "auto",
                                        draggable = TRUE, width = "350px", height = "auto",
                                        # map type
                                        radioButtons(inputId="mapType", "map type",
                                                     choices=c("dot", "choropleth"),
                                                     selected = "dot", inline = TRUE, width = "auto"),
                                        # map's date range
                                        dateRangeInput(inputId="dates", label="occurred between:",
                                                       start = min(as.Date(mpv_data$`Date of injury resulting in death (month/day/year)`)),
                                                       end = max(as.Date(mpv_data$`Date of injury resulting in death (month/day/year)`)),
                                                       format = "d M yyyy", startview = "year"),
                                        conditionalPanel(condition = "input.mapType == 'dot'",
                                                         selectInput(inputId="colour", label="colour", 
                                                                     choices=vars, selected=NULL, 
                                                                     multiple=FALSE)),
                                        a(id = "toggleFilters", "Show/hide filters", href = "#"),
                                        tags$br(),
                                        shinyjs::hidden(div(id="filters",
                                                            conditionalPanel(
                                                              condition = "input.mapType == 'dot'",
                                        checkboxGroupInput(inputId="race", label="race (victim)",
                                                           choices=levels(mpv_data$`Victim's race`),
                                                           selected=levels(mpv_data$`Victim's race`),
                                                           inline = TRUE, width = "auto"),
                                        checkboxGroupInput(inputId="gender", label="gender (victim)",
                                                           choices=levels(mpv_data$`Victim's gender`),
                                                           selected=levels(mpv_data$`Victim's gender`),
                                                           inline = TRUE, width = "auto"),
                                        checkboxGroupInput(inputId="age", label="age (victim)",
                                                           choices=levels(mpv_data$`Victim's age band`),
                                                           selected=levels(mpv_data$`Victim's age band`),
                                                           inline = TRUE, width = "auto")),
                                        conditionalPanel(
                                          condition = "input.mapType == 'choropleth'",
                                          checkboxGroupInput(inputId="raceCensus", label="race (victim)",
                                                             choices=levels(censusData$race),
                                                             selected=levels(censusData$race),
                                                             inline = TRUE, width = "auto"),
                                          checkboxGroupInput(inputId="genderCensus", label="gender (victim)",
                                                             choices=levels(censusData$gender),
                                                             selected=levels(censusData$gender),
                                                             inline = TRUE, width = "auto"),
                                          checkboxGroupInput(inputId="ageCensus", label="age (victim)",
                                                             choices=levels(censusData$age_band),
                                                             selected=levels(censusData$age_band),
                                                             inline = TRUE, width = "auto"))
                                        )
                                        ),
                                        tags$br(),
                                        plotOutput(outputId="cumPlot", height = 220)
                          )
                          )
                 )
                 )


# SERVER ----------------------------------------------------------

server <- function(input, output, session) {
  
  # Reactive expression for deaths data subsetted to what dates the user selected
  filtered_data <- reactive({
    
    if (input$mapType == "dot") {
    subset(mpv_data, `Date of injury resulting in death (month/day/year)` >= input$dates[1] & 
             `Date of injury resulting in death (month/day/year)` <= input$dates[2] & 
             `Victim's race` %in% input$race & `Victim's gender` %in% input$gender & 
             `Victim's age band` %in% input$age)
    } else if (input$mapType == "choropleth") {
      subset(mpv_data, `Date of injury resulting in death (month/day/year)` >= input$dates[1] & 
               `Date of injury resulting in death (month/day/year)` <= input$dates[2] & 
               `Victim's race` %in% input$raceCensus & `Victim's gender` %in% input$genderCensus & 
               `Victim's age band` %in% input$ageCensus)  
    }
    
  })
  
  # Reactive expression for census data, behaving similarly to the above expression
  filtered_census <- reactive({
    
    if (input$mapType == "choropleth")
    # census data to match user inputs
    subset(censusData, 
           race %in% input$raceCensus & gender %in% input$genderCensus & age_band %in% input$ageCensus)
    else
      NULL
  })
  
  # observer for hide/show filters
  observe({
    shinyjs::onclick("toggleFilters",
                     shinyjs::toggle(id = "filters", anim = TRUE)) 
  })
  
  # render cumulative plot
  output$cumPlot <- renderPlot({
    
    filtered_data() %>%
      filter(year >= max(year)-1) %>%
      group_by(year, month) %>%
      summarise(count=n()) %>%
      group_by(year) %>%
      mutate(Total = cumsum(count)) %>%
      ggplot(aes(x=month, y=Total, group=year, colour=factor(year))) +
      geom_line() +
      labs(x="month", y="people killed by police") +
      scale_x_discrete(breaks=c("Feb", "Apr", "Jun", "Aug", "Oct", "Dec")) +
      scale_color_manual(values = c("white", "#DC143C")) +
      theme(
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.background = element_rect(fill = "black"),
        legend.key = element_blank(),
        legend.text = element_text(colour = "white", size = 12),
        axis.text = element_text(colour = "white", size = 10),
        axis.title = element_text(colour = "white", size = 12),
        legend.position = "top"
      )
    
  })
   
  # create map output, define params that won't change dynamically 
  output$map <- renderLeaflet({
    
    leaflet(mpv_data) %>% addProviderTiles("CartoDB.DarkMatter") %>%
      setView(lng = -93.85, lat = 45, zoom = 3)
  })
  
  # observer to update map options selected
  observe({
    
    colourBy <- input$colour
    map_type <- input$mapType
    
    popup <- paste(sep = "<br/>", 
                   paste0("<img src='", mpv_data$`URL of image of victim`, "' height='120' width='120' />"), 
                   paste0("<br/><b>name: </b>", mpv_data$`Victim's name`),
                   paste0("<b>date of injury leading to death: </b>", format(mpv_data$`Date of injury resulting in death (month/day/year)`, format = "%A, %d %B %Y")),
                   paste0("<a href='", mpv_data$`Link to news article or photo of official document`, "'>news article / photo of official doc</a>"))
    
    if (colourBy == "none" & map_type == "dot") {
    
    leafletProxy("map", data = filtered_data()) %>%
      clearShapes() %>%
      clearControls() %>%
      addCircles(~lon, ~lat, radius = 0.2, fillOpacity = 0.3, color="#DC143C", fillColor = "#DC143C",
                 popup= ~popup, layerId=~mpv_data)
      
    } else if (colourBy != "none" & map_type == "dot") {
      
      colourData <- mpv_data[[colourBy]]
      pal <- colorFactor("viridis", colourData)
      
      leafletProxy("map", data = filtered_data()) %>%
        clearShapes() %>%
        clearControls() %>%
        addCircles(~lon, ~lat, radius = 0.2, fillOpacity = 0.3, color=pal(colourData), 
                   fillColor = pal(colourData), popup= ~popup, layerId=~mpv_data) %>%
        addLegend("bottomleft", pal=pal, values=colourData, title=colourBy,
                  layerId="dotLegend")
      
    } 
    
  })
  
  observe( {
    
    if (input$mapType == "choropleth") {
    
    # make summary population figure for each state
    data <- filtered_census() %>%
      group_by(GEOID, NAME) %>%
      summarise(value=sum(value, na.rm=TRUE))
    
    # join w/ shapefile
    states <- left_join(x = states, y = data, by="GEOID")
    
    data <- filtered_data() %>%
      group_by(`Location of death (state)`) %>%
      summarise(death_count=n()) %>%
      complete(`Location of death (state)`, fill = list(death_count = 0)) %>%
      right_join(states, by=c("Location of death (state)"="STUSPS")) %>%
      mutate(death_per_mil=round(death_count/value*1000000), digits=1) %>%
      filter(!is.na(death_count)) %>%
      st_as_sf() %>%
      st_transform(crs = "+init=epsg:4326")
    
  pal <- colorBin(palette = "magma", domain = data$death_per_mil, bins = 5, reverse = TRUE,
                  na.color = "#808080")
  
  labels <- sprintf("<strong>%s</strong><br/>%g deaths / million residents",
                    data$NAME.x, data$death_per_mil) %>% lapply(htmltools::HTML)
  
  leafletProxy("map", data=data) %>%
    clearControls() %>%
    clearMarkers() %>%
    clearShapes() %>%
    addPolygons(fillOpacity = 0.7, dashArray = "3", color = "white", weight = 1.5,
                fillColor = ~ pal(death_per_mil),
                highlight = highlightOptions(
                  weight = 5, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px", direction = "auto")) %>%
    addLegend("bottomleft", pal = pal, values = ~ death_per_mil, title = "deaths per million residents",
              opacity = 1, layerId="choroLegend") 
    } else
      NULL
  
})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

