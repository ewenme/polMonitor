# SETUP ---------------------------------------------------------------

setwd("~/Documents/Github/polMonitor")

# pkgs
library(shiny)
library(shinythemes)
library(tidyverse)
library(sf)
library(leaflet)
library(RColorBrewer)
library(forcats)
library(viridis)
library(tigris)
library(tidycensus)


# LOAD ----------------------------------------------------------------

#load state shapefile
states <- states()

# join them
states <- geo_join(spatial_data = states, data_frame = pops, by="GEOID")

# convert to sf
states <- st_as_sf(states)

# load deaths data
mpv_data <- read_csv("geocodedMPVDataset.csv")

# test sf chloropeth
test <- mpv_data %>%
  group_by(`Location of death (state)`) %>%
  summarise(death_count=n()) %>%
  complete(`Location of death (state)`, fill = list(death_count = 0)) %>%
  right_join(states, by=c("Location of death (state)"="STUSPS")) %>%
  mutate(death_per_mil=round(death_count/population*1000000), digits=1) %>%
  filter(!is.na(death_count)) %>%
  st_as_sf()

pal <- colorBin(palette = "magma", domain = test$death_per_mil, bins = 5, reverse = TRUE)

labels <- sprintf(
  "<strong>%s</strong><br/>%g deaths / million residents",
  test$NAME, test$death_per_mil
) %>% lapply(htmltools::HTML)

test %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addPolygons(fillOpacity = 0.7,
              dashArray = "3",
              color = "white",
              weight = 1.5,
              fillColor = ~ pal(death_per_mil),
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend("bottomleft", 
            pal = pal, 
            values = ~ death_per_mil,
            title = "deaths per million residents",
            opacity = 1)


# CLEAN ----------------------------------------------------------------

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
ui <- navbarPage(title="polMonitor", theme = shinytheme("cosmo"),
                 
                 tabPanel(shinyjs::useShinyjs(), title="dot map",
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
                                        draggable = TRUE, width = "350px", height = "auto",
                                        # map's date range 
                                        dateRangeInput(inputId="dates", label="occurred between:",
                                                       start = min(as.Date(mpv_data$`Date of injury resulting in death (month/day/year)`)),
                                                       end = max(as.Date(mpv_data$`Date of injury resulting in death (month/day/year)`)),
                                                       format = "d M yyyy", startview = "year"),
                                        selectInput(inputId="colour", label="colour", choices=vars,
                                                    selected=NULL, multiple=FALSE),
                                        a(id = "toggleFilters", "Show/hide filters", href = "#"),
                                        shinyjs::hidden(div(id="filters",
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
                                                           inline = TRUE, width = "auto")
                                        )
                                        )
                          )
                          )
                 )
                 )


# SERVER ----------------------------------------------------------

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what dates the user selected
  filtered_data <- reactive({
    
    subset(mpv_data, `Date of injury resulting in death (month/day/year)` >= input$dates[1] & 
             `Date of injury resulting in death (month/day/year)` <= input$dates[2] & 
             `Victim's race` %in% input$race & `Victim's gender` %in% input$gender & 
             `Victim's age band` %in% input$age)
    
  })
  
  observe({
    shinyjs::onclick("toggleFilters",
                     shinyjs::toggle(id = "filters", anim = TRUE)) 
  })
   
  # create map output, define params that won't change dynamically 
  output$dot_map <- renderLeaflet({
    
    leaflet(mpv_data) %>% addProviderTiles("CartoDB.DarkMatter") %>%
      setView(lng = -93.85, lat = 37.45, zoom = 3)
  })
  
  # observer to update map options selected
  observe({
    
    colourBy <- input$colour
    
    popup <- paste(sep = "<br/>", 
                   paste0("<img src='", mpv_data$`URL of image of victim`, "' height='120' width='120' />"), 
                   paste0("<br/><b>name: </b>", mpv_data$`Victim's name`),
                   paste0("<b>date of injury leading to death: </b>", format(mpv_data$`Date of injury resulting in death (month/day/year)`, format = "%A, %d %B %Y")),
                   paste0("<a href='", mpv_data$`Link to news article or photo of official document`, "'>news article / photo of official doc</a>"))
    
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

