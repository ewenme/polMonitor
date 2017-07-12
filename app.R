# SETUP ---------------------------------------------------------------

# pkgs
library(DT)
library(shiny)
library(shinythemes)
library(shinyjs)
library(tidyverse)
library(sf)
library(leaflet)
library(RColorBrewer)
library(viridis)
library(tigris)
library(tidycensus)
library(lubridate)
library(extrafont)

# LOAD ----------------------------------------------------------------

# load font
# font_import(pattern = "Work")

# load census data
censusData <- read_csv("censusData.csv")

#load state shapefile
states <- st_read("tl_2015_us_state")
state_names <- select(as.data.frame(states), STUSPS, NAME)

# load deaths data
mpv_data <- read_csv("geocodedMPVDataset.csv") %>%
  mutate(month=month(`Date of injury resulting in death (month/day/year)`, label=TRUE)) %>%
  left_join(state_names, by=c("Location of death (state)"="STUSPS"))


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
mpv_data$NAME <- as.factor(as.character(mpv_data$NAME))

# variables for data table
cleantable <- mpv_data %>%
  select(Name=`Victim's name`, Age=`Victim's age`, Gender=`Victim's gender`, Race=`Victim's race`, 
         Date=`Date of injury resulting in death (month/day/year)`, 
         City=`Location of death (city)`, State=NAME, Zipcode=`Location of death (zip code)`, 
         `Agency responsible for death`, `Cause of death`, `Armament`=Unarmed, 
         Description=`A brief description of the circumstances surrounding the death`,
         Evidence=`Link to news article or photo of official document`) %>%
  mutate(Evidence=paste0("<a href='",Evidence,"'>", "link","</a>"),
         Date=ymd(Date))

## UI ------------------------------------------------------------------

# Choices for drop-downs
vars <- c("none" = "none", "race (victim)" = "Victim's race", "age (victim)" = "Victim's age band",
          "gender (victim)" = "Victim's gender")

# make a navigation bar, set params
ui <- navbarPage(title="polMonitor", theme = shinytheme("cosmo"), collapsible = TRUE,
                 
                 tabPanel(shinyjs::useShinyjs(), title="map",
                          div(class="outer",
                          tags$head(
                            # Include custom CSS
                            includeCSS("styles.css")
                          ),
                          # output area for leaflet map
                          leafletOutput("map", width = "100%", height = "100%"),
                          tags$div(id="cite",
                                   'police killings compiled by ', tags$a(href='https://mappingpoliceviolence.org', 
                                                                          "Mapping Police Violence"), 
                                   '| population data taken from the', tags$a(href='https://www.census.gov/2010census/data/',
                                                                      "U.S. Census (2010)")
                          ),
                          # inset panel for map output options
                          absolutePanel(id="controls", class = "panel panel-default",
                                        top = 25, right = 100, left = "auto", bottom = "auto",
                                        draggable = TRUE, width = "350px", height = "auto",
                                        # map type
                                        radioButtons(inputId="mapType", "map type",
                                                     choices=c("dot", "choropleth"),
                                                     selected = "dot", inline = TRUE, width = "auto"),
                                        hr(),
                                        # map's date range
                                        dateRangeInput(inputId="dates", label="occurred between",
                                                       start = min(as.Date(mpv_data$`Date of injury resulting in death (month/day/year)`)),
                                                       end = max(as.Date(mpv_data$`Date of injury resulting in death (month/day/year)`)),
                                                       format = "d M yyyy", startview = "year"),
                                        conditionalPanel(condition = "input.mapType == 'dot'",
                                                         selectInput(inputId="state", label="state", 
                                                                     choices=c("all", levels(mpv_data$NAME)),
                                                                     selected="all", multiple=FALSE)),
                                        conditionalPanel(condition = "input.mapType == 'dot'",
                                                         selectInput(inputId="colour", label="colour", 
                                                                     choices=vars, selected=NULL, 
                                                                     multiple=FALSE)),
                                        hr(),
                                        a(id = "toggleFilters", "show/hide filters", href = "#"),
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
                                        
                                        conditionalPanel(condition = "input.mapType == 'dot'",
                                                         hr(),
                                                         tags$br(),
                                        plotOutput(outputId="cumPlot", height = 200))
                          )
                          )
                 ),
                 navbarMenu("more",
                 tabPanel(title="data",
                          fluidRow(
                            column(3, 
                                   selectInput("states", "states", c("All states"="", 
                                                                     levels(cleantable$State),
                                                                     multiple=TRUE)
                                   )),
                                   column(3, conditionalPanel("input.states", 
                                                              selectInput("cities", "Cities",
                                                                          c("All cities"=""), 
                                                                          multiple=TRUE)
                                   )),
                                   column(3, conditionalPanel("input.states",
                                                              selectInput("zipcodes", "Zipcodes", 
                                                                          c("All zipcodes"=""), 
                                                                          multiple=TRUE)
                                   ))),
                          hr(),
                          
                          DT::dataTableOutput("table")),
                 tabPanel(title="about",
                          fluidRow(
                            column(6,
                                   includeMarkdown("about.Rmd")
                            ))))
                 )
                 


# SERVER ----------------------------------------------------------

server <- function(input, output, session) {
  
  
  # MAP -----------------------------------------
  
  # observer for hide/show filters
  observe({
    shinyjs::onclick("toggleFilters",
                     shinyjs::toggle(id = "filters", anim = TRUE)) 
  })
  
  # Reactive expression for deaths data subsetted to what dates the user selected
  filtered_data <- reactive({
    
    if (input$mapType == "dot" & input$state == "all") {
    subset(mpv_data, `Date of injury resulting in death (month/day/year)` >= input$dates[1] & 
             `Date of injury resulting in death (month/day/year)` <= input$dates[2] & 
             `Victim's race` %in% input$race & `Victim's gender` %in% input$gender & 
             `Victim's age band` %in% input$age)
    } else if (input$mapType == "dot" & input$state != "all") {
      subset(mpv_data, `Date of injury resulting in death (month/day/year)` >= input$dates[1] & 
               `Date of injury resulting in death (month/day/year)` <= input$dates[2] & 
               `Victim's race` %in% input$race & `Victim's gender` %in% input$gender & 
               `Victim's age band` %in% input$age & NAME %in% input$state)
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
  
  # A reactive expression that returns data points in bounds right now
  dataInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(mpvdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(mpv_data, lat >= latRng[1] & lat <= latRng[2] &
           lon >= lngRng[1] & lon <= lngRng[2])
  })
  
  # render cumulative plot
  output$cumPlot <- renderPlot({
    req(input$map_bounds)
    
    dataInBounds() %>%
      filter(year >= max(year)-1) %>%
      group_by(year, month) %>%
      summarise(count=n()) %>%
      group_by(year) %>%
      mutate(Total = cumsum(count)) %>%
      ggplot(aes(x=month, y=Total, group=year, colour=factor(year))) +
      geom_line() +
      labs(x="month", y="no. people killed") +
      scale_x_discrete(breaks=c("Feb", "Apr", "Jun", "Aug", "Oct", "Dec")) +
      scale_color_manual(values = c("white", "#DC143C")) +
      theme(
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.background = element_rect(fill = "black"),
        legend.key = element_blank(),
        legend.text = element_text(colour = "white", size = 12, family="WorkSans-Regular"),
        axis.text = element_text(colour = "white", size = 10, family="WorkSans-Regular"),
        axis.title = element_text(colour = "white", size = 12, family="WorkSans-Regular"),
        legend.position = "top"
      )
    
  })
   
  # create map output, define params that won't change dynamically 
  output$map <- renderLeaflet({
    
    leaflet(mpv_data) %>% addProviderTiles("CartoDB.DarkMatter") %>%
      setView(lng = mean(mpv_data$lon), lat = mean(mpv_data$lat),
               zoom = 3)
  })
  
  # observer to update map options selected
  observe({
    
    colourBy <- input$colour
    map_type <- input$mapType
    
    popup <- paste(sep = "<br/>", 
                   paste0("<img src='", filtered_data()$`URL of image of victim`, "' height='120' width='120' />"), 
                   paste0("<br/><b>name: </b>", filtered_data()$`Victim's name`),
                   paste0("<b>date of injury leading to death: </b>", format(filtered_data()$`Date of injury resulting in death (month/day/year)`, format = "%A, %d %B %Y")),
                   paste0("<b>location: </b>", filtered_data()$address),
                   paste0("<a href='", filtered_data()$`Link to news article or photo of official document`, "'>news article / photo of official doc</a>"))
    
    proxy <- leafletProxy("map", data = filtered_data()) %>%
      clearShapes()
    
    if (colourBy == "none" & map_type == "dot") {
    
      proxy %>%
      clearControls() %>%
      addCircles(~lon, ~lat, radius = 0.2, fillOpacity = 0.3, color="#DC143C", fillColor = "#DC143C",
                 popup= ~popup) %>%
        setView(lng = mean(filtered_data()$lon), lat = mean(filtered_data()$lat),
                zoom = if_else(input$state == "all", 3, 5))
      
    } else if (colourBy != "none" & map_type == "dot") {
      
      colourData <- mpv_data[[colourBy]]
      pal <- colorFactor("viridis", colourData)
      
        proxy %>%
        clearControls() %>%
        addCircles(~lon, ~lat, radius = 0.2, fillOpacity = 0.3, color=pal(colourData), 
                   fillColor = pal(colourData), popup= ~popup, layerId=~mpv_data) %>%
        addLegend("bottomleft", pal=pal, values=colourData, title=colourBy,
                  layerId="dotLegend") %>%
        setView(lng = mean(filtered_data()$lon), lat = mean(filtered_data()$lat),
                zoom = if_else(input$state == "all", 3, 5))
      
    } 
    
  })
  
  # observer map inputs for choropleth
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
      mutate(death_per_mil=death_count/value*1000000) %>%
      filter(!is.na(death_count)) %>%
      st_as_sf() %>%
      st_transform(crs = "+init=epsg:4326")
    
  pal <- colorBin(palette = "magma", domain = round(data$death_per_mil, digits = 1), 
                  bins = 5, reverse = TRUE, na.color = "#808080")
  
  labels <- sprintf("<strong>%s</strong><br/> <strong>deaths:</strong> %s<br/> <strong>residents:</strong>
                    %s <br/> %.01f deaths / million residents",
                    data$NAME.x, data$death_count, prettyNum(data$value, big.mark = ","), 
                    data$death_per_mil) %>% lapply(htmltools::HTML)
  
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
  
  
  # TABLE -------------------------------------------------------
  
  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
                      selected = stillSelected)
  })
  
  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states, is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
                      selected = stillSelected)
  })
  
  # output for table of data inbounds
  output$table <- DT::renderDataTable({
    cleantable %>%
      filter(is.null(input$states) | State %in% input$states,
             is.null(input$cities) | City %in% input$cities,
             is.null(input$zipcodes) | Zipcode %in% input$zipcodes) %>%
      DT::datatable(rownames = FALSE, escape = FALSE,
                    options = list(pageLength = 5, dom = 'tip',
                                   autoWidth = TRUE, 
                                   columnDefs = list(list(className = 'dt-left', targets = 0:3)),
                                   initComplete = JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                     "$(this.api().table().body()).css({'background-color': '#000', 'color': '#fff'});",
                                     "}"))) %>%
      DT::formatStyle(columns = 1:13, color = "black")
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

