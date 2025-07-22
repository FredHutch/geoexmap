# MC :)

#TODO ELEMENTS:
## modal for intro/welcome message
## popover for actionable links
## add land use category (built environment)
## add table features
#library(waiter)

library(shiny)
library(shinyjs)
library(htmltools)

library(tidyverse)
library(sf)
library(data.table)

library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(mapview)
library(plotly)

library(RColorBrewer)
library(bslib)
library(bsicons)

library(dplyr)

library(rsconnect)
library(rlang)

data <- st_read("Data_Processed/complete/geoexmap_data.gpkg") 


# read point data
transit <- st_read("Data_Processed/complete/geoexmap_data.gpkg",
                   layer = "transit")

data <- data %>% 
  select(c(Particulate.Matter.2.5))

# -------- UI ELEMENTS --------
categories <- accordion(
  accordion_panel(
    "Natural Environment", icon = bs_icon("sun"),
    varSelectInput('naturalenv', selectize = TRUE, label = span("Select Measures", 
                                                                popover(bs_icon("lightbulb"),
                                                                        "See actionable tips for this category",
                                                                        title = "Actionable Tips",
                                                                        placement = "right")), data = data, multiple = TRUE)
  ),
  accordion_panel(
    "Built Environment", icon = bs_icon("buildings"),
    input_switch('transit', "Transit Stops", value = FALSE))
)

# -------- UI LAYOUT --------
ui <- page_navbar(
  title = tags$img(src = "/geoexmap_edit.png", height = '57.62px', width = '165.08px'),
  nav_spacer(),
  nav_panel("Map",
            layout_sidebar(
              sidebar = categories,
              leafletOutput("geoexmap"),
            )),
  window_title = "geoexmap | Geospatial Exposome Map at Fred Hutch Cancer Center"
  
)

# -------- SERVER --------
server <- function(input, output, session) {
  # define categories for palettes
  # "good", "bad", "neutral"
  g <- c("Green.Space")
  b <- c("Particulate.Matter.2.5") 
  n <- c("Nighttime.Radiance")#, names(sociodemo_inp))
  
  # palette helper function
  geoex.palette <- function(var) {
    tryCatch({
      # skip geometry column to avoid error
      if (var == "geometry" || inherits(data[[var]], "sfc")) {
        message("Skipping geometry column...")
        return(NULL)
      }
      
      domain = data[[var]]
      
      if (var %in% g) {
        return(colorQuantile(
          palette = "YlGn", domain = domain,
          na.color = "transparent", n = 5
        ))
      } else if (var %in% n) {
        # TODO: have multiple colors for this, one for each neutral variable
        # for now, use blue
        if (var == "Nighttime.Radiance") {
          return(colorQuantile(
            palette = c("#2E4057", "#96ADC8", "#D2CCA1", "#D96C06", "#C44536"), domain = domain,
            na.color="transparent", n = 5
          ))
        } else return(colorQuantile(
              palette = "Blues", domain = domain,
              na.color = "transparent", n = 5
        ))
        # otherwise, var is in "bad"
      } else {
        return(colorQuantile(
          palette = "YlOrRd", domain = domain,
          na.color = "transparent", n = 5
        ))
      }
    },
    
    error = function(e) {
      message("error in geoex.palette: ", e$message)
      return(NULL)
    })
  }
  
  legend.titles <- function(col) {
    if(col == "Particulate.Matter.2.5") return("Particulate Matter (PM) 2.5")
  }
  
  map_cols <- reactive({
    data %>% 
      dplyr::select(!!!input$naturalenv)
    
  }) %>% 
    bindCache(input$naturalenv) %>% # reduce work by server
    bindEvent(list(input$naturalenv))
  
  
  output$geoexmap <- renderLeaflet({
    map <- leaflet(map_cols()) %>% 
      setView(lng = -120.74, lat = 47.75, zoom = 7) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addSearchOSM()
  })
  
  observeEvent(input$transit, {
    {
      leafletProxy("geoexmap", data = transit) %>%
        {
          if (input$transit) {
            addMarkers(data = transit, lng = ~stop_lon, lat = ~stop_lat,
                       icon = bs_icon("bus-front"))
          }
        }
    }
  })
  
  observe({
    withProgress(message = "Plotting...", 
    {
      leafletProxy("geoexmap", data = map_cols()) %>% 
      clearControls()  %>% 
      clearShapes() %>% 
      {
        # for each chosen column, define the palette, and add polygons
        label = ""
        if (input$transit) {
          print("transit")
          #addMarkers(data = transit, lng = ~stop_lon, lat = ~stop_lat,
                                           #icon = bs_icon("bus-front"))
        }
        for (c in colnames(map_cols())) {
          print(c)
          pal <- geoex.palette(c)
          
          # skip null to avoid geometry
          if (!is.null(pal)){
              addPolygons(., fillColor = ~pal(map_cols()[[c]]), stroke = input$showbounds, weight = 0.75, color = "black",
                          fillOpacity = 0.3, highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
                          label = "Hey") %>% 
              addLegend(pal = pal, values = ~map_cols()[[c]], title = legend.titles(c)) %>% 
              addEasyprint(options = easyprintOptions()) 
          }
        }
      } 
    })  
  }) %>% 
    bindEvent(list(input$naturalenv, input$transit))
}

# -------- CREATE SHINY APP --------

options <- list()

if (!interactive()) {
  options$shiny.port = 3838
  options$shiny.host = "0.0.0.0"
}

shinyApp(ui = ui, server = server, options = options)