# MC :)

# TODO:
# PM 2.5 (Done)
# Wildfires from MTBS
# Greenspace/NDVI (Done)
# LAN (Done)
# ADI
# EJI

library(shiny)
library(tidyverse)
library(sf)
#library(readxl)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(mapview)
library(plotly)
#library(crosstalk)
library(RColorBrewer)
library(bslib)
library(bsicons)
library(dplyr)
#library(tigris)
library(rsconnect)
library(rlang)

# load pm2.5 layer
#pm2.5 <- st_read("Demo Data/wa_tracts_pm2.5.gpkg")

# NDVI
#ndvi <- st_read("Demo Data/wa_tracts_ndvi.gpkg")

data <- readRDS("Data_Processed/wa_tracts_pm25_ndvi_lan.rds")

df_vars <- data %>% 
  dplyr::select(c(NAMELSAD, pm2.5_t, ndvi_r, avg_rad))

# define filters
natural_env <- df_vars %>%
  dplyr::select(c(NAMELSAD, pm2.5_t))

built_env <- df_vars %>%
  dplyr::select(c(NAMELSAD, ndvi_r, avg_rad))

natural_env_inp <- natural_env %>%
  st_drop_geometry() #%>% 
  #rename("Particulate Matter 2.5" = pm2.5_t)

built_env_inp <- built_env %>%
  st_drop_geometry()

# ADI

# EJI

#TODO: add filter for year 

# -------- UI ELEMENTS --------
cards <- list(
  card(
    full_screen = TRUE,
    card_header("geoexmap")
  )
)

categories <- accordion(
  accordion_panel(
    "Sociodemographics", icon = bs_icon("person-vcard")
  ),
  accordion_panel(
    "Health Outcomes", icon = bs_icon("heart-pulse")
  ),
  accordion_panel(
    "Health Behaviors", icon = bs_icon("person-walking")
  ),
  accordion_panel(
    "Prevention", icon = tags$img(src = "/prevention.png", height = "20.48px", width = "20.48 px")
  ),
  accordion_panel(
    "Healthcare Access", icon = bs_icon("building-add")
  ),
  accordion_panel(
    "Natural Environment", icon = bs_icon("sun"),
    #!!!naturalenv_filters
    varSelectizeInput('naturalenv', label = "Select Measures", data = natural_env_inp, selected = 'pm2.5_t', multiple = TRUE)
  ),
  accordion_panel(
    "Built Environment", icon = bs_icon("buildings"),
    #!!!builtenv_filters
    varSelectizeInput('builtenv', label = "Select Measures", data = built_env_inp, selected = 'ndvi_r', multiple = TRUE)
  ),
  accordion_panel(
    "Social Environment", icon = tags$img(src = "/social-environment.png", height = "20.48px", width = "20.48px")#icon = bs_icon("house")
  ),
  accordion_panel(
    "Options", icon = bs_icon("gear"),
    checkboxInput("showbounds", "Show Tract Boundaries", value = TRUE),
    fileInput("upload", "Upload a Shapefile"),
    actionButton("print", "Print map", onclick = "$('#geoexmap').print();")
  )
  
)

# -------- UI LAYOUT --------
ui <- page_sidebar(
  title = tags$img(src = "/geoexmap-logo.png", height = '92.32px', width = '214.8px'),
  window_title = "geoexmap",
  
  sidebar = categories,
  card(#full_screen = TRUE,
       card_header = ("Map"),
       leafletOutput("geoexmap")),
  card(card_header = ("Chart"),
       plotlyOutput("chart"))
)

# -------- SERVER --------
server <- function(input, output, session) {
  # define categories for palettes
  # "good", "bad", "neutral"
  g <- c("ndvi_r")
  b <- c("pm2.5_t")
  n <- c("avg_rad")
  
  # use colorQuantile
  # palette helper function
  geoex.palette <- function(var) {
    tryCatch({
      # skip geometry column to avoid error
      if (var == "geometry" || inherits(df_vars[[var]], "sfc")) {
        message("Skipping geometry column...")
        return(NULL)
      }
      
      domain = df_vars[[var]]
      bins = quantile(df_vars[[var]])
      
      if (var %in% g) {
        return(colorQuantile(
          palette = "YlGn", domain = domain,
          na.color = "transparent", n = 5
        ))
      } else if (var %in% n) {
        # TODO: have multiple colors for this, one for each neutral variable
        # for now, use blue
        if (var == "avg_rad") {
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
    if(col == "pm2.5_t") return("Particulate Matter (PM) 2.5")
    if(col == "ndvi_r") return("Normalized Difference Vegetation Index (NDVI)")
    if(col == "avg_rad") return("Average Nighttime Radiance")
  }
  
  # TODO: pivot first (to longer), filter for selections (year, variables), then pivot wider for reactive values
  map_cols <- reactive({
    cbind(natural_env, built_env) %>% 
      dplyr::select(!!!input$naturalenv, !!!input$builtenv)
    
  }) %>% 
    bindCache(input$naturalenv, input$builtenv) %>% # reduce work by server
    bindEvent(list(input$naturalenv, input$builtenv))
  
  # label.titles <- function(cols) {
  #   cols <- map_cols()
  #   for (col in colnames(cols)) {
  #     # get value
  #     lab <- col
  #     paste(lab)
  #   }
  # }
  output$chart <- renderPlotly({
    plotly.dat <- map_cols() %>% 
      as.data.frame()
    # probably an issue with naming
    plot_ly(data = plotly.dat, x = plotly.dat[,1], y = plotly.dat[,2], type = "scatter") 
  })
  
  output$geoexmap <- renderLeaflet({
    #print(input$naturalenv, input$builtenv)
    
    map <- leaflet(map_cols()) %>% 
      setView(lng = -120.74, lat = 47.75, zoom = 7) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addSearchOSM()
  })
  
  observe({
    plotlyProxy("chart")
    
    leafletProxy("geoexmap", data = map_cols()) %>% 
      clearControls()  %>% 
      clearShapes() %>% 
      {
        # for each chosen column, define the palette, and add polygons
        for (c in colnames(map_cols())) {
          pal <- geoex.palette(c)
          
          # skip null to avoid geometry
          if (!is.null(pal)){
              addPolygons(., fillColor = ~pal(map_cols()[[c]]), stroke = input$showbounds, weight = 0.75, color = "black", popup = df_vars$NAMELSAD,
                          fillOpacity = 0.3, highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>% 
              addLegend(pal = pal, values = ~map_cols()[[c]], title = legend.titles(c))
          }
        }
      } 
  })
}

# -------- CREATE SHINY APP --------

options <- list()

if (!interactive()) {
  options$shiny.port = 3838
  options$shiny.host = "0.0.0.0"
}

shinyApp(ui = ui, server = server, options = options)