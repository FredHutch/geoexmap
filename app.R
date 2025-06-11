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

# load pm2.5 layer
#pm2.5 <- st_read("Demo Data/wa_tracts_pm2.5.gpkg")

# NDVI
#ndvi <- st_read("Demo Data/wa_tracts_ndvi.gpkg")

data <- readRDS("Data_Processed/wa_tracts_pm25_ndvi_lan.rds")
data <- st_read("Data_Processed/complete/geoexmap_data.gpkg") 

#names(data) <- gsub("\\.", " ", names(data))

# df_vars <- data %>% 
#   dplyr::select(c(pm2.5_t, ndvi_r, avg_rad))

df_vars <- data %>% 
  dplyr::select(c(2:76))

#names(df_vars) <- gsub("\\.", " ", names(df_vars))

# setnames(df_vars, old = names(df_vars),
#          new = c("Particulate Matter 2.5", "Green Space", "Nighttime Radiance", "Food Stamps", "Food Insecurity", "Housing Insecurity",
#                  "Utility Services Threat", "Lacking Reliable Transportation", "Lack of Social and Emotional Support", "Lack of Health Insurance",
#                  "Routine Checkup in the Past Year", "Visited Dentist in Past Year", "Taking Medicine to Control High Blood Pressure",
#                  "Cholesterol Screening", "Mammography Use among Women 50 to 74", "Colorectal Cancer Screening among Adults 45 to 75", 
#                  "Binge Drinking among Adults", "Cigarette Smoking among Adults", "No Leisure-time Physical Activity among Adults", 
#                  "Short Sleep Duration", "Arthritis among Adults", "Asthma among Adults", "High Blood Pressure among Adults", "Cancer or Melanoma among Adults", 
#                  "High Cholesterol among Screened Adults", "COPD among Adults", "Coronary Heart Disease among Adults", "Depression among Adults", "Diagnosed Diabetes among Adults",
#                  "Obesity among Adults", "All Teeth Lost among Adults 65 and Older", "Stroke among Adults",
#                  "Percent White NonHispanic", "Percent Black NonHispanic", "Percent American Indian Alaska Native NonHispanic",
#                  "Percent Asian NonHispanic", "Percent Native Hawaiian Pacific Islander NonHispanic", "Percent Other Race NonHispanic", 
#                  "Percent Two or More Races NonHispanic", "Percent Hispanic or Latino", "Percent White Hispanic or Latino", "Percent Black Hispanic or Latino", 
#                  "Percent American Indian Alaska Native Hispanic or Latino", "Percent Asian Hispanic or Latino", 
#                  "Percent Native Hawaiian Pacific Islander Hispanic or Latino", "Percent Other Race Hispanic or Latino", 
#                  "Percent Two or More Races Hispanic or Latino", "Percent Two or More Races Including Other Hispanic or Latino",
#                  "Percent Two or More Races Excluding Other Hispanic or Latino", "Percent Two or More Races Including Other NonHispanic", 
#                  "Percent Two or More Races Excluding Other NonHispanic", "Total Population", "Percent Male", "Percent Female", "Percent 0 to 4 years",
#                  "Percent 5 to 9 years", "Percent 10 to 14 years", "Percent 15 to 19 years", "Percent 20 to 24 years", "Percent 25 to 29 years", 
#                  "Percent 30 to 34 years", "Percent 35 to 39 years", "Percent 40 to 44 years", "Percent 45 to 49 years", "Percent 50 to 54 years",
#                  "Percent 55 to 59 years", "Percent 60 to 64 years", "Percent 65 to 69 years", "Percent 70 to 74 years", "Percent 75 to 79 years",
#                  "Percent 80 to 84 years", "Percent 85 and older", "Social Vulnerability Index", "Environmental Justice Index", "UV Index", "geometry"))

# define filters
health_outcomes <- df_vars %>% 
  dplyr::select(c(21:32))
#names(health_outcomes) <- gsub("\\.", " ", names(health_outcomes))

natural_env <- df_vars %>%
  dplyr::select(c(1))
#names(natural_env) <- gsub("\\.", " ", names(natural_env))

built_env <- df_vars %>%
  dplyr::select(c(2:3))
#names(built_env) <- gsub("\\.", " ", names(built_env))

health_outcomes_inp <- health_outcomes %>% 
  st_drop_geometry()

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
    "Health Outcomes", icon = bs_icon("heart-pulse"),
    varSelectizeInput("outcomes", label = "Select Measures", data = health_outcomes_inp, multiple = TRUE)
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
    varSelectizeInput('naturalenv', label = "Select Measures", data = natural_env_inp, selected = 'Particulate.Matter.2.5', multiple = TRUE)
  ),
  accordion_panel(
    "Built Environment", icon = bs_icon("buildings"),
    #!!!builtenv_filters
    varSelectizeInput('builtenv', label = "Select Measures", data = built_env_inp, selected = 'Green.Space', multiple = TRUE)
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
  g <- c("Green.Space")
  b <- c("Particulate.Matter.2.5", "Arthritis.among.Adults")
  n <- c("Nighttime.Radiance")
  
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
    if(col == "Green.Space") return("Normalized Difference Vegetation Index (NDVI)")
    if(col == "Nighttime.Radiance") return("Average Nighttime Radiance")
    if(col == "Arthritis.among.Adults") return("Arthritis Among Adults")
  }
  
  # TODO: pivot first (to longer), filter for selections (year, variables), then pivot wider for reactive values
  map_cols <- reactive({
    cbind(health_outcomes, natural_env, built_env) %>% 
      dplyr::select(!!!input$outcomes, !!!input$naturalenv, !!!input$builtenv)
    
  }) %>% 
    bindCache(input$outcomes, input$naturalenv, input$builtenv) %>% # reduce work by server
    bindEvent(list(input$outcomes, input$naturalenv, input$builtenv))
  
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
    
    if (ncol(plotly.dat) == 2) {
      plot_ly(data = plotly.dat, x = plotly.dat[,1]) %>% 
        layout(scene = list(xaxis = list(title = colnames(plotly.dat)[1]))) 
    } else if (ncol(plotly.dat) == 3) {
      plot_ly(data = plotly.dat, x = plotly.dat[,1], y = plotly.dat[,2]) %>% 
        layout(scene = list(xaxis = list(title = colnames(plotly.dat)[1]),
                            yaxis = list(title = colnames(plotly.dat)[2])))
    } else if (ncol(plotly.dat) == 4) {
      plot_ly(data = plotly.dat, x = plotly.dat[,1], y = plotly.dat[,2], z = plotly.dat[,3]) %>% 
        layout(scene = list(xaxis = list(title = colnames(plotly.dat)[1]),
                            yaxis = list(title = colnames(plotly.dat)[2]),
                            zaxis = list(title = colnames(plotly.dat)[3])))
    }
     
  })
  
  output$geoexmap <- renderLeaflet({
    #print(input$naturalenv, input$builtenv)
    
    map <- leaflet(map_cols()) %>% 
      setView(lng = -120.74, lat = 47.75, zoom = 7) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addSearchOSM()
  })
  
  observe({
    withProgress(message = "Plotting...", 
    {plotlyProxy("chart")
    
    leafletProxy("geoexmap", data = map_cols()) %>% 
      clearControls()  %>% 
      clearShapes() %>% 
      {
        # for each chosen column, define the palette, and add polygons
        for (c in colnames(map_cols())) {
          pal <- geoex.palette(c)
          
          # skip null to avoid geometry
          if (!is.null(pal)){
              addPolygons(., fillColor = ~pal(map_cols()[[c]]), stroke = input$showbounds, weight = 0.75, color = "black",
                          fillOpacity = 0.3, highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>% 
              addLegend(pal = pal, values = ~map_cols()[[c]], title = legend.titles(c))
          }
        }
      }}) 
  }) %>% 
    bindEvent(list(input$outcomes,input$naturalenv, input$builtenv))
}

# -------- CREATE SHINY APP --------

if (!interactive()) {
  options$shiny.port = 3838
  options$shiny.host = "0.0.0.0"
}

shinyApp(ui = ui, server = server, options = options)