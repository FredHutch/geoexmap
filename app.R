# MC :)

#TODO ELEMENTS:
## modal for intro/welcome message
## add land use category (built environment)
## add table features
#library(waiter)

library(shiny)
library(shinyjs)
library(htmltools)
#library(reactable)
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
#library(shinyBS)
library(dplyr)

library(rsconnect)
library(rlang)

data <- st_read("Data_Processed/complete/geoexmap_data.gpkg") 

food <- st_read("Data_Processed/complete/geoexmap_data.gpkg", layer = "food_env")

# read point data
transit <- st_read("Data_Processed/complete/geoexmap_data.gpkg",
                   layer = "transit")

data$Binge.Drinking.among.Adults <- as.numeric(data$Binge.Drinking.among.Adults)
data$Cigarette.Smoking.among.Adults <- as.numeric(data$Cigarette.Smoking.among.Adults)
data$No.Leisure.time.Physical.Activity.among.Adults <- as.numeric(data$No.Leisure.time.Physical.Activity.among.Adults)
data$Short.Sleep.Duration <- as.numeric(data$Short.Sleep.Duration)

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

health_behaviors <- df_vars %>% 
  dplyr::select(c(17:20))

health_prevention <- df_vars %>% 
  dplyr::select(c(10:16))

natural_env <- df_vars %>%
  dplyr::select(c(1, 75))
#names(natural_env) <- gsub("\\.", " ", names(natural_env))

built_env <- df_vars %>%
  dplyr::select(c(2:3))
#names(built_env) <- gsub("\\.", " ", names(built_env))

sociodemo <- df_vars %>% 
  dplyr::select(c(33:72))

#race <- df_vars %>% 
#  dplyr::select()

social_env <- df_vars %>% 
  dplyr::select(c(4:9, 73:74))

food_env <- food %>% 
  dplyr::select(c(11:50))

health_outcomes_inp <- health_outcomes %>% 
  st_drop_geometry()

health_behaviors_inp <- health_behaviors %>% 
  st_drop_geometry()

health_prevention_inp <- health_prevention %>% 
  st_drop_geometry()

natural_env_inp <- natural_env %>%
  st_drop_geometry() 

sociodemo_inp <- sociodemo %>% 
  st_drop_geometry()

social_env_inp <- social_env %>% 
  st_drop_geometry()

built_env_inp <- built_env %>%
  st_drop_geometry()

food_env_inp <- food_env %>% 
  st_drop_geometry()

# define markdown text for tips
food_env_md <- markdown("
Need help finding food? See: [Feeding Washington](https://feedingwashington.org/find-food/)
                        ")


# -------- UI ELEMENTS --------
categories <- accordion(
  accordion_panel(
    "Sociodemographics", icon = bs_icon("person-vcard"),
    varSelectInput('sociodemo', selectize = TRUE, label = span("Select Measures", #change to total population 
                                                                    popover(bs_icon("lightbulb"),
                                                                            "See actionable tips for this category",
                                                                            title = "Actionable Tips",
                                                                            placement = "right")),
                   data = sociodemo_inp, multiple = TRUE),
    accordion_panel("Race and Ethnicity", varSelectInput('race', label = NULL, data = sociodemo, multiple = TRUE)),
    accordion_panel("Sex", varSelectInput('sex', label = NULL, data = sociodemo, multiple = TRUE)),
    accordion_panel("Age", varSelectInput('age', label = NULL, data = sociodemo, multiple = TRUE))
    
  ),
  accordion_panel(
    "Health Outcomes", icon = bs_icon("heart-pulse"),
    varSelectInput('outcomes', selectize = TRUE, label = span("Select Measures", 
                                                              popover(bs_icon("lightbulb"),
                                                                      "See actionable tips for this category",
                                                                      title = "Actionable Tips",
                                                                      placement = "right")), data = health_outcomes_inp, multiple = TRUE)
  ),
  accordion_panel(
    "Health Behaviors", icon = bs_icon("person-walking"),
    varSelectInput('behaviors', selectize = TRUE, label = span("Select Measures", 
                                                               popover(bs_icon("lightbulb"),
                                                                       "See actionable tips for this category",
                                                                       title = "Actionable Tips",
                                                                       placement = "right")), data = health_behaviors_inp, multiple = TRUE)
  ),
  accordion_panel(
    "Prevention", icon = tags$img(src = "/prevention.png", height = "20.48px", width = "20.48 px"),
    varSelectInput('prevention', selectize = TRUE, label = span("Select Measures", 
                                                                popover(bs_icon("lightbulb"),
                                                                        "See actionable tips for this category",
                                                                        title = "Actionable Tips",
                                                                        placement = "right")), data = health_prevention_inp, multiple = TRUE)
  ),
  accordion_panel(
    "Healthcare Access", icon = bs_icon("building-add")
  ),
  accordion_panel(
    "Natural Environment", icon = bs_icon("sun"),
    varSelectInput('naturalenv', selectize = TRUE, label = span("Select Measures", 
                                                                popover(bs_icon("lightbulb"),
                                                                        "See actionable tips for this category",
                                                                        title = "Actionable Tips",
                                                                        placement = "right")), data = natural_env_inp, selected = 'Particulate.Matter.2.5', multiple = TRUE)
  ),
  accordion_panel(
    "Built Environment", icon = bs_icon("buildings"),
    varSelectInput('builtenv', selectize = TRUE, label = span("Select Measures", 
                                                              popover(bs_icon("lightbulb"),
                                                                      "See actionable tips for this category",
                                                                      title = "Actionable Tips",
                                                                      placement = "right")), data = built_env_inp, selected = 'Green.Space', multiple = TRUE),
    input_switch('transit', "Transit Stops", value = FALSE),
    accordion_panel(
      "Food Environment", icon = bs_icon("basket"),
      varSelectizeInput('foodenv', label = span("Select Measures", 
                                                popover(bs_icon("lightbulb"),
                                                        food_env_md,
                                                        title = "Actionable Tips",
                                                        placement = "right")), data = food_env_inp, multiple = TRUE)
    )
  ),
  accordion_panel(
    "Social Environment", icon = tags$img(src = "/social-environment.png", height = "20.48px", width = "20.48px"),
    varSelectInput('socialenv', selectize = TRUE, label = span("Select Measures", 
                                                               popover(bs_icon("lightbulb"),
                                                                       "See actionable tips for this category",
                                                                       title = "Actionable Tips",
                                                                       placement = "right")), data = social_env_inp, multiple = TRUE)
  ),
  accordion_panel(
    "Options", icon = bs_icon("gear"),
    input_switch("showbounds", "Show Tract Boundaries", value = TRUE),
    input_switch("showchart", "Show Chart", value = TRUE),
    fileInput("upload", "Upload a Shapefile"),
    downloadButton("download", "Download data")
  )
  
)

# -------- UI LAYOUT --------
ui <- page_navbar(
  #waiter::use_waiter(), 
  #shinyjs::useShinyjs(),
  #title = tags$img(src = "/geoexmap-logo.png", height = '92.32px', width = '214.8px'),
  title = tags$img(src = "/geoexmap_edit.png", height = '57.62px', width = '165.08px'),
  nav_spacer(),
  nav_panel("Map",
            layout_sidebar(
              sidebar = sidebar(categories,
                                width = "400px"),
              leafletOutput("geoexmap"),
              conditionalPanel(
                condition = "input.showchart == true",
                absolutePanel(
                  class = "panel panel-default",
                  draggable = TRUE,
                  fixed = TRUE,
                  left = "425px",
                  right = "1000px",
                  bottom = "200px",
                  height = "200px",
                  width = "400px",
                  wellPanel(plotlyOutput("chart"))
                ))
            )),
  nav_panel("Table",
            layout_sidebar(
              sidebar = categories
            )),
  nav_panel("Documentation"),
  nav_panel("Contact us",
            h3("Questions or comments?"),
            a("geoexmap@fredhutch.org", href = "mailto:geoexmap@fredhutch.org")),
  window_title = "geoexmap | Geospatial Exposome Map at Fred Hutch Cancer Center"
  
)

# -------- SERVER --------
server <- function(input, output, session) {
  # define categories for palettes
  # "good", "bad", "neutral"
  g <- c("Green.Space", "Routine.Checkup.in.the.Past.Year", "Visited.Denstist.in.Past.Year", "Cholesterol.Screening",
         "Taking.Medicine.to.Control.High.Blood.Pressure", "Mammography.Use.among.Women.50.to.74",
         "Colorectal.Cancer.Screening.among.Adults.45.to.75")
  b <- c("Particulate.Matter.2.5", "Arthritis.among.Adults", 
         "Food.Stamps", "Food.Insecurity",
         "Housing.Insecurity", "Utility.Services.Threat", "Lacking.Reliable.Transportation", 
         "Lack.Of.Health.Insurance", "Binge.Drinking.among.Adults", "Cigarette.Smoking.among.Adults",
         "No.Leisure.Time.Physical.Activity.among.Adults", "Short.Sleep.Duration",
         "Asthma.among.Adults", "High.Blood.Pressure.among.Adults", "High.Blood.Pressure.among.Adults",
         "Cancer.or.Melanoma.among.Adults", "High.Cholesterol.among.Screened.Adults", "COPD.among.Adults",
         "Coronary.Heart.Disease.among.Adults", "Depression.among.Adults", "Diagnosed.Diabetes.among.Adults",
         "Obesity.among.Adults", "All.Teeth.Lost.amond.Adults.65.and.older", "Stroke.amond.Adults") # CHANGE TO AMONG IN PROCESSING
  n <- c("Nighttime.Radiance", names(sociodemo_inp))
  
  # palette helper function
  geoex.palette <- function(var) {
    tryCatch({
      # skip geometry column to avoid error
      if (var == "geometry" || inherits(df_vars[[var]], "sfc")) {
        message("Skipping geometry column...")
        return(NULL)
      }
      
      domain = df_vars[[var]]
     # bins = unique(stats::quantile(df_vars[[var]], na.rm = TRUE))
      #numbins = length(bins)
      
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
    if(col == "Food.Stamps") return("Comparative Population on SNAP")
    if(col == "Food.Insecurity") return("Comparative Prevalence of Food Insecurity")
    if(col == "Housing.Insecurity") return("Comparative Prevalence of Housing Insecurity")
    if(col == "Utility.Services.Threat") return("Comparative Prevalence of Utility Services Threats")
    if(col == "Lacking.Reliable.Transportation") return("Comparative Population Lacking Reliable Transportation")
    if(col == "Lack.Of.Health.Insurance") return("Comparative Population Lacking Health Insurance")
    if(col == "Routine.Checkup.in.the.Past.Year") return("Comparative Prevalence of Routine Checkups (Past Year)")
    if(col == "Visited.Denstist.in.Past.Year") return("Comparative Prevalence of Dental Visits (Past Year)")
    if(col == "Taking.Medicine.to.Control.High.Blood.Pressure") return("Comparative Prevalence of Pop. Taking BP Medicine")
    if(col == "Cholesterol.Screening") return("Comparative Prevalence of Pop. with Cholesterol Screening")
    if(col == "Mammography.Use.among.Women.50.to.74") return("Comparative Mammography Use (Women 50-74)")
    if(col == "Colorectal.Cancer.Screening.among.Adults.45.to.75") return("Comparative Prevalence of Colorectal Cancer Screening (Adults 45-75)")
    if(col == "Binge.Drinking.among.Adults") return("Comparative Prevalence of Binge Drinking Among Adults")
    if(col == "Cigarette.Smoking.among.Adults") return("Comparative Prevalence of Cigarette Smoking Among Adults")
    if(col == "No.Leisure.Time.Physical.Activity.among.Adults") return("Comparative Lack of Leisurely Physical Activity Among Adults")
    if(col == "Short.Sleep.Duration") return("Comparative Prevalence of Short Sleep Duration Among Adults")
    if(col == "Arthritis.among.Adults") return("Comparative Prevalence of Arthritis Among Adults")
    if(col == "Asthma.among.Adults") return("Comparative Prevalence of Asthma among Adults")
    if(col == "High.Blood.Pressure.among.Adults") return("Comparative Prevalence High BP Among Adults")
    if(col == "Cancer.or.Melanoma.among.Adults") return("Comparative Prevalence of Cancer or Melanoma Among Adults")
    if(col == "High.Cholesterol.among.Screened.Adults") return("Comparative Prevalence of High Cholesterol Among Screened Adults")
    if(col == "COPD.among.Adults") return("Comparative Prevalence of COPD Among Adults")
    if(col == "Coronary.Heart.Disease.among.Adults") return("Comparative Prevalence of Coronary Heart Disease Among Adults")
    if(col == "Depression.among.Adults") return("Comparative Prevalence of Depression Among Adults")
    if(col == "Diagnosed.Diabetes.among.Adults") return("Comparative Prevalence of Diabetes Among Adults")
    if(col == "Obesity.among.Adults") return("Comparative Prevalence of Obesity Among Adults")
    if(col == "All.Teeth.Lost.among.Adults.65.and.older") return("Comparative Prevalence All Teeth Lost Among Adults 65+")
    if(col == "Stroke.amond.Adults") return("Comparative Prevalence Stroke Among Adults")
  }
  
  map_cols <- reactive({
    cbind(health_outcomes, sociodemo, social_env, health_prevention, health_behaviors, natural_env, built_env) %>% 
      dplyr::select(!!!input$outcomes, !!!input$sociodemo, !!!input$socialenv, !!!input$prevention, 
                    !!!input$behaviors, !!!input$naturalenv, !!!input$builtenv)
    
  }) %>% 
    bindCache(input$outcomes, input$sociodemo, input$socialenv, input$prevention, input$behaviors, input$naturalenv, input$builtenv) %>% # reduce work by server
    bindEvent(list(input$outcomes, input$sociodemo, input$socialenv, input$prevention, input$behaviors, input$naturalenv, input$builtenv))
  
  food_env_cols <- reactive({
    cbind(food_env) %>% 
      dplyr::select(!!!input$foodenv)
  })
  
  output$download <- downloadHandler(
    filename = "geoexmap_download.gpkg",
    content = function(file) {
      st_write(map_cols(), file)
    }
  )
  
  output$chart <- renderPlotly({
    plotly.dat <- map_cols() %>% 
      st_drop_geometry()
    
    if (ncol(plotly.dat) == 1) {
      plot_ly(data = plotly.dat, x = plotly.dat[,1]) %>% #~paste0("<b>", gsub("\\.", " ", names(plotly.dat)[1]), ": ", round(plotly.dat[,1], digits = 2))) %>% 
        layout(
          plot_bgcolor = '#e5ecf6',
          xaxis = list(title = names(plotly.dat)[1])) 
    } else if (ncol(plotly.dat) == 2) {
      plot_ly(data = plotly.dat, type = "scatter", x = plotly.dat[,1], y = plotly.dat[,2],
              text = ~paste0("<b>", gsub("\\.", " ", names(plotly.dat)[1]), ": ", round(plotly.dat[,1], digits = 2),
                             "<br>", gsub("\\.", " ", names(plotly.dat)[2]), ": ", round(plotly.dat[,2], digits = 2))) %>% 
        layout(
          plot_bgcolor = '#e5ecf6',
          xaxis = list(title = gsub("\\.", " ", names(plotly.dat)[1])),
          yaxis = list(title = gsub("\\.", " ", names(plotly.dat)[2])))
    } else if (ncol(plotly.dat) == 3) {
      plot_ly(data = plotly.dat, x = plotly.dat[,1], y = plotly.dat[,2], z = plotly.dat[,3],
              text = ~paste0("<b>", gsub("\\.", " ", names(plotly.dat)[1]), ": ", round(plotly.dat[,1], digits = 2),
                             "<br>", gsub("\\.", " ", names(plotly.dat)[2]), ": ", round(plotly.dat[,2], digits = 2),
                             "<br>", gsub("\\.", " ", names(plotly.dat)[3]), ": ", round(plotly.dat[,3], digits = 2))) %>% 
        layout(scene = list(xaxis = list(title = gsub("\\.", " ", names(plotly.dat)[1])),
                            yaxis = list(title = gsub("\\.", " ", names(plotly.dat)[2])),
                            zaxis = list(title = gsub("\\.", " ", names(plotly.dat)[3]))))
    } else {
      
    }
     
  })
  
  output$geoexmap <- renderLeaflet({
    map <- leaflet(map_cols()) %>% 
      setView(lng = -120.74, lat = 47.75, zoom = 7) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addSearchOSM()
  })
  
  # observeEvent(input$transit, {
  #   {
  #     leafletProxy("geoexmap", data = transit) %>% 
  #       {
  #         if (input$transit) {
  #           addMarkers(data = transit, lng = ~stop_lon, lat = ~stop_lat,
  #                      icon = bs_icon("bus-front"))
  #         }
  #       }
  #   }
  # })
  
  observe({
    # if (ncol(map_cols()) == 4) {
    #   print("Disabling select inputs...")
    #   shinyjs::disable("outcomes")
    #   shinyjs::disable("sociodemo")
    #   shinyjs::disable("socialenv")
    #   shinyjs::disable("behaviors")
    #   shinyjs::disable("prevention")
    #   shinyjs::disable("naturalenv")
    #   shinyjs::disable("builtenv")
    # }
    
    withProgress(message = "Plotting...", 
    {plotlyProxy("chart")
    
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
    bindEvent(list(input$outcomes, input$sociodemo, input$socialenv, input$behaviors, input$prevention, input$naturalenv, input$builtenv, input$transit, 
                   input$showbounds, input$upload))
}

# -------- CREATE SHINY APP --------

options <- list()

if (!interactive()) {
  options$shiny.port = 3838
  options$shiny.host = "0.0.0.0"
}

shinyApp(ui = ui, server = server, options = options)