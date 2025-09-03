# MC :)

#TODO ELEMENTS:
## modal for intro/welcome message
## add table features
#library(waiter)

library(shiny)
library(shinyjs)
library(htmltools)
library(reactable)
library(tidyverse)
library(sf)
library(data.table)
library(reactable)

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

#### LOAD DATA ####
# empty shapefiles
city.bounds <- st_read("Geo/city/cities.gpkg")
county.bounds <- st_read("Geo/county/counties.gpkg")

# polygon data
data <- st_read("Data_Processed/complete/geoexmap_data.gpkg", layer = "geoexmap_data") 

food <- st_read("Data_Processed/complete/geoexmap_data.gpkg", layer = "food_env")

# read point data
transit <- st_read("Data_Processed/complete/geoexmap_data.gpkg",
                   layer = "transit")

cancer.progs <- st_read("Data_Processed/complete/geoexmap_data.gpkg",
                        layer = "cancer_progs")

alc <- st_read("Data_Processed/complete/geoexmap_data.gpkg",
               layer = "alc_retailers")

#### DEFINE DATA CATEGORIES ####
data$Binge.Drinking.among.Adults <- as.numeric(data$Binge.Drinking.among.Adults)
data$Cigarette.Smoking.among.Adults <- as.numeric(data$Cigarette.Smoking.among.Adults)
data$No.Leisure.time.Physical.Activity.among.Adults <- as.numeric(data$No.Leisure.time.Physical.Activity.among.Adults)
data$Short.Sleep.Duration <- as.numeric(data$Short.Sleep.Duration)

df_vars <- data %>% 
  dplyr::select(c(2:100))

sociodemographics <- c("Population (total)" = "Total.Population")

racev <- c("Non-Hispanic White (percentage)" = "Percent.White.NonHispanic",
          "Non-Hispanic Black (percentage)" = "Percent.Black.NonHispanic",
          "Non-Hispanic Asian (percentage)" = "Percent.Asian.NonHispanic",
          "Non-Hispanic American Indian or Alaska Native (percentage)" = "Percent.American.Indian.Alaska.Native.NonHispanic",
          "Non-Hispanic Native Hawaiian or Pacific Islander (percentage)" =  "Percent.Native.Hawaiian.Pacific.Islander.NonHispanic",
          "Non-Hispanic Other (percentage)" = "Percent.Other.Race.NonHispanic",
          "Non-Hispanic two or more races (percentage)" = "Percent.Two.or.More.Races.NonHispanic",
          "Hispanic or Latino (percentage)" = "Percent.Hispanic.or.Latino",
          "Hispanic or Latino White (percentage)" = "Percent.White.Hispanic.or.Latino",
          "Hispanic or Latino Black (percentage)" = "Percent.Black.Hispanic.or.Latino",
          "Hispanic or Latino Asian (percentage)" = "Percent.Asian.Hispanic.or.Latino",
          "Hispanic or Latino American Indian or Alaska Native (percentage)" = "Percent.American.Indian.Alaska.Native.Hispanic.or.Latino",
          "Hispanic or Latino Native Hawaiian or Pacific Islander (percentage)" = "Percent.Native.Hawaiian.Pacific.Islander.Hispanic.or.Latino",
          "Hispanic or Latino Other (percentage)" = "Percent.Other.Race.Hispanic.or.Latino",
          "Hispanic or Latino two or more races (percentage)" = "Percent.Two.or.More.Races.Hispanic.or.Latino")

sexv <- c("Male (percentage)" = "Percent.Male",
         "Female (percentage)" = "Percent.Female")

agev <- c("0-4 years (percentage)" = "Percent.0.to.4.years",
         "5-9 years (percentage)" = "Percent.5.to.9.years",
         "10-14 years (percentage)" = "Percent.10.to.14.years",
         "15-19 years (percentage)" = "Percent.15.to.19.years",
         "20-24 years (percentage)" = "Percent.20.to.24.years",
         "25-29 years (percentage)" = "Percent.25.to.29.years",
         "30-34 years (percentage)" = "Percent.30.to.34.years",
         "35-39 years (percentage)" = "Percent.35.to.39.years",
         "40-44 years (percentage)" = "Percent.40.to.44.years",
         "45-49 years (percentage)" = "Percent.45.to.49.years",
         "50-54 years (percentage)" = "Percent.50.to.54.years",
         "55-59 years (percentage)" = "Percent.55.to.59.years",
         "60-64 years (percentage)" = "Percent.60.to.64.years",
         "65-69 years (percentage)" = "Percent.65.to.69.years",
         "70-74 years (percentage)" = "Percent.70.to.74.years",
         "75-79 years (percentage)" = "Percent.75.to.79.years",
         "80-84 years (percentage)" = "Percent.80.to.84.years",
         "85 years+ (percentage)" = "Percent.85.and.older")

behaviors <- c("Binge drinking" =  "Binge.Drinking.among.Adults",
               "Cigarette smoking" = "Cigarette.Smoking.among.Adults",
               "No physical activity" = "No.Leisure.time.Physical.Activity.among.Adults",
               "Short sleep duration" =  "Short.Sleep.Duration")

outcomes <- c("Arthritis" = "Arthritis.among.Adults",
              "Asthma" = "Asthma.among.Adults",
              "High blood pressure" = "High.Blood.Pressure.among.Adults",
              "Cancer" = "Cancer.or.Melanoma.among.Adults",
              "High cholesterol" = "High.Cholesterol.among.Screened.Adults",
              "Chronic obstructive pulmonary disease" = "COPD.among.Adults",
              "Coronary heart disease" = "Coronary.Heart.Disease.among.Adults",
              "Depression" = "Depression.among.Adults",
              "Diabetes" = "Diagnosed.Diabetes.among.Adults",
              "Obesity" = "Obesity.among.Adults",
              "All teeth lost" = "All.Teeth.Lost.among.Adults.65.and.Older",
              "Stroke" = "Stroke.among.Adults")

prevention <- c("Cholesterol screening" = "Cholesterol.Screening",
                "No health insurance" = "Lack.of.Health.Insurance",
                "Routine checkup in past year" = "Routine.Checkup.in.the.Past.Year",
                "Visited dentist in past year" = "Visited.Denstist.in.Past.Year",
                "Taking blood pressure medication" = "Taking.Medicine.to.Control.High.Blood.Pressure",
                "Cholesterol screening" = "Cholesterol.Screening",
                "Mammography screening for breast cancer" = "Mammography.Use.among.Women.50.to.74",
                "Colorectal cancer screening" = "Colorectal.Cancer.Screening.among.Adults.45.to.75")

airpol <- c("PM2.5" = "Particulate.Matter.2.5",
            "Wildfire smoke" = "Wildfire.smoke",
            "Nitrogen dioxide (No2)" = "Nitrogen.dioxide",
            "Sulfur dioxide (So2)" = "Sulfur.dioxide",
            "Carbon monoxide (CO)" = "Carbon.monoxide",
            "Ozone (O3)" = "Ozone")

naturalenv <- c("UV index" = "UV.Index",
                "Dew point" = "Dew.point",
                "Maximum temperature" = "Maximum.temperature",
                "Minimum temperature" = "Minimum.temperature",
                "Average temperature" = "Average.temperature",
                "Radon" = "Radon")

builtenv <- c("Walkability" = "Walkability",
              "Pesticide exposure" = "Pesticide.Exposure",
              "Green space" = "Green.Space",
              "Light at night" = "Nighttime.Radiance",
              "Noise pollution" = "Noise.Pollution")

socialenv <- c("Food insecurity" = "Food.Insecurity",
               "Housing insecurity" = "Housing.Insecurity",
               "Utility services threat" = "Utility.Services.Threat",
               "Lack of reliable transportation" = "Lacking.Reliable.Transportation",
               "Lack of social and emotional support" =  "Lack.of.Social.and.Emotional.Support",
               "No internet" = "No.broadband.internet",
               "No high school education" = "No.high.school.diploma",
               "Single parent households" = "Single.parent.households",
               "Housing cost burden" = "Housing.cost.burden",
               "Crowding" = "Crowding",
               "Poverty" = "Poverty",
               "Unemployment" = "Unemployment",
               "Social Vulnerability Index" = "Social.Vulnerability.Index",
               "Environmental Justice Index" = "Environmental.Justice.Index",
               "Segregation" = "Racial.Residential.Segregation",
               "Population density" = "Population.density")

# define filters
health_outcomes <- df_vars %>% 
  dplyr::select(c(21:32)) 

health_behaviors <- df_vars %>% 
  dplyr::select(c(17:20)) 

health_prevention <- df_vars %>% 
  dplyr::select(c(10:16)) 

natural_env <- df_vars %>%
  dplyr::select(c(75, 76, 88:92)) 

air_pol <- df_vars %>% 
  dplyr::select(c(1, 93:96)) 

built_env <- df_vars %>%
  dplyr::select(c(2:3, 74, 77, 78, 79)) 

sociodemo <- df_vars %>% 
  dplyr::select(c(52)) 

sex <- df_vars %>%
  dplyr::select(c(53, 54))

race <- df_vars %>%
  dplyr::select(c(33:47))

age <- df_vars %>%
  dplyr::select(c(55:72))

social_env <- df_vars %>% 
  dplyr::select(c(4:9, 73:74, 81:86)) 

food_env <- food %>% 
  dplyr::select(c(11:50)) 
food_env_inp <- food_env %>% 
  st_drop_geometry()

#### DEFINE MARKDOWN FOR TIPS ####
# define markdown text for tips
food_env_md <- markdown("
Need help finding food? See: [Feeding Washington](https://feedingwashington.org/find-food/)
                        ")

# -------- UI ELEMENTS --------
categories <- accordion(
  open = FALSE,
  accordion_panel(
    "Sociodemographics", icon = bs_icon("person-vcard"),
    selectInput('sociodemo', 
                span("Select variables", popover(bs_icon("lightbulb"),
                                                 "See actionable tips for this category",
                                                 title = "Actionable Tips",
                                                 placement = "right")),
                sociodemographics,
                selectize = TRUE, multiple = TRUE),
    accordion_panel("Race and Ethnicity", selectInput('race', NULL, racev, selectize = TRUE, multiple = TRUE)),
    accordion_panel("Sex", selectInput('sex', NULL, sexv, selectize = TRUE, multiple = TRUE)),
    accordion_panel("Age", selectInput('age', NULL, agev, selectize = TRUE, multiple = TRUE))
    
  ),
  accordion_panel(
    "Health Outcomes", icon = bs_icon("heart-pulse"),
    selectInput('outcomes', 
                span("Select variables", 
                     popover(bs_icon("lightbulb"),
                             "See actionable tips for this category",
                             title = "Actionable Tips",
                             placement = "right")), outcomes, selectize = TRUE, multiple = TRUE)
  ),
  accordion_panel(
    "Health Behaviors", icon = bs_icon("person-walking"),
    selectInput('behaviors', span("Select variables", 
                                  popover(bs_icon("lightbulb"),
                                          "See actionable tips for this category",
                                          title = "Actionable Tips",
                                          placement = "right")), behaviors,
                 multiple = TRUE, selectize = TRUE,)
  ),
  accordion_panel(
    "Prevention", icon = tags$img(src = "/prevention.png", height = "20.48px", width = "20.48 px"),
    selectInput('prevention', span("Select variables", 
                                      popover(bs_icon("lightbulb"),
                                              "See actionable tips for this category",
                                              title = "Actionable Tips",
                                              placement = "right")), prevention, selectize = TRUE, multiple = TRUE)
  ),
  accordion_panel(
    "Healthcare Access", icon = bs_icon("building-add"),
    input_switch('cancer', "Cancer Programs", value = FALSE)
  ),
  accordion_panel(
    "Natural Environment", icon = bs_icon("sun"),
    selectInput('naturalenv', span("Select variables", 
                                              popover(bs_icon("lightbulb"),
                                                      "See actionable tips for this category",
                                                      title = "Actionable Tips",
                                                      placement = "right")), naturalenv, selectize = TRUE, multiple = TRUE),
    accordion_panel("Air pollutants", icon = bs_icon("cloud-haze"),
                   selectInput('airpol', NULL, airpol, selectize = TRUE, multiple = TRUE))
  ),
  accordion_panel(
    "Built Environment", icon = bs_icon("buildings"),
    selectInput('builtenv', span("Select variables", 
                                 popover(bs_icon("lightbulb"),
                                         "See actionable tips for this category",
                                         title = "Actionable Tips",
                                         placement = "right")), builtenv, selectize = TRUE, multiple = TRUE),
    input_switch('transit', "Transit stops", value = FALSE),
    input_switch('alc', "Alcohol retailers", value = FALSE),
    accordion_panel(
      "Food Environment", icon = bs_icon("basket"),
      varSelectInput('foodenv', label = span("Select variables", 
                                 popover(bs_icon("lightbulb"),
                                         food_env_md,
                                         title = "Actionable Tips",
                                         placement = "right")), data = food_env_inp, multiple = TRUE)
    )
  ),
  accordion_panel(
    "Social Environment", icon = tags$img(src = "/social-environment.png", height = "20.48px", width = "20.48px"),
    selectInput('socialenv', span("Select variables", 
                                          popover(bs_icon("lightbulb"),
                                                  "See actionable tips for this category",
                                                  title = "Actionable Tips",
                                                  placement = "right")), socialenv, selectize = TRUE,  multiple = TRUE)
  ),
  accordion_panel(
    "Options", icon = bs_icon("gear"),
    input_switch("showbounds", "Show tract boundaries", value = TRUE),
    input_switch("showcounties", "Show county boundaries", value = FALSE),
    input_switch("showcities", "Show city boundaries", value = FALSE),
    input_switch("showchart", "Show graph", value = FALSE),
    fileInput("upload", "Upload a shapefile"),
    downloadButton("download", "Download data")
  )
  
)

# -------- UI LAYOUT --------
ui <- page_navbar(
  #shinyjs::useShinyjs(),
  #title = tags$img(src = "/geoexmap-logo.png", height = '92.32px', width = '214.8px'),
  tags$head(tags$link(rel = "shortcut icon", href = "/favicon.ico/geoexmap_favicon.png")),
  title = tags$img(src = "/geoexmap_edit.png", height = '57.62px', width = '165.08px'),
  nav_spacer(),
  nav_panel("Map",
            layout_sidebar(
              sidebar = sidebar(categories,
                                width = "400px"),
              leafletOutput("geoexmap"),
              conditionalPanel(
                condition = "(input.showchart == true ) || (input.clear == null && input.clear == 0)",
                absolutePanel(
                  class = "panel panel-default",
                  draggable = TRUE,
                  fixed = TRUE,
                  left = "425px",
                  right = "1100px",
                  bottom = "300px",
                  height = "200px",
                  width = "400px",
                  wellPanel(actionButton("clear", label = "X"),
                            plotlyOutput("chart"))
                ))
            )),
  nav_panel("Table",
            layout_sidebar(
              sidebar = sidebar(categories, 
                                width = "400px"),
              reactableOutput("table")
            )),
  nav_panel("Documentation",
            h2("Version History")),
  nav_panel("Contact us",
            h3("Questions or comments?"),
            a("geoexmap@fredhutch.org", href = "mailto:geoexmap@fredhutch.org")),
  window_title = "geoexmap | Geospatial Exposome Map at Fred Hutch Cancer Center"
  
)

# -------- SERVER --------
server <- function(input, output, session) {
  # turn switch off if clear button is clicked
  observeEvent(input$clear, {
    update_switch("showchart", value = FALSE)
  })
  
  #### PALETTE FUNCTION ####
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
         "Obesity.among.Adults", "All.Teeth.Lost.among.Adults.65.and.older", "Stroke.among.Adults") 
  n <- c("Nighttime.Radiance")
  
  
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
  
  #### LEGEND TITLES ####
  legend.titles <- function(col) {
    #return(col)
    if(col == "Particulate.Matter.2.5") return(paste0("PM<sub>2.5</sub> ", "(\U03BC", "g/m<sup>3</sup>)"))
    if(col == "Green.Space") return("Normalized Difference Vegetation Index")
    if(col == "Nighttime.Radiance") return("Light at Night (nW/cm<sup>2</sup>/sr)")
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
    if(col == "Stroke.among.Adults") return("Comparative Prevalence Stroke Among Adults")
  }
  
  #### CLEAR BUTTON OBSERVER ####
  # observe event for clear button click
  observeEvent(input$clear_all_vars, {
    # reset all selectInputs and switches
    updateSelectInput(session, "outcomes", selected = character(0))
    updateSelectInput(session, "sociodemo", selected = character(0))
    updateSelectInput(session, "age", selected = character(0))
    updateSelectInput(session, "sex", selected = character(0))
    updateSelectInput(session, "race", selected = character(0))
    updateSelectInput(session, "socialenv", selected = character(0))
    updateSelectInput(session, "prevention", selected = character(0))
    updateSelectInput(session, "behaviors", selected = character(0))
    updateSelectInput(session, "naturalenv", selected = character(0))
    updateSelectInput(session, "airpol", selected = character(0))
    updateSelectInput(session, "builtenv", selected = character(0))
    updateVarSelectInput(session, "foodenv", selected = character(0))
    
    # reset switches if needed
    update_switch("transit", value = FALSE)
    update_switch("showcounties", value = FALSE)
    update_switch("showcities", value = FALSE)
    
    # clear the map
    leafletProxy("geoexmap") %>%
      clearControls() %>%
      clearShapes()
  })
  
  #### REACTIVE VALUES ####  
  map_cols <- reactive({
    naturalenv <- cbind(natural_env, air_pol)
    sociodemo <- cbind(sociodemo, age, sex, race)
    
    df <- cbind(health_outcomes, sociodemo, social_env, health_prevention, health_behaviors, natural_env, built_env)
    
    df[, c(input$outcomes, input$sociodemo, input$age, input$sex, input$socialenv, input$prevention, input$behaviors, input$airpol, input$naturalenv, input$builtenv), drop = FALSE]
  }) %>% 
    bindCache(input$outcomes, input$sociodemo, input$race, input$age, input$sex, input$socialenv, input$prevention, input$behaviors, input$naturalenv, input$airpol, input$builtenv) # reduce work by server
  
  food_env_cols <- reactive({
    cbind(food_env) %>% 
      dplyr::select(!!!input$foodenv)
  })
  
  # track active variables
  values <- reactiveValues(
    active_variables = character(0)
  )
  
  table_cols <- reactive({
    map_cols() %>% 
      st_drop_geometry()
    })
  
  #### DOWNLOAD HANDLER ####
  output$download <- downloadHandler(
    filename = "geoexmap_download.gpkg",
    content = function(file) {
      st_write(map_cols(), file)
    }
  )
  
  #### PLOTLY RENDER ####
  output$chart <- renderPlotly({
    plotly.dat <- map_cols() %>%
      st_drop_geometry()

    if (ncol(plotly.dat) == 1) {
      plot_ly(data = plotly.dat, x = plotly.dat[,1]) %>% 
        layout(
          plot_bgcolor = '#e5ecf6',
          xaxis = list(title = names(plotly.dat)[1])) %>% 
        config()
    } else if (ncol(plotly.dat) == 2) {
      plot_ly(data = plotly.dat, type = "scatter", x = plotly.dat[,1], y = plotly.dat[,2],
              text = ~paste0("<b>", gsub("\\.", " ", names(plotly.dat)[1]), ": ", round(plotly.dat[,1], digits = 2),
                             "<br>", gsub("\\.", " ", names(plotly.dat)[2]), ": ", round(plotly.dat[,2], digits = 2))) %>%
        layout(
          plot_bgcolor = '#e5ecf6',
          xaxis = list(title = gsub("\\.", " ", names(plotly.dat)[1])),
          yaxis = list(title = gsub("\\.", " ", names(plotly.dat)[2]))) %>% 
        config(scrollZoom = TRUE)
    } else if (ncol(plotly.dat) == 3) {
      plot_ly(data = plotly.dat, x = plotly.dat[,1], y = plotly.dat[,2], z = plotly.dat[,3],
              text = ~paste0("<b>", gsub("\\.", " ", names(plotly.dat)[1]), ": ", round(plotly.dat[,1], digits = 2),
                             "<br>", gsub("\\.", " ", names(plotly.dat)[2]), ": ", round(plotly.dat[,2], digits = 2),
                             "<br>", gsub("\\.", " ", names(plotly.dat)[3]), ": ", round(plotly.dat[,3], digits = 2))) %>%
        layout(scene = list(xaxis = list(title = gsub("\\.", " ", names(plotly.dat)[1])),
                            yaxis = list(title = gsub("\\.", " ", names(plotly.dat)[2])),
                            zaxis = list(title = gsub("\\.", " ", names(plotly.dat)[3])))) %>% 
        config()
    } else {
      
    }

  })
  
  #### ACTIVE VARIABLE PANEL ####
  # create the variable panel HTML
  create_variable_panel <- function(variables) {
    if (length(variables) == 0) {
      return("")
    }
    
    # clickable variable items
    variable_items <- sapply(variables, function(var) {
      # shortened display name if needed
      display_name <- if(nchar(var) > 25) paste0(substr(var, 1, 22), "...") else var
      
      paste0(
        '<div class="variable-item" onclick="Shiny.setInputValue(\'remove_variable\', \'', var, '\', {priority: \'event\'});" ',
        'title="Click to remove: ', var, '">',
        '<span class="variable-name">', display_name, '</span>',
        '<span class="remove-icon">×</span>',
        '</div>'
      )
    })
    
    # combine full panel HTML
    panel_html <- paste0(
      '<div id="variable-panel" style="
        background: rgba(255, 255, 255, 0.9);
        border: 2px solid #ccc;
        border-radius: 5px;
        padding: 10px;
        margin: 10px;
        max-width: 250px;
        max-height: 300px;
        overflow-y: auto;
        box-shadow: 0 2px 5px rgba(0,0,0,0.2);
      ">',
      '<div style="font-weight: bold; margin-bottom: 8px; color: #333;">Active Variables</div>',
      paste(variable_items, collapse = ""),
      '</div>',
      '<style>
        .variable-item {
          display: flex;
          justify-content: space-between;
          align-items: center;
          padding: 5px 8px;
          margin: 2px 0;
          background: #f8f9fa;
          border: 1px solid #dee2e6;
          border-radius: 3px;
          cursor: pointer;
          transition: background-color 0.2s;
        }
        .variable-item:hover {
          background: #e9ecef;
          border-color: #adb5bd;
        }
        .variable-name {
          flex: 1;
          font-size: 12px;
          color: #495057;
        }
        .remove-icon {
          color: #dc3545;
          font-weight: bold;
          font-size: 16px;
          margin-left: 5px;
        }
        .variable-item:hover .remove-icon {
          color: #c82333;
        }
      </style>'
    )
    
    return(panel_html)
  }
  
  # handle individual variable removal
  observeEvent(input$remove_variable, {
    var_to_remove <- input$remove_variable
    
    if (!is.null(var_to_remove) && var_to_remove != "") {
      # lookup table for variable categories
      variable_lookup <- list(
        list(data = health_outcomes, input_id = "outcomes", update_fn = updateSelectInput),
        list(data = sociodemo, input_id = "sociodemo", update_fn = updateSelectInput),
        list(data = age, input_id = "age", update_fn = updateSelectInput),
        list(data = sex, input_id = "sex", update_fn = updateSelectInput),
        list(data = race, input_id = "race", update_fn = updateSelectInput),
        list(data = social_env, input_id = "socialenv", update_fn = updateSelectInput),
        list(data = health_prevention, input_id = "prevention", update_fn = updateSelectInput),
        list(data = health_behaviors, input_id = "behaviors", update_fn = updateSelectInput),
        list(data = natural_env, input_id = "naturalenv", update_fn = updateSelectInput),
        list(data = air_pol, input_id = "airpol", update_fn = updateSelectInput),
        list(data = built_env, input_id = "builtenv", update_fn = updateSelectInput),
        list(data = food_env_inp, input_id = "foodenv", update_fn = updateVarSelectInput)
      )
      
      # Find the matching category and update
      for (category in variable_lookup) {
        if (var_to_remove %in% colnames(category$data)) {
          current_selection <- input[[category$input_id]]
          new_selection <- setdiff(current_selection, var_to_remove)
          category$update_fn(session, category$input_id, selected = new_selection)
          break
        }
      }
    }
  })
  
  #### MAP RENDER ####
  output$geoexmap <- renderLeaflet({
    map <- leaflet(map_cols()) %>% 
      setView(lng = -120.74, lat = 47.75, zoom = 7) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addSearchOSM() %>% 
      addEasyButton(easyButton(
        icon = 'fa-remove',
        title = "Remove all variables",
        onClick = JS("function(btn, map){
                     Shiny.setInputValue('clear_all_vars', Math.random());
        }")
      ))
      return(map)
  })
  
  #### TABLE RENDER ####
  output$table <- renderReactable({
    validate(need(base::ncol(table_cols()) > 0, "Please select a variable."))
    
    reactable(table_cols())
    })
  
  
  #### MAIN OBSERVER LOGIC AND PROXIES ####
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
      
      current_vars <- colnames(map_cols())
      current_vars <- current_vars[!current_vars %in% c("geom")]
      values$active_variables <- current_vars
      
      proxy <- leafletProxy("geoexmap", data = map_cols()) %>% 
        clearControls() %>% 
        clearShapes() %>% 
        clearMarkers()
      
      # update the variable panel
      if (length(current_vars) > 0) {
        panel_html <- create_variable_panel(current_vars)
        proxy <- proxy %>% addControl(
            html = panel_html,
            position = "bottomright",
            layerId = "variable_panel"
          ) 
      }
      # for each chosen column, define the palette, and add polygons
      label = ""
      
      if (input$transit) {
        print("Adding points")
        
        html_legend <- '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-bus-front" viewBox="0 0 16 16">
  <path d="M5 11a1 1 0 1 1-2 0 1 1 0 0 1 2 0m8 0a1 1 0 1 1-2 0 1 1 0 0 1 2 0m-6-1a1 1 0 1 0 0 2h2a1 1 0 1 0 0-2zm1-6c-1.876 0-3.426.109-4.552.226A.5.5 0 0 0 3 4.723v3.554a.5.5 0 0 0 .448.497C4.574 8.891 6.124 9 8 9s3.426-.109 4.552-.226A.5.5 0 0 0 13 8.277V4.723a.5.5 0 0 0-.448-.497A44 44 0 0 0 8 4m0-1c-1.837 0-3.353.107-4.448.22a.5.5 0 1 1-.104-.994A44 44 0 0 1 8 2c1.876 0 3.426.109 4.552.226a.5.5 0 1 1-.104.994A43 43 0 0 0 8 3"/>
  <path d="M15 8a1 1 0 0 0 1-1V5a1 1 0 0 0-1-1V2.64c0-1.188-.845-2.232-2.064-2.372A44 44 0 0 0 8 0C5.9 0 4.208.136 3.064.268 1.845.408 1 1.452 1 2.64V4a1 1 0 0 0-1 1v2a1 1 0 0 0 1 1v3.5c0 .818.393 1.544 1 2v2a.5.5 0 0 0 .5.5h2a.5.5 0 0 0 .5-.5V14h6v1.5a.5.5 0 0 0 .5.5h2a.5.5 0 0 0 .5-.5v-2c.607-.456 1-1.182 1-2zM8 1c2.056 0 3.71.134 4.822.261.676.078 1.178.66 1.178 1.379v8.86a1.5 1.5 0 0 1-1.5 1.5h-9A1.5 1.5 0 0 1 2 11.5V2.64c0-.72.502-1.301 1.178-1.379A43 43 0 0 1 8 1"/>
</svg> Transit stops<br/>'
        
        clusterOptions <- markerClusterOptions(disableClusteringAtZoom = 14)
        
        proxy <- proxy %>% 
          addMarkers(
            data = transit,
            lng = ~stop_lon,
            lat = ~stop_lat,
            group = "transit_markers",
            popup = ~paste("Stop:", stop_name),
            icon = makeIcon("/bus-front.svg"),
            clusterOptions = clusterOptions
          ) %>% 
          addControl(html = html_legend, position = "topright")
      } else {
        proxy <- proxy %>% 
          clearGroup(group = "transit_markers")
      }
      
      if (input$cancer) {
        # add svg for icon
        html_legend <- '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-hospital-fill" viewBox="0 0 16 16">
  <path d="M6 0a1 1 0 0 0-1 1v1a1 1 0 0 0-1 1v4H1a1 1 0 0 0-1 1v7a1 1 0 0 0 1 1h6v-2.5a.5.5 0 0 1 .5-.5h1a.5.5 0 0 1 .5.5V16h6a1 1 0 0 0 1-1V8a1 1 0 0 0-1-1h-3V3a1 1 0 0 0-1-1V1a1 1 0 0 0-1-1zm2.5 5.034v1.1l.953-.55.5.867L9 7l.953.55-.5.866-.953-.55v1.1h-1v-1.1l-.953.55-.5-.866L7 7l-.953-.55.5-.866.953.55v-1.1zM2.25 9h.5a.25.25 0 0 1 .25.25v.5a.25.25 0 0 1-.25.25h-.5A.25.25 0 0 1 2 9.75v-.5A.25.25 0 0 1 2.25 9m0 2h.5a.25.25 0 0 1 .25.25v.5a.25.25 0 0 1-.25.25h-.5a.25.25 0 0 1-.25-.25v-.5a.25.25 0 0 1 .25-.25M2 13.25a.25.25 0 0 1 .25-.25h.5a.25.25 0 0 1 .25.25v.5a.25.25 0 0 1-.25.25h-.5a.25.25 0 0 1-.25-.25zM13.25 9h.5a.25.25 0 0 1 .25.25v.5a.25.25 0 0 1-.25.25h-.5a.25.25 0 0 1-.25-.25v-.5a.25.25 0 0 1 .25-.25M13 11.25a.25.25 0 0 1 .25-.25h.5a.25.25 0 0 1 .25.25v.5a.25.25 0 0 1-.25.25h-.5a.25.25 0 0 1-.25-.25zm.25 1.75h.5a.25.25 0 0 1 .25.25v.5a.25.25 0 0 1-.25.25h-.5a.25.25 0 0 1-.25-.25v-.5a.25.25 0 0 1 .25-.25"/>
</svg> Cancer Programs <br/>'
        
        proxy <- proxy %>% 
          addMarkers(data = cancer.progs,
                     popup = ~Center.or.Hospital.Name,
                     group = "cancer_programs",
                     icon = makeIcon("/hospital-fill.svg")) %>% 
          addControl(html = html_legend, position = "topright")
      } else {
        proxy <- proxy %>% 
          clearGroup(group = "cancer_programs")
      }
      
      if (input$alc) {
        print("adding alc")
        # svg for icon
        html_legend <- '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-shop" viewBox="0 0 16 16">
  <path d="M2.97 1.35A1 1 0 0 1 3.73 1h8.54a1 1 0 0 1 .76.35l2.609 3.044A1.5 1.5 0 0 1 16 5.37v.255a2.375 2.375 0 0 1-4.25 1.458A2.37 2.37 0 0 1 9.875 8 2.37 2.37 0 0 1 8 7.083 2.37 2.37 0 0 1 6.125 8a2.37 2.37 0 0 1-1.875-.917A2.375 2.375 0 0 1 0 5.625V5.37a1.5 1.5 0 0 1 .361-.976zm1.78 4.275a1.375 1.375 0 0 0 2.75 0 .5.5 0 0 1 1 0 1.375 1.375 0 0 0 2.75 0 .5.5 0 0 1 1 0 1.375 1.375 0 1 0 2.75 0V5.37a.5.5 0 0 0-.12-.325L12.27 2H3.73L1.12 5.045A.5.5 0 0 0 1 5.37v.255a1.375 1.375 0 0 0 2.75 0 .5.5 0 0 1 1 0M1.5 8.5A.5.5 0 0 1 2 9v6h1v-5a1 1 0 0 1 1-1h3a1 1 0 0 1 1 1v5h6V9a.5.5 0 0 1 1 0v6h.5a.5.5 0 0 1 0 1H.5a.5.5 0 0 1 0-1H1V9a.5.5 0 0 1 .5-.5M4 15h3v-5H4zm5-5a1 1 0 0 1 1-1h2a1 1 0 0 1 1 1v3a1 1 0 0 1-1 1h-2a1 1 0 0 1-1-1zm3 0h-2v3h2z"/>
</svg> Alcohol Retailers <br/>'
        
        proxy <- proxy %>% 
          addMarkers(data = alc,
                     popup = ~Licensee,
                     group = "alc_retailers",
                     icon = makeIcon("/shop.svg")) %>% 
          addControl(html = html_legend, position = "topright")
      } else {
        proxy <- proxy %>% 
          clearGroup(group = "alc_retailers")
      }
      
      for (c in colnames(food_env_cols())) {
        pal <- geoex.palette(c)
        
        if (!is.null(pal)) {
          proxy <- proxy %>% 
            addPolygons(data = food_env_cols(), fillColor = ~pal(food_env_cols()[[c]]), stroke = TRUE, weight = 0.9, color = "blue",
                        fillOpacity = 0.3, highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE)) %>% 
            addLegend(pal = pal, values = ~food_env_cols()[[c]], title = legend.titles(c))
        }
      }
      
      for (c in colnames(map_cols())) {
        print(c)
        pal <- geoex.palette(c)

        # skip null to avoid geometry
        if (!is.null(pal)){
          proxy <- proxy %>% 
            addPolygons(., fillColor = ~pal(map_cols()[[c]]), stroke = input$showbounds, weight = 0.75, color = "black",
                        fillOpacity = 0.3, highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
                        label = "Hey") %>% 
            addLegend(pal = pal, values = ~map_cols()[[c]], title = legend.titles(c)) 
        }
      }
      
      if (length(current_vars) == 0) {
        # remove panel if no variables
        proxy %>% 
          removeControl(layerId = "variable_panel")
      }
      
      if (input$showcounties) {
        html_legend = ''
        proxy <- proxy %>% 
          addPolygons(data = county.bounds, stroke = TRUE, weight = 2, color = "#85BDBF", fill = FALSE) 
      }
      
      if (input$showcities) {
        proxy <- proxy %>% 
          addPolygons(data = city.bounds, stroke = TRUE, weight = 2, color = "#57737A", fill = FALSE)
      }
      
      proxy <- proxy %>% 
        addEasyprint(options = easyprintOptions(filename = "geoexmap_output", hideControlContainer = FALSE))
      
    })  
  }) %>% 
    bindEvent(list(input$outcomes, input$sociodemo, input$socialenv, input$behaviors, input$prevention, input$naturalenv, input$builtenv, input$transit, input$alc,
                   input$cancer, input$showcities, input$showcounties, input$showbounds, input$upload, input$foodenv))
}

# -------- CREATE SHINY APP --------

options <- list()

if (!interactive()) {
  options$shiny.port = 3838
  options$shiny.host = "0.0.0.0"
}

shinyApp(ui = ui, server = server, options = options)