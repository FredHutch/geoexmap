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

data <- st_read("Data_Processed/complete/geoexmap_data.gpkg", layer = "geoexmap_data") 

food <- st_read("Data_Processed/complete/geoexmap_data.gpkg", layer = "food_env")

# read point data
transit <- st_read("Data_Processed/complete/geoexmap_data.gpkg",
                   layer = "transit")

data$Binge.Drinking.among.Adults <- as.numeric(data$Binge.Drinking.among.Adults)
data$Cigarette.Smoking.among.Adults <- as.numeric(data$Cigarette.Smoking.among.Adults)
data$No.Leisure.time.Physical.Activity.among.Adults <- as.numeric(data$No.Leisure.time.Physical.Activity.among.Adults)
data$Short.Sleep.Duration <- as.numeric(data$Short.Sleep.Duration)

df_vars <- data %>% 
  dplyr::select(c(2:99))


# df_vars <- df_vars %>%
#   rename(`PM2.5` = Particulate.Matter.2.5,
#          `Green space` = Green.Space,
#          `Nighttime radiance` = Nighttime.Radiance,
#          `Received SNAP Benefits` = Food.Stamps,
#          `Food insecurity` = Food.Insecurity,
#          `Housing insecurity` = Housing.Insecurity,
#          `Utility services threat` = Utility.Services.Threat,
#          `Lack of reliable transportation` = Lacking.Reliable.Transportation,
#          `Lack of social and emotional support` =  Lack.of.Social.and.Emotional.Support,
#          `Lack of health insurance` = Lack.of.Health.Insurance,
#          `Routine checkup in past year` = Routine.Checkup.in.the.Past.Year,
#          `Visited dentist in past year` = Visited.Denstist.in.Past.Year,
#          `Taking blood pressure medication` = Taking.Medicine.to.Control.High.Blood.Pressure,
#          `Cholesterol screening` = Cholesterol.Screening,
#          `Mammography screening for breast cancer` = Mammography.Use.among.Women.50.to.74,
#          `Colorectal cancer screening` = Colorectal.Cancer.Screening.among.Adults.45.to.75,
#          `Binge drinking` =  Binge.Drinking.among.Adults,
#          `Cigarette smoking` = Cigarette.Smoking.among.Adults,
#          `No physical activity` = No.Leisure.time.Physical.Activity.among.Adults,
#          `Short sleep duration` =  Short.Sleep.Duration,
#          `Arthritis` = Arthritis.among.Adults,
#          `Asthma` = `Asthma.among.Adults`,
#          `High blood pressure` = High.Blood.Pressure.among.Adults,
#          `Cancer` = Cancer.or.Melanoma.among.Adults,
#          `High cholesterol` = High.Cholesterol.among.Screened.Adults,
#          `Chronic obstructive pulmonary disease` = COPD.among.Adults,
#          `Coronary heart disease` = Coronary.Heart.Disease.among.Adults,
#          `Depression` = Depression.among.Adults,
#          `Diabetes` = Diagnosed.Diabetes.among.Adults,
#          `Obesity` = Obesity.among.Adults,
#          `All teeth lost` = All.Teeth.Lost.among.Adults.65.and.Older,
#          `Stroke` = Stroke.among.Adults,
#          `Cholesterol screening` = Cholesterol.Screening,
#          `Non-Hispanic White (percentage)` = Percent.White.NonHispanic,
#          `Non-Hispanic Black (percentage)` = Percent.Black.NonHispanic,
#          `Non-Hispanic Asian (percentage)` = Percent.Asian.NonHispanic,
#          `Non-Hispanic American Indian or Alaska Native (percentage)` = Percent.American.Indian.Alaska.Native.NonHispanic,
#          `Non-Hispanic Native Hawaiian or Pacific Islander (percentage)` =  Percent.Native.Hawaiian.Pacific.Islander.NonHispanic,
#          `Non-Hispanic Other (percentage)` = Percent.Other.Race.NonHispanic,
#          `Non-Hispanic two or more races (percentage)` = Percent.Two.or.More.Races.NonHispanic,
#          `Hispanic or Latino (percentage)` = Percent.Hispanic.or.Latino,
#          `Hispanic or Latino White (percentage)` = Percent.White.Hispanic.or.Latino,
#          `Hispanic or Latino Black (percentage)` = Percent.Black.Hispanic.or.Latino,
#          `Hispanic or Latino Asian (percentage)` = Percent.Asian.Hispanic.or.Latino,
#          `Hispanic or Latino American Indian or Alaska Native (percentage)` = Percent.American.Indian.Alaska.Native.Hispanic.or.Latino,
#          `Hispanic or Latino Native Hawaiian or Pacific Islander (percentage)` = Percent.Native.Hawaiian.Pacific.Islander.Hispanic.or.Latino,
#          `Hispanic or Latino Other (percentage)` = Percent.Other.Race.Hispanic.or.Latino,
#          `Hispanic or Latino two or more races (percentage)` = Percent.Two.or.More.Races.Hispanic.or.Latino,
#          `Total population` = Total.Population,
#          `Male (percentage)` = Percent.Male,
#          `Female (percentage)` = Percent.Female,
#          `0-4 years (percentage)` = Percent.0.to.4.years,
#          `5-9 years (percentage)` = Percent.5.to.9.years,
#          `10-14 years (percentage)` = Percent.10.to.14.years,
#          `15-19 years (percentage)` = Percent.15.to.19.years,
#          `20-24 years (percentage)` = Percent.20.to.24.years,
#          `25-29 years (percentage)` = Percent.25.to.29.years,
#          `30-34 years (percentage)` = Percent.30.to.34.years,
#          `35-39 years (percentage)` = Percent.35.to.39.years,
#          `40-44 years (percentage)` = Percent.40.to.44.years,
#          `45-49 years (percentage)` = Percent.45.to.49.years,
#          `50-54 years (percentage)` = Percent.50.to.54.years,
#          `55-59 years (percentage)` = Percent.55.to.59.years,
#          `60-64 years (percentage)` = Percent.60.to.64.years,
#          `65-69 years (percentage)` = Percent.65.to.69.years,
#          `70-74 years (percentage)` = Percent.70.to.74.years,
#          `75-79 years (percentage)` = Percent.75.to.79.years,
#          `80-84 years (percentage)` = Percent.80.to.84.years,
#          `85 years+ (percentage)` = Percent.85.and.older,
#          `Social Vulnerability Index` = Social.Vulnerability.Index,
#          `Environmental Justice Index` = Environmental.Justice.Index,
#          `UV index` = UV.Index,
#          `Pesticide exposure` = Pesticide.Exposure,
#          `Segregation` = Racial.Residential.Segregation,
#          `Noise pollution` = Noise.Pollution,
#          `No internet` = No.broadband.internet,
#          `No high school education` = No.high.school.diploma,
#          `Single parent households` = Single.parent.households,
#          `Housing cost burden` = Housing.cost.burden,
#          `Dew point` = Dew.point,
#          `Maximum temperature` = Maximum.temperature,
#          `Minimum temperature` = Minimum.temperature,
#          `Average temperature` = Average.temperature,
#          `Wildfire smoke` = Wildfire.smoke,
#          `Nitrogen dioxide (No2)` = Nitrogen.dioxide,
#          `Sulfur dioxide (So2)` = Sulfur.dioxide,
#          `Carbon monoxide (CO)` = Carbon.monoxide,
#          `Population density` = Population.density
#          )

# define filters
health_outcomes <- df_vars %>% 
  dplyr::select(c(21:32)) 

health_outcomes_inp <- health_outcomes %>% 
  st_drop_geometry()

health_behaviors <- df_vars %>% 
  dplyr::select(c(17:20)) 
health_behaviors_inp <- health_outcomes %>% 
  st_drop_geometry()

health_prevention <- df_vars %>% 
  dplyr::select(c(10:16)) 
health_prevention_inp <- health_prevention %>% 
  st_drop_geometry()

natural_env <- df_vars %>%
  dplyr::select(c(1, 75, 76, 88:92)) 
natural_env_inp <- natural_env %>% 
  st_drop_geometry()

air_pol <- df_vars %>% 
  dplyr::select(c(93:96)) 
air_pol_inp <- air_pol %>% 
  st_drop_geometry()

built_env <- df_vars %>%
  dplyr::select(c(2:3, 74, 77, 78, 79)) 
built_env_inp <- built_env %>% 
  st_drop_geometry()

sociodemo <- df_vars %>% 
  dplyr::select(c(52)) 
sociodemo_inp <- sociodemo %>% 
  st_drop_geometry()

sex <- df_vars %>% 
  dplyr::select(c(53, 54)) 
sex_inp <- sex %>% 
  st_drop_geometry()

race <- df_vars %>% 
  dplyr::select(c(33:47)) 
race_inp <- race %>% 
  st_drop_geometry()

age <- df_vars %>% 
  dplyr::select(c(55:72)) 
age_inp <- df_vars %>% 
  st_drop_geometry()

social_env <- df_vars %>% 
  dplyr::select(c(4:9, 73:74, 81:86)) 
social_env_inp <- social_env %>% 
  st_drop_geometry()

food_env <- food %>% 
  dplyr::select(c(11:50)) 
food_env_inp <- food_env %>% 
  st_drop_geometry()

# define markdown text for tips
food_env_md <- markdown("
Need help finding food? See: [Feeding Washington](https://feedingwashington.org/find-food/)
                        ")


# -------- UI ELEMENTS --------
categories <- accordion(
  open = FALSE,
  accordion_panel(
    "Sociodemographics", icon = bs_icon("person-vcard"),
    varSelectInput('sociodemo', selectize = TRUE, data = sociodemo_inp,
                   label = span("Select variables", #change to total population 
                                                                    popover(bs_icon("lightbulb"),
                                                                            "See actionable tips for this category",
                                                                            title = "Actionable Tips",
                                                                            placement = "right")),
                    multiple = TRUE),
   # accordion_panel("Race and Ethnicity", varSelectInput('race', selectize = TRUE, label = NULL, data = race_inp, multiple = TRUE)),
    #accordion_panel("Sex", varSelectInput('sex', selectize = TRUE, label = NULL, data = sex_inp, multiple = TRUE)),
   # accordion_panel("Age", varSelectInput('age', selectize = TRUE, label = NULL, data = age_inp, multiple = TRUE))
    
  ),
  accordion_panel(
    "Health Outcomes", icon = bs_icon("heart-pulse"),
    varSelectInput('outcomes', selectize = TRUE, label = span("Select variables", 
                                                              popover(bs_icon("lightbulb"),
                                                                      "See actionable tips for this category",
                                                                      title = "Actionable Tips",
                                                                      placement = "right")), data = health_outcomes_inp, multiple = TRUE)
  ),
  accordion_panel(
    "Health Behaviors", icon = bs_icon("person-walking"),
    varSelectInput('behaviors', selectize = TRUE, label = span("Select variables", 
                                                               popover(bs_icon("lightbulb"),
                                                                       "See actionable tips for this category",
                                                                       title = "Actionable Tips",
                                                                       placement = "right")), data = health_behaviors_inp, multiple = TRUE)
  ),
  accordion_panel(
    "Prevention", icon = tags$img(src = "/prevention.png", height = "20.48px", width = "20.48 px"),
    varSelectInput('prevention', selectize = TRUE, label = span("Select variables", 
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
    varSelectInput('naturalenv', selectize = TRUE, label = span("Select variables", 
                                                                popover(bs_icon("lightbulb"),
                                                                        "See actionable tips for this category",
                                                                        title = "Actionable Tips",
                                                                        placement = "right")), data = natural_env_inp, multiple = TRUE),
    #accordion_panel("Air pollutants", icon = bs_icon("cloud-haze"),
    #                varSelectInput('airpol', selectize = TRUE, label = NULL, multiple = TRUE, data = air_pol_inp))
  ),
  accordion_panel(
    "Built Environment", icon = bs_icon("buildings"),
    varSelectInput('builtenv', selectize = TRUE, label = span("Select variables", 
                                                              popover(bs_icon("lightbulb"),
                                                                      "See actionable tips for this category",
                                                                      title = "Actionable Tips",
                                                                      placement = "right")), data = built_env_inp, multiple = TRUE),
    input_switch('transit', "Transit Stops", value = FALSE),
    accordion_panel(
      "Food Environment", icon = bs_icon("basket"),
      varSelectizeInput('foodenv', label = span("Select variables", 
                                                popover(bs_icon("lightbulb"),
                                                        food_env_md,
                                                        title = "Actionable Tips",
                                                        placement = "right")), data = food_env_inp, multiple = TRUE)
    )
  ),
  accordion_panel(
    "Social Environment", icon = tags$img(src = "/social-environment.png", height = "20.48px", width = "20.48px"),
    varSelectInput('socialenv', selectize = TRUE, label = span("Select variables", 
                                                               popover(bs_icon("lightbulb"),
                                                                       "See actionable tips for this category",
                                                                       title = "Actionable Tips",
                                                                       placement = "right")), data = social_env_inp, multiple = TRUE)
  ),
  accordion_panel(
    "Options", icon = bs_icon("gear"),
    input_switch("showbounds", "Show Tract Boundaries", value = TRUE),
    input_switch("showchart", "Show Graph", value = FALSE),
    fileInput("upload", "Upload a Shapefile"),
    downloadButton("download", "Download data")
  )
  
)

# -------- UI LAYOUT --------
ui <- page_navbar(
  #waiter::use_waiter(), 
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
              sidebar = sidebar(categories, 
                                width = "400px")
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
         "Obesity.among.Adults", "All.Teeth.Lost.amond.Adults.65.and.older", "Stroke.amond.Adults",
         names(air_pol_inp)) # CHANGE TO AMONG IN PROCESSING
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
  
#   map_cols <- reactive({
#   # Helper function to safely convert list inputs to character vectors
#   safe_chars <- function(x) {
#     if(is.null(x) || length(x) == 0) {
#       character(0)
#     } else {
#       as.character(unlist(x))  # Convert list to character vector
#     }
#   }
#   
#   cbind(health_outcomes, sociodemo, age, sex, race, social_env, health_prevention, health_behaviors, natural_env, air_pol, built_env) %>%
#     dplyr::select(!!!syms(safe_chars(input$outcomes)), 
#                   !!!syms(safe_chars(input$sociodemo)), 
#                   !!!syms(safe_chars(input$race)), 
#                   !!!syms(safe_chars(input$sex)), 
#                   !!!syms(safe_chars(input$age)), 
#                   !!!syms(safe_chars(input$socialenv)), 
#                   !!!syms(safe_chars(input$prevention)), 
#                   !!!syms(safe_chars(input$behaviors)), 
#                   !!!syms(safe_chars(input$naturalenv)), 
#                   !!!syms(safe_chars(input$airpol)), 
#                   !!!syms(safe_chars(input$builtenv)))
# }) %>% 
#   bindCache(input$outcomes, input$sociodemo, input$race, input$age, input$sex, input$socialenv, input$prevention, input$behaviors, input$naturalenv, input$airpol, input$builtenv) %>%
#   bindEvent(list(input$outcomes, input$sociodemo, input$race, input$age, input$sex, input$socialenv, input$prevention, input$behaviors, input$naturalenv, input$airpol, input$builtenv))
  
  map_cols <- reactive({
    # to_syms <- function(x) {
    #   if(is.null(x) || length(x) == 0) return(list())
    #   syms(x)
    # }
    cbind(health_outcomes, sociodemo, age, sex, race, social_env, health_prevention, health_behaviors, natural_env, air_pol, built_env) %>%
      dplyr::select(!!!input$outcomes, !!!input$sociodemo, !!!input$race, !!!input$sex, !!!input$age,
                    !!!input$socialenv, !!!input$prevention, !!!input$behaviors, !!!input$airpol, !!!input$naturalenv, , !!!input$builtenv)

  }) %>%
    bindCache(input$outcomes, input$sociodemo, input$race, input$age, input$sex, input$socialenv, input$prevention, input$behaviors, input$naturalenv, input$airpol, input$builtenv) %>% # reduce work by server
    bindEvent(list(input$outcomes, input$sociodemo, input$race, input$age, input$sex, input$socialenv, input$prevention, input$behaviors, input$naturalenv, input$airpol, input$builtenv))

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