# MC :)

#TODO ELEMENTS:
## modal for intro/welcome message
## add table features

library(shiny)
library(shinyjs)
# library(shinycssloaders)
library(htmltools)
#library(htmlwidgets)
#library(crosstalk)
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

# polygon data tied to census tracts or counties
data <- st_read("Data_Processed/complete/geoexmap_data.gpkg", layer = "geoexmap_data") 

food <- st_read("Data_Processed/complete/geoexmap_data.gpkg", layer = "food_env")

crime <- st_read("Data_Processed/complete/geoexmap_data.gpkg", layer = "county_crime")

wscr.inc <- fread("Data_Processed/wscr_inc.csv")
wscr.mort <- fread("Data_Processed/wscr_mort.csv") %>% 
  mutate(Stage.At.Diagnosis = "Invasive")

# point data
transit <- st_read("Data_Processed/complete/geoexmap_data.gpkg",
                   layer = "transit")

cancer.progs <- st_read("Data_Processed/complete/geoexmap_data.gpkg",
                        layer = "cancer_progs")

clinics <- st_read("Data_Processed/complete/geoexmap_data.gpkg", layer = "clinics") %>% 
  dplyr::filter(!is.na(POINT_X)) 
ems <- st_read("Data_Processed/complete/geoexmap_data.gpkg", layer = "ems")
hospitals <- st_read("Data_Processed/complete/geoexmap_data.gpkg", layer = "hospitals")
pharmacies <- st_read("Data_Processed/complete/geoexmap_data.gpkg", layer = "pharmacies")
wic.clinics <- st_read("Data_Processed/complete/geoexmap_data.gpkg", layer = "wic_clinics")
wic.retailers <- st_read("Data_Processed/complete/geoexmap_data.gpkg", layer = "wic_retailers")

fqhc <- st_read("Data_Processed/complete/geoexmap_data.gpkg", layer = "fqhc")

alc <- st_read("Data_Processed/complete/geoexmap_data.gpkg", layer = "alc_retailers")

# polygon data not tied to census tracts
superfund <- st_read("Data_Processed/complete/geoexmap_data.gpkg", layer = "superfund")

parks <- st_read("Data_Processed/complete/geoexmap_data.gpkg", layer = "parks")

#### DEFINE DATA CATEGORIES ####
data$Binge.Drinking.among.Adults <- as.numeric(data$Binge.Drinking.among.Adults)
data$Cigarette.Smoking.among.Adults <- as.numeric(data$Cigarette.Smoking.among.Adults)
data$No.Leisure.time.Physical.Activity.among.Adults <- as.numeric(data$No.Leisure.time.Physical.Activity.among.Adults)
data$Short.Sleep.Duration <- as.numeric(data$Short.Sleep.Duration)

df_vars <- data

sociodemographics <- c("Population (total)" = "Total.Population")

racev <- c("Non-Hispanic White (total)" = "White.NonHispanic", "Non-Hispanic White (percentage)" = "Percent.White.NonHispanic",
          "Non-Hispanic Black (total)" = "Black.NonHispanic", "Non-Hispanic Black (percentage)" = "Percent.Black.NonHispanic",
          "Non-Hispanic Asian (total)" = "Asian.NonHispanic", "Non-Hispanic Asian (percentage)" = "Percent.Asian.NonHispanic",
          "Non-Hispanic American Indian or Alaska Native (total)" = "American.Indian.Alaska.Native.NonHispanic", "Non-Hispanic American Indian or Alaska Native (percentage)" = "Percent.American.Indian.Alaska.Native.NonHispanic",
          "Non-Hispanic Native Hawaiian or Pacific Islander (total)" = "Native.Hawaiian.Pacific.Islander.NonHispanic",  "Non-Hispanic Native Hawaiian or Pacific Islander (percentage)" =  "Percent.Native.Hawaiian.Pacific.Islander.NonHispanic",
          "Non-Hispanic other race (total)" = "Other.Race.NonHispanic", "Non-Hispanic other race (percentage)" = "Percent.Other.Race.NonHispanic",
          "Non-Hispanic two or more races (total)" = "Two.or.More.Races.NonHispanic", "Non-Hispanic two or more races (percentage)" = "Percent.Two.or.More.Races.NonHispanic",
          "Hispanic or Latino (total)" = "Hispanic.or.Latino", "Hispanic or Latino (percentage)" = "Percent.Hispanic.or.Latino",
          "Hispanic or Latino White (total)" = "White.Hispanic.or.Latino", "Hispanic or Latino White (percentage)" = "Percent.White.Hispanic.or.Latino",
          "Hispanic or Latino Black (total)" = "Black.Hispanic.or.Latino", "Hispanic or Latino Black (percentage)" = "Percent.Black.Hispanic.or.Latino",
          "Hispanic or Latino Asian (total)" = "Asian.Hispanic.or.Latino", "Hispanic or Latino Asian (percentage)" = "Percent.Asian.Hispanic.or.Latino",
          "Hispanic or Latino American Indian or Alaska Native (total)" = "Asian.Hispanic.or.Latino", "Hispanic or Latino American Indian or Alaska Native (percentage)" = "Percent.American.Indian.Alaska.Native.Hispanic.or.Latino",
          "Hispanic or Latino Native Hawaiian or Pacific Islander (total)" = "Native.Hawaiian.Pacific.Islander.Hispanic.or.Latino", "Hispanic or Latino Native Hawaiian or Pacific Islander (percentage)" = "Percent.Native.Hawaiian.Pacific.Islander.Hispanic.or.Latino",
          "Hispanic or Latino other race (total)" = "Other.Race.Hispanic.or.Latino", "Hispanic or Latino other race (percentage)" = "Percent.Other.Race.Hispanic.or.Latino",
          "Hispanic or Latino two or more races (total)" = "", "Hispanic or Latino two or more races (percentage)" = "Percent.Two.or.More.Races.Hispanic.or.Latino")

sexv <- c("Male (total)" = "Total.Male.Population",
          "Male (percentage)" = "Percent.Male",
          "Female (total)" = "Total.Female.Population",
         "Female (percentage)" = "Percent.Female")

agev <- c("0-4 years (total)" = "Total.0.to.4.years", "0-4 years (percentage)" = "Percent.0.to.4.years",
         "5-9 years (total)" = "Total.5.to.9.years", "5-9 years (percentage)" = "Percent.5.to.9.years",
         "10-14 years (total)" = "Total.10.to.14.years", "10-14 years (percentage)" = "Percent.10.to.14.years",
         "15-19 years (total)" = "Total.15.to.19.years", "15-19 years (percentage)" = "Percent.15.to.19.years",
         "20-24 years (total)" = "Total.20.to.24.years", "20-24 years (percentage)" = "Percent.20.to.24.years",
         "25-29 years (total)" = "Total.25.to.29.years", "25-29 years (percentage)" = "Percent.25.to.29.years",
         "30-34 years (total)" = "Total.30.to.34.years", "30-34 years (percentage)" = "Percent.30.to.34.years",
         "35-39 years (total)" = "Total.35.to.39.years", "35-39 years (percentage)" = "Percent.35.to.39.years",
         "40-44 years (total)" = "Total.40.to.44.years", "40-44 years (percentage)" = "Percent.40.to.44.years",
         "45-49 years (total)" = "Total.45.to.49.years", "45-49 years (percentage)" = "Percent.45.to.49.years",
         "50-54 years (total)" = "Total.50.to.54.years", "50-54 years (percentage)" = "Percent.50.to.54.years",
         "55-59 years (total)" = "Total.55.to.59.years", "55-59 years (percentage)" = "Percent.55.to.59.years",
         "60-64 years (total)" = "Total.60.to.64.years", "60-64 years (percentage)" = "Percent.60.to.64.years",
         "65-69 years (total)" = "Total.65.to.69.years", "65-69 years (percentage)" = "Percent.65.to.69.years",
         "70-74 years (total)" = "Total.70.to.74.years", "70-74 years (percentage)" = "Percent.70.to.74.years",
         "75-79 years (total)" = "Total.75.to.79.years", "75-79 years (percentage)" = "Percent.75.to.79.years",
         "80-84 years (total)" = "Total.80.to.84.years", "80-84 years (percentage)" = "Percent.80.to.84.years",
         "85 years+ (total)" = "Total.85.and.older", "85 years+ (percentage)" = "Percent.85.and.older")

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
                "Visited dentist in past year" = "Visited.Dentist.in.Past.Year",
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
                "Radon" = "Radon",
                "Avalanche risk" = "Avalanche.Risk.Score",
                "Coastal flooding risk" = "Coastal.Flooding.Risk.Score",
                "Cold wave risk" = "Cold.Wave.Risk.Score",
                "Drought risk" = "Drought.Risk.Score",
                "Earthquake risk" = "Earthquake.Risk.Score",
                "Hail risk" = "Hail.Risk.Score",
                "Heat wave risk" = "Heat.Wave.Risk.Score",
                "Hurricane risk" = "Hurricane.Risk.Score",
                "Ice storm risk" = "Ice.Storm.Risk.Score",
                "Landslide risk" = "Landslide.Risk.Score",
                "Lightning risk" = "Lightning.Risk.Score",
                "Riverine flooding risk" = "Riverine.Flooding.Risk.Score",
                "Strong wind risk" = "Strong.Wind.Risk.Score",
                "Tornado risk" = "Tornado.Risk.Score",
                "Tsunami risk" = "Tsunami.Risk.Score",
                "Volcanic activity risk" = "Volcanic.Activity.Risk.Score",
                "Wildfire risk" = "Wildfire.Risk.Score",
                "Winter weather risk" = "Winter.Weather.Risk.Score"
                )

builtenv <- c("Walkability" = "Walkability",
              "Pesticide exposure" = "Pesticide.Exposure",
              "Green space" = "Green.Space",
              "Light at night" = "Nighttime.Radiance",
              "Blue space" = "bluespace",
              "Persons exposed to noise LAeq >=45-50 dB (total)" = "N.Noise.More.than.LAeq.45.to.50.db",
              "Persons exposed to noise LAeq >=45-50 dB (percentage)" = "Pct.Noise.More.than.LAeq.45.to.50.db",
              "Persons exposed to noise LAeq >=50-60 dB (total)" = "N.Noise.More.than.LAeq.50.to.60.db",
              "Persons exposed to noise LAeq >=50-60 dB (percentage)" = "Pct.Noise.More.than.LAeq.50.to.60.db",
              "Persons exposed to noise LAeq >=60-70 dB (total)" = "N.Noise.More.than.LAeq.60.to.70.db",
              "Persons exposed to noise LAeq >=60-70 dB (percentage)" = "Pct.Noise.More.than.LAeq.60.to.70.db",
              "Persons exposed to noise LAeq >=70-80 dB (total)" = "N.Noise.More.than.LAeq.70.to.80.db",
              "Persons exposed to noise LAeq >=70-80 dB (percentage)" = "Pct.Noise.More.than.LAeq.70.to.80.db",
              "Persons exposed to noise LAeq >=80-90 dB (total)" = "N.Noise.More.than.LAeq.80.to.90.db",
              "Persons exposed to noise LAeq >=80-90 dB (percentage)" = "Pct.Noise.More.than.LAeq.80.to.90.db",
              "Persons exposed to noise LAeq >=90 dB (total)" = "N.Noise.More.than.LAeq.90.db",
              "Persons exposed to noise LAeq >=90 dB (percentage)" = "Pct.Noise.More.than.LAeq.90.db")

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
               "Population density" = "Population.density",
               "Social capital" = "social_capital",
               "Median household income" = "Median.HH.Income",
               "Housing and transportation affordability" = "HT_Index")

crimeenv <- c("Part I Offenses (Count)" = "total_p1",
              "Part I Offenses (Rate)" = "p1_rate",
              "Part II Offenses (Count)" = "total_p2",
              "Part II Offenses (Rate)" = "p2_rate")

# define filters
health_outcomes <- df_vars %>% 
  dplyr::select(c(22:33)) 

health_behaviors <- df_vars %>% 
  dplyr::select(c(18:21)) 

health_prevention <- df_vars %>% 
  dplyr::select(c(11:17)) 

natural_env <- df_vars %>%
  dplyr::select(c(107:108, 131:135, 142:159, 162)) 

air_pol <- df_vars %>% 
  dplyr::select(c(2, 136:140)) 

built_env <- df_vars %>%
  dplyr::select(c(3:4, 109:110, 112:123, 160)) 

sociodemo <- df_vars %>% 
  dplyr::select(c(34)) 

sex <- df_vars %>%
  dplyr::select(c(65:68))

race <- df_vars %>%
  dplyr::select(c(35:64))

age <- df_vars %>%
  dplyr::select(c(69:102))

social_env <- df_vars %>% 
  dplyr::select(c(5:10, 124:130, 105:106, 163, 164)) 

food_env <- food %>% 
  dplyr::select(c(11:50)) 
food_env_inp <- food_env %>% 
  st_drop_geometry()

#### DEFINE MARKDOWN FOR TIPS ####
# define markdown text for tips
health_out_md <- markdown("
                          - Diabetes: learn about ways to [prevent diabetes](https://www.cdc.gov/diabetes/prevention-type-2/index.html)
                          - Obesity: learn about ways to [prevent obesity](https://www.nhlbi.nih.gov/health/overweight-and-obesity/prevention)
                          - Cancer incidence: learn about [risk factors for cancer in general and ways to prevent cancer](https://www.cancer.gov/about-cancer/causes-prevention/patient-prevention-overview-pdq)
                          ")
health_bh_md <- markdown("
                         - Cigarette smoking: learn about ways to [quit smoking](https://www.cdc.gov/tobacco/campaign/tips/quit-smoking/index.html)
                         - No leisure-time physical activity: learn about ways to [get more exercise](https://www.cdc.gov/healthy-weight-growth/physical-activity/getting-started.html)
                         - Short sleep duration: learn about ways to [get better sleep](https://www.cdc.gov/sleep/about/index.html)
                         ")
prev_md <- markdown("
                    - Lack of health insurance: learn about [how to apply for Apple Health](https://www.wahealthplanfinder.org/us/en/my-account/my-coverage/learnapplehealth.html), which is the name for Medicaid in Washington
                    - Routine checkup: learn about the [benefits of staying up to date on your preventive care](https://www.cdc.gov/chronic-disease/prevention/preventive-care.html)
                    - Mammography use: learn about the [benefits of mammography](https://www.cdc.gov/breast-cancer/screening/index.html)
                    - Colorectal cancer screening: learn about the [benefits of colorectal cancer screening](https://www.cdc.gov/colorectal-cancer/screening/index.html)
                    ")
nat_md <- markdown("
                   - Air pollutants: learn about ways to [protect yourself from air pollution](http://www.breatheasy.tips)
                   - Ultraviolet radiation (UV): learn about [sun safety](https://www.cdc.gov/skin-cancer/sun-safety/index.html)
                   - Radon: learn about ways to [test for radon in your home](https://doh.wa.gov/community-and-environment/contaminants/radon)
                   - PFAS in drinking water: learn about ways to [reduce exposure to PFAS](https://doh.wa.gov/community-and-environment/contaminants/pfas)
                   ")

built_md <- markdown("
                     - Walkability: learn about the [health benefits of walking](https://www.heart.org/en/healthy-living/fitness/walking/why-is-walking-the-most-popular-form-of-exercise)
                     - Pesticide use: learn about ways to [reduce pesticide exposure from foods](https://www.epa.gov/safepestcontrol/pesticides-and-food-healthy-sensible-food-practices) and [while using pesticides](https://icash.public-health.uiowa.edu/wp-content/uploads/2017/02/UO218.pdf)
                     - Green space: learn more about the [health benefits of green space](https://www.countyhealthrankings.org/strategies-and-solutions/what-works-for-health/strategies/green-space-parks)
                     ")

food_env_md <- markdown("
                        - Food environment/healthy food: search for [nearby local foods](https://www.usdalocalfoodportal.com/) such as farmers markets
                        ")

soc_md <- markdown("
                   - Food insecurity: call 2-1-1 or text '211WAOD' to 898211 for nearby food banks and free meals from the [WA statewide helpline](https://wa211.org/). Call 1-866-HUNGRY for assistance programs from the [National Hunger Hotline](https://www.hungerfreeamerica.org/en-us/national-hunger-hotline). Find the closest [food bank or meal program from Feeding Washington](https://feedingwashington.org/find-food/).
                   - Housing insecurity: learn about [housing resources](https://www.dshs.wa.gov/esa/community-services-offices/housing-resources) including emergency housing. Call 2-1-1 or text '211WAOD' to 898211 for other housing resources from the [WA statewide helpline](https://wa211.org/)
                   - Lack of reliable transportation: call 2-1-1 or text '211WAOD' to 898211 for help with transportation from the [WA statewide helpline](https://wa211.org/)
                   ")

# -------- UI ELEMENTS --------
categories <- accordion(
  open = FALSE,
  accordion_panel(
    "Sociodemographics", icon = bs_icon("person-vcard"),
    selectInput('sociodemo', 
                span("Select variables"),
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
                             health_out_md,
                             title = "Tips",
                             placement = "right")), outcomes, selectize = TRUE, multiple = TRUE),
    # options to filter by cancer site, stage at diagnosis, gender
    accordion_panel("Cancer Incidence",
                    selectInput('incsite', "Cancer Site", choices = c("Please choose a site" = "", unique(wscr.inc$Cancer.Site)), selectize = TRUE, selected = ""),
                    selectInput('incstage', "Stage at Diagnosis", choices = c("Please choose a stage" = "", unique(wscr.inc$Stage.At.Diagnosis)), selectize = TRUE, selected = ""),
                    selectInput('incsex', "Sex", choices = c("Please choose a sex" = "", unique(wscr.inc$Gender)), selectize = TRUE, selected = ""),
                    actionButton('incbutton', "Reset filters")),
    accordion_panel("Cancer Mortality",
                    selectInput('mortsite', "Cancer Site", choices = c("Please choose a site" = "", unique(wscr.mort$Cancer.Site)), selectize = TRUE, selected = ""),
                    selectInput('mortsex', "Sex", choices = c("Please choose a sex" = "", unique(wscr.mort$Gender)), selectize = TRUE, selected = ""),
                    actionButton('mortbutton', "Reset filters"))
  ),
  accordion_panel(
    "Health Behaviors", icon = bs_icon("person-walking"),
    selectInput('behaviors', span("Select variables", 
                                  popover(bs_icon("lightbulb"),
                                          health_bh_md,
                                          title = "Tips",
                                          placement = "right")), behaviors,
                 multiple = TRUE, selectize = TRUE,)
  ),
  accordion_panel(
    "Prevention", icon = tags$img(src = "/prevention.png", height = "20.48px", width = "20.48 px"),
    selectInput('prevention', span("Select variables", 
                                      popover(bs_icon("lightbulb"),
                                              prev_md,
                                              title = "Tips",
                                              placement = "right")), prevention, selectize = TRUE, multiple = TRUE)
  ),
  accordion_panel(
    "Healthcare Access", icon = bs_icon("building-add"),
    input_switch('cancer', "Cancer Programs", value = FALSE),
    input_switch('clinics', "Clinics", value = FALSE), 
    input_switch('ems', "Emergency Medical Stations", value = FALSE),
    input_switch('hospitals', "Hospitals", value = FALSE),
    input_switch('wic_clinics', "WIC Clinics", value = FALSE),
    input_switch('wic_retailers', "WIC Retailers", value = FALSE),
    input_switch('fqhc', "Federally Qualified Health Centers", value = FALSE)
  ),
  accordion_panel(
    "Natural Environment", icon = bs_icon("sun"),
    selectInput('naturalenv', span("Select variables", 
                                              popover(bs_icon("lightbulb"),
                                                      nat_md,
                                                      title = "Tips",
                                                      placement = "right")), naturalenv, selectize = TRUE, multiple = TRUE),
    accordion_panel("Air pollutants", icon = bs_icon("cloud-haze"),
                   selectInput('airpol', NULL, airpol, selectize = TRUE, multiple = TRUE))
  ),
  accordion_panel(
    "Built Environment", icon = bs_icon("buildings"),
    selectInput('builtenv', span("Select variables", 
                                 popover(bs_icon("lightbulb"),
                                         built_md,
                                         title = "Tips",
                                         placement = "right")), builtenv, selectize = TRUE, multiple = TRUE),
    input_switch('transit', "Transit stops", value = FALSE),
    input_switch('alc', "Alcohol retailers", value = FALSE),
    input_switch('parks', "Parks", value = FALSE),
    input_switch('superfund', "Superfund sites", value = FALSE),
    accordion_panel(
      "Food Environment", icon = bs_icon("basket"),
      varSelectInput('foodenv', label = span("Select variables", 
                                 popover(bs_icon("lightbulb"),
                                         food_env_md,
                                         title = "Tips",
                                         placement = "right")), data = food_env_inp, multiple = TRUE)
    )
  ),
  accordion_panel(
    "Social Environment", icon = tags$img(src = "/social-environment.png", height = "20.48px", width = "20.48px"),
    selectInput('socialenv', span("Select variables", 
                                          popover(bs_icon("lightbulb"),
                                                  soc_md,
                                                  title = "Tips",
                                                  placement = "right")), socialenv, selectize = TRUE,  multiple = TRUE),
    accordion_panel('Crime', icon = bs_icon('file-earmark-lock'),
                    selectInput('crime', NULL, crimeenv, selectize = TRUE, multiple = TRUE))
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
  tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico/geoexmap_favicon.png")),
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
  showModal(modalDialog(
    title = "Welcome to geoexmap!",
    HTML("Thank you for visiting geoexmap! Please note that this tool is still <b>under development</b>. 
    <br><br><b>Please do not distribute data or images of geoexmap at this time.</b>")
  ))
  # turn switch off if clear button is clicked
  observeEvent(input$clear, {
    update_switch("showchart", value = FALSE)
   # update_switch("")
  })
  
  #### PALETTE FUNCTION ####
  # define categories for palettes
  # "good", "bad", "neutral"
  g <- c("Green.Space", "Routine.Checkup.in.the.Past.Year", "Visited.Denstist.in.Past.Year", "Cholesterol.Screening",
         "Taking.Medicine.to.Control.High.Blood.Pressure", "Mammography.Use.among.Women.50.to.74",
         "Colorectal.Cancer.Screening.among.Adults.45.to.75", "Walkability")
  b <- c("Particulate.Matter.2.5", "Arthritis.among.Adults", 
         "Food.Stamps", "Food.Insecurity",
         "Housing.Insecurity", "Utility.Services.Threat", "Lacking.Reliable.Transportation", 
         "Lack.Of.Health.Insurance", "Binge.Drinking.among.Adults", "Cigarette.Smoking.among.Adults",
         "No.Leisure.Time.Physical.Activity.among.Adults", "Short.Sleep.Duration",
         "Asthma.among.Adults", "High.Blood.Pressure.among.Adults", "High.Blood.Pressure.among.Adults",
         "Cancer.or.Melanoma.among.Adults", "High.Cholesterol.among.Screened.Adults", "COPD.among.Adults",
         "Coronary.Heart.Disease.among.Adults", "Depression.among.Adults", "Diagnosed.Diabetes.among.Adults",
         "Obesity.among.Adults", "All.Teeth.Lost.among.Adults.65.and.older", "Stroke.among.Adults") 
  n <- c("Nighttime.Radiance", "Total.Population")
  
  
  geoex.palette <- function(var) {
    tryCatch({
      # skip geometry column to avoid error
      if (var == "geometry" || inherits(df_vars[[var]], "sfc") || var == "GEOID") {
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
    if(col == "Food.Stamps") return("Population on SNAP")
    if(col == "Food.Insecurity") return("Prevalence of Food Insecurity")
    if(col == "Housing.Insecurity") return("Prevalence of Housing Insecurity")
    if(col == "Utility.Services.Threat") return("Prevalence of Utility Services Threats")
    if(col == "Lacking.Reliable.Transportation") return("Population Lacking Reliable Transportation")
    if(col == "Lack.Of.Health.Insurance") return("Population Lacking Health Insurance")
    if(col == "Routine.Checkup.in.the.Past.Year") return("Routine Checkups (Past Year)")
    if(col == "Visited.Denstist.in.Past.Year") return("Dental Visits (Past Year)")
    if(col == "Taking.Medicine.to.Control.High.Blood.Pressure") return("Population Taking BP Medicine")
    if(col == "Cholesterol.Screening") return("Prevalence of Cholesterol Screening")
    if(col == "Mammography.Use.among.Women.50.to.74") return("Mammography Use (Women 50-74)")
    if(col == "Colorectal.Cancer.Screening.among.Adults.45.to.75") return("Colorectal Cancer Screening (Adults 45-75)")
    if(col == "Binge.Drinking.among.Adults") return("Binge Drinking Among Adults")
    if(col == "Cigarette.Smoking.among.Adults") return("Cigarette Smoking Among Adults")
    if(col == "No.Leisure.Time.Physical.Activity.among.Adults") return("Lack of Leisurely Physical Activity Among Adults")
    if(col == "Short.Sleep.Duration") return("Short Sleep Duration Among Adults")
    if(col == "Arthritis.among.Adults") return("Arthritis Among Adults")
    if(col == "Asthma.among.Adults") return("Asthma among Adults")
    if(col == "High.Blood.Pressure.among.Adults") return("High Blood Pressure Among Adults")
    if(col == "Cancer.or.Melanoma.among.Adults") return("Cancer or Melanoma Among Adults")
    if(col == "High.Cholesterol.among.Screened.Adults") return("High Cholesterol Among Screened Adults")
    if(col == "COPD.among.Adults") return("COPD Among Adults")
    if(col == "Coronary.Heart.Disease.among.Adults") return("Coronary Heart Disease Among Adults")
    if(col == "Depression.among.Adults") return("Depression Among Adults")
    if(col == "Diagnosed.Diabetes.among.Adults") return("Diabetes Among Adults")
    if(col == "Obesity.among.Adults") return("Obesity Among Adults")
    if(col == "All.Teeth.Lost.among.Adults.65.and.older") return("All Teeth Lost Among Adults 65+")
    if(col == "Stroke.among.Adults") return("Stroke Among Adults")
    if(col == "Total.Population") return("Total Population")
    if(col == "Earthquake.Risk.Score") return("Earthquake Risk")
    if(col == "Walkability") return("Walkability")
    if(col == "No.broadband.internet") return("Lack of Internet Access")
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
    updateSelectInput(session, "incsite", selected = "")
    updateSelectInput(session, "incstage", selected = "")
    updateSelectInput(session, "incsex", selected = "")
    updateSelectInput(session, "mortsite", selected = "")
    updateSelectInput(session, "mortsex", selected = "")
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
  
  #### OBSERVERS FOR WSCR DATA ####
  filtered.inc <- reactive({
    req(input$incsite != "", input$incstage != "", input$incsex != "")
    wscr.inc %>% 
      filter(Cancer.Site == input$incsite,
             Stage.At.Diagnosis == input$incstage,
             Gender == input$incsex) 
  })
  
  # Helper: no filter if input is "All" or NULL, filter otherwise
  filter_if_needed <- function(df, col, val) {
    if (is.null(val) || val == "All" || val == "") df else df[df[[col]] == val, , drop = FALSE]
  }
  
  # On mortstage or mortsex change, update sites with a *union* of sites available for selected sex
  observe({
    df <- wscr.inc
    df <- filter_if_needed(df, "Stage.At.Diagnosis", input$incstage)
    df <- filter_if_needed(df, "Cancer.Site", input$incsite)
    # Instead of filtering by mortsex, select all sites available for either sex (or All)
    if (!is.null(input$incsex) && input$incsex != "All") {
      df <- df[df$Gender %in% c("All", input$incsex), , drop = FALSE]
    }
    sites <- sort(unique(df$Cancer.Site))
    updateSelectInput(session, "incsite", choices = c("Please choose a site" = "", sites),
                      selected = isolate(input$incsite))
  })
  
  # On mortsite or mortsex change, update stages 
  observe({
    df <- wscr.inc
    df <- filter_if_needed(df, "Cancer.Site", input$incsite)
    if (!is.null(input$incsex) && input$incsex != "All") {
      df <- df[df$Gender %in% c("All", input$incsex), , drop = FALSE]
    }
    stages <- sort(unique(df$Stage.At.Diagnosis))
    updateSelectInput(session, "incstage", choices = c("Please choose a stage" = "", stages),
                      selected = isolate(input$incstage))
  })
  
  # On mortsite or mortstage change, update sexes
  observe({
    df <- wscr.inc
    df <- filter_if_needed(df, "Cancer.Site", input$incsite)
    df <- filter_if_needed(df, "Stage.At.Diagnosis", input$incstage)
    genders <- sort(unique(df$Gender))
    updateSelectInput(session, "incsex", choices = c("Please choose a sex" = "", genders),
                      selected = isolate(input$incsex))
  })
  
  observeEvent(input$incbutton, {
    updateSelectInput(session, "incsite", selected = "")
    updateSelectInput(session, "incstage", selected = "")
    updateSelectInput(session, "incsex", selected = "")
    
    leafletProxy("geoexmap") %>% 
      clearGroup("cancerincidence") %>% 
      clearControls()
  })
  
  filtered.mort <- reactive({
    req(input$mortsite != "", input$mortsex != "")
    wscr.mort %>% 
      filter(Cancer.Site == input$mortsite,
             Gender == input$mortsex)
  })
  
  # On mortstage or mortsex change, update sites with a *union* of sites available for selected sex
  observe({
    df <- wscr.mort
    # Instead of filtering by mortsex, select all sites available for either sex (or All)
    if (!is.null(input$mortsex) && input$mortsex != "" && input$mortsex != "All") {
      df <- df[df$Gender %in% c("All", input$mortsex), , drop = FALSE]
    }
    sites <- sort(unique(df$Cancer.Site))
    updateSelectInput(session, "mortsite", choices = c("Please choose a site" = "", sites),
                      selected = isolate(input$mortsite))
  })
  
  # On mortsite or mortstage change, update sexes
  observe({
    df <- wscr.mort
    df <- filter_if_needed(df, "Cancer.Site", input$mortsite)
    genders <- sort(unique(df$Gender))
    updateSelectInput(session, "mortsex", choices = c("Please choose a sex" = "", genders),
                      selected = isolate(input$mortsex))
  })
  
  observeEvent(input$mortbutton, {
    updateSelectInput(session, "mortsite", selected = "")
    updateSelectInput(session, "mortsex", selected = "")
    
    leafletProxy("geoexmap") %>% 
      clearGroup("cancermortality") %>% 
      clearControls()
  })
  
  # track active variables
  values <- reactiveValues(
    active_variables = character(0)
  )
  
  table_cols <- reactive({
    data[, 1] %>% 
      cbind(map_cols()) %>% 
      select(-contains("geom")) %>% 
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
  
  #### INITIAL MAP RENDER ####
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
    validate(need(base::ncol(table_cols()) > 1, "Please select a variable."))
    
    reactable(table_cols(),
              defaultColDef = colDef(
                header = function(value) gsub(".", " ", value, fixed = TRUE),
                cell = function(value) if(is.numeric(value)) round(value, 3) else value,
                align = "left"
              ))
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
      
      if (input$superfund) {
        proxy <- proxy %>% 
          addPolygons(data = superfund,
                      popup = ~SITE_NAME,
                      group = "superfund",
                      stroke = TRUE, weight = 0.9, color = "blue",
                      fillOpacity = 0.3, highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE))
      } else {
        proxy <- proxy %>% 
          clearGroup(group = "superfund")
      }
      
      if (input$parks) {
        proxy <- proxy %>% 
          addPolygons(data = parks,
                      popup = ~NAME,
                      group = "parks",
                      stroke = TRUE, weight = 0.9, color = "green",
                      fillOpacity = 0.3, highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE))
      } else {
        
      }
      
      if (input$clinics) {
        html_legend <- '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-clipboard-plus-fill" viewBox="0 0 16 16">
          <path d="M6.5 0A1.5 1.5 0 0 0 5 1.5v1A1.5 1.5 0 0 0 6.5 4h3A1.5 1.5 0 0 0 11 2.5v-1A1.5 1.5 0 0 0 9.5 0zm3 1a.5.5 0 0 1 .5.5v1a.5.5 0 0 1-.5.5h-3a.5.5 0 0 1-.5-.5v-1a.5.5 0 0 1 .5-.5z"/>
            <path d="M4 1.5H3a2 2 0 0 0-2 2V14a2 2 0 0 0 2 2h10a2 2 0 0 0 2-2V3.5a2 2 0 0 0-2-2h-1v1A2.5 2.5 0 0 1 9.5 5h-3A2.5 2.5 0 0 1 4 2.5zm4.5 6V9H10a.5.5 0 0 1 0 1H8.5v1.5a.5.5 0 0 1-1 0V10H6a.5.5 0 0 1 0-1h1.5V7.5a.5.5 0 0 1 1 0"/>
              </svg> Clinics <br/>'
        print("adding clinics")
        
        proxy <- proxy %>% 
          addMarkers(data = clinics,
                     popup = ~NAME,
                     group = "clinics",
                     icon = makeIcon("/clipboard-plus-fill.svg")) %>% 
          addControl(html = html_legend, position = "topright")
      } else {
        proxy <- proxy %>% 
          clearGroup(group = "clinics")
      }
      
      if (input$ems) {
        html_legend <- "EMS Stations </br>"
        
        proxy <- proxy %>% 
          addMarkers(data = ems,
                     popup = ~AGENCY,
                     group = "ems") %>% 
          addControl(html = html_legend, position = "topright")
      } else {
        proxy <- proxy %>% 
          clearGroup(group = "ems")
      }
      
      if (input$hospitals) {
        html_legend <- 'Hospitals <br/>'
        
        proxy <- proxy %>% 
          addMarkers(data = hospitals,
                     popup = ~NAME,
                     group = "hospitals") %>% 
          addControl(html = html_legend, position = "topright")
      } else {
        proxy <- proxy %>% 
          clearGroup(group = "hospitals")
      }
      
      if (input$wic_clinics) {
        html_legend <- 'WIC Clinics <br/>'
        
        proxy <- proxy %>% 
          addMarkers(data = wic.clinics,
                     popup = ~Clinic,
                     group = "wic_clinics") %>% 
          addControl(html = html_legend, position = "topright")
      } else {
        proxy <- proxy %>% 
          clearGroup(group = "wic_clinics")
      }
      
      if (input$wic_retailers) {
        html_legend <- 'WIC Retailers <br/>'
        
        proxy <- proxy %>% 
          addMarkers(data = wic.retailers,
                     popup = ~Retailer,
                     group = "wic_retailers") %>% 
          addControl(html = html_legend, position = "topright")
      } else {
        proxy <- proxy %>% 
          clearGroup(group = "wic_retailers")
      }
      
      if (input$fqhc) {
        html_legend <- 'Federally Qualified Health Centers (FQHCs)'
        
        proxy <- proxy %>% 
          addMarkers(data = fqhc,
                     popup = ~Facility,
                     group = "fqhc") %>% 
          addControl(html = html_legend, position = "topright")
      } else {
        proxy <- proxy %>% 
          clearGroup(group = "fqhc")
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

        # if (map_cols()[[c]] == "PFAS_dw") {
        #   proxy <- proxy %>% 
        #     addPolygons(., fillColor = ~pal(map_cols()[[c]]))
        # }
        # skip null to avoid geometry
        # else ...
        if (!is.null(pal)){
          proxy <- proxy %>% 
            addPolygons(., fillColor = ~pal(map_cols()[[c]]), stroke = input$showbounds, weight = 0.75, color = "black",
                        fillOpacity = 0.3, highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
                        label = "") %>% 
            addLegend(pal = pal, values = ~map_cols()[[c]], title = legend.titles(c)) 
        }
        
        
      }
      observe({
        if (!is.null(filtered.inc()) && nrow(filtered.inc()) > 0) {
          geo.inc <- base::merge(county.bounds, filtered.inc(), by.x = "NAME", by.y = "counties") %>% 
            mutate(Age.Adj..Rate.per.100.000 = as.numeric(Age.Adj..Rate.per.100.000))
          print(geo.inc)
          if (nrow(geo.inc) > 0) {
            pal <- colorNumeric("YlOrRd", domain = geo.inc$Age.Adj..Rate.per.100.000)
            val <- sort(geo.inc$Age.Adj..Rate.per.100.000)
            proxy <- proxy %>%
              addPolygons(data = geo.inc, fillColor = ~pal(Age.Adj..Rate.per.100.000),
                          popup = ~paste(NAMELSAD, "<br>Site:", Cancer.Site, "<br>Stage:", Stage.At.Diagnosis, "<br>Sex:", Gender,
                                         "<br>Age-Adjusted Rate:", Age.Adj..Rate.per.100.000),
                          group = "cancerincidence", weight = 0.75, color = "black", fillOpacity = 0.3, highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE)) %>% 
              addLegend(pal = pal, values = val, title = paste(unique(geo.inc$Cancer.Site), "Cancer Incidence Rate per 100,000:"))
          }
          
        } else {
          proxy <- proxy %>%
            clearGroup("cancerincidence")
        }
      })
      
      observe({
        if (!is.null(filtered.mort()) && nrow(filtered.mort()) > 0) {
          geo.mort <- base::merge(county.bounds, filtered.mort(), by.x = "NAME", by.y = "counties") %>% 
            mutate(Age.Adj..Rate.per.100.000 = as.numeric(Age.Adj..Rate.per.100.000))
          print(geo.mort)
          if (nrow(geo.mort) > 0) {
            pal <- colorNumeric("YlOrRd", domain = geo.mort$Age.Adj..Rate.per.100.000, n = 5)
            val <- sort(geo.mort$Age.Adj..Rate.per.100.000)
            proxy <- proxy %>%
              addPolygons(data = geo.mort, fillColor = ~pal(Age.Adj..Rate.per.100.000),
                          popup = ~paste(NAMELSAD, "<br>Site:", Cancer.Site, "<br>Stage:", Stage.At.Diagnosis, "<br>Sex:", Gender,
                                         "<br>Age-Adjusted Rate:", Age.Adj..Rate.per.100.000),
                          group = "cancermortality", weight = 0.75, color = "black", fillOpacity = 0.3, highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE)) %>% 
              addLegend(pal = pal, values = val, title = paste(unique(geo.mort$Cancer.Site), "Cancer Mortality Rate per 100,000:"))
          }
          
        } else {
          proxy <- proxy %>%
            clearGroup("cancermortality")
        }
      })
      
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
    bindEvent(list(input$outcomes, input$sociodemo, input$socialenv, input$behaviors, input$prevention, input$naturalenv, input$builtenv, input$transit, input$alc, input$superfund,
                   input$parks, input$cancer, input$clinics, input$ems, input$hospitals, input$wic_clinics, input$wic_retailers, input$fqhc, input$showcities, input$showcounties, input$showbounds, 
                   input$upload, input$foodenv))
}

# -------- CREATE SHINY APP --------

options <- list()

if (!interactive()) {
  options$shiny.port = 3838
  options$shiny.host = "0.0.0.0"
}

shinyApp(ui = ui, server = server, options = options)