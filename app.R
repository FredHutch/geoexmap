# Geoexmap: map-based visualization tool for all things health and environment

# MC :)

library(shiny)
library(shinyjs)

library(htmltools)
library(rintrojs)
library(reactable)
library(tidyverse)
library(sf)
library(data.table)
library(markdown)

library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(leaflegend)
library(plotly)
#library(crosstalk)

library(RColorBrewer)
library(bslib)
library(bsicons)
library(dplyr)

library(rlang)

#### LOAD DATA #### 
# empty shapefiles
city.bounds <- st_read("Geo/city/cities.gpkg")
county.bounds <- st_read("Geo/county/counties.gpkg")
tract.bounds <- st_read("Geo/2020/wa_tracts_2020.gpkg") %>% 
  dplyr::select(c(GEOID, NAMELSAD, NAMELSADCO))

# polygon data tied to census tracts or counties
data <- st_read("Data_Processed/complete/geoexmap_data.gpkg", layer = "geoexmap_data")

og.data <- data # keep original data for data download/tables

# change here so that mapped values are NA if 0--to map transparently on the map
#data <- data %>% 
#  mutate(across(where(is.numeric), ~na_if(., 0)))

food <- st_read("Data_Processed/complete/geoexmap_data.gpkg", layer = "food_env") %>% 
  mutate(lapop1 = as.numeric(lapop1), lapop1share = as.numeric(lapop1share))

crime <- st_read("Data_Processed/complete/geoexmap_data.gpkg", layer = "county_crime")

wscr.inc <- fread("Data_Processed/wscr_inc.csv")
wscr.mort <- fread("Data_Processed/wscr_mort.csv") %>% 
  mutate(Stage.At.Diagnosis = "Invasive")

# point data
# TODO: clean up data (make more consistent)
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

microplastics <- st_read("Data_Processed/complete/geoexmap_data.gpkg", layer = "microplastics")
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
          "Non-Hispanic Native Hawaiian or Other Pacific Islander (total)" = "Native.Hawaiian.Pacific.Islander.NonHispanic",  "Non-Hispanic Native Hawaiian or Other Pacific Islander (percentage)" =  "Percent.Native.Hawaiian.Pacific.Islander.NonHispanic",
          "Non-Hispanic Other race (total)" = "Other.Race.NonHispanic", "Non-Hispanic Other race (percentage)" = "Percent.Other.Race.NonHispanic",
          "Non-Hispanic two or more races (total)" = "Two.or.More.Races.NonHispanic", "Non-Hispanic two or more races (percentage)" = "Percent.Two.or.More.Races.NonHispanic",
          "Hispanic or Latino (total)" = "Hispanic.or.Latino", "Hispanic or Latino (percentage)" = "Percent.Hispanic.or.Latino",
          "Hispanic or Latino White (total)" = "White.Hispanic.or.Latino", "Hispanic or Latino White (percentage)" = "Percent.White.Hispanic.or.Latino",
          "Hispanic or Latino Black (total)" = "Black.Hispanic.or.Latino", "Hispanic or Latino Black (percentage)" = "Percent.Black.Hispanic.or.Latino",
          "Hispanic or Latino Asian (total)" = "Asian.Hispanic.or.Latino", "Hispanic or Latino Asian (percentage)" = "Percent.Asian.Hispanic.or.Latino",
          "Hispanic or Latino American Indian or Alaska Native (total)" = "Asian.Hispanic.or.Latino", "Hispanic or Latino American Indian or Alaska Native (percentage)" = "Percent.American.Indian.Alaska.Native.Hispanic.or.Latino",
          "Hispanic or Latino Native Hawaiian or Other Pacific Islander (total)" = "Native.Hawaiian.Pacific.Islander.Hispanic.or.Latino", "Hispanic or Latino Native Hawaiian or Other Pacific Islander (percentage)" = "Percent.Native.Hawaiian.Pacific.Islander.Hispanic.or.Latino",
          "Hispanic or Latino Other race (total)" = "Other.Race.Hispanic.or.Latino", "Hispanic or Latino Other race (percentage)" = "Percent.Other.Race.Hispanic.or.Latino",
          "Hispanic or Latino two or more races (total)" = "Two.or.More.Races.Hispanic.or.Latino", "Hispanic or Latino two or more races (percentage)" = "Percent.Two.or.More.Races.Hispanic.or.Latino")

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
              "Cancer prevalence" = "Cancer.or.Melanoma.among.Adults",
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
                "Routine checkup" = "Routine.Checkup.in.the.Past.Year",
                "Visited dentist" = "Visited.Dentist.in.Past.Year",
                "Taking blood pressure medication" = "Taking.Medicine.to.Control.High.Blood.Pressure",
                "Cholesterol screening" = "Cholesterol.Screening",
                "Mammography screening for breast cancer" = "Mammography.Use.among.Women.50.to.74",
                "Colorectal cancer screening" = "Colorectal.Cancer.Screening.among.Adults.45.to.75")

airpol <- c("Particulate matter <2.5 microns in diameter (PM\U2082.\U0323\U2085)" = "Particulate.Matter.2.5",
            "Wildfire smoke PM\U2082.\U0323\U2085" = "Wildfire.smoke",
            "Nitrogen dioxide (NO\U2082)" = "Nitrogen.dioxide",
            "Sulfur dioxide (SO\U2082)" = "Sulfur.dioxide",
            "Carbon monoxide (CO)" = "Carbon.monoxide",
            "Ozone (O\U2083)" = "Ozone")

naturalenv <- c("Ultraviolet radiation index (UVI)" = "UV.Index",
                "Maximum temperature" = "Maximum.temperature",
                "Minimum temperature" = "Minimum.temperature",
                "Average temperature" = "Average.temperature",
                "Radon" = "Radon",
                "Per- and polyfluoroalkyl substances (PFAS) in drinking water" = "PFAS_dw"
                )

hazardenv <- c("Avalanche risk" = "Avalanche.Risk.Score",
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
               "Winter weather risk" = "Winter.Weather.Risk.Score")

builtenv <- c("Walkability" = "Walkability",
              "Pesticide use" = "Pesticide.Exposure",
              "Green space" = "Green.Space",
              "Light at night" = "Nighttime.Radiance",
              "Blue space" = "bluespace"
              )

noiseenv <- c("Population exposed to noise LAeq \U2265 45-50 dB (total)" = "N.Noise.More.than.LAeq.45.to.50.db",
               "Population exposed to noise LAeq \U2265 45-50 dB (percentage)" = "Pct.Noise.More.than.LAeq.45.to.50.db",
               "Population exposed to noise LAeq \U2265 50-60 dB (total)" = "N.Noise.More.than.LAeq.50.to.60.db",
               "Population exposed to noise LAeq \U2265 50-60 dB (percentage)" = "Pct.Noise.More.than.LAeq.50.to.60.db",
               "Population exposed to noise LAeq \U2265 60-70 dB (total)" = "N.Noise.More.than.LAeq.60.to.70.db",
               "Population exposed to noise LAeq \U2265 60-70 dB (percentage)" = "Pct.Noise.More.than.LAeq.60.to.70.db",
               "Population exposed to noise LAeq \U2265 70-80 dB (total)" = "N.Noise.More.than.LAeq.70.to.80.db",
               "Population exposed to noise LAeq \U2265 70-80 dB (percentage)" = "Pct.Noise.More.than.LAeq.70.to.80.db",
               "Population exposed to noise LAeq \U2265 80-90 dB (total)" = "N.Noise.More.than.LAeq.80.to.90.db",
               "Population exposed to noise LAeq \U2265 80-90 dB (percentage)" = "Pct.Noise.More.than.LAeq.80.to.90.db",
               "Population exposed to noise LAeq \U2265 90 dB (total)" = "N.Noise.More.than.LAeq.90.db",
               "Population exposed to noise LAeq \U2265 90 dB (percentage)" = "Pct.Noise.More.than.LAeq.90.db")

landuseenv <- c("Open water" = "pct_Open_Water",
                 "Developed open land" = "pct_Developed_Open",
                 "Low development" = "pct_Developed_Low",
                 "Moderate development" = "pct_Developed_Medium",
                 "High development" = "pct_Developed_High",
                 "Barren land" = "pct_Barren",
                 "Evergreen forest" = "pct_Evergreen_Forest",
                 "Shrubland" = "pct_Shrub",
                 "Grassland" = "pct_Grassland",
                 "Pasture" = "pct_Pasture",
                 "Cropland" = "pct_Crops",
                 "Woody wetlands" = "pct_Woody_Wetlands",
                 "Herbaceous wetlands" = "pct_Herbaceous_Wetlands",
                 "Deciduous forest" = "pct_Deciduous_Forest",
                 "Mixed forest" = "pct_Mixed_Forest",
                 "Perennial ice" = "pct_Perennial_Ice")

socialenv <- c("Food insecurity" = "Food.Insecurity",
               "Housing insecurity" = "Housing.Insecurity",
               "Utility services threat" = "Utility.Services.Threat",
               "Lack of reliable transportation" = "Lacking.Reliable.Transportation",
               "Lack of social and emotional support" =  "Lack.of.Social.and.Emotional.Support",
               "No internet" = "No.broadband.internet",
               "No high school education" = "No.high.school.diploma",
               "Single parent households" = "Single.parent.households",
               "Housing cost burden" = "Housing.cost.burden",
               "Household crowding" = "Crowding",
               "Poverty" = "Poverty",
               "Unemployment" = "Unemployment",
               "Social Vulnerability Index" = "Social.Vulnerability.Index",
               "Environmental Justice Index" = "Environmental.Justice.Index",
               "Racial residential segregation" = "Racial.Residential.Segregation",
               "Population density" = "Population.density",
               "Social capital" = "social_capital",
               "Median household income" = "Median.HH.Income",
               "Housing and Transportation (H+T\U00AE) Affordability Index" = "HT_Index",
               "Historic redlining" = "Historic.Redlining.Score")

crimeenv <- c("Part I offenses (count)" = "total_p1",
              "Part I offenses (rate)" = "p1_rate",
              "Part II offenses (count)" = "total_p2",
              "Part II offenses (rate)" = "p2_rate")

foodenv <- c("Population > 1 mile from supermarket (total)" = "lapop1",
             "Population > 1 mile from supermarket (percentage)" = "lapop1share",
             "Low-income population > 1 mile from supermarket (total)" = "lalowi1",
             "Low-income population > 1 mile from supermarket (percentage)" = "lalowi1share",
             "Children age 0-17 > 1 mile from supermarket (total)"  = "lakids1",
             "Children age 0-17 > 1 mile from supermarket (percentage)" = "lakids1share",
             "Seniors age 65+ > 1 mile from supermarket (total)" = "laseniors1",
             "Seniors age 65+ > 1 mile from supermarket (percentage)" = "laseniors1share",
             "White population > 1 mile from supermarket (total)" = "lawhite1",
             "White population > 1 mile from supermarket (percentage)" = "lawhite1share",
             "Black population > 1 mile from supermarket (total)" = "lablack1",
             "Black population > 1 mile from supermarket (percentage)" = "lablack1share",
             "Asian population > 1 mile from supermarket (total)" = "laasian1",
             "Asian population > 1 mile from supermarket (percentage)" = "laasian1share",
             "Native Hawaiian or Other Pacific Islander population > 1 mile from supermarket (total)" = "lanhopi1",
             "Native Hawaiian or Other Pacific Islander population > 1 mile from supermarket (percentage)" = "lanhopi1share",
             "American Indian or Alaska Native population > 1 mile from supermarket (total)" = "laaian1",
             "American Indian or Alaska Native population > 1 mile from supermarket (percentage)" = "laaian1share",
             "Other/Multiple race population > 1 mile from supermarket (total)" = "laomultir1",
             "Other/Multiple race population > 1 mile from supermarket (percentage)" = "laomultir1share",
             "Hispanic or Latino population > 1 mile from supermarket (total)" = "lahisp1",
             "Hispanic or Latino population > 1 mile from supermarket (percentage)" = "lahisp1share",
             "Housing units without a vehicle > 1 mile from supermarket (total)" = "lahunv1",
             "Housing units without a vehicle > 1 mile from supermarket (percentage)" = "lahunv1share",
             "Housing units receiving SNAP > 1 mile from supermarket (total)" = "lasnap1",
             "Housing units receiving SNAP > 1 mile from supermarket (percentage)" = "lasnap1share")

# define filters
health_outcomes <- df_vars %>% 
  dplyr::select(c(22:33)) 

health_behaviors <- df_vars %>% 
  dplyr::select(c(18:21)) 

health_prevention <- df_vars %>% 
  dplyr::select(c(11:17)) 

natural_env <- df_vars %>%
  dplyr::select(c(107:108, 131:134, 161)) 

hazard_env <- df_vars %>% 
  dplyr::select(141:158)

air_pol <- df_vars %>% 
  dplyr::select(c(2, 135:139)) 

built_env <- df_vars %>%
  dplyr::select(c(3:4, 109:110,  159)) 

noise_env <- df_vars %>% 
  dplyr::select(112:123)

landuse_env <- df_vars %>% 
  dplyr::select(165:180)

sociodemo <- df_vars %>% 
  dplyr::select(c(34)) 

sex <- df_vars %>%
  dplyr::select(c(65:68))

race <- df_vars %>%
  dplyr::select(c(35:64))

age <- df_vars %>%
  dplyr::select(c(69:104))

social_env <- df_vars %>% 
  dplyr::select(c(5:10, 124:130, 105:106, 111, 140, 160, 162, 163, 164)) 

food_env <- food %>% 
  dplyr::select(c(11:37)) 
food_env_inp <- food_env %>% 
  st_drop_geometry()

#### DEFINE MARKDOWN FOR TIPS ####
health_out_md <- list(
  "Arthritis.among.Adults" = "- **Arthritis**: What are <a href='https://www.cdc.gov/arthritis/basics/index.html' target='_blank' rel='noopener noreferrer'>risk factors for arthritis</a>? What are ways to <a href='https://www.arthritis.org/treatments' target='_blank' rel='noopener noreferrer'>treat arthritis</a> and <a href='https://www.cdc.gov/arthritis/prevention/index.html' target='_blank' rel='noopener noreferrer'>manage arthritis</a>?",
  "Asthma.among.Adults" = "- **Asthma**: What are <a href='https://www.lung.org/lung-health-diseases/lung-disease-lookup/asthma/learn-about-asthma/what-causes-asthma' target='_blank' rel='noopener noreferrer'>risk factors for asthma</a>? What are ways to <a href='https://www.nhlbi.nih.gov/health/asthma/treatment-action-plan#How-is-asthma-treated?' target='_blank' rel='noopener noreferrer'>treat asthma</a> and <a href='https://www.cdc.gov/asthma/control/index.html' target='_blank' rel='noopener noreferrer'>manage asthma</a>?",
  "High.Blood.Pressure.among.Adults" = "- **High blood pressure**: What are <a href='' target='_blank' rel='noopener noreferrer'>risk factors for high blood pressure</a>? What are ways to <a href='https://www.nhlbi.nih.gov/health/high-blood-pressure/treatment' target='_blank' rel='noopener noreferrer'>treat high blood pressure</a> and <a href='https://www.heart.org/en/health-topics/high-blood-pressure/changes-you-can-make-to-manage-high-blood-pressure' target='_blank' rel='noopener noreferrer'>control high blood pressure</a>?",
  "Cancer.or.Melanoma.among.Adults" = "- **Cancer**: What are <a href='https://www.cancer.org/cancer/risk-prevention/understanding-cancer-risk.html' target='_blank' rel='noopener noreferrer'>risk factors for cancer</a>? What are ways to <a href='https://www.cdc.gov/cancer/prevention/index.html' target='_blank' rel='noopener noreferrer'>prevent cancer</a>? What are ways to <a href='https://www.cancer.org/cancer/managing-cancer.html' target='_blank' rel='noopener noreferrer'>treat cancer</a> and <a href='https://www.cancer.org/cancer/survivorship.html' target='_blank' rel='noopener noreferrer'>live well after cancer treatment</a>? What are <a href='https://www.cookforyourlife.org/' target='_blank' rel='noopener noreferrer'>healthy recipes and nutrition resources</a> for people affected by cancer? How can you visit the Fred Hutch Cancer Center Survivorship Clinic to get a <a href='https://www.fredhutch.org/en/patient-care/services/survivorship/survivorship-clinic.html' target='_blank' rel='noopener noreferrer'>Survivorship Care Plan</a>?",
  "High.Cholesterol.among.Screened.Adults" = "- **High cholesterol**: What are <a href='https://www.cdc.gov/cholesterol/risk-factors/index.html' target='_blank' rel='noopener noreferrer'>risk factors for high cholesterol</a>? What are ways to <a href='https://www.heart.org/en/health-topics/cholesterol/prevention-and-treatment-of-high-cholesterol-hyperlipidemia' target='_blank' rel='noopener noreferrer'>treat high cholesterol</a> and <a href='https://www.heart.org/en/healthy-living/healthy-lifestyle/lifes-essential-8/how-to-control-cholesterol-fact-sheet' target='_blank' rel='noopener noreferrer'>manage high cholesterol</a>?",
  "COPD.among.Adults" = "- **Chronic obstructive pulmonary disease (COPD)**: What are <a href='https://www.nhlbi.nih.gov/health/copd/causes' target='_blank' rel='noopener noreferrer'>risk factors for COPD</a>? What are ways to <a href='https://www.lung.org/lung-health-diseases/lung-disease-lookup/copd/treating' target='_blank' rel='noopener noreferrer'>treat COPD</a> and <a href='https://www.lung.org/lung-health-diseases/lung-disease-lookup/copd/living-with-copd' target='_blank' rel='noopener noreferrer'>manage COPD</a>?",
  "Coronary.Heart.Disease.among.Adults" = "- **Heart disease**: What are <a href='https://www.nhlbi.nih.gov/health/coronary-heart-disease/risk-factors' target='_blank' rel='noopener noreferrer'>risk factors for heart disease</a>? What are ways to <a href='https://www.nhlbi.nih.gov/health/coronary-heart-disease/treatment' target='_blank' rel='noopener noreferrer'>treat heart disease</a> and <a href='https://www.nhlbi.nih.gov/health/coronary-heart-disease/living-with' target='_blank' rel='noopener noreferrer'>manage heart disease</a>?",
  "Depression.among.Adults" = "- **Depression**: What are <a href='https://www.psychiatry.org/patients-families/depression/what-is-depression#section_0' target='_blank' rel='noopener noreferrer'>risk factors for depression</a>? What are ways to <a href='https://www.cdc.gov/tobacco/campaign/tips/diseases/depression-anxiety.html#treatments' target='_blank' rel='noopener noreferrer'>treat depression</a> and <a href='https://adaa.org/understanding-anxiety/depression/tips' target='_blank' rel='noopener noreferrer'>manage depression</a>?",
  "Diagnosed.Diabetes.among.Adults" = "- **Diabetes**: What are <a href='https://www.cdc.gov/diabetes/risk-factors/index.html' target='_blank' rel='noopener noreferrer'>risk factors for diabetes</a>? What are ways to <a href='https://diabetes.org/living-with-diabetes/treatment-care' target='_blank' rel='noopener noreferrer'>treat diabetes</a> and <a href='https://www.cdc.gov/diabetes/living-with/index.html' target='_blank' rel='noopener noreferrer'>manage diabetes</a>?",
  "Obesity.among.Adults" = "- **Obesity**: What are <a href='https://www.cdc.gov/obesity/risk-factors/risk-factors.html' target='_blank' rel='noopener noreferrer'>risk factors for obesity</a>? What are ways to <a href='https://diabetes.org/obesity' target='_blank' rel='noopener noreferrer'>treat obesity</a> and <a href='https://www.cdc.gov/diabetes/living-with/index.html' target='_blank' rel='noopener noreferrer'>manage your weight</a>?",
  "All.Teeth.Lost.among.Adults.65.and.Older" = "- **Tooth loss**: What are <a href='https://www.cdc.gov/oral-health/about/about-tooth-loss.html#cdc_disease_basics_population-who-is-at-risk' target='_blank' rel='noopener noreferrer'>risk factors for tooth loss</a>? What are ways to <a href='https://www.cdc.gov/oral-health/about/about-tooth-loss.html#cdc_disease_basics_treatment-treatment-and-recovery' target='_blank' rel='noopener noreferrer'>treat tooth loss</a> and <a href='https://www.cdc.gov/oral-health/prevention/oral-health-tips-for-adults.html' target='_blank' rel='noopener noreferrer'>manage oral health</a>?",
  "Stroke.among.Adults" = "- **Stroke**: What are <a href='https://www.cdc.gov/stroke/risk-factors/index.html' target='_blank' rel='noopener noreferrer'>risk factors for stroke</a>? What are ways to <a href='https://www.cdc.gov/stroke/treatment/index.html' target='_blank' rel='noopener noreferrer'>treat stroke</a> and <a href='https://www.stroke.org/en/life-after-stroke' target='_blank' rel='noopener noreferrer'>rehab after experiencing a stroke</a>?"
)

health_bh_md <- list(
  "Binge.Drinking.among.Adults" = "- **Binge drinking**: What are ways that can help with <a href='https://www.cdc.gov/drink-less-be-your-best/getting-started-with-drinking-less/index.html' target='_blank' rel='noopener noreferrer'>starting to drink less</a>?",
  "Cigarette.Smoking.among.Adults" = "- **Cigarette smoking**: What are ways to <a href='https://www.cdc.gov/tobacco/campaign/tips/quit-smoking/index.html' target='_blank' rel='noopener noreferrer'>quit smoking</a>? How do I download <a href='https://quitbot.net/' target='_blank' rel='noopener noreferrer'>QuitBot</a>, a free smartphone app to help quit smoking?",
  "No.Leisure.time.Physical.Activity.among.Adults" = "- **No leisure-time physical activity**: What are ways to help with <a href='https://www.cdc.gov/healthy-weight-growth/physical-activity/getting-started.html' target='_blank' rel='noopener noreferrer'>starting to exercise</a>?",
  "Short.Sleep.Duration" = "- **Short sleep duration**: What are ways to help with <a href='https://www.cdc.gov/sleep/about/index.html' target='_blank' rel='noopener noreferrer'>getting better sleep</a>?"
)

health_prev_md <- list(
  "Lack.of.Health.Insurance" = "- **Lack of health insurance**: How can I <a href='https://www.wahealthplanfinder.org/us/en/my-account/my-coverage/learnapplehealth.html' target='_blank' rel='noopener noreferrer'>apply for Apple Health</a>, which is the name for free or low-cost Medicaid health insurance in Washington state?",
  "Routine.Checkup.in.the.Past.Year" = "- **Routine checkup**: What are ways to <a href='https://www.cdc.gov/chronic-disease/prevention/preventive-care.html' target='_blank' rel='noopener noreferrer'>stay up to date on your preventive care</a>?",
  "Visited.Dentist.in.Past.Year" = "- **Dental care**: What are ways to <a href='https://www.cdc.gov/oral-health/prevention/oral-health-tips-for-adults.html' target='_blank' rel='noopener noreferrer'>maintain dental health</a>?",
  "Taking.Medicine.to.Control.High.Blood.Pressure" = "- **High blood pressure medication use**: What are ways to <a href='https://www.heart.org/en/health-topics/high-blood-pressure/changes-you-can-make-to-manage-high-blood-pressure/managing-high-blood-pressure-medications' target='_blank' rel='noopener noreferrer'>better manage taking blood pressure medication</a>?",
  "Cholesterol.Screening" = "- **Cholesterol screening**: What are ways to <a href='https://www.cdc.gov/cholesterol/testing/index.html' target='_blank' rel='noopener noreferrer'>test for cholesterol</a>?",
  "Mammography.Use.among.Women.50.to.74" = "- **Mammography**: What are ways to get a <a href='https://www.komen.org/breast-cancer/screening/' target='_blank' rel='noopener noreferrer'>mammogram</a>, which helps to screen for breast cancer? How can I schedule a mammogram on the <a href='https://www.fredhutch.org/en/patient-care/prevention/breast-cancer-screening/mammogram-van.html' target='_blank' rel='noopener noreferrer'>Fred Hutch Mammography Van</a>?",
  "Colorectal.Cancer.Screening.among.Adults.45.to.75" = "- **Colorectal cancer screening**: What are ways to get <a href='https://www.fredhutch.org/en/research/institutes-networks-ircs/population-health-colorectal-cancer-screening-program/resources.html' target='_blank' rel='noopener noreferrer'>screened for colorectal cancer</a>? How can you use <a href='https://mygenerisk-colon.fredhutch.org/' target='_blank' rel='noopener noreferrer'>MyGeneRisk</a>, a free tool to learn about your risk of developing colorectal cancer?"
)

airpol_md_list <- list(
  "Particulate.Matter.2.5" = "- **Particulate matter <2.5 microns in diameter (PM<sub>2.5</sub>)**: What is <a href='https://www.stateofglobalair.org/pollution-sources/pm25' target='_blank' rel='noopener noreferrer'>PM<sub>2.5</sub></a>? What are ways to <a href='http://www.breatheasy.tips/' target='_blank' rel='noopener noreferrer'>protect yourself from air pollution</a>? How can you use the <a href='https://www.breatheasy.tips/#aqi' target='_blank' rel='noopener noreferrer'>Air Quality Index (AQI)</a>, a free tool to help plan your outdoor activities and learn about unhealthy air pollution levels?",
  "Nitrogen.dioxide" = "- **Nitrogen dioxide (NO<sub>2</sub>)**: What is <a href='https://www.lung.org/clean-air/outdoors/what-makes-air-unhealthy/nitrogen-dioxide' target='_blank' rel='noopener noreferrer'>NO<sub>2</sub></a>? What are ways to <a href='http://www.breatheasy.tips/' target='_blank' rel='noopener noreferrer'>protect yourself from air pollution</a>? How can you use the <a href='https://www.breatheasy.tips/#aqi' target='_blank' rel='noopener noreferrer'>Air Quality Index (AQI)</a>, a free tool to help plan your outdoor activities and learn about unhealthy air pollution levels?",
  "Ozone" = "- **Ozone (O<sub>3</sub>)**: What is <a href='https://www.lung.org/clean-air/outdoors/what-makes-air-unhealthy/ozone' target='_blank' rel='noopener noreferrer'>O<sub>3</sub></a>? What are ways to <a href='http://www.breatheasy.tips/' target='_blank' rel='noopener noreferrer'>protect yourself from air pollution</a>? How can you use the <a href='https://www.breatheasy.tips/#aqi' target='_blank' rel='noopener noreferrer'>Air Quality Index (AQI)</a>, a free tool to help plan your outdoor activities and learn about unhealthy air pollution levels?",
  "Carbon.monoxide" = "- **Carbon monoxide (CO)**: What is <a href='https://www.lung.org/clean-air/indoor-air/indoor-air-pollutants/carbon-monoxide' target='_blank' rel='noopener noreferrer'>CO</a>? What are ways to <a href='http://www.breatheasy.tips/' target='_blank' rel='noopener noreferrer'>protect yourself from air pollution</a>? How can you use the <a href='https://www.breatheasy.tips/#aqi' target='_blank' rel='noopener noreferrer'>Air Quality Index (AQI)</a>, a free tool to help plan your outdoor activities and learn about unhealthy air pollution levels?",
  "Sulfur.dioxide" = "- **Sulfur dioxide (SO<sub>2</sub>)**: What is <a href='https://www.lung.org/clean-air/outdoors/what-makes-air-unhealthy/sulfur-dioxide' target='_blank' rel='noopener noreferrer'>SO<sub>2</sub></a>? What are ways to <a href='http://www.breatheasy.tips/' target='_blank' rel='noopener noreferrer'>protect yourself from air pollution</a>? How can you use the <a href='https://www.breatheasy.tips/#aqi' target='_blank' rel='noopener noreferrer'>Air Quality Index (AQI)</a>, a free tool to help plan your outdoor activities and learn about unhealthy air pollution levels?",
  "Wildfire.smoke" = "- **Wildfire smoke**: What is <a href='https://ecology.wa.gov/air-climate/air-quality/smoke-fire/wildfire-smoke' target='_blank' rel='noopener noreferrer'>wildfire smoke</a>? What are ways to <a href='http://www.breatheasy.tips/' target='_blank' rel='noopener noreferrer'>protect yourself from air pollution</a>? How can you use the <a href='https://www.breatheasy.tips/#aqi' target='_blank' rel='noopener noreferrer'>Air Quality Index (AQI)</a>, a free tool to help plan your outdoor activities and learn about unhealthy air pollution levels? How can you learn about the current <a href='https://airqualitymap.ecology.wa.gov/?view=forecast' target='_blank' rel='noopener noreferrer'>wildfire smoke forecast</a> in your area (select <u>**Smoke Forecast**</u> from the View menu)? How can you <a href='https://doh.wa.gov/emergencies/be-prepared-be-safe/severe-weather-and-natural-disasters/wildfires' target='_blank' rel='noopener noreferrer'>prepare for wildfires</a>?"
)

# all natural disaster events have same key--use to avoid redundancy and repeated same tip
nat_dis_key <- list(nat_disaster = "- **Extreme weather events and natural disasters**: What can you do before, during, and after an <a href=https://doh.wa.gov/emergencies/be-prepared-be-safe/severe-weather-and-natural-disasters target='_blank' rel='noopener noreferrer'>extreme weather event or natural disaster</a>?",
                    temperature = "- **Temperature**: What are ways to help with <a href=https://www.cdc.gov/heat-health/about/index.html?CDC_AA_refVal=https%3A%2F%2Fwww.cdc.gov%2Fextreme-heat%2Fabout%2Findex.html target='_blank' rel='noopener noreferrer'>heat waves</a>? How can you find <a href=https://search.wa211.org/search?location=&query=TH-2600.1900-180&query_type=taxonomy&query_label=Extreme+Heat+Cooling+Centers target='_blank' rel='noopener noreferrer'>extreme heat cooling centers</a>[]() in your area? What are ways to help with [cold spells](https://www.cdc.gov/winter-weather/safety/stay-safe-during-after-a-winter-storm-safety.html)?")

nat_md_list <- list(
  "UV.Index" = "- **Ultraviolet radiation (UV)**: What is <a href=https://www.cdc.gov/radiation-health/data-research/facts-stats/ultraviolet-radiation.html target='_blank' rel='noopener noreferrer'>UV</a>? What are ways to help with [sun safety](https://www.cdc.gov/skin-cancer/sun-safety/index.html)?",
  "Maximum.temperature" = "temperature",
  "Minimum.temperature" = "temperature",
  "Average.temperature" = "temperature",
  "Precipitation" = "- **Precipitation**: What to do before, during, and after a <a href=https://doh.wa.gov/emergencies/be-prepared-be-safe/severe-weather-and-natural-disasters/floods target='_blank' rel='noopener noreferrer'>flood</a> []()?",
  "Radon" = "- **Radon**: What is <a href=https://doh.wa.gov/community-and-environment/contaminants/radon target='_blank' rel='noopener noreferrer'>radon and ways to test for radon in your home</a> []()?",
  "PFAS_dw" = "- **Per- and polyfluoroalkyl substances (PFAS) in drinking water**: What are <a href=https://doh.wa.gov/community-and-environment/contaminants/pfas target='_blank' rel='noopener noreferrer'>PFAS and ways to reduce your exposure to PFAS</a> []()?"
)

nat_dis_list <- list(
  "Avalanche.Risk.Score" = "nat_disaster", # use key mappings
  "Coastal.Flooding.Risk.Score" = "nat_disaster",
  "Cold.Wave.Risk.Score" = "nat_disaster",
  "Drought.Risk.Score" = "nat_disaster",
  "Earthquake.Risk.Score" = "nat_disaster",
  "Hail.Risk.Score" = "nat_disaster",
  "Heat.Wave.Risk.Score" = "nat_disaster",
  "Hurricane.Risk.Score" = "nat_disaster",
  "Ice.Storm.Risk.Score" = "nat_disaster",
  "Landslide.Risk.Score" = "nat_disaster",
  "Lightning.Risk.Score" = "nat_disaster",
  "Riverine.Flooding.Risk.Score" = "nat_disaster",
  "Strong.Wind.Risk.Score" = "nat_disaster",
  "Tornado.Risk.Score" = "nat_disaster",
  "Tsunami.Risk.Score" = "nat_disaster",
  "Volcanic.Activity.Risk.Score" = "nat_disaster",
  "Wildfire.Risk.Score" = "nat_disaster",
  "Winter.Weather.Risk.Score" = "nat_disaster")

built_noise_key <- list(noise = "- **Noise**: What is <a href=https://www.who.int/tools/compendium-on-health-and-environment/environmental-noise target='_blank' rel='noopener noreferrer'>noise</a> []() that comes from the environment? What are <a href=https://doh.wa.gov/community-and-environment/noise target='_blank' rel='noopener noreferrer'>health effects of noise</a>[]()?",
                        land = "- **Land use and land cover**: What is <a href=https://oceanservice.noaa.gov/facts/lclu.html target='_blank' rel='noopener noreferrer'>land use and land cover</a> []()?")

built_md_list <- list(
  "Walkability" = "- **Neighborhood walkability**: What is <a href='https://usafacts.org/articles/what-is-walkability-what-does-the-government-spend-on-it/' target='_blank' rel='noopener noreferrer'>walkability</a>? What are <a href='https://www.heart.org/en/healthy-living/fitness/walking/why-is-walking-the-most-popular-form-of-exercise' target='_blank' rel='noopener noreferrer'>health benefits of walking</a>?",
  "Pesticide.Exposure" = "- **Agricultural pesticide use**: What are <a href='https://doh.wa.gov/community-and-environment/contaminants/pesticides' target='_blank' rel='noopener noreferrer'>pesticides</a>? What are ways to reduce pesticide exposure from <a href='https://www.epa.gov/safepestcontrol/pesticides-and-food-healthy-sensible-food-practices' target='_blank' rel='noopener noreferrer'>foods</a> and during <a href='https://icash.public-health.uiowa.edu/wp-content/uploads/2017/02/UO218.pdf' target='_blank' rel='noopener noreferrer'>usage</a>?",
  "Green.Space" = "- **Green space**: What is <a href='https://www.countyhealthrankings.org/strategies-and-solutions/what-works-for-health/strategies/green-space-parks' target='_blank' rel='noopener noreferrer'>green space</a>? What are <a href='https://www.countyhealthrankings.org/strategies-and-solutions/what-works-for-health/strategies/green-space-parks' target='_blank' rel='noopener noreferrer'>health benefits of green space</a>?",
  "bluespace" = "- **Blue space**: <a href='https://pubmed.ncbi.nlm.nih.gov/32971082/' target='_blank' rel='noopener noreferrer'>Blue space</a> is any water body such as ponds, lakes, rivers, and oceans. What are <a href='https://www.apa.org/monitor/2020/04/nurtured-nature' target='_blank' rel='noopener noreferrer'>health benefits of blue space</a>?",
  "Nighttime.Radiance" = "- **Outdoor light at night**: What is <a href='' target='_blank' rel='noopener noreferrer'>outdoor light at night</a>, which is also known as light pollution? What are <a href='https://journalofethics.ama-assn.org/article/were-all-healthier-under-starry-sky/2024-10#:~:text=Blue%20wavelengths%20of%20light%20are,to%20many%20kinds%20of%20illness.' target='_blank' rel='noopener noreferrer'>health effects of outdoor light at night</a>?"
)


noise_md_list <- list(
  "N.Noise.More.than.LAeq.45.to.50.db" = "noise",
  "N.Noise.More.than.LAeq.50.to.60.db" = "noise",
  "N.Noise.More.than.LAeq.60.to.70.db" = "noise",
  "N.Noise.More.than.LAeq.70.to.80.db" = "noise",
  "N.Noise.More.than.LAeq.80.to.90.db" = "noise",
  "N.Noise.More.than.LAeq.90.db" = "noise",
  "Pct.Noise.More.than.LAeq.45.to.50.db" = "noise",
  "Pct.Noise.More.than.LAeq.50.to.60.db" = "noise",
  "Pct.Noise.More.than.LAeq.60.to.70.db" = "noise",
  "Pct.Noise.More.than.LAeq.70.to.80.db" = "noise",
  "Pct.Noise.More.than.LAeq.80.to.90.db" = "noise",
  "Pct.Noise.More.than.LAeq.90.db" = "noise")

land_md_list <- list(
  "pct_Open_Water" = "land",
  "pct_Developed_Open" = "land",
  "pct_Developed_Low" = "land",
  "pct_Developed_Medium" = "land",
  "pct_Developed_High" = "land",
  "pct_Barren" = "land",
  "pct_Evergreen_Forest" = "land",
  "pct_Shrub" = "land",
  "pct_Grassland" = "land",
  "pct_Pasture" = "land",
  "pct_Crops" = "land",
  "pct_Woody_Wetlands" = "land",
  "pct_Crops" = "land",
  "pct_Woody_Wetlands" = "land",
  "pct_Herbaceous_Wetlands" = "land",
  "pct_Deciduous_Forest" = "land",
  "pct_Mixed_Forest" = "land",
  "pct_Perennial_Ice" = "land"
)

food_env_md <- markdown("
                        - **Food environment/healthy food**: How can you find <a href=https://www.usdalocalfoodportal.com/ target='_blank' rel='noopener noreferrer'>local healthy foods</a> in your area such as farmers markets?
                        ")

crime_md <- markdown("
                     - **Crime**: Where can you learn more information about <a href=https://nibrs.fbi.gov/2024/ target='_blank' rel='noopener noreferrer'>crimes in Washington state</a> from the Federal Bureau of Investigation?
                     ")

soc_md_list <- list(
  "Food.Insecurity" = "- **Food insecurity**: Call 2-1-1 or text '211WAOD' to 898211 for nearby food banks and free meals from the <a href='https://search.wa211.org/' target='_blank' rel='noopener noreferrer'>Washington helpline</a>. Call 1-866-HUNGRY for food assistance programs from the <a href='https://www.hungerfreeamerica.org/en-us/national-hunger-hotline' target='_blank' rel='noopener noreferrer'>National Hunger Hotline</a>. Find the closest food bank or meal program from <a href='https://feedingwashington.org/find-food/' target='_blank' rel='noopener noreferrer'>Feeding Washington</a>. Find other available resources from <a href='https://www.washingtonconnection.org/home/exploreoptions.go' target='_blank' rel='noopener noreferrer'>Washington Connection</a>.",
  "Housing.Insecurity" = "- **Housing insecurity**: Call 2-1-1 or text '211WAOD' to 898211 for housing resources from the <a href='https://search.wa211.org/' target='_blank' rel='noopener noreferrer'>Washington helpline</a>. Find other available resources, including emergency housing, from the <a href='https://www.dshs.wa.gov/esa/community-services-offices/housing-resources' target='_blank' rel='noopener noreferrer'>Washington State Department of Social and Health Services</a> and <a href='https://www.washingtonconnection.org/home/exploreoptions.go' target='_blank' rel='noopener noreferrer'>Washington Connection</a>.",
  "Utility.Services.Threat" = "- **Utility services threat**: Call 2-1-1 or text '211WAOD' to 898211 for help with utilities from the <a href='https://search.wa211.org/' target='_blank' rel='noopener noreferrer'>Washington helpline</a>. Find other available resources, including energy assistance programs, from the <a href='https://www.utc.wa.gov/consumers/energy/energy-assistance-programs' target='_blank' rel='noopener noreferrer'>Washington Utilities and Transportation Commission</a> and <a href='https://www.washingtonconnection.org/home/exploreoptions.go' target='_blank' rel='noopener noreferrer'>Washington Connection</a>.",
  "Lacking.Reliable.Transportation" = "- **Lack of reliable transportation**: Call 2-1-1 or text '211WAOD' to 898211 for help with transportation from the <a href='https://search.wa211.org/' target='_blank' rel='noopener noreferrer'>Washington helpline</a>. Find other available resources from <a href='https://www.washingtonconnection.org/home/exploreoptions.go' target='_blank' rel='noopener noreferrer'>Washington Connection</a>.",
  "Lack.of.Social.and.Emotional.Support" = "- **Lack of social and emotional support**: Call 2-1-1 or text '211WAOD' to 898211 for counseling and mental health resources from the <a href=https://search.wa211.org/ target='_blank' rel='noopener noreferrer'>Washington Helpline</a>. Find other available resources from <a href=https://www.washingtonconnection.org/home/exploreoptions.go target='_blank' rel='noopener noreferrer'>Washington Connection</a>.",
  "No.broadband.internet" = "- **No internet**: Call 2-1-1 or text '211WAOD' to 898211 for help with getting internet from the <a href='https://search.wa211.org/' target='_blank' rel='noopener noreferrer'>Washington helpline</a>. Find other available resources from <a href='https://www.washingtonconnection.org/home/exploreoptions.go' target='_blank' rel='noopener noreferrer'>Washington Connection</a>.",
  "Crowding" = "- **Household crowding**: Call 2-1-1 or text '211WAOD' to 898211 for housing resources from the <a href='https://search.wa211.org/' target='_blank' rel='noopener noreferrer'>Washington helpline</a>. Find other available resources, including emergency housing, from the <a href='https://www.dshs.wa.gov/esa/community-services-offices/housing-resources' target='_blank' rel='noopener noreferrer'>Washington State Department of Social and Health Services</a> and <a href='https://www.washingtonconnection.org/home/exploreoptions.go' target='_blank' rel='noopener noreferrer'>Washington Connection</a>.",
  "Housing.cost.burden" = "- **Housing cost burden**: Call 2-1-1 or text '211WAOD' to 898211 for housing resources from the <a href='https://search.wa211.org/' target='_blank' rel='noopener noreferrer'>Washington helpline</a>. Find other available resources, including emergency housing, from the <a href='https://www.dshs.wa.gov/esa/community-services-offices/housing-resources' target='_blank' rel='noopener noreferrer'>Washington State Department of Social and Health Services</a> and <a href='https://www.washingtonconnection.org/home/exploreoptions.go' target='_blank' rel='noopener noreferrer'>Washington Connection</a>.",
  "No.high.school.diploma" = "- **No high school education**: Call 2-1-1 or text '211WAOD' to 898211 for education resources from the <a href='https://search.wa211.org/' target='_blank' rel='noopener noreferrer'>Washington helpline</a>. Find other available resources from <a href='https://www.washingtonconnection.org/home/exploreoptions.go' target='_blank' rel='noopener noreferrer'>Washington Connection</a>.",
  "Poverty" = "- **People living below 150% of the poverty level**: 150% of the poverty level: Call 2-1-1 or text '211WAOD' to 898211 for financial resources from the <a href='https://search.wa211.org/' target='_blank' rel='noopener noreferrer'>Washington helpline</a>. Find other available resources from <a href='https://www.washingtonconnection.org/home/exploreoptions.go' target='_blank' rel='noopener noreferrer'>Washington Connection</a>.",
  "Single.parent.households" = "- Single parent households: Find resources for families, including childcare, from the <a href='https://dcyf.wa.gov/services/housing-basic-needs' target='_blank' rel='noopener noreferrer'>Washington State Department of Children, Youth, and Families</a> and <a href='https://www.washingtonconnection.org/home/exploreoptions.go' target='_blank' rel='noopener noreferrer'>Washington Connection</a>.",
  "Unemployment" = "- **Unemployment**: Call 2-1-1 or text '211WAOD' to 898211 for employment resources from the <a href='https://search.wa211.org/' target='_blank' rel='noopener noreferrer'>Washington helpline</a>. Find other available resources from <a href='https://www.washingtonconnection.org/home/exploreoptions.go' target='_blank' rel='noopener noreferrer'>Washington Connection</a>.",
  "Environmental.Justice.Index" = "- **Environmental Justice Index (EJI)*: What is the <a href='https://www.atsdr.cdc.gov/place-health/php/eji/eji-frequently-asked-questions-faqs.html' target='_blank' rel='noopener noreferrer'>Environmental Justice Index</a>?",
  "Social.Vulnerability.Index" = "- **Social Vulnerability Index (SVI)**: What is the <a href='https://www.atsdr.cdc.gov/place-health/php/svi/svi-frequently-asked-questions-faqs.html' target='_blank' rel='noopener noreferrer'>Social Vulnerability Index</a>?",
  "Median.HH.Income" = "- **Median household income**: What does a <a href='https://usafacts.org/answers/what-is-the-income-of-a-us-household/country/united-states/' target='_blank' rel='noopener noreferrer'>median household income</a> mean?",
  "HT_Index" = "- **Housing and Transportation (H + T\\U00AE) Affordability Index**: Why is it important to consider <a href='https://cnt.org/tools/housing-and-transportation-affordability-index' target='_blank' rel='noopener noreferrer'>transportation costs with affordability</a>?",
  "Racial.Residential.Segregation" = "- **Residential segregation**: What is the <a href='https://www.khanacademy.org/test-prep/mcat/social-inequality/social-class/v/residential-segregation' target='_blank' rel='noopener noreferrer'>Dissimilarity Index</a>, which is a measure of residential segregation?",
  "Historic.Redlining.Score" = "- **Redlining**: What is <a href='https://education.nationalgeographic.org/resource/mapmaker-redlining-united-states/' target='_blank' rel='noopener noreferrer'>redlining</a>?",
  "social_capital" = "- **Social capital**: What is <a href='https://aspe.hhs.gov/sites/default/files/private/pdf/263491/What-is-social-capital.pdf' target='_blank' rel='noopener noreferrer'>social capital</a>?",
  "Population.density" = "- **Urbanicity/rurality**: What are resources to improve health and healthcare in <a href='https://doh.wa.gov/public-health-provider-resources/rural-health' target='_blank' rel='noopener noreferrer'>rural communities</a>?"
)

# -------- UI ELEMENTS --------
categories <- accordion(
  id = "main_acc",
  open = FALSE,
  accordion_panel(
    title = HTML("<b>Sociodemographics</b>"), icon = uiOutput("sociodemo_icon"), 
    selectInput('sociodemo', "Select variables",
                sociodemographics,
                selectize = TRUE, multiple = TRUE),
    accordion_panel("Race and Ethnicity", icon = uiOutput("race_icon"), selectInput('race', NULL, racev, selectize = TRUE, multiple = TRUE)),
    accordion_panel("Sex", icon = uiOutput("sex_icon"), selectInput('sex', NULL, sexv, selectize = TRUE, multiple = TRUE)),
    accordion_panel("Age", icon = uiOutput("age_icon"), selectInput('age', NULL, agev, selectize = TRUE, multiple = TRUE))
    
  ),
  accordion_panel(id = "acc_outcomes",
    HTML("<b>Health Outcomes</b>"), icon = uiOutput("outcomes_icon"),
    selectInput('outcomes', 
                htmltools::span("Select variables",
                     popover(htmltools::span(id = "help_icon", bs_icon("lightbulb")),
                             "Select one or more health outcomes to see tips.",
                             title = "Tips",
                             placement = "right",
                             id = "outcome_popover")), outcomes, selectize = TRUE, multiple = TRUE),
    # options to filter by cancer site, stage at diagnosis, gender
    accordion_panel("Cancer Incidence", icon = uiOutput("inc_icon"),
                    selectInput('incsite', "Cancer Site", choices = c("Please choose a site" = "", unique(wscr.inc$Cancer.Site)), selectize = TRUE, selected = ""),
                    selectInput('incstage', "Stage at Diagnosis", choices = c("Please choose a stage" = "", unique(wscr.inc$Stage.At.Diagnosis)), selectize = TRUE, selected = ""),
                    selectInput('incsex', "Sex", choices = c("Please choose a sex" = "", unique(wscr.inc$Gender)), selectize = TRUE, selected = ""),
                    actionButton('incbutton', "Reset filters")),
    # options to filter by cancer site, gender
    accordion_panel("Cancer Mortality", icon = uiOutput("mort_icon"),
                    selectInput('mortsite', "Cancer Site", choices = c("Please choose a site" = "", unique(wscr.mort$Cancer.Site)), selectize = TRUE, selected = ""),
                    selectInput('mortsex', "Sex", choices = c("Please choose a sex" = "", unique(wscr.mort$Gender)), selectize = TRUE, selected = ""),
                    actionButton('mortbutton', "Reset filters"))
  ),
  accordion_panel(
    HTML("<b>Health Behaviors</b>"), icon = uiOutput("behaviors_icon"),
    selectInput('behaviors', htmltools::span("Select variables", 
                                  popover(bs_icon("lightbulb"),
                                          "Select one or more health behaviors to see tips.",
                                          title = "Tips",
                                          placement = "right",
                                          id = "behavior_popover")), behaviors,
                 multiple = TRUE, selectize = TRUE, selected = "")
  ),
  accordion_panel(
    HTML("<b>Prevention</b>"), icon = uiOutput("prevention_icon"),
    selectInput('prevention', htmltools::span("Select variables", 
                                      popover(bs_icon("lightbulb"),
                                              "Select one or more prevention measures to see tips.",
                                              title = "Tips",
                                              placement = "right",
                                              id = "prevention_popover")), prevention, selectize = TRUE, multiple = TRUE)
  ),
  accordion_panel(id = "access-panel",
    HTML("<b>Healthcare Access</b>"), icon = uiOutput("access_icon"),
    h6("Select features", htmltools::span(popover(bs_icon("lightbulb"),
            "Switch on one or more healthcare access features to see tips.",
            title = "Tips",
            placement = "right",
            id = "healthaccpopover"))),
    input_switch('cancer', "Commission on Cancer (CoC)-accredited programs ", value = FALSE),
    input_switch('clinics', "Clinics", value = FALSE), 
    input_switch('ems', "Emergency Medical Services (EMS) stations", value = FALSE),
    input_switch('hospitals', "Hospitals", value = FALSE),
    input_switch('pharmacies', "Pharmacies", value = FALSE),
    input_switch('wic_clinics', "Nutrition Program for Women, Infants, and Children (WIC) clinics", value = FALSE),
    input_switch('wic_retailers', "WIC retailers", value = FALSE),
    input_switch('fqhc', "Federally Qualified Health Centers (FQHCs)", value = FALSE)
  ),
  accordion_panel(
    HTML("<b>Natural Environment</b>"), icon = uiOutput("natural_icon"),
    selectInput('naturalenv', htmltools::span("Select variables", 
                                              popover(bs_icon("lightbulb"),
                                                      "Select one or more natural environment measures to see tips.",
                                                      title = "Tips",
                                                      placement = "right",
                                                      id = "natenvpopover")), naturalenv, selectize = TRUE, multiple = TRUE),
    input_switch('microplastics', "Microplastics", value = FALSE),
    div(id = 'micro_div', selectInput('micro', '', choices = c("Please choose a marine setting" = "", unique(microplastics$Marine.Setting)), selectize = TRUE, multiple = TRUE)),
    accordion_panel("Air Pollutants", icon = uiOutput("airpol_icon"),
                   selectInput('airpol', htmltools::span("Select variables",
                                                         popover(bs_icon("lightbulb"),
                                                                 "Select one or more air pollutants to see tips.",
                                                                 title = "Tips",
                                                                 placement = "right",
                                                                 id = "airpopover")), airpol, selectize = TRUE, multiple = TRUE)),
    accordion_panel("Natural Hazard Risk", icon = uiOutput("hazard_icon"),
                    selectInput('hazards', label = htmltools::span("Select variables", 
                                                                 popover(bs_icon("lightbulb"),
                                                                         "Select one or more natural hazard risk measures to see tips.",
                                                                         title = "Tips",
                                                                         placement = "right",
                                                                         id = "hazardpopover")), hazardenv, selectize = TRUE, multiple = TRUE))
  ),
  accordion_panel(
    HTML("<b>Built Environment</b>"), icon = uiOutput("built_icon"),
    selectInput('builtenv', htmltools::span("Select variables", 
                                 popover(bs_icon("lightbulb"),
                                         "Select one or more built environment measures to see tips.",
                                         title = "Tips",
                                         placement = "right",
                                         id = "builtenvpopover")), builtenv, selectize = TRUE, multiple = TRUE),
    input_switch('transit', "Transit stops", value = FALSE),
    input_switch('alc', "Alcohol retailers", value = FALSE),
    input_switch('parks', "Parks", value = FALSE),
    input_switch('superfund', "Superfund sites", value = FALSE),
    accordion_panel("Transportation Noise", icon = uiOutput("noise_icon"),#uiOutput("noise_icon"),
                    selectInput('noise', label = htmltools::span("Select variables", 
                                                                 popover(bs_icon("lightbulb"),
                                                                         "Select one or more transportation noise measures to see tips.",
                                                                         title = "Tips",
                                                                         placement = "right",
                                                                         id = "noisepopover")), noiseenv, selectize = TRUE, multiple = TRUE)),
    accordion_panel("Land Use/Land Cover", icon = uiOutput("land_icon"),
                    selectInput('land', label = htmltools::span("Select variables", 
                                                                 popover(bs_icon("lightbulb"),
                                                                         "Select one or more land use measures to see tips.",
                                                                         title = "Tips",
                                                                         placement = "right",
                                                                         id = "landpopover")), landuseenv, selectize = TRUE, multiple = TRUE)),
    accordion_panel(
      "Food Environment", icon = uiOutput("food_icon"),
      selectInput('foodenv', label = htmltools::span("Select variables", 
                                 popover(bs_icon("lightbulb"),
                                         food_env_md,
                                         title = "Tips",
                                         placement = "right")), foodenv, selectize = TRUE, multiple = TRUE)
    )
  ),
  accordion_panel(
    HTML("<b>Social Environment</b>"), icon = uiOutput("social_icon"),
    selectInput('socialenv', htmltools::span("Select variables", 
                                          popover(bs_icon("lightbulb"),
                                                  "Select one or more social environment measures to see tips.",
                                                  title = "Tips",
                                                  placement = "right",
                                                  id = "socenvpopover")), socialenv, selectize = TRUE,  multiple = TRUE),
    accordion_panel('Crime', icon = uiOutput("crime_icon"),
                    selectInput('crime', htmltools::span("Select variables",
                                                         popover(bs_icon("lightbulb"),
                                                                 crime_md,
                                                                 title = "Tips",
                                                                 placement = "right")), crimeenv, selectize = TRUE, multiple = TRUE))
  ),
  accordion_panel(id = "acc_options",
    HTML("<b>Options</b>"), icon = bs_icon("gear"),
    input_switch("showbounds", "Show tract boundaries", value = TRUE),
    input_switch("showcounties", "Show county boundaries", value = FALSE),
    input_switch("showcities", "Show city boundaries", value = FALSE),
    input_switch("showchart", "Show graph", value = FALSE),
    fileInput("upload", htmltools::span("Upload shapefile (.shp, .shx, .dbf)", tooltip(bs_icon("question-circle"), "Help", title = "Upload all components of your shapefile. .shp, .shx, and .dbf are needed to display your uploaded shapefile.")), 
              multiple = TRUE, accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj", ".cpg"))
  ),
)

# define table categories
table.cats <- accordion(
  open = FALSE,
  id = "table_cats",
  accordion_panel(
    title = HTML("<b>Sociodemographics</b>"), icon = bs_icon("person-vcard", class = "text-secondary"),
    selectInput('sociodemo_tab', "Select variables",
                sociodemographics,
                selectize = TRUE, multiple = TRUE),
    accordion_panel("Race and Ethnicity", selectInput('race_tab', NULL, racev, selectize = TRUE, multiple = TRUE)),
    accordion_panel("Sex", selectInput('sex_tab', NULL, sexv, selectize = TRUE, multiple = TRUE)),
    accordion_panel("Age", selectInput('age_tab', NULL, agev, selectize = TRUE, multiple = TRUE))
    
  ),
  accordion_panel(
    HTML("<b>Health Outcomes</b>"), icon = bs_icon("heart-pulse", class = "text-secondary"),
    selectInput('outcomes_tab', "Select variables", outcomes, selectize = TRUE, multiple = TRUE),
    # options to filter by cancer site, stage at diagnosis, gender
    accordion_panel("Cancer Incidence",
                    selectInput('incsite_tab', "Cancer Site", choices = c("Please choose a site" = "", unique(wscr.inc$Cancer.Site)), selectize = TRUE, selected = ""),
                    selectInput('incstage_tab', "Stage at Diagnosis", choices = c("Please choose a stage" = "", unique(wscr.inc$Stage.At.Diagnosis)), selectize = TRUE, selected = ""),
                    selectInput('incsex_tab', "Sex", choices = c("Please choose a sex" = "", unique(wscr.inc$Gender)), selectize = TRUE, selected = ""),
                    actionButton('incbutton_tab', "Reset filters")),
    # options to filter by cancer site, gender
    accordion_panel("Cancer Mortality",
                    selectInput('mortsite_tab', "Cancer Site", choices = c("Please choose a site" = "", unique(wscr.mort$Cancer.Site)), selectize = TRUE, selected = ""),
                    selectInput('mortsex_tab', "Sex", choices = c("Please choose a sex" = "", unique(wscr.mort$Gender)), selectize = TRUE, selected = ""),
                    actionButton('mortbutton_tab', "Reset filters"))
  ),
  accordion_panel(
    HTML("<b>Health Behaviors</b>"), icon = bs_icon("person-walking", class = "text-secondary"),
    selectInput('behaviors_tab', "Select variables", behaviors,
                multiple = TRUE, selectize = TRUE, selected = "")
  ),
  accordion_panel(
    HTML("<b>Prevention</b>"), icon = tags$img(src = "/prevention.png", height = "16px", width = "16px"),
    selectInput('prevention_tab', "Select variables", prevention, selectize = TRUE, multiple = TRUE)
  ),
  accordion_panel(
    HTML("<b>Natural Environment</b>"), icon = bs_icon("sun", class = "text-secondary"),
    selectInput('naturalenv_tab', "Select variables", naturalenv, selectize = TRUE, multiple = TRUE),
    accordion_panel("Air Pollutants", icon = bs_icon("cloud-haze", class = "text-secondary"),
                    selectInput('airpol_tab', "Select variables", airpol, selectize = TRUE, multiple = TRUE)),
    accordion_panel("Natural Hazard Risk", icon = bs_icon("tornado", class = "text-secondary"),
                    selectInput('hazards_tab', "Select variables", hazardenv, selectize = TRUE, multiple = TRUE))
  ),
  accordion_panel(
    HTML("<b>Built Environment</b>"), icon = bs_icon("buildings", class = "text-secondary"),
    selectInput('builtenv_tab', "Select variables", builtenv, selectize = TRUE, multiple = TRUE),
    accordion_panel("Transportation Noise", icon = bs_icon("volume-up", class = "text-secondary"),
                    selectInput('noise_tab', "Select variables", noiseenv, selectize = TRUE, multiple = TRUE)),
    accordion_panel("Land Use/Land Cover", icon = bs_icon("water", class = "text-secondary"),
                    selectInput('land_tab', "Select variables", landuseenv, selectize = TRUE, multiple = TRUE)),
    accordion_panel(
      "Food Environment", icon = bs_icon("basket", class = "text-secondary"),
      selectInput('foodenv_tab', "Select variables", foodenv, selectize = TRUE, multiple = TRUE)
    )
  ),
  accordion_panel(
    HTML("<b>Social Environment</b>"), icon = tags$img(src = "/social-environment.png", height = "16px", width = "16px"),
    selectInput('socialenv_tab', "Select variables", socialenv, selectize = TRUE,  multiple = TRUE),
    accordion_panel('Crime', icon = bs_icon("file-earmark-lock", class = "text-secondary"),
                    selectInput('crime_tab', "Select variables", crimeenv, selectize = TRUE, multiple = TRUE))
  )
)

standalone_tab <- c("Choose dataset" = "",
                    "Commission on Cancer (CoC)-Accredited Programs" = "cancer.progs",
                    "Clinics" = "clinics",
                    "Emergency Medical Stations" = "ems",
                    "Federally Qualified Health Centers" = "fqhc",
                    "Hospitals" = "hospitals",
                    "Pharmacies" = "pharmacies",
                    "WIC clinics" = "wic.clinics",
                    "WIC retailers" = "wic.retailers",
                    "Microplastics" = "microplastics",
                    "Transit stops" = "transit",
                    "Alcohol retailers" = "alcohol.retailers",
                    "Parks" = "parks",
                    "Superfund sites" = "superfund") 

# -------- UI LAYOUT --------
ui <- page_navbar(
  introjsUI(),
  shinyjs::useShinyjs(),
  tags$head( # define style and scripts
    tags$style(HTML(" 
    .leaflet-control.my-centered-num-legend {
      background: white;
      border: 1px solid #ccc;
      border-radius: 4px;
      padding: 4px 6px;
    }
    
    /* center the SVG bar inside the legend box */
    .leaflet-control.my-centered-num-legend svg {
      display: block;
      margin-left: auto;
      margin-right: auto;
    }

    /* keep label centering without touching fill colors */
    .leaflet-control.my-centered-num-legend text {
      text-anchor: middle;
      fill: #333;              /* text color */
    }
    
    #clear-btn {
      color: #fff;
      background-color: #ff0000;
    }

    #help {
    background-color: #00C1D5;
    transition: 0.3s ease;
    }
    
    #help:hover {
    background-color: #0A799A;
    } 
    
    #help2 {
    background-color: #00C1D5;
    transition: 0.3s ease;
    }
    
    #help2:hover {
    background-color: #0A799A;
    }
    
  ")),
    tags$link(rel = "stylesheet", 
              href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"),
    tags$script(
    HTML("
    document.addEventListener('shown.bs.modal', function(){}); // no-op to ensure BS is loaded

    document.addEventListener('DOMContentLoaded', function () {
      var tooltipTriggerList = [].slice.call(
        document.querySelectorAll('[data-bs-toggle=\"tooltip\"]')
      );
      tooltipTriggerList.forEach(function (tooltipTriggerEl) {
        new bootstrap.Tooltip(tooltipTriggerEl);
      });
    });

    // re-initialize when Leaflet redraws legends:
    document.addEventListener('DOMNodeInserted', function(e) {
      if (e.target && e.target.classList &&
          e.target.classList.contains('leaflet-control')) {
        var tooltipTriggerList = [].slice.call(
          e.target.querySelectorAll('[data-bs-toggle=\"tooltip\"]')
        );
        tooltipTriggerList.forEach(function (tooltipTriggerEl) {
          if (!bootstrap.Tooltip.getInstance(tooltipTriggerEl)) {
            new bootstrap.Tooltip(tooltipTriggerEl);
          }
        });
      }
    });
    
    // have links open in new tab
    window.addEventListener('DOMContentLoaded', function() {
    var links = document.links;
    for (var i = 0; i < links.length; i++) {
      if (links[i].hostname != window.location.hostname) {
        links[i].target = '_blank';
      }
    }
  });
  ")),
    includeHTML("google-analytics.html")
  ),
  id = "app_nav",
  # TODO: make logo clickable
  title = tags$img(src = "/geoexmap_edit.png", height = '57.62px', width = '165.08px'),
  selected = "map",
  nav_spacer(),
  nav_panel("About", fluidPage(
    HTML(
      "<h5>Thank you for visiting the Geospatial Exposome Map (geo<b>ex</b>map).</h5><br>
      <h6>geo<b>ex</b>map was developed by the Fred Hutch Cancer Center <a href='https://www.geoexlab.com/'>Geospatial Exposome Lab</a> as an open-source geospatial web app to support community outreach and engagement work in reducing cancer burden within the state of Washington, the catchment area for the Fred Hutch Cancer Center.</h6><br>
      <h6><b>Key highlights:</b></h6><br>
      <ul>
        <li>Through integrating <b>geospatial science</b> (location-based technologies) with the <b>exposome</b> (the totality of environmental exposures that we experience throughout our lives) (<a href='https://aacrjournals.org/cebp/article/33/4/451/742073/Geospatial-Science-for-the-Environmental'>reference</a>), geo<b>ex</b>map enables the visualization of numerous neighborhood-level health and environmental data to better characterize and understand the <b>Washington State catchment area population, health disparities, and underserved communities.</b></li>
        <li>We proudly implemented a <b>community-engaged approach</b> to developing geo<b>ex</b>map, incorporating valuable input from community members and organizations through <a href='https://www.fredhutch.org/en/research/institutes-networks-ircs/ocoe/about-ocoe/collaborate-with-us.html'>Fred Hutch Cancer Center Community Coalitions</a>.</li>
        <li>geo<b>ex</b>map contains information from the latest available datasets (2019-present) at the census tract (or neighborhood) and county levels for factors in the domains of sociodemographics, prevention, healthcare access, and the exposome (natural environment, built environment, and social environment).</li>
        <li>geo<b>ex</b>map includes <b>user-centered functionalities</b> for mapping, customizable graphs and tables, data importing and exporting, overlaying multiple spatial data layers, neighborhood search, and documentation.</li>
        <li>To promote <b>disease prevention and control</b> efforts, health and prevention tips <i class='bi bi-lightbulb' style = 'color:black;'></i> are provided with links to actionable, practical, evidence-based strategies for exposure mitigation of modifiable risk factors (e.g., access to free radon test kits) as well as resources for health and well-being.</li>
        <li>geo<b>ex</b>map was designed with <b>reproducible</b> and <b>scalable</b> methods that can be adopted by other cancer centers and institutions, created using open-source software (R Shiny, leaflet, Docker) and publicly available geospatial data. See our <a href='https://github.com/FredHutch/geoexmap'>GitHub repository</a>.</li>
      </ul>
      <br>
      <h6> Feel free to contact us with any questions, suggestions, and feedback at <a href='mailto:geoexmap@fredhutch.org'>geoexmap@fredhutch.org</a>.</h6><br>
      <h6>geo<b>ex</b>map is supported by funding from the Dillon Family Foundation. </h6>
      <br>
      "
    )
  )),
  nav_panel("Map", value = "map",
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
                  # TODO: add explanation for graph 
                  wellPanel(actionButton("clear", icon = icon("x"), class = "btn-danger", label = NULL),
                            plotlyOutput("chart"))
                ))
            )),
  nav_panel("Table", value = "tab",
            layout_sidebar(
              sidebar = sidebar(table.cats, 
                                width = "400px"),
              accordion(open = FALSE, id = "table_accs",
                accordion(id = "table_subacc1",
                          accordion_panel("Census Tract Data", 
                                accordion_panel("2020 Census Tracts", reactableOutput("ct_table"), br(), downloadButton('downloadcttab', "Download .csv"), actionButton('clear2020', "Clear variables", class = "btn-danger")),
                                accordion_panel("2010 Census Tracts (Food Environment)", reactableOutput("food_table"), br(), downloadButton('downloadfoodtab', "Download .csv"), actionButton('clear2010', "Clear variables", class = "btn-danger")))),
                accordion_panel("County Data", 
                                accordion_panel("Crime", reactableOutput("cnty_crime_table"), br(), downloadButton('downloadcntycrime', "Download .csv"), actionButton('clearcrime', "Clear variables", class = "btn-danger")),
                                accordion_panel("Cancer Incidence", reactableOutput("cnty_inc_table"), br(), downloadButton('downloadcntyinc', "Download .csv"), actionButton('clearinc', "Clear variables", class = "btn-danger")),
                                accordion_panel("Cancer Mortality", reactableOutput("cnty_mort_table"), br(), downloadButton('downloadcntymort', "Download .csv"), actionButton('clearmort', "Clear variables", class = "btn-danger"))),
                accordion_panel("Standalone Data", selectInput('standalone', "", choices = standalone_tab), br(), reactableOutput("standalone_table"), downloadButton('downloadstandalone', "Download .csv"), actionButton('clearstandalone', "Clear variables", class = "btn-danger"))
              )

            )),
  nav_panel("Documentation",
            h2("Version History"),
            h2("Technical Documentation"),
            a("Download", target = "_blank", href = "geoexmap_technical_doc.pdf")),
  nav_panel("Contact us",
            h3("Contact us"),
            p("Connect with us regarding any questions, suggestions, and feedback at ", a("geoexmap@fredhutch.org", href = "mailto:geoexmap@fredhutch.org"), "or reach out directly to Dr. Trang VoPham (", a("trang@fredhutch.org", href = "mailto:trang@fredhutch.org", .noWS = "outside"), ").")),
  window_title = "geoexmap | Geospatial Exposome Map at Fred Hutch Cancer Center"
  
)

# -------- SERVER --------
server <- function(input, output, session) {
  showModal(modalDialog(
    title = HTML("Welcome to geo<b>ex</b>map"),
    HTML("This geospatial web app allows you to map and download neighborhood-level health and environmental data across Washington state."),
    footer = tagList(modalButton("Dismiss"), actionButton("help", label = "Show tour"))
  ))
  
  #### APP TUTORIAL ####
  steps <- reactive({
    data.frame(
      element = c("#geoexmap", "#geoexmap", "#main_acc", "#help_icon", "#access-panel", "#acc_options"),
      # TODO: add neighborhood search
      # TODO: add point locations
      intro = c("This is the main map. It displays up to three layers of neighborhood- and county-level data at a time.",
                "Hover over the <i class='bi bi-info-circle-fill'></i> icon in the legend to see details for each data layer such as data sources.",
                #"Click on the *magnifying glass* icon to search for an address.",
                "Use these categories to choose which variables or features appear on the map.",
                "Click on this icon to learn about health and prevention tips related to the variables that you selected.",
                "For variables that are point locations (rather than census tracts or counties), you can use the toggle buttons to add as many layers as you would like.",
                "In Options, you can choose to show different boundaries, show graphs of the selected variables, and upload a shapefile."),
      position = c("right", "right", "right", "right", "right", "right"),
      stringsAsFactors = FALSE)
  })
  
  # app tutorial
  observeEvent(input$help, {
    removeModal()
    accordion_panel_open(id = "main_acc", values = c("<b>Health Outcomes</b>", "<b>Healthcare Access</b>", "<b>Options</b>"))
    updateSelectInput(session, 'outcomes', selected = "Cancer.or.Melanoma.among.Adults")
    introjs(session,
            options = list(
              steps = steps(),
              showBullets = TRUE,
              nextLabel = "Next",
              prevLabel = "Back",
              skipLabel = "Skip tour",
              scrollToElement = TRUE,
              scrollPadding = 0))
  })
  
  tab_steps <- reactive({
    data.frame(
      element = c("#table_cats", "#table_accs", "#downloadcttab"),
      intro = c("Use these categories to choose which variables or features to view tables for.",
                "View tables for selected data here.",
                "Use this button to download .csv files for selected data."),
      position = c("right", "right", "right"),
      stringsAsFactors = FALSE)
  })
  
  observeEvent(input$app_nav, {
    if(input$app_nav == "tab") {
      showModal(modalDialog(
        title = "Tables and Data Download",
        HTML("On this page you can view and download data in geo<b>ex</b>map."),
        footer = tagList(modalButton("Dismiss"), actionButton("help2", label = "Show tour"))
      ))
      
      observeEvent(input$help2, {
        removeModal()
        accordion_panel_open(id = "table_cats", values = "<b>Health Outcomes</b>")
        accordion_panel_open(id = "table_accs", values = c("Census Tract Data"))
        accordion_panel_open(id = "table_subacc1", values = c("2020 Census Tracts"))
        
        introjs(session, options = list(
          steps = tab_steps(),
          showBullets = TRUE,
          nextLabel = "Next",
          prevLabel = "Back",
          skipLabel = "Skip tour",
          scrollToElement = TRUE,
          scrollPadding = 0))
      })
      
    }
      
  })
  
  # turn switch off if clear button is clicked
  observeEvent(input$clear, {
    update_switch("showchart", value = FALSE)
  })
  
  #### ICONS ####
  output$sociodemo_icon <- renderUI({
    # if all values are null or empty, use original icon or no icon
    if ((is.null(input$sociodemo) || identical(input$sociodemo, "")) && (is.null(input$race) || identical(input$race, "")) && (is.null(input$age) || identical(input$age, "")) && (is.null(input$sex) || identical(input$sex, ""))) {
      bs_icon("person-vcard", class = "text-secondary", title = "No selection yet")
    } else {
      bs_icon("check-circle", class = "text-success", title = "Variables selected")
    }
  })
  
  output$race_icon <- renderUI({
    if (is.null(input$race) || identical(input$race, "")) {
      NULL
    } else {
      bs_icon("check-circle", class = "text-success", title = "Variables selected")
    }
  })
  
  output$sex_icon <- renderUI({
    if (is.null(input$sex) || identical(input$sex, "")) {
      NULL
    } else {
      bs_icon("check-circle", class = "text-success", title = "Variables selected")
    }
  })
  
  output$age_icon <- renderUI({
    if (is.null(input$age) || identical(input$age, "")) {
      NULL
    } else {
      bs_icon("check-circle", class = "text-success", title = "Variables selected")
    }
  })

  output$outcomes_icon <- renderUI({
    if ((is.null(input$outcomes) || identical(input$outcomes, "")) && (!inc.ready()) && (!mort.ready())) { # if outcomes, cancer incidence and mortality not ready
      bs_icon("heart-pulse", class = "text-secondary", title = "No selection yet")
    } else {
      bs_icon("check-circle", class = "text-success", title = "Variables selected")
    }
  })
  
  output$inc_icon <- renderUI({
    if (!inc.ready()) {
      NULL
    } else {
      bs_icon("check-circle", class = "text-success", title = "Variables selected")
    }
  })
  
  output$mort_icon <- renderUI({
    if (!mort.ready()) {
      NULL
    } else {
      bs_icon("check-circle", class = "text-success", title = "Variables selected")
    }
  })
  
  output$behaviors_icon <- renderUI({
    if (is.null(input$behaviors) || identical(input$behaviors, "")) {
      bs_icon("person-walking", class = "text-secondary", title = "No selection yet")
    } else {
      bs_icon("check-circle", class = "text-success", title = "Variables selected")
    }
  })
  
  output$prevention_icon <- renderUI({
    if (is.null(input$prevention) || identical(input$prevention, "")) {
      tags$img(src = "/prevention.png", height = "16px", width = "16px", title = "No selection yet")
    } else {
      bs_icon("check-circle", class = "text-success", title = "Variables selected")
    }
    
  })
  
  output$access_icon <- renderUI({
    if(!input$cancer && !input$clinics && !input$ems && !input$hospitals && !input$pharmacies && !input$wic_clinics && !input$wic_retailers && !input$fqhc) {
      bs_icon("building-add", class = "text-secondary", title = "No selection yet")
    } else {
      bs_icon("check-circle", class = "text-success", title = "Variables selected")
    }
  })
  
  output$natural_icon <- renderUI({
    if ((is.null(input$naturalenv) || identical(input$naturalenv, "")) && (is.null(input$airpol) || identical(input$airpol, "")) && (is.null(input$hazards) || identical(input$hazards, "")) && !input$microplastics) {
      bs_icon("sun", class = "text-secondary", title = "No selection yet")
    } else {
      bs_icon("check-circle", class = "text-success", title = "Variables selected")
    }
  })
  
  output$airpol_icon <- renderUI({
    if (is.null(input$airpol) || identical(input$airpol, "")) {
      bs_icon("cloud-haze", class = "text-secondary", title = "No selection yet")
    } else {
      bs_icon("check-circle", class = "text-success", title = "Variables selected")
    }
  })
  
  output$built_icon <- renderUI({
    if ((is.null(input$builtenv) || identical(input$builtenv, "")) && (is.null(input$foodenv) || identical(input$foodenv, "")) && (is.null(input$land) || identical(input$land, "")) && (is.null(input$noise) || identical(input$noise, "")) && !input$transit && !input$parks && !input$superfund && !input$alc) {
      bs_icon("buildings", class = "text-secondary", title = "No selection yet")
    } else {
      bs_icon("check-circle", class = "text-success", title = "Variables selected")
    }
  })
  
  output$food_icon <- renderUI({
    if (is.null(input$foodenv) || identical(input$foodenv, "")) {
      bs_icon("basket", class = "text-secondary", title = "No selection yet")
    } else {
      bs_icon("check-circle", class = "text-success", title = "Variables selected")
    }
  })
  
  output$social_icon <- renderUI({
    if ((is.null(input$socialenv) || identical(input$socialenv, "")) && (is.null(input$crime) || identical(input$crime, ""))) {
      tags$img(src = "/social-environment.png", height = "16px", width = "16px", title = "No selection yet")
    } else {
      bs_icon("check-circle", class = "text-success", title = "Variables selected")
    }
  })
  
  output$crime_icon <- renderUI({
    if (is.null(input$crime) || identical(input$crime, "")) {
      bs_icon("file-earmark-lock", class = "text-secondary", title = "No selection yet")
    } else {
      bs_icon("check-circle", class = "text-success", title = "Variables selected")
    }
  })
  
  output$noise_icon <- renderUI({
    if (is.null(input$noise) || identical(input$noise, "")) {
      bs_icon("volume-up", class = "text-secondary", title = "No selection yet")
    } else {
      bs_icon("check-circle", class = "text-success", title = "Variables selected")
    }
  })
  
  output$land_icon <- renderUI({
    if (is.null(input$land) || identical(input$land, "")) {
      bs_icon("water", class = "text-secondary", title = "No selection yet")
    } else {
      bs_icon("check-circle", class = "text-success", title = "Variables selected")
    }
  })
  
  output$hazard_icon <- renderUI({
    if (is.null(input$hazards) || identical(input$hazards, "")) {
      bs_icon("tornado", class = "text-secondary", title = "No selection yet")
    } else {
      bs_icon("check-circle", class = "text-success", title = "Variables selected")
    }
  })
  
  #### DYNAMIC TIP LOGIC ####
  # TODO: open webpages in new tab
  outcomes_md <- reactive({
    if (is.null(input$outcomes) || length(input$outcomes) == 0) {
      return("Select one or more health outcomes to see tips.")
    }
    
    has_tip <- input$outcomes %in% names(health_out_md) # create vector for inputs that have corresponding tips
    sel_with_tip <- input$outcomes[has_tip] # subset
    
    if (length(sel_with_tip) == 0) {
      return("No tips available for selected outcomes.")
    }
    
    paste(unlist(health_out_md[input$outcomes]), collapse = "\n")
  })
  
  behaviors_md <- reactive({
    if (is.null(input$behaviors) || length(input$behaviors) == 0) {
      return("Select one or more health outcomes to see tips.")
    }
    
    has_tip <- input$behaviors %in% names(health_bh_md) # create vector for inputs that have corresponding tips
    sel_with_tip <- input$behaviors[has_tip] # subset
    
    if (length(sel_with_tip) == 0) {
      return("No tips available for selected behaviors.")
    }
    
    paste(unlist(health_bh_md[input$behaviors]), collapse = "\n")
  })
  
  prev_md <- reactive({
    if (is.null(input$prevention) || length(input$prevention) == 0) {
      return("Select one or more prevention measures to see tips.")
    }
    
    has_tip <- input$prevention %in% names(health_prev_md) # create vector for inputs that have corresponding tips
    sel_with_tip <- input$prevention[has_tip] # subset
    
    if (length(sel_with_tip) == 0) {
      return("No tips available for selected prevention measures.")
    }
    
    paste(unlist(health_prev_md[input$prevention]), collapse = "\n")
  })
  
  airpol_md <- reactive({
    if (is.null(input$airpol) || length(input$airpol) == 0) {
      return("Select one or more air pollutants to see tips.")
    }
    
    has_tip <- input$airpol %in% names(airpol_md_list) # create vector for inputs that have corresponding tips
    sel_with_tip <- input$airpol[has_tip] # subset
    
    if (length(sel_with_tip) == 0) {
      return("No tips available for selected air pollutants.")
    }
    
    paste(unlist(airpol_md_list[input$airpol]), collapse = "\n")
  })
  
  noise_md <- reactive({
    if(is.null(input$noise) || length(input$noise) == 0) {
      return("Select one or more transportation noise measures to see tips.")
    }
    
    raw <- noise_md_list[input$noise]
    print(raw)
    
    is_key <- raw %in% names(built_noise_key) # split into already markdown vs keys

    dir_text <- unlist(raw[!is_key], use.names = FALSE) # direct markdown

    key_vals <- unname(unlist(raw[is_key], use.names = FALSE))
    shared_text <- unlist(built_noise_key[key_vals], use.names = FALSE)
    
    all_text <- c(dir_text, shared_text)
    all_text <- unique(all_text) # prevent repeated texts
    
    if (length(all_text) == 0) {
      return("No tips available for selected transportation noise measures.")
    }
    
    paste(all_text, collapse = "\n")
  })
  
  land_md <- reactive({
    if(is.null(input$land) || length(input$land) == 0) {
      return("Select one or more transportation noise measures to see tips.")
    }
    
    raw <- land_md_list[input$land]
    print(raw)
    
    is_key <- raw %in% names(built_noise_key) # split into already markdown vs keys
    
    dir_text <- unlist(raw[!is_key], use.names = FALSE) # direct markdown
    
    key_vals <- unname(unlist(raw[is_key], use.names = FALSE))
    shared_text <- unlist(built_noise_key[key_vals], use.names = FALSE)
    
    all_text <- c(dir_text, shared_text)
    all_text <- unique(all_text) # prevent repeated texts
    
    if (length(all_text) == 0) {
      return("No tips available for selected transportation noise measures.")
    }
    
    paste(all_text, collapse = "\n")
  })
  
  built_md <- reactive({
    
    if (is.null(input$builtenv) || length(input$builtenv) == 0) { # condition for if all switches are false
      return("Select one or more built environment measures to see tips.")
    }
    
    raw <- built_md_list[input$builtenv]
    
    is_key <- raw %in% names(built_noise_key) # split into already markdown vs keys
    
    dir_text <- unlist(raw[!is_key], use.names = FALSE) # direct markdown
    
    key_vals <- unname(unlist(raw[is_key], use.names = FALSE))
    shared_text <- unlist(built_noise_key[key_vals], use.names = FALSE)
    
    all_text <- c(dir_text, shared_text)
    all_text <- unique(all_text) # prevent repeated texts
    
    if (length(all_text) == 0) {
      return("No tips available for selected built environment measures.")
    }
    
    paste(all_text, collapse = "\n")
  })
  
  # get switch markdowns
  built_switch_md <- reactive({
    switch_md <- character(0)
    if (input$transit) {
      switch_md <- c(switch_md, "- **Transit access**: How can you find [transit access](https://watransitaccessmap.org/) in your area?")
    } 
    
    if (input$superfund) {
      switch_md <- c(switch_md, "- **Superfund sites**: What are [Superfund sites](https://education.nationalgeographic.org/resource/superfund/)?")
    } 
    
    if (input$parks) {
      switch_md <- c(switch_md, "- **Parks**: How can you find [parks](https://parks.wa.gov/) and [trails](https://doh.wa.gov/you-and-your-family/nutrition-and-physical-activity/active-living/resources/trails) in your area?")
      
    } 
    
    if (input$alc) {
      switch_md <- c(switch_md, "- **Alcohol retailers**: Higher density of [alcohol retailers](https://pubmed.ncbi.nlm.nih.gov/31264024/) has been associated with greater neighborhood disadvantage, alcohol misuse, and other issues. What are [health effects of alcohol](https://www.cdc.gov/drink-less-be-your-best/facts-about-excessive-drinking/index.html)?")
    }
    
    if (length(switch_md) == 0) {return(NULL)}
    
    paste(switch_md, collapse = "\n")
  })
  
  built_popover_md <- reactive({
    base <- built_md()
    switches <- built_switch_md()
    
    if (!is.null(switches) && grepl("^Select one or more", base)) { # drop default text when any switch is turned on
      return(switches)
    }
    
    txts <- c(built_md(), built_switch_md())
    txts <- txts[!vapply(txts, is.null, logical(1))]
    paste(unique(txts), collapse = "\n")
  })
  
  hazard_md <- reactive({
    if (is.null(input$hazards) || length(input$hazards) == 0) {
      return("Select one or more natural hazard risk measures to see tips.")
    }
    
    raw <- nat_dis_list[input$hazards]
    
    is_key <- raw %in% names(nat_dis_key) # split into already markdown vs keys
    
    dir_text <- unlist(raw[!is_key], use.names = FALSE) # direct markdown
    
    key_vals <- unname(unlist(raw[is_key], use.names = FALSE))
    shared_text <- unlist(nat_dis_key[key_vals], use.names = FALSE)
    
    all_text <- c(dir_text, shared_text)
    all_text <- unique(all_text) # prevent repeated texts
    
    if (length(all_text) == 0) {
      return("No tips available for selected natural hazard risk measures.")
    }
    
    paste(all_text, collapse = "\n")
  })
  
  nat_md <- reactive({
    if((is.null(input$naturalenv) || length(input$naturalenv) == 0) && isFALSE(input$microplastics)) {
      return("Select one or more natural environment measures to see tips.")
    }
    
    raw <- nat_md_list[input$naturalenv]
    
    is_key <- raw %in% names(nat_dis_key) # split into already markdown vs keys
    
    dir_text <- unlist(raw[!is_key], use.names = FALSE) # direct markdown
    
    key_vals <- unname(unlist(raw[is_key], use.names = FALSE))
    shared_text <- unlist(nat_dis_key[key_vals], use.names = FALSE)
    
    all_text <- c(dir_text, shared_text)
    all_text <- unique(all_text) # prevent repeated texts
    
    if (length(all_text) == 0 && isFALSE(input$microplastics)) {
      return("No tips available for selected natural environment measures.")
    }
    
    paste(all_text, collapse = "\n")
  })
  
  nat_switch_md <- reactive({
    if (input$microplastics) {
      "- **Microplastics**: What are [microplastics](https://www.unep.org/news-and-stories/story/everything-you-should-know-about-microplastics)?"
    } else {
      NULL
    }
  })
  
  soc_md <- reactive({
    if (is.null(input$socialenv) || length(input$socialenv) == 0) {
      return("Select one or more health outcomes to see tips.")
    }
    
    has_tip <- input$socialenv %in% names(soc_md_list) # create vector for inputs that have corresponding tips
    sel_with_tip <- input$socialenv[has_tip] # subset
    
    if (length(sel_with_tip) == 0) {
      return("No tips available for selected outcomes.")
    }
    
    paste(unlist(soc_md_list[input$socialenv]), collapse = "\n")
  })
  
  nat_popover_md <- reactive({
    txts <- c(nat_md(), nat_switch_md())
    txts <- txts[!vapply(txts, is.null, logical(1))]
    paste(unique(txts), collapse = "\n")
  })
  
  # get switch markdowns
  h_access_switch_md <- reactive({
    switch_md <- character(0)
    if (input$clinics) {
      switch_md <- c(switch_md, "- **Clinics**: How can you find [free and low cost clinics](https://www.wahealthcareaccessalliance.org/search-for-clinics)?")
    } 
    
    if (input$ems) {
      switch_md <- c(switch_md, "- **Emergency Medical Services (EMS) stations**: Where can you find [EMS stations and trauma care](https://wadoh.maps.arcgis.com/apps/instant/basic/index.html?appid=c7e3f2249bb34175a20849c2d02fc06a) in your area?")
    } 
    
    if (input$hospitals) {
      switch_md <- c(switch_md, "- **Hospitals**: Where can you find [hospitals](https://geo.wa.gov/datasets/WADOH::hospitals/explore) in your area?")
      
    } 
    
    if (input$pharmacies) {
      switch_md <- c(switch_md, "- **Pharmacies**: Where can you find [pharmacy services](https://www.wsparx.org/page/PharMap) in your area?")
    }
    
    if (input$wic_clinics) {
      switch_md <- c(switch_md, "- **Women, Infants, and Children (WIC) Clinics**: What is the [WIC Nutrition Program](https://doh.wa.gov/you-and-your-family/wic)?")
    }
    
    if (input$wic_retailers) {
      switch_md <- c(switch_md, "- **Women, Infants, and Children (WIC) Retailers**: What is the [WIC Nutrition Program](https://doh.wa.gov/you-and-your-family/wic)?")
    }
    
    if (input$cancer) {
      switch_md <- c(switch_md, "- **Commission on Cancer (CoC)-accredited programs**: How can you find [programs accredited by the CoC](https://www.facs.org/find-a-hospital/?nearMe=off&companyType=CoC)?")
    }
    
    if (input$fqhc) {
      switch_md <- c(switch_md, "- **Federally Qualified Health Centers (FQHCs)**: What are [FQHCs](https://www.wacommunityhealth.org/community-health-centers-1)?")
    }
    
    if (length(switch_md) == 0) {return("Select one or more healthcare access features to see tips.")}
    
    paste(switch_md, collapse = "\n")
  })
  
  observeEvent(input$outcomes, {
    update_popover(
      "outcome_popover",
      content = markdown(outcomes_md())
    )
  })
  
  observeEvent(input$behaviors, {
    update_popover(
      "behavior_popover",
      content = markdown(behaviors_md())
    )
  })
  
  observeEvent(input$prevention, {
    update_popover(
      "prevention_popover",
      content = markdown(prev_md())
    )
  })
  
  observeEvent(input$airpol, {
    update_popover(
      "airpopover",
      content = markdown(airpol_md())
    )
  })
  
  observeEvent(input$noise, {
    update_popover(
      "noisepopover",
      content = markdown(noise_md())
    )
  })
  
  observeEvent(input$land, {
    update_popover(
      "landpopover",
      content = markdown(land_md())
    )
  })
  
  observeEvent(list(input$naturalenv, input$microplastics), {
    update_popover(
      "natenvpopover",
      content = markdown(nat_popover_md())
    )
  })
  
  observeEvent(input$hazards, {
    update_popover(
      "hazardpopover",
      content = markdown(hazard_md())
    )
  })
  
  observeEvent(list(input$builtenv, input$transit, input$superfund, input$parks, input$alc), {
    update_popover(
      "builtenvpopover",
      content = markdown(built_popover_md())
    )
  })
  
  observeEvent(list(input$clinics, input$hospitals, input$ems, input$pharmacies, input$wic_clinics, input$wic_retailers, input$cancer, input$fqhc), {
    update_popover(
      "healthaccpopover",
      content = markdown(h_access_switch_md())
    )
  })
  
  observeEvent(input$socialenv, {
    update_popover(
      "socenvpopover",
      content = markdown(soc_md())
    )
  })
  
  #### PALETTE ####
  # define palette by layer number 
  layer_base_palette <- function(layer_index) {
    switch(
      as.character(layer_index),
      "1" = colorRampPalette(c("#FFFFFF", "#008B8B"))(5),
      "2" = colorRampPalette(c("#FFFFFF", "#8B008B"))(5),
      "3" = colorRampPalette(c("#FFFFFF", "#B8860B"))(5),
      "4" = gray.colors(5)
    )
  }
  
  # define palette by variable
  geoex.palette <- function(var, df, layer_index) {
    tryCatch({
      # skip geometry column to avoid error
      if (var == "geometry" || inherits(df[[var]], "sfc") || var == "GEOID") {
        return(NULL)
      }
      
      # base ramp
      base_ramp <- layer_base_palette(layer_index)
      
      domain <- df[[var]] 
      
      if (var == "PFAS_dw") {
        # TODO: make PFAS sentence case
        return(colorFactor(
          palette = c("#780000", "#fdf0d5"), domain = domain,
          levels = c(TRUE, FALSE)
        ))
      }
      
      return(colorNumeric(palette = base_ramp, domain = domain, na.color = "#5D5D5D"))
    },
    
    error = function(e) {
      message("error in geoex.palette: ", e$message)
      return(NULL)
    })
  }
  
  #### LEGEND AND LABELS ####
  # defines legend titles based on defined column
  #TODO: fix title sub and superscripts to work with leaflegend
  legend.titles <- function(col) {
    if(col == "Particulate.Matter.2.5") return(HTML("PM<sub>2.5</sub> (\U03BC", "g/m<sup>3</sup>) concentrations in 2022 "))
    if(col == "Green.Space") return("Normalized difference vegetation index (NDVI) in July 2024")
    if(col == "Nighttime.Radiance") return(HTML("Light at night (nW/cm<sup>2</sup>/sr) in 2022"))
    if(col == "Food.Stamps") return("SNAP benefits in 2023 (%)")
    if(col == "Food.Insecurity") return("Food insecurity in 2023 (%)")
    if(col == "Housing.Insecurity") return("Housing insecurity in 2023 (%)")
    if(col == "Utility.Services.Threat") return("Utility services threat in 2023 (%)")
    if(col == "Lacking.Reliable.Transportation") return("Lack of reliable transportation in 2023 (%)")
    if(col == "Lack.of.Social.and.Emotional.Support") return("Lack of social and emotional support in 2023 (%)")
    if(col == "Lack.of.Health.Insurance") return("No health insurance in 2021 (%)")
    if(col == "Routine.Checkup.in.the.Past.Year") return("Routine checkup in 2023 (%)")
    if(col == "Visited.Dentist.in.Past.Year") return("Visited dentist in 2023 (%)")
    if(col == "Taking.Medicine.to.Control.High.Blood.Pressure") return("Taking blood pressure medication in 2023 (%)")
    if(col == "Cholesterol.Screening") return("Cholesterol screening in 2023 (%)")
    if(col == "Mammography.Use.among.Women.50.to.74") return("Mammography screening for breast cancer  in 2023 (%)")
    if(col == "Colorectal.Cancer.Screening.among.Adults.45.to.75") return("Colorectal cancer screening in 2023 (%)")
    if(col == "Binge.Drinking.among.Adults") return("Binge drinking in 2023 (%)")
    if(col == "Cigarette.Smoking.among.Adults") return("Cigarette smoking in 2023 (%)")
    if(col == "No.Leisure.time.Physical.Activity.among.Adults") return("No physical activity in 2023 (%)")
    if(col == "Short.Sleep.Duration") return("Short sleep duration in 2023 (%)")
    if(col == "Arthritis.among.Adults") return("Arthritis in 2023 (%)")
    if(col == "Asthma.among.Adults") return("Asthma in 2023 (%)")
    if(col == "High.Blood.Pressure.among.Adults") return("High blood pressure in 2023 (%)")
    if(col == "Cancer.or.Melanoma.among.Adults") return("Cancer prevalence in 2023 (%)")
    if(col == "High.Cholesterol.among.Screened.Adults") return("High cholesterol in 2023 (%)")
    if(col == "COPD.among.Adults") return("Chronic obstructive pulmonary disease in 2023 (%)")
    if(col == "Coronary.Heart.Disease.among.Adults") return("Coronary heart disease in 2023 (%)")
    if(col == "Depression.among.Adults") return("Depression in 2023 (%)")
    if(col == "Diagnosed.Diabetes.among.Adults") return("Diabetes in 2023 (%)")
    if(col == "Obesity.among.Adults") return("Obesity in 2023 (%)")
    if(col == "All.Teeth.Lost.among.Adults.65.and.Older") return("All teeth lost in 2023 (%)")
    if(col == "Stroke.among.Adults") return("Stroke in 2023 (%)")
    
    if(col == "Total.Population") return("Population in 2023 (total)")
    if(col == "Hispanic.or.Latino") return("Hispanic or Latino population in 2023 (total)")
    if(col == "Percent.Hispanic.or.Latino") return("Hispanic or Latino population in 2023 (%)")
    if(col == "White.NonHispanic") return("Non-Hispanic White population in 2023 (total)")
    if(col == "Percent.White.NonHispanic") return("Non-Hispanic White population in 2023 (%)")
    if(col == "Black.NonHispanic") return("Non-Hispanic Black population in 2023 (total)")
    if(col == "Percent.Black.NonHispanic") return("Non-Hispanic Black population in 2023 (%)")
    if(col == "American.Indian.Alaska.Native.NonHispanic") return("Non-Hispanic American Indian or Alaska Native population in 2023 (total)")
    if(col == "Percent.American.Indian.Alaska.Native.NonHispanic") return("Non-Hispanic American Indian or Alaska Native population in 2023 (%)")
    if(col == "Asian.NonHispanic") return("Non-Hispanic Asian population in 2023 (total)")
    if(col == "Percent.Asian.NonHispanic") return("Non-Hispanic Asian population in 2023 (%)")
    if(col == "Native.Hawaiian.Pacific.Islander.NonHispanic") return("Non-Hispanic Native Hawaiian or Other Pacific Islander population in 2023 (total)")
    if(col == "Percent.Native.Hawaiian.Pacific.Islander.NonHispanic") return("Non-Hispanic Native Hawaiian or Other Pacific Islander population in 2023 (%)")
    if(col == "Other.Race.NonHispanic") return("Non-Hispanic Other race population in 2023 (total)")
    if(col == "Percent.Other.Race.NonHispanic") return("Non-Hispanic Other race population in 2023 (%)")
    if(col == "Two.or.More.Races.NonHispanic") return("Non-Hispanic two or more races population in 2023 (total)")
    if(col == "Percent.Two.or.More.Races.NonHispanic") return("Non-Hispanic two or more races population in 2023 (%)")
    
    # hispanic or latino subcats
    if(col == "White.Hispanic.or.Latino") return("Hispanic or Latino White population in 2023 (total)")
    if(col == "Percent.White.Hispanic.or.Latino") return("Hispanic or Latino White population in 2023 (%)")
    if(col == "Black.Hispanic.or.Latino") return("Hispanic or Latino Black population in 2023 (total)")
    if(col == "Percent.Black.Hispanic.or.Latino") return("Hispanic or Latino Black population in 2023 (%)")
    if(col == "American.Indian.Alaska.Native.Hispanic.or.Latino") return("Hispanic or Latino American Indian or Alaska Native population in 2023 (total)")
    if(col == "Percent.American.Indian.Alaska.Native.Hispanic.or.Latino") return("Hispanic or Latino American Indian or Alaska Native population in 2023 (%)")
    if(col == "Asian.Hispanic.or.Latino") return("Hispanic or Latino Asian population in 2023 (total)")
    if(col == "Percent.Asian.Hispanic.or.Latino") return("Hispanic or Latino Asian population in 2023 (%)")
    if(col == "Native.Hawaiian.Pacific.Islander.Hispanic.or.Latino") return("Hispanic or Latino Native Hawaiian or Other Pacific Islander population in 2023 (total)")
    if(col == "Percent.Native.Hawaiian.Pacific.Islander.Hispanic.or.Latino") return("Hispanic or Latino Native Hawaiian or Other Pacific Islander population in 2023 (%)")
    if(col == "Other.Race.Hispanic.or.Latino") return("Hispanic or Latino Other race population in 2023 (total)")
    if(col == "Percent.Other.Race.Hispanic.or.Latino") return("Hispanic or Latino Other race population in 2023 (%)")
    if(col == "Two.or.More.Races.Hispanic.or.Latino") return("Hispanic or Latino two or more races population in 2023 (total)")
    if(col == "Percent.Two.or.More.Races.Hispanic.or.Latino") return("Hispanic or Latino two or more races population in 2023 (%)")
    
    # sex
    if(col == "Total.Male.Population") return("Male population in 2023 (total)")
    if(col == "Total.Female.Population") return("Female population in 2023 (total)")
    if(col == "Percent.Male") return("Male population in 2023 (%)")
    if(col == "Percent.Female") return("Female population in 2023 (%)")
    
    # age
    if(col == "Total.0.to.4.years") return("0-4 years population in 2023 (total)")
    if(col == "Percent.0.to.4.years") return("0-4 years population in 2023 (%)")
    if(col == "Total.5.to.9.years") return("5-9 years population in 2023 (total)")
    if(col == "Percent.5.to.9.years") return("5-9 years population in 2023 (%)")
    if(col == "Total.10.to.14.years") return("10-14 years population in 2023 (total)")
    if(col == "Percent.10.to.14.years") return("10-14 years population in 2023 (%)")
    if(col == "Total.15.to.19.years") return("15-19 years population in 2023 (total)")
    if(col == "Percent.15.to.19.years") return("15-19 years population in 2023 (%)")
    if(col == "Total.20.to.24.years") return("20-24 years population in 2023 (total)")
    if(col == "Percent.20.to.24.years") return("20-24 years population in 2023 (%)")
    if(col == "Total.25.to.29.years") return("25-29 years population in 2023 (total)")
    if(col == "Percent.25.to.29.years") return("25-29 years population in 2023 (%)")
    if(col == "Total.30.to.34.years") return("30-34 years population in 2023 (total)")
    if(col == "Percent.30.to.34.years") return("30-34 years population in 2023 (%)")
    if(col == "Total.35.to.39.years") return("35-39 years population in 2023 (total)")
    if(col == "Percent.35.to.39.years") return("35-39 years population in 2023 (%)")
    if(col == "Total.40.to.44.years") return("40-44 years population in 2023 (total)")
    if(col == "Percent.40.to.44.years") return("40-44 years population in 2023 (%)")
    if(col == "Total.45.to.49.years") return("45-49 years population in 2023 (total)")
    if(col == "Percent.45.to.49.years") return("45-49 years population in 2023 (%)")
    if(col == "Total.50.to.54.years") return("50-54 years population in 2023 (total)")
    if(col == "Percent.50.to.54.years") return("50-54 years population in 2023 (%)")
    if(col == "Total.55.to.59.years") return("55-59 years population in 2023 (total)")
    if(col == "Percent.55.to.59.years") return("55-59 years population in 2023 (%)")
    if(col == "Total.60.to.64.years") return("60-64 years population in 2023 (total)")
    if(col == "Percent.60.to.64.years") return("60-64 years population in 2023 (%)")
    if(col == "Total.65.to.69.years") return("65-69 years population in 2023 (total)")
    if(col == "Percent.65.to.69.years") return("65-69 years population in 2023 (%)")
    if(col == "Total.70.to.74.years") return("70-74 years population in 2023 (total)")
    if(col == "Percent.70.to.74.years") return("70-74 years population in 2023 (%)")
    if(col == "Total.75.to.79.years") return("75-79 years population in 2023 (total)")
    if(col == "Percent.75.to.79.years") return("75-79 years population in 2023 (%)")
    if(col == "Total.80.to.84.years") return("80-84 years population in 2023 (total)")
    if(col == "Percent.80.to.84.years") return("80-84 years population in 2023 (%)")
    if(col == "Total.85.and.older") return("85+ years population in 2023 (total)")
    if(col == "Percent.85.and.older") return("85+ years population in 2023 (%)")
    
    if(col == "Social.Vulnerability.Index") return("SVI in 2022")
    if(col == "Environmental.Justice.Index") return("EJI in 2019")
    if(col == "Unemployment") return("Unemployment in 2021 (%)")
    
    if(col == "UV.Index") return("UVI in 2024")
    
    if(col == "Radon") return(HTML("Radon gas concentration (Bq/m<sup>3</sup>) in 2021"))
    
    if(col == "Pesticide.Exposure") return(HTML("Agricultural pesticide use (lb/mi<sup>2</sup>) in 2019 "))
    
    if(col == "Racial.Residential.Segregation") return("Racial residential segregation in 2020")
    
    # transportation noise model
    if(col == "N.Noise.More.than.LAeq.45.to.50.db") return("Population exposed to noise levels LAeq \U2265 45-50 dB in 2020 (total)")
    if(col == "Pct.Noise.More.than.LAeq.45.to.50.db") return("Population exposed to noise levels LAeq \U2265 45-50 dB in 2020 (%)")
    if(col == "N.Noise.More.than.LAeq.50.to.60.db") return("Population exposed to noise levels LAeq \U2265 50-60 dB in 2020 (total)")
    if(col == "Pct.Noise.More.than.LAeq.50.to.60.db") return("Population exposed to noise levels LAeq \U2265 50-60 dB in 2020 (%)")
    if(col == "N.Noise.More.than.LAeq.60.to.70.db") return("Population exposed to noise levels LAeq \U2265 60-70 dB in 2020 (total)")
    if(col == "Pct.Noise.More.than.LAeq.60.to.70.db") return("Population exposed to noise levels LAeq \U2265 60-70 dB in 2020 (%)")
    if(col == "N.Noise.More.than.LAeq.70.to.80.db") return("Population exposed to noise levels LAeq \U2265 70-80 dB in 2020 (total)")
    if(col == "Pct.Noise.More.than.LAeq.70.to.80.db") return("Population exposed to noise levels LAeq \U2265 70-80 dB in 2020 (%)")
    if(col == "N.Noise.More.than.LAeq.80.to.90.db") return("Population exposed to noise levels LAeq \U2265 80-90 dB in 2020 (total)")
    if(col == "Pct.Noise.More.than.LAeq.80.to.90.db") return("Population exposed to noise levels LAeq \U2265 80-90 dB in 2020 (%)")
    if(col == "N.Noise.More.than.LAeq.90.db") return("Population exposed to noise levels LAeq \U2265 90 dB in 2020 (total)")
    if(col == "Pct.Noise.More.than.LAeq.90.db") return("Population exposed to noise levels LAeq \U2265 90 dB in 2020 (%)")
    
    if(col == "Walkability") return("Walkability score in 2019")
    
    if(col == "No.broadband.internet") return("No internet in 2021 (%)")
    if(col == "No.high.school.diploma") return("No high school diploma in 2021 (%)")
    if(col == "Single.parent.households") return("Single parent households in 2021 (%)")
    if(col == "Crowding") return("Crowding among households in 2021 (%)")
    if(col == "Poverty") return("Poverty in 2021 (%)")
    if(col == "Housing.cost.burden") return("Housing cost burden in 2021 (%)")
    
    if(col == "Dew.point") return(paste0("Dew point in 2020 ", "(\U00B0", "F)"))
    if(col == "Maximum.temperature") return(paste0("Maximum temperature in 2020 ", "(\U00B0", "F)"))
    if(col == "Minimum.temperature") return(paste0("Minimum temperature in 2020 ", "(\U00B0", "F)"))
    if(col == "Average.temperature") return(paste0("Average temperature in 2020 ", "(\U00B0", "F)"))
    if(col == "Precipitation") return("Precipitation in 2020 (in.)")
    
    if(col == "Wildfire.smoke") return(HTML("Wildfire smoke PM<sub>2.5</sub> (\U03BC", "g/m<sup>3</sup>) in 2023"))
    if(col == "Nitrogen.dioxide") return(HTML("Nitrogen dioxide (NO<sub>2</sub>) (ppb) in 2020"))
    if(col == "Sulfur.dioxide") return(HTML("Sulfur dioxide (SO<sub>2</sub>) (ppb) in 2020"))
    if(col == "Carbon.monoxide") return(HTML("Carbon monoxide (CO) (ppm) in 2020"))
    if(col == "Ozone") return(HTML("Ozone (O<sub>3</sub>) (ppb) in 2020"))
    
    if(col == "Population.density") return("Population density in 2023 (population per square mile)")
    if(col == "Avalanche.Risk.Score") return("Avalanche risk value in 2024 ($)")
    if(col == "Coastal.Flooding.Risk.Score") return("Coastal flooding risk value in 2024 ($)")
    if(col == "Cold.Wave.Risk.Score") return("Cold wave risk value in 2024 ($)")
    if(col == "Drought.Risk.Score") return("Drought risk value in 2024 ($)")
    if(col == "Earthquake.Risk.Score") return("Earthquake risk value in 2024 ($)")
    if(col == "Hail.Risk.Score") return("Hail risk value in 2024 ($)")
    if(col == "Heat.Wave.Risk.Score") return("Heat wave risk value in 2024 ($)")
    if(col == "Hurricane.Risk.Score") return("Hurricane risk value in 2024 ($)")
    if(col == "Ice.Storm.Risk.Score") return("Ice storm risk value in 2024 ($)")
    if(col == "Landslide.Risk.Score") return("Landslide risk value in 2024 ($)")
    if(col == "Lightning.Risk.Score") return("Lightning risk value in 2024 ($)")
    if(col == "Riverine.Flooding.Risk.Score") return("Riverine flooding risk value in 2024 ($)")
    if(col == "Strong.Wind.Risk.Score") return("Strong wind risk value in 2024 ($)")
    if(col == "Tornado.Risk.Score") return("Tornado risk value in 2024 ($)")
    if(col == "Tsunami.Risk.Score") return("Tsunami risk value in 2024 ($)")
    if(col == "Volcanic.Activity.Risk.Score") return("Volcanic activity risk value in 2024 ($)")
    if(col == "Wildfire.Risk.Score") return("Wildfire risk value in 2024 ($)")
    if(col == "Winter.Weather.Risk.Score") return("Winter weather risk value in 2024 ($)")
    
    if(col == "bluespace") return("Blue space coverage in 2021 (%)")
    if(col == "social_capital") return("Social capital in 2022")
    
    if(col == "PFAS_dw") return("PFAS in drinking water in 2021")
    if(col == "Median.HH.Income") return("Median household income in 2023 ($)")
    if(col == "HT_Index") return("Housing and Transportation (H + T\U00AE) Affordability Index in 2022")
    if(col == "Historic.Redlining.Score") return("Historic redlining score")
    
    if(col == "pct_Open_Water") return("Open water in 2024 (%)")
    if(col == "pct_Developed_Open") return("Developed open land in 2024 (%)")
    if(col == "pct_Developed_Low") return("Minimally developed land in 2024 (%)")
    if(col == "pct_Developed_Medium") return("Moderately developed land in 2024 (%)")
    if(col == "pct_Developed_High") return("Highly developed land in 2024 (%)")
    if(col == "pct_Barren") return("Barren land in 2024 (%)")
    if(col == "pct_Evergreen_Forest") return("Evergreen forest in 2024 (%)")
    if(col == "pct_Shrub") return("Shrubland in 2024 (%)")
    if(col == "pct_Grassland") return("Grassland in 2024 (%)")
    if(col == "pct_Pasture") return("Pasture in 2024 (%)")
    if(col == "pct_Crops") return("Cropland in 2024 (%)")
    if(col == "pct_Woody_Wetlands") return("Woody wetlands in 2024 (%)")
    if(col == "pct_Herbaceous_Wetlands") return("Herbaceous wetlands in 2024 (%)")
    if(col == "pct_Deciduous_Forest") return("Deciduous forest in 2024 (%)")
    if(col == "pct_Mixed_Forest") return("Mixed forest in 2024 (%)")
    if(col == "pct_Perennial_Ice") return("Perennial ice in 2024 (%)")
    
    if(col == "total_p1") return("Part I offenses in 2023 (total)")
    if(col == "p1_rate") return("Part I offenses in 2023 (per 1,000 population)")
    
    if(col == "total_p2") return("Part II offenses in 2023 (total)")
    if(col == "p2_rate") return("Part II offenses in 2023 (per 1,000 population)")
    
    # TODO: add legend titles for food environment columns
    if(col == "lapop1") return("Low access population at 1 mile in 2019 (total)")
    if(col == "lapop1share") return("Low access population at 1 mile in 2019 (percentage)")
    if(col == "lalowi1") return("Low access, low income population at 1 mile in 2019 (total)")
    if(col == "lalowi1share") return("Low access, low income population at 1 mile in 2019 (percentage)")
    if(col == "lakids1") return("Low access, age 0-17 at 1 mile in 2019 (total)")
    if(col == "lakids1share") return("Low access, age 0-17 at 1 mile in 2019 (percentage)")
    if(col == "laseniors1") return("Low access, age 65+ at 1 mile in 2019 (total)")
    if(col == "laseniors1share") return("Low access, age 65+  at 1 mile in 2019 (percentage)")
    if(col == "lawhite1") return("Low access, White population at 1 mile in 2019 (total)")
    if(col == "lawhite1share") return("Low access, White population at 1 mile in 2019 (percentage)")
    if(col == "lablack1") return("Low access, Black population at 1 mile in 2019 (total)")
    if(col == "lablack1share") return("Low access, Black population at 1 mile in 2019 (percentage)")
    if(col == "laasian1") return("Low access, Asian population at 1 mile in 2019 (total)")
    if(col == "laasian1share") return("Low access, Asian population at 1 mile in 2019 (percentage)")
    if(col == "lanhopi1") return("Low access, Native Hawaiian or Other Pacific Islander population at 1 mile in 2019 (total)")
    if(col == "lanhopi1share") return("Low access, Native Hawaiian or Other Pacific Islander population at 1 mile in 2019 (percentage)")
    if(col == "laaian1") return("Low access, Alaska Native or American Indian population at 1 mile in 2019 (total)")
    if(col == "laaian1share") return("Low access, Alaska Native or American Indian population at 1 mile in 2019 (percentage)")
    if(col == "laomultir1") return("Low access, Other/multiple race population at 1 mile in 2019 (total)")
    if(col == "laomultir1share") return("Low access, Other/multiple population at 1 mile in 2019 (percentage)")
    if(col == "lahisp1") return("Low access, Hispanic or Latino population at 1 mile in 2019 (total)")
    if(col == "lahisp1share") return("Low access, Hispanic or Latino population at 1 mile in 2019 (percentage)")
    if(col == "lahunv1") return("Low access, households without vehicle at 1 mile in 2019 (total)")
    if(col == "lahunv1share") return("Low access households without vehicle at 1 mile in 2019 (percentage)")
    if(col == "lasnap1") return("Low access households receiving SNAP benefits at 1 mile in 2019 (total)")
    if(col == "lasnap1share") return("Low access households receiving SNAP benefits at 1 mile in 2019 (percentage)")
  }
  
  layer.titles <- function(col) {
    if(col == "Particulate.Matter.2.5") return(HTML(paste0("PM<sub>2.5</sub> ", "(\U03BC", "g/m<sup>3</sup>)")))
    if(col == "Green.Space") return("NDVI")
    if(col == "Nighttime.Radiance") return(HTML("Light at Night (nW/cm<sup>2</sup>/sr)"))
    if(col == "Food.Stamps") return("SNAP benefits  (%)")
    if(col == "Food.Insecurity") return("Food insecurity (%)")
    if(col == "Housing.Insecurity") return("Housing insecurity (%)")
    if(col == "Utility.Services.Threat") return("Utility services threat (%)")
    if(col == "Lacking.Reliable.Transportation") return("Lack of reliable transportation (%)")
    if(col == "Lack.of.Social.and.Emotional.Support") return("Lack of social and emotional support (%)")
    if(col == "Lack.of.Health.Insurance") return("No health insurance (%)")
    if(col == "Routine.Checkup.in.the.Past.Year") return("Routine checkup (%)")
    if(col == "Visited.Dentist.in.Past.Year") return("Visited dentist (%)")
    if(col == "Taking.Medicine.to.Control.High.Blood.Pressure") return("Taking blood pressure medication (%)")
    if(col == "Cholesterol.Screening") return("Cholesterol screening (%)")
    if(col == "Mammography.Use.among.Women.50.to.74") return("Mammography screening for breast cancer  (%)")
    if(col == "Colorectal.Cancer.Screening.among.Adults.45.to.75") return("Colorectal cancer screening (%)")
    if(col == "Binge.Drinking.among.Adults") return("Binge drinking (%)")
    if(col == "Cigarette.Smoking.among.Adults") return("Cigarette smoking (%)")
    if(col == "No.Leisure.time.Physical.Activity.among.Adults") return("No physical activity (%)")
    if(col == "Short.Sleep.Duration") return("Short sleep duration (%)")
    if(col == "Arthritis.among.Adults") return("Arthritis (%)")
    if(col == "Asthma.among.Adults") return("Asthma (%)")
    if(col == "High.Blood.Pressure.among.Adults") return("High blood pressure (%)")
    if(col == "Cancer.or.Melanoma.among.Adults") return("Cancer prevalence (%)")
    if(col == "High.Cholesterol.among.Screened.Adults") return("High cholesterol (%)")
    if(col == "COPD.among.Adults") return("Chronic obstructive pulmonary disease (%)")
    if(col == "Coronary.Heart.Disease.among.Adults") return("Coronary heart disease (%)")
    if(col == "Depression.among.Adults") return("Depression (%)")
    if(col == "Diagnosed.Diabetes.among.Adults") return("Diabetes (%)")
    if(col == "Obesity.among.Adults") return("Obesity (%)")
    if(col == "All.Teeth.Lost.among.Adults.65.and.Older") return("All teeth lost (%)")
    if(col == "Stroke.among.Adults") return("Stroke (%)")
    
    if(col == "Total.Population") return("Population (total)")
    if(col == "Hispanic.or.Latino") return("Hispanic or Latino population (total)")
    if(col == "Percent.Hispanic.or.Latino") return("Hispanic or Latino population (%)")
    if(col == "White.NonHispanic") return("Non-Hispanic White population (total)")
    if(col == "Percent.White.NonHispanic") return("Non-Hispanic White population (%)")
    if(col == "Black.NonHispanic") return("Non-Hispanic Black population (total)")
    if(col == "Percent.Black.NonHispanic") return("Non-Hispanic Black population (%)")
    if(col == "American.Indian.Alaska.Native.NonHispanic") return("Non-Hispanic American Indian or Alaska Native population (total)")
    if(col == "Percent.American.Indian.Alaska.Native.NonHispanic") return("Non-Hispanic American Indian or Alaska Native population (%)")
    if(col == "Asian.NonHispanic") return("Non-Hispanic Asian population (total)")
    if(col == "Percent.Asian.NonHispanic") return("Non-Hispanic Asian population (%)")
    if(col == "Native.Hawaiian.Pacific.Islander.NonHispanic") return("Non-Hispanic Native Hawaiian or Other Pacific Islander population (total)")
    if(col == "Percent.Native.Hawaiian.Pacific.Islander.NonHispanic") return("Non-Hispanic Native Hawaiian or Other Pacific Islander population (%)")
    if(col == "Other.Race.NonHispanic") return("Non-Hispanic Other race population (total)")
    if(col == "Percent.Other.Race.NonHispanic") return("Non-Hispanic Other race population (%)")
    if(col == "Two.or.More.Races.NonHispanic") return("Non-Hispanic two or more races population (total)")
    if(col == "Percent.Two.or.More.Races.NonHispanic") return("Non-Hispanic two or more races population (%)")
    
    # hispanic or latino subcats
    if(col == "White.Hispanic.or.Latino") return("Hispanic or Latino White (total)")
    if(col == "Percent.White.Hispanic.or.Latino") return("Hispanic or Latino White (%)")
    if(col == "Black.Hispanic.or.Latino") return("Hispanic or Latino Black (total)")
    if(col == "Percent.Black.Hispanic.or.Latino") return("Hispanic or Latino Black (%)")
    if(col == "American.Indian.Alaska.Native.Hispanic.or.Latino") return("Hispanic or Latino American Indian or Alaska Native (total)")
    if(col == "Percent.American.Indian.Alaska.Native.Hispanic.or.Latino") return("Hispanic or Latino American Indian or Alaska Native (%)")
    if(col == "Asian.Hispanic.or.Latino") return("Hispanic or Latino Asian (total)")
    if(col == "Percent.Asian.Hispanic.or.Latino") return("Hispanic or Latino Asian (%)")
    if(col == "Native.Hawaiian.Pacific.Islander.Hispanic.or.Latino") return("Hispanic or Latino Native Hawaiian or Other Pacific Islander (total)")
    if(col == "Percent.Native.Hawaiian.Pacific.Islander.Hispanic.or.Latino") return("Hispanic or Latino Native Hawaiian or Other Pacific Islander (%)")
    if(col == "Other.Race.Hispanic.or.Latino") return("Hispanic or Latino other race (total)")
    if(col == "Percent.Other.Race.Hispanic.or.Latino") return("Hispanic or Latino other race (%)")
    if(col == "Two.or.More.Races.Hispanic.or.Latino") return("Hispanic or Latino two or more races (total)")
    if(col == "Percent.Two.or.More.Races.Hispanic.or.Latino") return("Hispanic or Latino two or more races (%)")
    
    # sex
    if(col == "Total.Male.Population") return("Male (total)")
    if(col == "Total.Female.Population") return("Female (total)")
    if(col == "Percent.Male") return("Male (%)")
    if(col == "Percent.Female") return("Female (%)")
    
    # age
    if(col == "Total.0.to.4.years") return("0-4 years (total)")
    if(col == "Percent.0.to.4.years") return("0-4 years (%)")
    if(col == "Total.5.to.9.years") return("5-9 years (total)")
    if(col == "Percent.5.to.9.years") return("5-9 years (%)")
    if(col == "Total.10.to.14.years") return("10-14 years (total)")
    if(col == "Percent.10.to.14.years") return("10-14 years (%)")
    if(col == "Total.15.to.19.years") return("15-19 years (total)")
    if(col == "Percent.15.to.19.years") return("15-19 years (%)")
    if(col == "Total.20.to.24.years") return("20-24 years (total)")
    if(col == "Percent.20.to.24.years") return("20-24 years (%)")
    if(col == "Total.25.to.29.years") return("25-29 years (total)")
    if(col == "Percent.25.to.29.years") return("25-29 years (%)")
    if(col == "Total.30.to.34.years") return("30-34 years (total)")
    if(col == "Percent.30.to.34.years") return("30-34 years (%)")
    if(col == "Total.35.to.39.years") return("35-39 years (total)")
    if(col == "Percent.35.to.39.years") return("35-39 years (%)")
    if(col == "Total.40.to.44.years") return("40-44 years (total)")
    if(col == "Percent.40.to.44.years") return("40-44 years (%)")
    if(col == "Total.45.to.49.years") return("45-49 years (total)")
    if(col == "Percent.45.to.49.years") return("45-49 years (%)")
    if(col == "Total.50.to.54.years") return("50-54 years (total)")
    if(col == "Percent.50.to.54.years") return("50-54 years (%)")
    if(col == "Total.55.to.59.years") return("55-59 years (total)")
    if(col == "Percent.55.to.59.years") return("55-59 years (%)")
    if(col == "Total.60.to.64.years") return("60-64 years (total)")
    if(col == "Percent.60.to.64.years") return("60-64 years (%)")
    if(col == "Total.65.to.69.years") return("65-69 years (total)")
    if(col == "Percent.65.to.69.years") return("65-69 years (%)")
    if(col == "Total.70.to.74.years") return("70-74 years (total)")
    if(col == "Percent.70.to.74.years") return("70-74 years (%)")
    if(col == "Total.75.to.79.years") return("75-79 years (total)")
    if(col == "Percent.75.to.79.years") return("75-79 years (%)")
    if(col == "Total.80.to.84.years") return("80-84 years (total)")
    if(col == "Percent.80.to.84.years") return("80-84 years (%)")
    if(col == "Total.85.and.older") return("85+ years (total)")
    if(col == "Percent.85.and.older") return("85+ years (%)")
    
    if(col == "Social.Vulnerability.Index") return("Social Vulnerability Index (SVI)")
    if(col == "Environmental.Justice.Index") return("Environmental Justice Index (EJI)")
    if(col == "Unemployment") return("Unemployment (%)")
    
    if(col == "UV.Index") return("UVI")
    
    if(col == "Radon") return(HTML("Radon gas concentration (Bq/m<sup>3</sup>)"))
    
    if(col == "Pesticide.Exposure") return(HTML("Agricultural pesticide use (lb/mi<sup>2</sup>)"))
    
    if(col == "Racial.Residential.Segregation") return("Racial residential segregation")
    
    # transportation noise model
    if(col == "N.Noise.More.than.LAeq.45.to.50.db") return("Population exposed to noise levels LAeq \U2265 45-50 dB (total)")
    if(col == "Pct.Noise.More.than.LAeq.45.to.50.db") return("Population exposed to noise levels LAeq \U2265 45-50 dB (%)")
    if(col == "N.Noise.More.than.LAeq.50.to.60.db") return("Population exposed to noise levels LAeq \U2265 50-60 dB (total)")
    if(col == "Pct.Noise.More.than.LAeq.50.to.60.db") return("Population exposed to noise levels LAeq \U2265 50-60 dB (%)")
    if(col == "N.Noise.More.than.LAeq.60.to.70.db") return("Population exposed to noise levels LAeq \U2265 60-70 dB (total)")
    if(col == "Pct.Noise.More.than.LAeq.60.to.70.db") return("Population exposed to noise levels LAeq \U2265 60-70 dB (%)")
    if(col == "N.Noise.More.than.LAeq.70.to.80.db") return("Population exposed to noise levels LAeq \U2265 70-80 dB (total)")
    if(col == "Pct.Noise.More.than.LAeq.70.to.80.db") return("Population exposed to noise levels LAeq \U2265 70-80 dB (%)")
    if(col == "N.Noise.More.than.LAeq.80.to.90.db") return("Population exposed to noise levels LAeq \U2265 80-90 dB (total)")
    if(col == "Pct.Noise.More.than.LAeq.80.to.90.db") return("Population exposed to noise levels LAeq \U2265 80-90 dB (%)")
    if(col == "N.Noise.More.than.LAeq.90.db") return("Population exposed to noise levels LAeq \U2265 90 dB (total)")
    if(col == "Pct.Noise.More.than.LAeq.90.db") return("Population exposed to noise levels LAeq \U2265 90 dB (%)")
    
    if(col == "Walkability") return("Walkability")
    
    if(col == "No.broadband.internet") return("No internet (%)")
    if(col == "No.high.school.diploma") return("No high school diploma (%)")
    if(col == "Single.parent.households") return("Single parent households (%)")
    if(col == "Crowding") return("Crowding among housing units (%)")
    if(col == "Poverty") return("Poverty (%)")
    if(col == "Housing.cost.burden") return("Housing cost burden (%)")
    
    if(col == "Dew.point") return(paste0("Dew point ", "(\U00B0", "F)"))
    if(col == "Maximum.temperature") return(paste0("Maximum temperature ", "(\U00B0", "F)"))
    if(col == "Minimum.temperature") return(paste0("Minimum temperature ", "(\U00B0", "F)"))
    if(col == "Average.temperature") return(paste0("Average temperature ", "(\U00B0", "F)"))
    if(col == "Precipitation") return("Precipitation (in.)")
    
    if(col == "Wildfire.smoke") return(HTML(paste0("Wildfire smoke PM<sub>2.5</sub> ", "(\U03BC", "g/m<sup>3</sup>)")))
    if(col == "Nitrogen.dioxide") return(HTML(paste0("Nitrogen dioxide (NO<sub>2</sub>) (ppb)")))
    if(col == "Sulfur.dioxide") return(HTML("Sulfur dioxide (SO<sub>2</sub>) (ppb)"))
    if(col == "Carbon.monoxide") return(HTML("Carbon monoxide (CO) (ppm)"))
    if(col == "Ozone") return(HTML("Ozone (O<sub>3</sub>) (ppb)"))
    
    if(col == "Population.density") return("Population density (persons per square mile)")
    if(col == "Avalanche.Risk.Score") return("Avalanche risk value ($)")
    if(col == "Coastal.Flooding.Risk.Score") return("Coastal flooding risk value ($)")
    if(col == "Cold.Wave.Risk.Score") return("Cold wave risk value ($)")
    if(col == "Drought.Risk.Score") return("Drought risk value ($)")
    if(col == "Earthquake.Risk.Score") return("Earthquake risk value ($)")
    if(col == "Hail.Risk.Score") return("Hail risk value ($)")
    if(col == "Heat.Wave.Risk.Score") return("Heat wave risk value ($)")
    if(col == "Hurricane.Risk.Score") return("Hurricane risk value ($)")
    if(col == "Ice.Storm.Risk.Score") return("Ice storm risk value ($)")
    if(col == "Landslide.Risk.Score") return("Landslide risk value ($)")
    if(col == "Lightning.Risk.Score") return("Lightning risk value ($)")
    if(col == "Riverine.Flooding.Risk.Score") return("Riverine flooding risk value ($)")
    if(col == "Strong.Wind.Risk.Score") return("Strong wind risk value ($)")
    if(col == "Tornado.Risk.Score") return("Tornado risk value ($)")
    if(col == "Tsunami.Risk.Score") return("Tsunami risk value ($)")
    if(col == "Volcanic.Activity.Risk.Score") return("Volcanic activity risk value ($)")
    if(col == "Wildfire.Risk.Score") return("Wildfire risk value ($)")
    if(col == "Winter.Weather.Risk.Score") return("Winter weather risk value ($)")
    
    if(col == "bluespace") return("Blue space (%)")
    if(col == "social_capital") return("Social capital")
    
    if(col == "PFAS_dw") return("Water tested positive for PFAS")
    if(col == "Median.HH.Income") return("Median household income ($)")
    if(col == "HT_Index") return("Housing and Transportation (H + T\U00AE) Affordability Index")
    if(col == "Historic.Redlining.Score") return("Historic redlining")
    
    if(col == "pct_Open_Water") return("Open water (%)")
    if(col == "pct_Developed_Open") return("Developed open land (%)")
    if(col == "pct_Developed_Low") return("Minimally developed land (%)")
    if(col == "pct_Developed_Medium") return("Moderately developed land (%)")
    if(col == "pct_Developed_High") return("Highly developed land (%)")
    if(col == "pct_Barren") return("Barren land (%)")
    if(col == "pct_Evergreen_Forest") return("Evergreen forest (%)")
    if(col == "pct_Shrub") return("Shrubland (%)")
    if(col == "pct_Grassland") return("Grassland (%)")
    if(col == "pct_Pasture") return("Pasture (%)")
    if(col == "pct_Crops") return("Cropland (%)")
    if(col == "pct_Woody_Wetlands") return("Woody wetlands (%)")
    if(col == "pct_Herbaceous_Wetlands") return("Herbaceous wetlands (%)")
    if(col == "pct_Deciduous_Forest") return("Deciduous forest (%)")
    if(col == "pct_Mixed_Forest") return("Mixed forest (%)")
    if(col == "pct_Perennial_Ice") return("Perennial ice (%)")
    
    if(col == "total_p1") return("Part I offenses (total)")
    if(col == "p1_rate") return("Part I offenses (per 1,000 population)")
    
    if(col == "total_p2") return("Part II offenses (total)")
    if(col == "p2_rate") return("Part II offenses (per 1,000 population)")
    
    # TODO: add legend titles for food environment columns
    # TODO: multiple x100 to get percentage, not proportion
    if(col == "lapop1") return("Low access population at 1 mile (total)")
    if(col == "lapop1share") return("Low access population at 1 mile (percentage)")
    if(col == "lalowi1") return("Low access, low income population at 1 mile (total)")
    if(col == "lalowi1share") return("Low access, low income population at 1 mile (percentage)")
    if(col == "lakids1") return("Low access, age 0-17 at 1 mile (total)")
    if(col == "lakids1share") return("Low access, age 0-17 at 1 mile (percentage)")
    if(col == "laseniors1") return("Low access, age 65+ at 1 mile (total)")
    if(col == "laseniors1share") return("Low access, age 65+  at 1 mile (percentage)")
    if(col == "lawhite1") return("Low access, White population at 1 mile (total)")
    if(col == "lawhite1share") return("Low access, White population at 1 mile (percentage)")
    if(col == "lablack1") return("Low access, Black population at 1 mile (total)")
    if(col == "lablack1share") return("Low access, Black population at 1 mile (percentage)")
    if(col == "laasian1") return("Low access, Asian population at 1 mile (total)")
    if(col == "laasian1share") return("Low access, Asian population at 1 mile (percentage)")
    if(col == "lanhopi1") return("Low access, Native Hawaiian and Pacific Islander population at 1 mile (total)")
    if(col == "lanhopi1share") return("Low access, Native Hawaiian and Pacific Islander population at 1 mile (percentage)")
    if(col == "laaian1") return("Low access, American Indian or Alaska Native population at 1 mile (total)")
    if(col == "laaian1share") return("Low access, American Indian or Alaska Native population at 1 mile (percentage)")
    if(col == "laomultir1") return("Low access, other/multiple race population at 1 mile (total)")
    if(col == "laomultir1share") return("Low access, other/multiple population at 1 mile (percentage)")
    if(col == "lahisp1") return("Low access, Hispanic or Latino population at 1 mile (total)")
    if(col == "lahisp1share") return("Low access, Hispanic or Latino population at 1 mile (percentage)")
    if(col == "lahunv1") return("Low access, households without vehicle at 1 mile (total)")
    if(col == "lahunv1share") return("Low access households without vehicle at 1 mile (percentage)")
    if(col == "lasnap1") return("Low access households receiving SNAP benefits at 1 mile (total)")
    if(col == "lasnap1share") return("Low access households receiving SNAP benefits at 1 mile (percentage)")
  }
  
  var.info <- function(col) {
    if(col == "Particulate.Matter.2.5") return("Average concentration of particulate matter less than 2.5 microns in diameter in a census tract using data from the Washington University in St. Louis (WUSTL) Atmospheric Composition Analysis Group (2024 release)")
    if(col == "Green.Space") return("Average green space vegetation health and intensity in a census tract measured using the normalized differernce vegetation index (NDVI) using Sentinel-2 Multi-Spectral Instrument (MSI) satellite data (daily average from July 2024)")
    if(col == "Nighttime.Radiance") return("Outdoor light at night (LAN), also known as nighttime radiance or light pollution, from the National Aeronautics and Space Administration Visible Infrared Imaging Radiometer Suite (VIIRS) (average from Jan. 1 - Dec. 31 2023)")
    if(col == "Food.Stamps") return("Estimate of the percentage of adults in a census tract who reported receiving food stamps, also called SNAP, the Supplemental Nutrition Assistance Program, on an EBT card from CDC PLACES (2025 release)")
    if(col == "Food.Insecurity") return("Estimate of the percentage of adults in a census tract who reported that the food that they bought always/usually/sometimes did not last, and they didn’t have money to get more from CDC PLACES (2025 release)")
    if(col == "Housing.Insecurity") return("Estimate of the percentage of adults in a census tract who reported they were not able to pay mortgage, rent, or utility bill in the past 12 months from CDC PLACES (2025 release)")
    if(col == "Utility.Services.Threat") return("Estimate of the percentage of adults in a census tract who reported that an electric, gas, oil, or water company threatened to shut off services at any time during the prior 12 months from CDC PLACES (2025 release)")
    if(col == "Lacking.Reliable.Transportation") return("Estimate of the percentage of adults in a census tract who reported a lack of reliable transportation keeping them from medical appointments, meetings, work, or from getting things needed for daily living in the past 12 months from CDC PLACES (2025 release)")
    if(col == "Lack.of.Social.and.Emotional.Support") return("Estimate of the percentage of adults in a census tract who reported sometimes, rarely, or never getting the social and emotional support needed from CDC PLACES (2025 release)")
    if(col == "Lack.of.Health.Insurance") return("Estimate of the percentage of adults aged 18-64 in a census tract who reported having no current health insurance coverage from CDC PLACES (2025 release)")
    if(col == "Routine.Checkup.in.the.Past.Year") return("Estimate of the percentage of adults in a census tract who reported having been to a doctor for a routine checkup (e.g., a general physical exam, not an exam for a specific injury, illness, or condition) in the previous year from CDC PLACES (2025 release)")
    if(col == "Visited.Dentist.in.Past.Year") return("Estimate of the percentage of adults in a census tract who reported having been to the dentist or dental clinic in the past year from CDC PLACES (2025 release)")
    if(col == "Taking.Medicine.to.Control.High.Blood.Pressure") return("Estimate of the percentage of adults in a census tract with high blood pressure who reported currently taking medicine for high blood pressure from CDC PLACES (2025 release)")
    if(col == "Cholesterol.Screening") return("Estimate of the percentage of adults in a census tract who reported having their cholesterol checked within the previous 5 years from CDC PLACES (2025 release)")
    if(col == "Mammography.Use.among.Women.50.to.74") return("Estimate of the percentage of women in a census tract 50-74 years who reported having had a mammogram within the previous 2 years from CDC PLACES (2025 release)")
    if(col == "Colorectal.Cancer.Screening.among.Adults.45.to.75") return("Estimate of the percentage of adults in a census tract who reported having one of the following: a fecal occult blood test (FOBT) within the previous year, a fecal immunochemical test (FIT)-DNA test within the previous 3 years, a sigmoidoscopy within the previous 5 years, a sigmoidoscopy within the previous 10 years with a FIT in the past year, a colonoscopy within the previous 10 years, or a CT colonography (virtual colonoscopy) within the previous 5 years from CDC PLACES (2025 release)")
    if(col == "Binge.Drinking.among.Adults") return("Estimate of the percentage of adults in a census tract who reported having \U2265 5 drinks (men) or \U2265 4 drinks (women) on \U2265 1 occasion during the previous 30 days from CDC PLACES (2025 release)")
    if(col == "Cigarette.Smoking.among.Adults") return("Estimate of the percentage of adults in a census tract who reported having smoked \U2265 100 cigarettes in their lifetime and currently smoke every day or some days from CDC PLACES (2025 release)")
    if(col == "No.Leisure.time.Physical.Activity.among.Adults") return("Estimate of the percentage of adults in a census tract who reported 'no' to the question: 'During the past month, other than your regular job, did you participate in any physical activities or exercises such as running, calisthenics, golf, gardening, or walking for exercise?' from CDC PLACES (2025 release)")
    if(col == "Short.Sleep.Duration") return("Estimate of the percentage of adults in a census tract who reported usually getting insufficient sleep duration (\U003C 7 hours, on average, during a 24-hour period) from CDC PLACES (2025 release)")
    if(col == "Arthritis.among.Adults") return("Estimate of the percentage of adults in a census tract who responded 'yes' to the question: 'Have you ever been told by a doctor or other health professional that you have some form of arthritis, rheumatoid arthritis, gout, lupus, or fibromyalgia?' from CDC PLACES (2025 release)")
    if(col == "Asthma.among.Adults") return("Estimate of the percentage of adults in a census tract who responded 'yes' to both of the questions, 'Have you ever been told by a doctor, nurse, or other health professional that you have asthma?' and 'Do you still have asthma?' from CDC PLACES (2025 release)")
    if(col == "High.Blood.Pressure.among.Adults") return("Estimate of the percentage of adults in a census tract who reported ever having been told by a doctor, nurse, or other health professional that they have high blood pressure from CDC PLACES (2025 release)")
    if(col == "Cancer.or.Melanoma.among.Adults") return("Estimate of the percentage of adults in a census tract who responded 'yes' to the question: 'Have you ever been told by a doctor, nurse, or other health professional that you had melanoma or any other types of cancer?' and 'no' to the question: 'Have you ever been told by a doctor, nurse, or other health professional that you had skin cancer that is not melanoma?' from CDC PLACES (2025 release)")
    if(col == "High.Cholesterol.among.Screened.Adults") return("Estimate of the percentage of adults in a census tract who reported having ever been screened for high cholesterol and told by a doctor, nurse, or other health professional that they had high cholesterol from CDC PLACES (2025 release)")
    if(col == "COPD.among.Adults") return("Estimate of the percentage of adults in a census tract who reported having ever been told by a doctor, nurse, or other health professional they had chronic obstructive pulmonary disease (COPD), emphysema, or chronic bronchitis from CDC PLACES (2025 release)")
    if(col == "Coronary.Heart.Disease.among.Adults") return("Estimate of the percentage of adults in a census tract who reported ever having been told by a doctor, nurse, or other health professional that they had angina or coronary heart disease from CDC PLACES (2025 release)")
    if(col == "Depression.among.Adults") return("Estimate of the percentage of adults in a census tract who responded 'yes' to having ever been told by a doctor, nurse, or other health professional they had a depressive disorder, including depression, major depression, dysthymia, or minor depression from CDC PLACES (2025 release)")
    if(col == "Diagnosed.Diabetes.among.Adults") return("Estimate of the percentage of adults in a census tract who reported being told by a doctor or other health professional that they have diabetes (other than diabetes during pregnancy for female respondents) from CDC PLACES (2025 release)")
    if(col == "Obesity.among.Adults") return("Estimate of the percentage of adults in a census tract who have a body mass index (BMI) \U2265 30.0 kg/m\U00B2 from CDC PLACES (2025 release)")
    if(col == "All.Teeth.Lost.among.Adults.65.and.Older") return("Estimate of the percentage of adults in a census tract \U2265 65 years who reported having lost all of their natural teeth due to tooth decay and gum disease from CDC PLACES (2025 release)")
    if(col == "Stroke.among.Adults") return("Estimate of the percentage of adults in a census tract who reported ever having been told by a doctor, nurse, or other health professional that they have had a stroke from CDC PLACES (2025 release)")
    
    if(col == "Total.Population") return("Total number of individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Hispanic.or.Latino") return("Total number of Hispanic or Latino individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.Hispanic.or.Latino") return("Percentage of Hispanic or Latino individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "White.NonHispanic") return("Total number of non-Hispanic White individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.White.NonHispanic") return("Percentage of non-Hispanic White individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Black.NonHispanic") return("Total number of non-Hispanic Black individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.Black.NonHispanic") return("Percentage of non-Hispanic Black individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "American.Indian.Alaska.Native.NonHispanic") return("Total number of non-Hispanic American Indian or Alaska Native individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.American.Indian.Alaska.Native.NonHispanic") return("Percentage of non-Hispanic American Indian or Alaska Native individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Asian.NonHispanic") return("Total number of non-Hispanic Asian individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.Asian.NonHispanic") return("Percentage of non-Hispanic Asian individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Native.Hawaiian.Pacific.Islander.NonHispanic") return("Total number of non-Hispanic Native Hawaiian or Other Pacific Islander individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.Native.Hawaiian.Pacific.Islander.NonHispanic") return("Percentage of non-Hispanic Native Hawaiian or Other Pacific Islander individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Other.Race.NonHispanic") return("Total number of non-Hispanic other race individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.Other.Race.NonHispanic") return("Percentage of non-Hispanic other race individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Two.or.More.Races.NonHispanic") return("Total number of non-Hispanic two or more race individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.Two.or.More.Races.NonHispanic") return("Percentage of non-Hispanic two or more race individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    
    # hispanic or latino subcats
    if(col == "White.Hispanic.or.Latino") return("Total number of Hispanic or Latino White individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.White.Hispanic.or.Latino") return("Percentage of Hispanic or Latino White individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Black.Hispanic.or.Latino") return("Total number of Hispanic or Latino Black individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.Black.Hispanic.or.Latino") return("Percentage of Hispanic or Latino Black individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "American.Indian.Alaska.Native.Hispanic.or.Latino") return("Total number of Hispanic or Latino American Indian or Alaska Native individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.American.Indian.Alaska.Native.Hispanic.or.Latino") return("Percentage of Hispanic or Latino American Indian or Alaska Native individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Asian.Hispanic.or.Latino") return("Total number of Hispanic or Latino Asian individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.Asian.Hispanic.or.Latino") return("Percentage of Hispanic or Latino Asian individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Native.Hawaiian.Pacific.Islander.Hispanic.or.Latino") return("Total number of Hispanic or Latino Native Hawaiian or Other Pacific Islander individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.Native.Hawaiian.Pacific.Islander.Hispanic.or.Latino") return("Percentage of Hispanic or Latino Native Hawaiian or Other Pacific Islander individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Other.Race.Hispanic.or.Latino") return("Total number of Hispanic or Latino Other race individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.Other.Race.Hispanic.or.Latino") return("Percentage of Hispanic or Latino Other race individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Two.or.More.Races.Hispanic.or.Latino") return("Total number of Hispanic or Latino two or more race individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.Two.or.More.Races.Hispanic.or.Latino") return("Percentage of Hispanic or Latino two or more race individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    
    # sex
    if(col == "Total.Male.Population") return("Total number of male individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Total.Female.Population") return("Total number of female individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.Male") return("Percentage of male individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.Female") return("Percentage of female individuals in a census tract using American Community Survey 5-year data (2019-2023)")
    
    # age
    if(col == "Total.0.to.4.years") return("Total number of individuals 0-4 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.0.to.4.years") return("Percentage of individuals 0-4 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Total.5.to.9.years") return("Total number of individuals 5-9 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.5.to.9.years") return("Percentage of individuals 5-9 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Total.10.to.14.years") return("Total number of individuals 10-14 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.10.to.14.years") return("Percentage of individuals 10-14 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Total.15.to.19.years") return("Total number of individuals 15-19 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.15.to.19.years") return("Percentage of individuals 15-19 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Total.20.to.24.years") return("Total number of individuals 20-24 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.20.to.24.years") return("Percentage of individuals 20-24 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Total.25.to.29.years") return("Total number of individuals 25-29 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.25.to.29.years") return("Percentage of individuals 25-29 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Total.30.to.34.years") return("Total number of individuals 30-34 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.30.to.34.years") return("Percentage of individuals 30-34 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Total.35.to.39.years") return("Total number of individuals 35-39 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.35.to.39.years") return("Percentage of individuals 35-39 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Total.40.to.44.years") return("Total number of individuals 40-44 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.40.to.44.years") return("Percentage of individuals 40-44 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Total.45.to.49.years") return("Total number of individuals 45-49 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.45.to.49.years") return("Percentage of individuals 45-49 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Total.50.to.54.years") return("Total number of individuals 50-54 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.50.to.54.years") return("Percentage of individuals 50-54 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Total.55.to.59.years") return("Total number of individuals 55-59 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.55.to.59.years") return("Percentage of individuals 55-59 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Total.60.to.64.years") return("Total number of individuals 60-64 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.60.to.64.years") return("Percentage of individuals 60-64 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Total.65.to.69.years") return("Total number of individuals 65-69 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.65.to.69.years") return("Percentage of individuals 65-69 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Total.70.to.74.years") return("Total number of individuals 70-74 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.70.to.74.years") return("Percentage of individuals 70-74 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Total.75.to.79.years") return("Total number of individuals 75-79 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.75.to.79.years") return("Percentage of individuals 75-79 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Total.80.to.84.years") return("Total number of individuals 80-84 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.80.to.84.years") return("Percentage of individuals 80-84 years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Total.85.and.older") return("Total number of individuals 85+ years old in a census tract using American Community Survey 5-year data (2019-2023)")
    if(col == "Percent.85.and.older") return("Percentage of individuals 85+ years old in a census tract using American Community Survey 5-year data (2019-2023)")
    
    if(col == "Social.Vulnerability.Index") return("Social vulnerability of a census tract, defined as the degree to which a community exhibits certain social conditions, including high poverty, low percentage of vehicle access, or crowded households, among others, which may affect a community's ability to prevent human suffering and financial loss in the event of a disaster, using the Centers for Disease Control and Prevention (CDC)/Agency for Toxic Substances and Disease Registry (ATSDR) Social Vulnerability Index (2022 release)")
    if(col == "Environmental.Justice.Index") return("Environmental Justice Index score of a census tract, which ranks the cumulative impacts of environmental injustice on health, using the Centers for Disease Control and Prevention (CDC)/Agency for Toxic Substances and Disease Registry (ATSDR) Environmental Justice Index (2024 release)")
    if(col == "Unemployment") return("Estimate of the percentage of individuals in a census tract who are 16-64 years old in the labor force and unemployed from CDC PLACES (2023 release)")
    
    if(col == "UV.Index") return("Average ultraviolet radiation index from Tropospheric Emission Monitoring Internet Service (TEMIS) satellite data (2024)")
    
    if(col == "Radon") return("Radon gas concentration using data from Li et al. Proc Natl Acad Sci (2024)")
    
    if(col == "Pesticide.Exposure") return("Agricultural pesticide use data from the Washington State Department of Health Washington Tracking Network (WTN) (2023 release)")
    
    if(col == "Racial.Residential.Segregation") return("Racial residential segregation, calculated using Duncan's multi-group dissimilarity index with 2020 race data by block group from the National Historical Geographic Information Systems (NHGIS) data portal")
    
    # transportation noise model
    if(col == "N.Noise.More.than.LAeq.45.to.50.db") return("Total number of individuals in a census tract exposed to noise levels \U2265 45-50 dB from Huang and Seto. Environ Impact Assess Rev (2024)")
    if(col == "Pct.Noise.More.than.LAeq.45.to.50.db") return("Percentage of individuals in a census tract exposed to noise levels LAeq \U2265 45-50 dB from Huang and Seto. Environ Impact Assess Rev (2024)")
    if(col == "N.Noise.More.than.LAeq.50.to.60.db") return("Total number of individuals in a census tract exposed to noise levels \U2265 50-60 dB from Huang and Seto. Environ Impact Assess Rev (2024)")
    if(col == "Pct.Noise.More.than.LAeq.50.to.60.db") return("Percentage of individuals in a census tract exposed to noise levels \U2265 50-60 dB from Huang and Seto. Environ Impact Assess Rev (2024)")
    if(col == "N.Noise.More.than.LAeq.60.to.70.db") return("Total number of individuals in a census tract exposed to noise levels \U2265 60-70 dB from Huang and Seto. Environ Impact Assess Rev (2024)")
    if(col == "Pct.Noise.More.than.LAeq.60.to.70.db") return("Percentage of individuals in a census tract exposed to noise levels \U2265 60-70 dB from Huang and Seto. Environ Impact Assess Rev (2024)")
    if(col == "N.Noise.More.than.LAeq.70.to.80.db") return("Total number of individuals in a census tract exposed to noise levels \U2265 70-80 dB from Huang and Seto. Environ Impact Assess Rev (2024)")
    if(col == "Pct.Noise.More.than.LAeq.70.to.80.db") return("Percentage of individuals in a census tract exposed to noise levels \U2265 70-80 dB from Huang and Seto. Environ Impact Assess Rev (2024)")
    if(col == "N.Noise.More.than.LAeq.80.to.90.db") return("Total number of individuals in a census tract exposed to noise levels \U2265 80-90 dB from Huang and Seto. Environ Impact Assess Rev (2024)")
    if(col == "Pct.Noise.More.than.LAeq.80.to.90.db") return("Percentage of individuals in a census tract exposed to noise levels \U2265 80-90 dB from Huang and Seto. Environ Impact Assess Rev (2024)")
    if(col == "N.Noise.More.than.LAeq.90.db") return("Total number of individuals in a census tract exposed to noise levels \U2265 90 dB from Huang and Seto. Environ Impact Assess Rev (2024)")
    if(col == "Pct.Noise.More.than.LAeq.90.db") return("Percentage of individuals in a census tract exposed to noise levels \U2265 90 dB from Huang and Seto. Environ Impact Assess Rev (2024)")
    
    if(col == "Walkability") return("Walkability of a census tract using Environmental Protection Agency (EPA) data (2021 release)")
    
    if(col == "No.broadband.internet") return("Estimate of the percentage of households without any type of broadband internet subscription from CDC PLACES (2023 release)")
    if(col == "No.high.school.diploma") return("Estimate of the percentage of adults aged 25+ who have not completed 4 years of high school education or equivalent from CDC PLACES (2023 release)")
    if(col == "Single.parent.households") return("Estimate of the percentage of households with a female or male householder with no spouse/partner present with children of the householder under 18 years from CDC PLACES (2023 release)")
    if(col == "Crowding") return("Estimate of the percentage of occupied housing units with 1.01 to 1.50 and 1.51 or more occupants per room from CDC PLACES (2023 release)")
    if(col == "Poverty") return("Estimate of the percentage of individuals in a census tract living below 150% of the poverty threshold from CDC PLACES (2023 release)")
    if(col == "Housing.cost.burden") return("Estimate of the percentage of households with annual income less than $75,000 that spend 30% or more of their household income on housing from CDC PLACES (2023 release)")
    
    if(col == "Dew.point") return("Average annual mean dew point temperature in a census tract from 1991-2020, Copyright\U00A9 2021 PRISM Climate Group at Oregon State University, https://prism.oregonstate.edu")
    if(col == "Maximum.temperature") return("Average annual maximum temperature in a census tract from 1991-2020, Copyright\U00A9 2021 PRISM Climate Group at Oregon State University, https://prism.oregonstate.edu")
    if(col == "Minimum.temperature") return("Average annual Minimum temperature in a census tract from 1991-2020, Copyright\U00A9 2021 PRISM Climate Group at Oregon State University, https://prism.oregonstate.edu")
    if(col == "Average.temperature") return("Average annual average temperature in a census tract from 1991-2020, Copyright\U00A9 2021 PRISM Climate Group at Oregon State University, https://prism.oregonstate.edu")
    if(col == "Precipitation") return("Precipitation (in.)")
    
    if(col == "Wildfire.smoke") return("Average concentration of wildfire smoke in a census tract from Stanford Environmental Change and Human Outcomes (ECHO) Lab (2023 release)")
    if(col == "Nitrogen.dioxide") return("Average concentration of nitrogen dioxide in a census tract from the Center for Air, Climate, & Energy Solutions (CACES) land use regression model (2025 release)")
    if(col == "Sulfur.dioxide") return("Average concentration of sulfur dioxide in a census tract from the Center for Air, Climate, & Energy Solutions (CACES) land use regression model (2025 release)")
    if(col == "Carbon.monoxide") return("Average concentration of carbon monoxide in a census tract from the Center for Air, Climate, & Energy Solutions (CACES) land use regression model (2025 release)")
    if(col == "Ozone") return("Average concentration of ozone in a census tract from Centers for Disease Control and Prevention (CDC) (2023 release)")
    
    if(col == "Population.density") return("Population density in 2023 (population per square mile)")
    if(col == "Avalanche.Risk.Score") return("Expected annual loss (EAL) on average in dollars to buildings, population, and/or agriculture due to avalanches from the Federal Emergency Management Agency (FEMA) National Risk Index (2025 release)")
    if(col == "Coastal.Flooding.Risk.Score") return("Expected annual loss (EAL) on average in dollars to buildings, population, and/or agriculture due to coastal flooding from the Federal Emergency Management Agency (FEMA) National Risk Index (2025 release)")
    if(col == "Cold.Wave.Risk.Score") return("Expected annual loss (EAL) on average in dollars to buildings, population, and/or agriculture due to cold waves from the Federal Emergency Management Agency (FEMA) National Risk Index (2025 release)")
    if(col == "Drought.Risk.Score") return("Expected annual loss (EAL) on average in dollars to buildings, population, and/or agriculture due to droughts from the Federal Emergency Management Agency (FEMA) National Risk Index (2025 release)")
    if(col == "Earthquake.Risk.Score") return("Expected annual loss (EAL) on average in dollars to buildings, population, and/or agriculture due to earthquakes from the Federal Emergency Management Agency (FEMA) National Risk Index (2025 release)")
    if(col == "Hail.Risk.Score") return("Expected annual loss (EAL) on average in dollars to buildings, population, and/or agriculture due to hail from the Federal Emergency Management Agency (FEMA) National Risk Index (2025 release)")
    if(col == "Heat.Wave.Risk.Score") return("Expected annual loss (EAL) on average in dollars to buildings, population, and/or agriculture due to heat waves from the Federal Emergency Management Agency (FEMA) National Risk Index (2025 release)")
    if(col == "Hurricane.Risk.Score") return("Expected annual loss (EAL) on average in dollars to buildings, population, and/or agriculture due to hurricanes from the Federal Emergency Management Agency (FEMA) National Risk Index (2025 release)")
    if(col == "Ice.Storm.Risk.Score") return("Expected annual loss (EAL) on average in dollars to buildings, population, and/or agriculture due to ice storms from the Federal Emergency Management Agency (FEMA) National Risk Index (2025 release)")
    if(col == "Landslide.Risk.Score") return("Expected annual loss (EAL) on average in dollars to buildings, population, and/or agriculture due to landslides from the Federal Emergency Management Agency (FEMA) National Risk Index (2025 release)")
    if(col == "Lightning.Risk.Score") return("Expected annual loss (EAL) on average in dollars to buildings, population, and/or agriculture due to lightning from the Federal Emergency Management Agency (FEMA) National Risk Index (2025 release)")
    if(col == "Riverine.Flooding.Risk.Score") return("Expected annual loss (EAL) on average in dollars to buildings, population, and/or agriculture due to riverine flooding from the Federal Emergency Management Agency (FEMA) National Risk Index (2025 release)")
    if(col == "Strong.Wind.Risk.Score") return("Expected annual loss (EAL) on average in dollars to buildings, population, and/or agriculture due to strong wind from the Federal Emergency Management Agency (FEMA) National Risk Index (2025 release)")
    if(col == "Tornado.Risk.Score") return("Expected annual loss (EAL) on average in dollars to buildings, population, and/or agriculture due to tornadoes from the Federal Emergency Management Agency (FEMA) National Risk Index (2025 release)")
    if(col == "Tsunami.Risk.Score") return("Expected annual loss (EAL) on average in dollars to buildings, population, and/or agriculture due to tsunamis from the Federal Emergency Management Agency (FEMA) National Risk Index (2025 release)")
    if(col == "Volcanic.Activity.Risk.Score") return("Expected annual loss (EAL) on average in dollars to buildings, population, and/or agriculture due to volcanic activity from the Federal Emergency Management Agency (FEMA) National Risk Index (2025 release)")
    if(col == "Wildfire.Risk.Score") return("Expected annual loss (EAL) on average in dollars to buildings, population, and/or agriculture due to wildfires from the Federal Emergency Management Agency (FEMA) National Risk Index (2025 release)")
    if(col == "Winter.Weather.Risk.Score") return("Expected annual loss (EAL) on average in dollars to buildings, population, and/or agriculture due to winter weather from the Federal Emergency Management Agency (FEMA) National Risk Index (2025 release)")
    
    if(col == "bluespace") return("Percentage of blue space coverage in a census tract using the global surface water dataset from the European Commission (EC) Joint Research Commission (JRC)/Google and the Copernicus Programme (2024 release)")
    if(col == "social_capital") return("Social capital index, indicating the overall strength of social infrastructures within communities of a census tract using data from Fraser et al. Scientific Reports 2022")
    
    if(col == "PFAS_dw") return("Whether PFAS has been found in water within a census tract using data from the Environmental Protection Agency (EPA) Unregulated Contaminant Monitoring Rule (UCMR) 5 (2025 release)")
    if(col == "Median.HH.Income") return("Median income of households within a census tract using data from American Community Survey 5-year data (2019-2023). Values are censored for tracts with a median household income of over $250,000") # TODO: reword this
    if(col == "HT_Index") return("Housing and Transportation (H + T\U00AE) Affordability Index from the Center for Neighborhood Technology. The Center for Neighborhood Technology bears no responsibility for the analyses or interpretations of the data presented here.")
    if(col == "Historic.Redlining.Score") return("Historic Home Owners' Loan Corporation (HOLC) score of a census tract from 1-4. 1 = A (most 'desirable' neighborhoods); 4 = D (least 'desirable' neighborhoods)")
    
    if(col == "pct_Open_Water") return("Percent area in a census tract with open water, generally with less than 25% cover of vegetation or soil from the National Land Cover Database (NLCD) of the Multi-Resolution Land Characteristics Consortium (MRLC) (2024 release)")
    if(col == "pct_Developed_Open") return("Percent area in a census tract with a mixture of some constructed materials, but mostly vegetation in the form of lawn grasses from the National Land Cover Database (NLCD) of the Multi-Resolution Land Characteristics Consortium (MRLC) (2024 release). Impervious surfaces account for less than 20% of total cover. These areas most commonly include large-lot single-family housing units, parks, golf courses, and vegetation planted in developed settings for recreation, erosion control, or aesthetic purposes")
    if(col == "pct_Developed_Low") return("Percent area in a census tract with minimally developed land from the National Land Cover Database (NLCD) of the Multi-Resolution Land Characteristics Consortium (MRLC) (2024 release). Impervious surfaces account for 20% to 49% percent of the total cover. These areas most commonly include single-family housing units")
    if(col == "pct_Developed_Medium") return("Percent area in a census tract with moderately developed land from the National Land Cover Database (NLCD) of the Multi-Resolution Land Characteristics Consortium (MRLC) (2024 release). Impervious surfaces account for 50% to 79% of the total cover. These areas most commonly include single-family housing units")
    if(col == "pct_Developed_High") return("Percent area in a census tract with highly developed land, where people reside or work in high numbers from the National Land Cover Database (NLCD) of the Multi-Resolution Land Characteristics Consortium (MRLC) (2024 release). Examples include apartment complexes, row houses and commercial/industrial. Impervious surfaces account for 80% to 100% of the total cover")
    if(col == "pct_Barren") return("Percent area in a census tract with bedrock, desert pavement, scarps, talus, slides, volcanic material, glacial debris, sand dunes, strip mines, gravel pits and other accumulations of earthen material from the National Land Cover Database (NLCD) of the Multi-Resolution Land Characteristics Consortium (MRLC) (2024 release)")
    if(col == "pct_Evergreen_Forest") return("Percent area in a census tract with areas dominated by trees generally greater than 5 meters tall, and greater than 20% of total vegetation cover from the National Land Cover Database (NLCD) of the Multi-Resolution Land Characteristics Consortium (MRLC) (2024 release). More than 75% of the tree species maintain their leaves all year. Canopy is never without green foliage")
    if(col == "pct_Shrub") return("Percent area in a census tract dominated by shrubs; less than 5 meters tall with shrub canopy typically greater than 20% of total vegetation from the National Land Cover Database (NLCD) of the Multi-Resolution Land Characteristics Consortium (MRLC) (2024 release). This class includes true shrubs, young trees in an early successional stage or trees stunted from environmental conditions")
    if(col == "pct_Grassland") return("Percent area in a census tract dominated by gramanoid or herbaceous vegetation, generally greater than 80% of total vegetation from the National Land Cover Database (NLCD) of the Multi-Resolution Land Characteristics Consortium (MRLC) (2024 release). These areas are not subject to intensive management such as tilling, but can be utilized for grazing")
    if(col == "pct_Pasture") return("Percent area in a census tract with areas of grasses, legumes, or grass-legume mixtures planted for livestock grazing or the production of seed or hay crops, typically on a perennial cycle from the National Land Cover Database (NLCD) of the Multi-Resolution Land Characteristics Consortium (MRLC) (2024 release). Pasture/hay vegetation accounts for greater than 20% of total vegetation")
    if(col == "pct_Crops") return("Percent area in a census tract with areas used for the production of annual crops, such as corn, soybeans, vegetables, tobacco, and cotton, and also perennial woody crops such as orchards and vineyards from the National Land Cover Database (NLCD) of the Multi-Resolution Land Characteristics Consortium (MRLC) (2024 release). Crop vegetation accounts for greater than 20% of total vegetation. This class also includes all land being actively tilled")
    if(col == "pct_Woody_Wetlands") return("Percent area in a census tract where forest or shrubland vegetation accounts for greater than 20% of vegetative cover and the soil or substrate is periodically saturated with or covered with water from the National Land Cover Database (NLCD) of the Multi-Resolution Land Characteristics Consortium (MRLC) (2024 release)")
    if(col == "pct_Herbaceous_Wetlands") return("Percent area in a census tract where perennial herbaceous vegetation accounts for greater than 80% of vegetative cover and the soil or substrate is periodically saturated with or covered with water from the National Land Cover Database (NLCD) of the Multi-Resolution Land Characteristics Consortium (MRLC) (2024 release)")
    if(col == "pct_Deciduous_Forest") return("Percent area in a census tract dominated by trees generally greater than 5 meters tall, and greater than 20% of total vegetation cover from the National Land Cover Database (NLCD) of the Multi-Resolution Land Characteristics Consortium (MRLC) (2024 release). More than 75% of the tree species shed foliage simultaneously in response to seasonal change")
    if(col == "pct_Mixed_Forest") return("Percent area in a census tract dominated by trees generally greater than 5 meters tall, and greater than 20% of total vegetation cover from the National Land Cover Database (NLCD) of the Multi-Resolution Land Characteristics Consortium (MRLC) (2024 release). Neither deciduous nor evergreen species are greater than 75% of total tree cover")
    if(col == "pct_Perennial_Ice") return("Percent area in a census tract characterized by a perennial cover of ice and/or snow, generally greater than 25% of total cover from the National Land Cover Database (NLCD) of the Multi-Resolution Land Characteristics Consortium (MRLC) (2024 release)")
    
    if(col == "total_p1") return("Total Part I offenses in a county from the Federal Bureau of Investigation (FBI) Uniform Crime Reporting (UCR) National Incident-Based Reporting System (NIBRS), defined as criminal homicide, rape, robbery, aggravated assault, burglary (breaking or entering), larceny-theft (except motor vehicle theft), motor vehicle theft, arson, and human trafficking (2024 release). Animal cruelty is also included due to its recent classification by FBI as a felony")
    if(col == "p1_rate") return("Total Part I offenses per 1,000 population in a county from the Federal Bureau of Investigation (FBI) Uniform Crime Reporting (UCR) National Incident-Based Reporting System (NIBRS), defined as criminal homicide, rape, robbery, aggravated assault, burglary (breaking or entering), larceny-theft (except motor vehicle theft), motor vehicle theft, arson, and human trafficking (2024 release). Animal cruelty is also included due to its recent classification by FBI as a felony")
    
    if(col == "total_p2") return("Total Part II offenses in a county from the Federal Bureau of Investigation (FBI) Uniform Crime Reporting (UCR) National Incident-Based Reporting System (NIBRS), defined as simple assault, forgery & counterfeiting, fraud, embezzlement, stolen property (buying, receiving, possessing), vandalism, weapons violations, prostitution, sex offenses (except rape, prostitution, and commercialized vice), drug abuse violations, gambling, offenses against family and children, DUI, liquor laws violations, drunkenness, disorderly conduct, vagrancy, and curfew & loitering (2024 release)")
    if(col == "p2_rate") return("Total Part II offenses in a county from the Federal Bureau of Investigation (FBI) Uniform Crime Reporting (UCR) National Incident-Based Reporting System (NIBRS), defined as simple assault, forgery & counterfeiting, fraud, embezzlement, stolen property (buying, receiving, possessing), vandalism, weapons violations, prostitution, sex offenses (except rape, prostitution, and commercialized vice), drug abuse violations, gambling, offenses against family and children, DUI, liquor laws violations, drunkenness, disorderly conduct, vagrancy, and curfew & loitering (2024 release)")
    
    # TODO: add legend titles for food environment columns
    if(col == "lapop1") return("Population count beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
    if(col == "lapop1share") return("Percentage of tract population that are beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
    if(col == "lalowi1") return("Low income population count beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
    if(col == "lalowi1share") return("Percentage of tract population that are low income individuals beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
    if(col == "lakids1") return("Children age 0-17 population count beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
    if(col == "lakids1share") return("Percentage of tract population that are children age 0-17 beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
    if(col == "laseniors1") return("Age 65+ population count beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
    if(col == "laseniors1share") return("Percentage of tract population that are age 65+ beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
    if(col == "lawhite1") return("White population count beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
    if(col == "lawhite1share") return("Percentage of tract population that are white beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
    if(col == "lablack1") return("Black or African American population count beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
    if(col == "lablack1share") return("Percentage of tract population that are Black or African American beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
    if(col == "laasian1") return("Asian population count beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
    if(col == "laasian1share") return("Percentage of tract population that are Asian beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
    if(col == "lanhopi1") return("Native Hawaiian or Other Pacific Islander population count beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
    if(col == "lanhopi1share") return("Percentage of tract population that are Native Hawaiian or Other Pacific Islander beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
    if(col == "laaian1") return("American Indian or Alaska Native population count beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
    if(col == "laaian1share") return("Percentage of tract population that are American Indian or Alaska Native beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
    if(col == "laomultir1") return("Other/Multiple race population count beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
    if(col == "laomultir1share") return("Percentage of tract population that are Other/Multiple race beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
    if(col == "lahisp1") return("Hispanic or Latino ethnicity population count beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
    if(col == "lahisp1share") return("Percentage of tract population that are of Hispanic or Latino ethnicity beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
    if(col == "lahunv1") return("Housing units without vehicle count beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
    if(col == "lahunv1share") return("percentage of tract housing units that are without vehicle and beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
    if(col == "lasnap1") return("Housing units receiving SNAP benefits count beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
    if(col == "lasnap1share") return("Percentage of tract housing units receiving SNAP benefits count beyond 1 mile from supermarket from the US Department of Agriculture (USDA) Food Access Research Atlas (2021 release)")
  }
  
  #### CLEAR BUTTON OBSERVERS ####
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
    updateSelectInput(session, "hazards", selected = character(0))
    updateSelectInput(session, "airpol", selected = character(0))
    updateSelectInput(session, "builtenv", selected = character(0))
    updateSelectInput(session, "noise", selected = character(0))
    updateSelectInput(session, "land", selected = character(0))
    updateSelectInput(session, "incsite", selected = "")
    updateSelectInput(session, "incstage", selected = "")
    updateSelectInput(session, "incsex", selected = "")
    updateSelectInput(session, "mortsite", selected = "")
    updateSelectInput(session, "mortsex", selected = "")
    updateSelectInput(session, "foodenv", selected = character(0))
    
    updateSelectInput(session, "crime", selected = character(0))
    
    # reset switches if needed
    update_switch("transit", value = FALSE)
    update_switch("parks", value = FALSE)
    update_switch("superfund", value = FALSE)
    update_switch("showcounties", value = FALSE)
    update_switch("showcities", value = FALSE)
    update_switch("showchart", value = FALSE)
    update_switch("microplastics", value = FALSE)
    
    update_switch("cancer", value = FALSE)
    update_switch("clinics", value = FALSE)
    update_switch("ems", value = FALSE)
    update_switch("hospitals", value = FALSE)
    update_switch("pharmacies", value = FALSE)
    update_switch("wic_clinics", value = FALSE)
    update_switch("wic_retailers", value = FALSE)
    update_switch("fqhc", value = FALSE)
    
    # clear the map
    leafletProxy("geoexmap") %>%
      clearControls() %>%
      clearShapes() 
  })
  
  observeEvent(input$clear2020, {
    updateSelectInput(session, "outcomes_tab", selected = character(0))
    updateSelectInput(session, "sociodemo_tab", selected = character(0))
    updateSelectInput(session, "age_tab", selected = character(0))
    updateSelectInput(session, "sex_tab", selected = character(0))
    updateSelectInput(session, "race_tab", selected = character(0))
    updateSelectInput(session, "socialenv_tab", selected = character(0))
    updateSelectInput(session, "prevention_tab", selected = character(0))
    updateSelectInput(session, "behaviors_tab", selected = character(0))
    updateSelectInput(session, "naturalenv_tab", selected = character(0))
    updateSelectInput(session, "hazards_tab", selected = character(0))
    updateSelectInput(session, "airpol_tab", selected = character(0))
    updateSelectInput(session, "builtenv_tab", selected = character(0))
    updateSelectInput(session, "noise_tab", selected = character(0))
    updateSelectInput(session, "land_tab", selected = character(0))
  })
  
  observeEvent(input$clear2010, {
    updateSelectInput(session, "foodenv_tab", selected = character(0))
  })
  
  observeEvent(input$clearinc, {
    updateSelectInput(session, "incsite_tab", selected = "")
    updateSelectInput(session, "incstage_tab", selected = "")
    updateSelectInput(session, "incsex_tab", selected = "")
  })
  
  observeEvent(input$clearmort, {
    updateSelectInput(session, "mortsite_tab", selected = "")
    updateSelectInput(session, "mortsex_tab", selected = "")
  })
  
  observeEvent(input$clearcrime, {
    updateSelectInput(session, "crime_tab", selected = character(0))
  })
  
  observeEvent(input$clearstandalone, {
    updateSelectInput(session, "standalone", selected = "")
  })
  
  #### REACTIVE VALUES #### 
  # layer ids for inputs to control number of layers 
  layer_ids <- c("outcomes", "sociodemo", "age", "sex", "race", "socialenv", "prevention", "behaviors", "naturalenv",
                 "airpol", "builtenv", "incsite", "incstage", "incsex", "mortsite", "mortsex", "foodenv", "crime", "noise", "land", "hazards")
  
  # track all layers
  prev <- reactiveValues() # previous selected values
  prev_total <- reactiveVal(0)
  
  last_changed <- reactiveVal(character(0)) # one value for last changed
  
  done_init <- reactiveVal(FALSE)
  
  # helpers <- define if incidence and mortality is "ready" to be counted as a layer
  inc.ready <- reactive({
    site <- input$incsite
    stage <- input$incstage
    sex <- input$incsex
    
    all(!is.null(site),
        !is.null(stage),
        !is.null(sex),
        nzchar(site %||% ""),
        nzchar(stage %||% ""),
        nzchar(sex %||% ""))
  })
  
  mort.ready <- reactive({
    site <- input$mortsite
    sex <- input$mortsex
    
    all(!is.null(site),
        !is.null(sex),
        nzchar(site %||% ""),
        nzchar(sex %||% ""))
  })
  
  layer_counter <- reactiveVal(0) # global layer counter for color schemes
  
  # keep track of total layers
  total_cols <- reactive({
    tot <- 0
    n_map <- if (is.null(map_cols())) 0 else max(ncol(map_cols()) - 1, 0)
    n_food <- if (is.null(food_env_cols())) 0 else max(ncol(food_env_cols()) - 1, 0)
    n_crime <- if (is.null(crime_cols())) 0 else max(ncol(crime_cols()) - 1, 0)
    
    if (mort.ready()) {
      tot <- tot + 1
    }
    
    if (inc.ready()) {
      tot <- tot + 1
    }
    
    tot <- tot + n_map + n_food + n_crime
    tot
  })
  
  # track data layer selection
  layer_selection_state <- reactive({
    list(main = list(outcomes = input$outcomes, sociodemographics = input$sociodemo, age = input$age,
                     sex = input$sex, race = input$race, airpollution = input$airpol, socialenvironment = input$socialenv,
                     behaviors = input$behaviors, prevention = input$prevention, naturalenvironment = input$naturalenv,
                     builtenvironment = input$builtenv, noise = input$noise, landuse = input$land, hazards = input$hazards),
         crime = input$crime,
         food = input$foodenv,
         cancer = list(incidence = c(input$incsite, input$incstage, input$incsex),
                       mortality = c(input$mortsite, input$mortsex)))
  })
  
  # track last changed input
  observeEvent(input$socialenv, {last_changed("socialenv")})
  observeEvent(input$crime, {last_changed("crime")})
  observeEvent(input$outcomes,  {last_changed("outcomes")})
  observeEvent(input$sociodemo,  {last_changed("sociodemo")})
  observeEvent(input$age,  {last_changed("age")})
  observeEvent(input$sex,  {last_changed("sex")})
  observeEvent(input$race,  {last_changed("race")})
  observeEvent(input$prevention,  {last_changed("prevention")})
  observeEvent(input$behaviors,  {last_changed("behaviors")})
  observeEvent(input$naturalenv,  {last_changed("naturalenv")})
  observeEvent(input$airpol,  {last_changed("airpol")})
  observeEvent(input$builtenv,  {last_changed("builtenv")})
  observeEvent(input$foodenv, {last_changed("foodenv")})
  observeEvent(input$incsite,  {last_changed("incsite")})
  observeEvent(input$incstage,  {last_changed("incstage")})
  observeEvent(input$incsex,  {last_changed("incsex")})
  observeEvent(input$mortsite,  {last_changed("mortsite")})
  observeEvent(input$mortsex,  {last_changed("mortsex")})
  
  
  # enforce 3 max layers
  observe({
    layer_counter(0)
    tot <- total_cols()
    ptot <- prev_total()
    
    if (ptot <= 3 && tot > 3) {
      print("entered if prev <= 3 && tot > 3\n")
      exceed <- last_changed()
      
      if (exceed %in% c("incsite", "incstage", "incsex") && inc.ready()) {
        shinyjs::click("incbutton")
        showNotification("You can only display up to 3 tract or county layers. Cancer incidence selections cleared.", type = "warning", duration = 5)
        return(NULL)
      }
      
      if (exceed %in% c("mortsite", "mortsex") && mort.ready()) {
        shinyjs::click("mortbutton")
        showNotification("You can only display up to 3 tract or county layers. Cancer mortality selections cleared.", type = "warning", duration = 5)
        return(NULL)
      }
      
      if (!is.null(exceed)) {
        cur_vals  <- input[[exceed]]
        if (is.null(cur_vals)) cur_vals <- character(0) else cur_vals <- as.character(cur_vals)
        old_vals  <- prev[[exceed]]
        
        # values newly added (in cur but not in old)
        added <- setdiff(cur_vals, old_vals)
        
        # if there is a newly added value, drop it
        if (length(added) > 0) {
          keep <- setdiff(cur_vals, added[1])
          
          updateSelectInput(
            session,
            inputId  = exceed,
            selected = keep
          )
        } else {
          # fallback: revert entirely
          updateSelectInput(
            session,
            inputId  = exceed,
            selected = old_vals
          )
        }
      }
      
      showNotification("You can only display up to 3 tract or county layers.",
                       type = "warning",
                       duration = 5)
    } else {
      for (id in layer_ids) {
        vals <- input[[id]]
        
        if (is.null(vals) || length(vals) == 0) vals <- ""
        prev[[id]] <- vals
        
      }
      prev_total(tot)
    }
  }) %>% 
    bindEvent(total_cols())
  
  ## main map (census tract) columns
  
  map_cols <- reactive({
    df <- cbind(health_outcomes, sociodemo, age, sex, race, social_env, health_prevention, air_pol, health_behaviors, natural_env, built_env, noise_env, landuse_env, hazard_env)
    
    df[, c(input$outcomes, input$sociodemo, input$age, input$sex, input$race, input$socialenv, input$prevention, input$behaviors, input$airpol, input$naturalenv, input$builtenv, input$noise, input$land, input$hazards), drop = FALSE]
  }) %>% 
    bindCache(input$outcomes, input$sociodemo, input$age, input$sex, input$race, input$socialenv, input$prevention, input$behaviors, input$naturalenv, input$airpol, input$builtenv, input$noise, input$land, input$hazards) # reduce work by server
  
  # shared for crosstalk
  #map_cols_sd <- SharedData$new(map_cols, key = ~GEOID, group = "main")
  
  tab_cols <- reactive({
    df <- og.data
    
    df[, c(input$outcomes_tab, input$sociodemo_tab, input$age_tab, input$sex_tab, input$race_tab, input$socialenv_tab, input$prevention_tab, input$behaviors_tab, input$naturalenv_tab, input$hazards_tab, input$airpol_tab, input$builtenv_tab, input$land_tab, input$noise_tab)]
  })
  
  # food env
  food_env_cols <- reactive({
    cbind(food_env) %>% 
      dplyr::select(!!!input$foodenv)
  })
  
  food_env_cols_tab <- reactive({
    df <- food
    
    df[, c(input$foodenv_tab)]
  })
  
  # county crime
  crime_cols <- reactive({
    df <- crime
    df[, c(input$crime)]
  })
  
  crime_cols_tab <- reactive({
    df <- crime
    df[, c(input$crime_tab)]
  })
  
  # census tract table
  # TODO: update to reflect new categories
  ct_table_cols <- reactive({
    og.data[, 1] %>% 
      st_drop_geometry() %>% 
      merge(tract.bounds, by = "GEOID") %>% 
      cbind(tab_cols()) %>% 
      dplyr::select(-contains("geom")) %>% 
      st_drop_geometry()
  })
  
  ct_food_table_cols <- reactive({
    food[, 1] %>% 
      st_drop_geometry() %>% 
      cbind(food_env_cols_tab()) %>% 
      dplyr::select(-contains('geom')) %>% 
      st_drop_geometry()
  })
  
  # county tables
  cnty_crime_table_cols <- reactive({
    selected_crime <- crime_cols_tab() %>%  # selected columns from crime
      st_drop_geometry()
    
    crime_df <- crime %>%  
      dplyr::select(-contains("geom"))
    
    selected_col_names <- names(selected_crime) # selected column names
    
    all_cols <- names(crime_df) # vector of column names
    
    is_p1 <- any(grepl("_p1$|^p1_", selected_col_names, ignore.case = TRUE)) # find if p1 is in any column names
    
    if (is_p1) {
      p1_col <- selected_col_names[grepl("_p1$|^p1_", selected_col_names, ignore.case = TRUE)]
      selected_idx <- which(all_cols == p1_col)
      crime_df[, all_cols[1:selected_idx]] %>% 
        dplyr::select(-contains("geom")) %>% 
        st_drop_geometry()
    } else {
      p2_col <- selected_col_names[grepl("_p2$|^p2_", selected_col_names, ignore.case = TRUE)]
      
      p1_index <- which(all_cols == "p1_rate")  
      selected_idx <- which(all_cols == p2_col)
      
      crime_df[, all_cols[c(1, 2, (p1_index + 1):selected_idx)]] %>%
        dplyr::select(-contains("geom")) %>% 
        st_drop_geometry()
    }
  })
  
  cnty_wscr_table_inc <- reactive({
    req(input$incsite_tab != "", input$incstage_tab != "", input$incsex_tab != "")
    wscr.inc %>% 
      filter(Cancer.Site == input$incsite_tab,
             Stage.At.Diagnosis == input$incstage_tab,
             Gender == input$incsex_tab)
  })
  
  cnty_wscr_table_mort <- reactive({
    req(input$mortsite_tab != "", input$mortstage_tab != "", input$mortsex_tab != "")
    wscr.mort %>% 
      filter(Cancer.Site == input$mortsite_tab,
             Stage.At.Diagnosis == input$mortstage_tab,
             Gender == input$mortsex_tab)
  })
  
  # Helper: no filter if input is "All" or NULL, filter otherwise
  filter_if_needed <- function(df, col, val) {
    if (is.null(val) || val == "All" || val == "") df else df[df[[col]] == val, , drop = FALSE]
  }
  
  # On mortstage or mortsex change, update sites with a *union* of sites available for selected sex
  observe({
    df <- wscr.inc
    df <- filter_if_needed(df, "Stage.At.Diagnosis", input$incstage_tab)
    df <- filter_if_needed(df, "Cancer.Site", input$incsite_tab)
    # Instead of filtering by mortsex, select all sites available for either sex (or All)
    if (!is.null(input$incsex_tab) && input$incsex_tab != "All") {
      df <- df[df$Gender %in% c("All", input$incsex_tab), , drop = FALSE]
    }
    sites <- sort(unique(df$Cancer.Site))
    updateSelectInput(session, "incsite_tab", choices = c("Please choose a site" = "", sites),
                      selected = isolate(input$incsite_tab))
  })
  
  # On mortsite or mortsex change, update stages
  # TODO: CHANGE FUNCTIONALITY TO MATCH THOSE OF MAP
  observe({
    df <- wscr.inc
    df <- filter_if_needed(df, "Cancer.Site", input$incsite_tab)
    if (!is.null(input$incsex_tab) && input$incsex_tab != "All") {
      df <- df[df$Gender %in% c("All", input$incsex_tab), , drop = FALSE]
    }
    stages <- sort(unique(df$Stage.At.Diagnosis))
    updateSelectInput(session, "incstage_tab", choices = c("Please choose a stage" = "", stages),
                      selected = isolate(input$incstage_tab))
  })
  
  # On mortsite or mortstage change, update sexes
  observe({
    df <- wscr.inc
    df <- filter_if_needed(df, "Cancer.Site", input$incsite_tab)
    df <- filter_if_needed(df, "Stage.At.Diagnosis", input$incstage_tab)
    genders <- sort(unique(df$Gender))
    updateSelectInput(session, "incsex_tab", choices = c("Please choose a sex" = "", genders),
                      selected = isolate(input$incsex_tab))
  })
  
  observeEvent(input$incbutton_tab, {
    updateSelectInput(session, "incsite_tab", selected = "")
    updateSelectInput(session, "incstage_tab", selected = "")
    updateSelectInput(session, "incsex_tab", selected = "")
  })
  
  # On mortstage or mortsex change, update sites with a *union* of sites available for selected sex
  observe({
    df <- wscr.mort
    # Instead of filtering by mortsex, select all sites available for either sex (or All)
    if (!is.null(input$mortsex_tab) && input$mortsex_tab != "" && input$mortsex_tab != "All") {
      df <- df[df$Gender %in% c("All", input$mortsex_tab), , drop = FALSE]
    }
    sites <- sort(unique(df$Cancer.Site))
    updateSelectInput(session, "mortsite_tab", choices = c("Please choose a site" = "", sites),
                      selected = isolate(input$mortsite_tab))
  })
  
  # On mortsite or mortstage change, update sexes
  observe({
    df <- wscr.mort
    df <- filter_if_needed(df, "Cancer.Site", input$mortsite_tab)
    genders <- sort(unique(df$Gender))
    updateSelectInput(session, "mortsex_tab", choices = c("Please choose a sex" = "", genders),
                      selected = isolate(input$mortsex_tab))
  })
  
  observeEvent(input$mortbutton_tab, {
    updateSelectInput(session, "mortsite_tab", selected = "")
    updateSelectInput(session, "mortsex_tab", selected = "")
  })
  
  # point tables
  tab_point_data <- reactive({
    switch(input$standalone,
           "cancer.progs" = cancer.progs,
           "clinics" = clinics, "ems" = ems, "fqhc" = fqhc, "hospitals" = hospitals,
           "pharmacies" = pharmacies, "wic.clinics" = wic.clinics, "wic.retailers" = wic.retailers,
           "microplastics" = microplastics, "transit" = transit, "parks" = parks, "superfund" = superfund,
           "alcohol.retailers" = alc)
  })
  
  ##### UPLOAD REACTIVE #####
  # TODO: robust error handling
  user_polys <- reactive({
    exts <- tools::file_ext(input$upload$name)
    need_ok <- all(c("shp", "shx", "dbf") %in% exts)  # + "prj" if you require it
    
    validate(
      need(need_ok,
           "Please upload all shapefile components: .shp, .shx, .dbf (and .prj if available)."))
    req(input$upload)
    
    # if we get here, it's safe to proceed
    idx_shp <- which(exts == "shp")[1]
    input$upload$datapath[idx_shp]
    
    shpdf <- input$upload
    print(shpdf)
    
    # files projected with names
    # 0.dbf, 1.prj, 2.shp, 3.xml, 4.shx
    # need to rename files with actual names
    # Just in case any other files get uploaded
    shpdf <- shpdf[grepl("\\.(shp|dbf|shx|prj)$",
                         shpdf$name, ignore.case = TRUE), ]
    req(nrow(shpdf) > 0)
    
    # Row corresponding to the .shp file
    
    # name of temp dir where files are uploaded
    targetdir <- tempfile("shp_")
    dir.create(targetdir)
    
    base <- "upload"
    #cat("temp dir:", tmpdirname)
    
    for (i in seq_len(nrow(shpdf))) {
      ext <- tools::file_ext(shpdf$name[i])
      new_name <- paste0(base, ".", ext)
      file.copy(
        from = shpdf$datapath[i],
        to   = file.path(targetdir, new_name),
        overwrite = TRUE
      )
    }
    
    shp_path <- file.path(targetdir, paste0(base, ".shp"))
    shp <- st_read(shp_path, quiet = TRUE)
    
    # polygon geom only
    gtypes <- unique(st_geometry_type(shp, by_geometry = TRUE))
    allowed <- c("POLYGON", "MULTIPOLYGON")
    
    validate(need(all(as.character(gtypes) %in% allowed),
                  "Uploaded shapefile must contain only polygons or multipolygons."))
    
    # Ensure WGS84 for Leaflet
    if (is.na(st_crs(shp))) {
      shp <- st_set_crs(shp, 4326)
    } else if (st_crs(shp)$epsg != 4326) {
      shp <- st_transform(shp, 4326)
    }
    
    shp
  })
  
  #### OBSERVERS (WSCR) ####
  filtered.inc <- reactive({
    req(input$incsite != "", input$incstage != "", input$incsex != "")
    wscr.inc %>% 
      filter(Cancer.Site == input$incsite,
             Stage.At.Diagnosis == input$incstage,
             Gender == input$incsex) 
  })
  
  # On mortstage or mortsex change, update sites with a *union* of sites available for selected sex
  observe({
    df <- wscr.inc
    df <- filter_if_needed(df, "Stage.At.Diagnosis", input$incstage)
    df <- filter_if_needed(df, "Gender", input$incsex)

    sites <- sort(unique(df$Cancer.Site))
    updateSelectInput(session, "incsite", choices = c("Please choose a site" = "", sites),
                      selected = isolate(input$incsite))
  })
  
  # On mortsite or mortsex change, update stages 
  observe({
    df <- wscr.inc
    df <- filter_if_needed(df, "Cancer.Site", input$incsite)
    df <- filter_if_needed(df, "Gender", input$incsex)

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
      removeControl("cancerincidence")
  })
  
  filtered.mort <- reactive({
    req(input$mortsite != "", input$mortsex != "")
    wscr.mort %>% 
      filter(Cancer.Site == input$mortsite,
             Gender == input$mortsex)
  })
  
  micro.dat <- reactive({
    req(input$micro != "")
    microplastics %>% 
      filter(Marine.Setting %in% input$micro)
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
      removeControl("cancermortality")
  })
  
  # if microplastics switch is true, then show filtering criteria for microplastics
  observeEvent(input$microplastics, {
    if (input$microplastics) {
      shinyjs::show('micro_div')
    } else {
      shinyjs::hide('micro_div')
    }
    
  })
  
   observe({
     hide('micro_div')
   })

  # track active variables
  values <- reactiveValues(
    active_variables = character(0)
  )
  
  
  
  #### DOWNLOAD HANDLERS ####
  # TODO: add map download button
  # output$download <- downloadHandler(
  #   filename = function() {paste0(Sys.Date(), "geoexmap_download.gpkg")},
  #   content = function(file) {
  #     st_write(map_cols(), file)
  #   }
  # )
  
  # TODO: csv download instead of dbf
  output$downloadcttab <- downloadHandler(
    filename = function() {paste0(Sys.Date(), "_geoexmap_2020_tract_download.csv")},
    content = function(file) {
      st_write(ct_table_cols(), file)
    }
    
  )
  
  output$downloadfoodtab <- downloadHandler(
    filename = function() {paste0(Sys.Date(), "_geoexmap_2010_food_env_download.csv")},
    content = function(file) {
      st_write(ct_food_table_cols(), file)
    }
    
  )
  
  output$downloadcntycrime <- downloadHandler(
    filename = function() {paste0(Sys.Date(), "_geoexmap_county_crime_download.csv")},
    content = function(file) {
      st_write(crime_cols_tab(), file)
    }
  )
  
  output$downloadcntyinc <- downloadHandler(
    filename = function() {paste0(Sys.Date(), "_geoexmap_county_cancer_inc_download.csv")},
    content = function(file) {
      st_write(cnty_wscr_table_inc(), file)
    }
  )
  
  output$downloadcntymort <- downloadHandler(
    filename = function() {paste0(Sys.Date(), "_geoexmap_county_cancer_mort_download.csv")},
    content = function(file) {
      st_write(cnty_wscr_table_mort(), file)
    }
  )
  
  output$downloadstandalone <- downloadHandler(
    filename = function() {paste0(Sys.Date(), "_geoexmap_standalone_download.csv")},
    content = function(file) {
      st_write(tab_point_data(), file)
    }
  )
  
  # TODO: add download for standalone data
  
  #### PLOTLY RENDER ####
  output$chart <- renderPlotly({
    plotly.dat <- map_cols() %>%
      st_drop_geometry()

    if (ncol(plotly.dat) == 1) {
      plot_ly(data = plotly.dat, x = plotly.dat[,1]) %>% 
        layout(
          plot_bgcolor = '#e5ecf6',
          xaxis = list(title = layer.titles(names(plotly.dat)[1])),
          yaxis = list(title = "Frequency")) %>% 
        config()
    } else if (ncol(plotly.dat) == 2) {
      plot_ly(data = plotly.dat, type = "scatter", x = plotly.dat[,1], y = plotly.dat[,2],
              text = ~paste0("<b>", layer.titles(names(plotly.dat)[1]), ": ", round(plotly.dat[,1], digits = 2),
                             "<br>", layer.titles(names(plotly.dat)[2]), ": ", round(plotly.dat[,2], digits = 2))) %>%
        layout(
          plot_bgcolor = '#e5ecf6',
          xaxis = list(title = layer.titles(names(plotly.dat)[1])),
          yaxis = list(title = layer.titles(names(plotly.dat)[2]))) %>% 
        config(scrollZoom = TRUE)
    } else if (ncol(plotly.dat) == 3) {
      plot_ly(data = plotly.dat, x = plotly.dat[,1], y = plotly.dat[,2], z = plotly.dat[,3],
              text = ~paste0("<b>", layer.titles(names(plotly.dat)[1]), ": ", round(plotly.dat[,1], digits = 2),
                             "<br>", layer.titles(names(plotly.dat)[2]), ": ", round(plotly.dat[,2], digits = 2),
                             "<br>", layer.titles(names(plotly.dat)[3]), ": ", round(plotly.dat[,3], digits = 2))) %>%
        layout(scene = list(xaxis = list(title = layer.titles(names(plotly.dat)[1])),
                            yaxis = list(title = layer.titles(names(plotly.dat)[2])),
                            zaxis = list(title = layer.titles(names(plotly.dat)[3])))) %>% 
        config()
    } else {
      #TODO: add to render UI to choose at least 1 variable
    }

  })
  
  
  #### INITIAL MAP RENDER ####
  
  output$geoexmap <- renderLeaflet({
    map <- leaflet(options = leafletOptions(zoomSnap = 0.1, zoomDelta = 0.5)) %>% 
      setView(lng = -120.74, lat = 47.75, zoom = 7) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addSearchOSM(options = searchOptions(textPlaceholder = "Search", zoom = 15)) %>% 
      addEasyButton(easyButton(
        icon = 'fa-remove',
        title = "Remove all variables",
        onClick = JS("function(btn, map){
                     Shiny.setInputValue('clear_all_vars', Math.random());
        }"),
        id = "clear-btn"
      )) 
      return(map)
  })
  
  #### TABLE RENDER ####
  # TODO: add year for variables displayed
  output$ct_table <- renderReactable({
    validate(need(base::ncol(ct_table_cols()) > 3, "Please select a variable."))
    
    reactable(ct_table_cols(),
              defaultColDef = colDef(
                header = function(value) gsub(".", " ", value, fixed = TRUE),
                cell = function(value) if(is.numeric(value)) round(value, 3) else value,
                align = "left"
              ))
    })
  
  output$food_table <- renderReactable({
    validate(need(base::ncol(ct_food_table_cols()) > 1, "Please select a food environment variable."))
    
    reactable(ct_food_table_cols(),
              defaultColDef = colDef(
                header = function(value) gsub(".", " ", value, fixed = TRUE),
                cell = function(value) if(is.numeric(value)) round(value, 3) else value,
                align = "left"
              ))
  })
  
  output$cnty_crime_table <- renderReactable({
    validate(need(base::ncol(crime_cols_tab()) > 1, "Please select a crime variable."))
    
    reactable(cnty_crime_table_cols())
  })
  
  output$cnty_mort_table <- renderReactable({
    validate(need(base::nrow(cnty_wscr_table_mort()) > 1, "Please select variables for cancer mortality."))
    reactable(cnty_wscr_table_mort())
  })
  
  output$cnty_inc_table <- renderReactable({
    validate(need(base::nrow(cnty_wscr_table_inc()) > 1, "Please select variables for cancer incidence."))
    reactable(cnty_wscr_table_inc())
  })
  
  output$standalone_table <- renderReactable({
    req(tab_point_data())
    reactable(tab_point_data())
  })
  
  #### MAIN OBSERVER LOGIC ####
  observe({
    layer_counter(0)
    
    cat("RESET LAYER COUNTER TO 0\n")
    withProgress(message = "Working...", 
    {plotlyProxy("chart")
      
      # TODO: change to accommodate all variables, not just main map cols
      current_vars <- colnames(map_cols())
      current_vars <- current_vars[!current_vars %in% c("geom")]
      values$active_variables <- current_vars
      
      proxy <- leafletProxy("geoexmap", data = map_cols()) %>% 
        clearControls() %>% 
        clearShapes() %>% 
        clearMarkers()
        # addProviderTiles(providers$CartoDB.Positron)
      
      # TODO: 
      
      ##### point control flow #####
      # TODO: name and address popups for points where available
      if (input$transit) {
        html_legend <- '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-bus-front" viewBox="0 0 16 16">
  <path d="M5 11a1 1 0 1 1-2 0 1 1 0 0 1 2 0m8 0a1 1 0 1 1-2 0 1 1 0 0 1 2 0m-6-1a1 1 0 1 0 0 2h2a1 1 0 1 0 0-2zm1-6c-1.876 0-3.426.109-4.552.226A.5.5 0 0 0 3 4.723v3.554a.5.5 0 0 0 .448.497C4.574 8.891 6.124 9 8 9s3.426-.109 4.552-.226A.5.5 0 0 0 13 8.277V4.723a.5.5 0 0 0-.448-.497A44 44 0 0 0 8 4m0-1c-1.837 0-3.353.107-4.448.22a.5.5 0 1 1-.104-.994A44 44 0 0 1 8 2c1.876 0 3.426.109 4.552.226a.5.5 0 1 1-.104.994A43 43 0 0 0 8 3"/>
  <path d="M15 8a1 1 0 0 0 1-1V5a1 1 0 0 0-1-1V2.64c0-1.188-.845-2.232-2.064-2.372A44 44 0 0 0 8 0C5.9 0 4.208.136 3.064.268 1.845.408 1 1.452 1 2.64V4a1 1 0 0 0-1 1v2a1 1 0 0 0 1 1v3.5c0 .818.393 1.544 1 2v2a.5.5 0 0 0 .5.5h2a.5.5 0 0 0 .5-.5V14h6v1.5a.5.5 0 0 0 .5.5h2a.5.5 0 0 0 .5-.5v-2c.607-.456 1-1.182 1-2zM8 1c2.056 0 3.71.134 4.822.261.676.078 1.178.66 1.178 1.379v8.86a1.5 1.5 0 0 1-1.5 1.5h-9A1.5 1.5 0 0 1 2 11.5V2.64c0-.72.502-1.301 1.178-1.379A43 43 0 0 1 8 1"/>
</svg> Transit stops (zoom to view stops)<br/>'
        
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
        symbol <- makeSymbol(shape = "circle", 
                              fillColor = "#A6CEE3",
                             color = "black",
                             opacity = 1,
                             fillOpacity = 0.8,
                             height = 20,
                             width = 20, 'stroke-width' = 1)
        proxy <- proxy %>% 
          addMarkers(data = cancer.progs,
                     popup = ~paste0("Name: ", Center.or.Hospital.Name, "<br>Address: ", Address),
                     group = "cancer_programs",
                     icon = ~ icons(iconUrl = symbol,
                                    iconWidth = 20,
                                    iconHeight = 20)
                     ) %>% 
          addLegendImage(
            images = symbol,
            labels = "CoC-accredited programs",
            width = 20,
            height = 20,
            orientation = "vertical",
            position = 'topright',
            labelStyle = "font-size: 12px;"
          )
                     #color = "#8DD3C7",
                     #fillColor = "#8DD3C7") %>% 
          #addLegendImage(html = html_legend, position = "topright")
      } else {
        proxy <- proxy %>% 
          clearGroup(group = "cancer_programs")
      }
      
      if (input$alc) {
        symbol <- makeSymbol(shape = "circle", 
                             fillColor = "#1F78B4",
                             color = "black",
                             opacity = 1,
                             fillOpacity = 0.8,
                             height = 20,
                             width = 20, 'stroke-width' = 1)
        
        proxy <- proxy %>% 
          addMarkers(data = alc,
                     popup = ~Licensee,
                     group = "alc_retailers",
                     icon = ~ icons(iconUrl = symbol,
                                    iconWidth = 20,
                                    iconHeight = 20)) %>% 
          addLegendImage(
            images = symbol,
            labels = "Alcohol retailers",
            width = 20,
            height = 20,
            orientation = "vertical",
            position = 'topright',
            labelStyle = "font-size: 12px;"
          )
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
                      fillOpacity = 0.3, highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE)) %>% 
          addLegend(colors = c("blue"), labels = "Superfund sites")
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
                      fillOpacity = 0.3, highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE)) %>% 
          addLegend(colors = c("green"), labels = "Parks")
      } else {
        proxy <- proxy %>% 
          clearGroup(group = "parks")
      }
      
      if (input$clinics) {
        symbol <- makeSymbol(shape = "circle", 
                             fillColor = "#B2DF8A",
                             color = "black",
                             opacity = 1,
                             fillOpacity = 0.8,
                             height = 20,
                             width = 20, 'stroke-width' = 1)
        
        proxy <- proxy %>% 
          addMarkers(data = clinics,
                     popup = ~paste0("Name: ", NAME, "<br>Address: ", ADDRESS, ", ", CITY, ", WA ", ZIP),
                     group = "clinics",
                     icon = ~ icons(iconUrl = symbol,
                                    iconWidth = 20,
                                    iconHeight = 20)) %>% 
          addLegendImage(
            images = symbol,
            labels = "Clinics",
            width = 20,
            height = 20,
            orientation = "vertical",
            position = 'topright',
            labelStyle = "font-size: 12px;"
          )
      } else {
        proxy <- proxy %>% 
          clearGroup(group = "clinics")
      }
      
      if (input$ems) {
        symbol <- makeSymbol(shape = "circle", 
                             fillColor = "#33A02C",
                             color = "black",
                             opacity = 1,
                             fillOpacity = 0.8,
                             height = 20,
                             width = 20, 'stroke-width' = 1)
        
        proxy <- proxy %>% 
          addMarkers(data = ems,
                     popup = ~AGENCY,
                     group = "ems",
                     icon = ~ icons(iconUrl = symbol,
                                    iconWidth = 20,
                                    iconHeight = 20)) %>% 
          addLegendImage(
            images = symbol,
            labels = "EMS stations",
            width = 20,
            height = 20,
            orientation = "vertical",
            position = 'topright',
            labelStyle = "font-size: 12px;"
          )
      } else {
        proxy <- proxy %>% 
          clearGroup(group = "ems")
      }
      
      if (input$hospitals) {
        symbol <- makeSymbol(shape = "circle", 
                             fillColor = "#FB9A99",
                             color = "black",
                             opacity = 1,
                             fillOpacity = 0.8,
                             height = 20,
                             width = 20, 'stroke-width' = 1)
        
        proxy <- proxy %>% 
          addMarkers(data = hospitals,
                     popup = ~NAME,
                     group = "hospitals",
                     icon = ~ icons(iconUrl = symbol,
                                  iconWidth = 20,
                                  iconHeight = 20)) %>% 
          addLegendImage(
            images = symbol,
            labels = "Hospitals",
            width = 20,
            height = 20,
            orientation = "vertical",
            position = 'topright',
            labelStyle = "font-size: 12px;"
          )
      } else {
        proxy <- proxy %>% 
          clearGroup(group = "hospitals")
      }
      
      if (input$pharmacies) {
        symbol <- makeSymbol(shape = "circle", 
                             fillColor = "#E31A1CFF",
                             color = "black",
                             opacity = 1,
                             fillOpacity = 0.8,
                             height = 20,
                             width = 20, 'stroke-width' = 1)
        proxy <- proxy %>% 
          addMarkers(data = pharmacies,
                     popup = ~inFacility,
                     group = "pharmacies",
                     icon = ~ icons(iconUrl = symbol,
                                    iconWidth = 20,
                                    iconHeight = 20)) %>% 
          addLegendImage(
            images = symbol,
            labels = "Pharmacies",
            width = 20,
            height = 20,
            orientation = "vertical",
            position = 'topright',
            labelStyle = "font-size: 12px;"
          )
      } else {
        proxy <- proxy %>% 
          clearGroup(group = "pharmacies")
      }
      
      if (input$wic_clinics) {
        symbol <- makeSymbol(shape = "circle", 
                             fillColor = "#FDBF6FFF",
                             color = "black",
                             opacity = 1,
                             fillOpacity = 0.8,
                             height = 20,
                             width = 20, 'stroke-width' = 1)
        
        proxy <- proxy %>% 
          addMarkers(data = wic.clinics,
                     popup = ~Clinic,
                     group = "wic_clinics",
                     icon = ~ icons(iconUrl = symbol,
                             iconWidth = 20,
                             iconHeight = 20)) %>% 
          addLegendImage(
            images = symbol,
            labels = "WIC clinics",
            width = 20,
            height = 20,
            orientation = "vertical",
            position = 'topright',
            labelStyle = "font-size: 12px;"
          ) 
      } else {
        proxy <- proxy %>% 
          clearGroup(group = "wic_clinics")
      }
      
      if (input$wic_retailers) {
        symbol <- makeSymbol(shape = "circle", 
                             fillColor = "#FF7F00FF",
                             color = "black",
                             opacity = 1,
                             fillOpacity = 0.8,
                             height = 20,
                             width = 20, 'stroke-width' = 1)
        
        proxy <- proxy %>% 
          addMarkers(data = wic.retailers,
                     popup = ~Retailer,
                     group = "wic_retailers",
                     icon = ~ icons(iconUrl = symbol,
                                    iconWidth = 20,
                                    iconHeight = 20)) %>% 
          addLegendImage(
            images = symbol,
            labels = "WIC retailers",
            width = 20,
            height = 20,
            orientation = "vertical",
            position = 'topright',
            labelStyle = "font-size: 12px;"
          )
      } else {
        proxy <- proxy %>% 
          clearGroup(group = "wic_retailers")
      }
      
      if (input$fqhc) {
        symbol <- makeSymbol(shape = "circle", 
                             fillColor = "#CAB2D6FF",
                             color = "black",
                             opacity = 1,
                             fillOpacity = 0.8,
                             height = 20,
                             width = 20, 'stroke-width' = 1)
        
        proxy <- proxy %>% 
          addMarkers(data = fqhc,
                     popup = ~paste0("Name: ", Facility, "<br>Address: ", Address, ", ", City, ", WA ,", Zip),
                     group = "fqhc",
                     icon = ~ icons(iconUrl = symbol,
                                    iconWidth = 20,
                                    iconHeight = 20)) %>%
          addLegendImage(
            images = symbol,
            labels = "FQHCs",
            width = 20,
            height = 20,
            orientation = "vertical",
            position = 'topright',
            labelStyle = "font-size: 12px;"
          )
      } else {
        proxy <- proxy %>% 
          clearGroup(group = "fqhc")
      }
      
      if (input$microplastics) {
        # duplicate to avoid error with retrieving cols from reactive dataset
        # TODO: reorder microplastics legend (Very Low to High)
        micro_data <- micro.dat()
        micro_data_conc <- factor(micro_data$Concentration.class.text,
                              levels = c("Very Low", "Low", "Medium", "High"),
                              ordered = TRUE)
        pal <- colorFactor("OrRd", domain = levels(micro_data_conc), ordered = TRUE)
        proxy <- proxy %>% 
          addCircleMarkers(data = micro_data, lng = ~x, lat = ~y, color = ~pal(Concentration.class.text), group = "microplastics",
                           radius = 4, fillOpacity = 0.8, popup = ~paste("<b>Region:</b>", Region, "<b><br>Sampling method:</b>", Sampling.Method,
                                                                         "<b><br>Date of collection:</b>", Date..MM.DD.YYYY.)) %>% 
          # TODO: add info text for Ocean vs. Beach-Nurdle Patrol, concentrations, data source and download date
          addLegend(pal = pal,
                    values = ~micro_data$Concentration.class.text,
                    title = "Microplastics concentration")
      } else {
        proxy <- proxy %>% 
          clearGroup("microplastics")
      }
      
      ##### map (main data) #####
      groups <- character(0)
        for (i in seq_along(colnames(map_cols()))) {
          c_name <- colnames(map_cols())[i]
          x <- map_cols()[[c_name]]
          n <- length(map_cols())
          k <- NULL
          
          m <- map_cols() %>%
            sf::st_drop_geometry()
          
          label_cols <- colnames(m)
          label_names <- sapply(label_cols, layer.titles, USE.NAMES = FALSE)
          
          # construct label
          label <- lapply(
            #m[, label_cols, drop = FALSE],
            seq(nrow(m)),
            function(i) {
              row_vals <- m[i, , drop = TRUE]
              paste(
                paste0("<b>", label_names, "</b>", ": ", prettyNum(round(row_vals, 2), big.mark = ",")),
                collapse = "<br/>"
              )
            }
          ) %>% 
            lapply(htmltools::HTML)
          
          if (ncol(map_cols()) > 1 && c_name != "geometry" && c_name != "geom") {
            k <- isolate(layer_counter()) + 1
            layer_counter(k) # set the new layer count
            print(paste("LAYER COUNTER", layer_counter()))
          }
          step <- if (n > 0) 1 / n else 1
          
          # increment progress bar
          incProgress(step, detail = paste("Adding", layer.titles(c_name)))
          
          pal <- geoex.palette(c_name, df_vars, layer_index = layer_counter())
          
          if (!is.null(pal)){
            opacity <- if (k == 1) 0.5 else 0.2
            
            groups <- append(groups, layer.titles(c_name))
            
            info_id <- paste0("legend-info-", c_name)
            print(info_id)
            info_txt <- var.info(c_name)
            
            if (c_name == "PFAS_dw") {
              proxy <- proxy %>% 
                addPolygons(data = map_cols(), fillColor = ~pal(x), stroke = FALSE,
                            fillOpacity = opacity, highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE, sendToBack = TRUE),
                            group = layer.titles(c_name), label = label) %>% 
                addLegendFactor(pal = pal, values = x, fillOpacity = opacity, 
                                 orientation = "horizontal", shape = "circle", 
                                 title = htmltools::tags$div(style = "display:flex; align-items:center; justify-content:center; gap:6px; width:100%;",
                                                             htmltools::tags$span(
                                                               legend.titles(c_name),
                                                               style = "flex:0 1 auto; text-align:center; font-weight: bold; font-size: 12px;"
                                                             ),
                                                             htmltools::tags$span(
                                                               bs_icon('info-circle-fill'), # circled 'i'
                                                               id    = info_id,
                                                               `data-bs-toggle` = "tooltip",
                                                               `data-bs-placement` = "top",
                                                               title = info_txt,
                                                               style = "cursor:pointer; font-weight:bold; text-align: center;"
                                                             )
                                 ),
                                 labelStyle = "text-align: center;",
                                 className  = "my-centered-num-legend",
                                 position   = "bottomright") %>% 
                addLayersControl(overlayGroups = groups,
                                 position = "topright",
                                 options = layersControlOptions(collapsed = FALSE))
            }
            
            else {
              proxy <- proxy %>% 
                addPolygons(data = map_cols(), fillColor = ~pal(x), stroke = FALSE,
                            fillOpacity = opacity, highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
                            group = layer.titles(c_name), label = label) %>% 
                addLegendNumeric(pal = pal, values = x, fillOpacity = opacity, 
                                 orientation = "horizontal", shape = "stadium", 
                                 width = 300,   # wider bar
                                 height = 18, bins = 5,
                                 naLabel = "Not available",
                                 title = htmltools::tags$div(style = "display:flex; align-items:center; justify-content:center; gap:6px; width:100%;",
                                                             htmltools::tags$span(
                                                               legend.titles(c_name),
                                                               style = "flex:0 1 auto; text-align:center; font-weight: bold; font-size: 12px;"
                                                             ),
                                                             htmltools::tags$span(
                                                               bs_icon('info-circle-fill'), # circled 'i'
                                                               id    = info_id,
                                                               `data-bs-toggle` = "tooltip",
                                                               `data-bs-placement` = "top",
                                                               title = info_txt,
                                                               class = "legend-info-icon",
                                                               style = "cursor:pointer; font-weight:bold; text-align: center;"
                                                             )
                                 ),
                                 labelStyle = "text-align: center;",
                                 className  = "my-centered-num-legend",
                                 position   = "bottomright") %>% 
                addLayersControl(overlayGroups = groups,
                                 position = "topright",
                                 options = layersControlOptions(collapsed = FALSE))
            }
            
            session$sendCustomMessage(
              "modeLegendTooltip",
              list(text = "more about variable")
            )
            }
            
        }
      
      ##### map (food environment) #####
      # TODO: 
      for (i in seq_along(colnames(food_env_cols()))) {
        c_name <- colnames(food_env_cols())[i]
        x <- food_env_cols()[[c_name]]
        k <- NULL
        n <- length(food_env_cols())
        
        m <- food_env_cols() %>%
          sf::st_drop_geometry()
        
        label_cols <- colnames(m)
        label_names <- sapply(label_cols, layer.titles, USE.NAMES = FALSE)
        
        # construct label
        label <- lapply(
          #m[, label_cols, drop = FALSE],
          seq(nrow(m)),
          function(i) {
            row_vals <- m[i, , drop = TRUE]
            paste(
              paste0("<b>", label_names, "</b>", ": ", prettyNum(round(row_vals, 2), big.mark = ",")),
              collapse = "<br/>"
            )
          }
        ) %>% 
          lapply(htmltools::HTML)
        
        if (ncol(food_env_cols()) > 1 && c_name != "geometry" && c_name != "geom") {
          k <- isolate(layer_counter()) + 1
          layer_counter(k) # set the new layer count
          print(paste("LAYER COUNTER", layer_counter()))
        }

        pal <- geoex.palette(c_name, food, layer_index = layer_counter())
        
        step <- if (n > 0) 1 / n else 1
        
        # increment progress bar
        incProgress(step, detail = paste("Adding", layer.titles(c_name)))
        
        if (!is.null(pal)) {
          groups <- append(groups, layer.titles(c_name))
          opacity <- if (k == 1) 0.5 else 0.2
          
          info_id <- paste0("legend-info-", c_name)
          info_txt <- var.info(c_name)
          
          proxy <- proxy %>% 
            addPolygons(data = food_env_cols(), fillColor = ~pal(x), stroke = TRUE, weight = 0.25, color = "blue", group = layer.titles(c_name),
                        fillOpacity = opacity, highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE), label = label) %>% 
            addLegendNumeric(pal = pal, values = x, fillOpacity = opacity, 
                             orientation = "horizontal", shape = "stadium", 
                             width = 300,   # wider bar
                             height = 18, bins = 5,
                             naLabel = "Not available",
                             title = htmltools::tags$div(style = "display:flex; align-items:center; justify-content:center; gap:6px; width:100%;",
                                                         htmltools::tags$span(
                                                           legend.titles(c_name),
                                                           style = "flex:0 1 auto; text-align:center; font-weight: bold; font-size: 12px;"
                                                         ),
                                                         htmltools::tags$span(
                                                           bs_icon('info-circle-fill'),                    # circled 'i'
                                                           id    = info_id,
                                                           `data-bs-toggle` = "tooltip",
                                                           `data-bs-placement` = "top",
                                                           title = info_txt,
                                                           style = "cursor:pointer; font-weight:bold; text-align: center; font-weight: bold"
                                                         )
                             ),
                             labelStyle = "text-align: center;",
                             className  = "my-centered-num-legend",
                             position   = "bottomright") %>% 
            addLayersControl(overlayGroups = groups,
                             position = "topright",
                             options = layersControlOptions(collapsed = FALSE))
          #addLegend(pal = pal, values = x, title = legend.titles(c_name))
        }
      }
      
      ##### map (user upload) #####
      if (!is.null(input$upload)) {
        groups <- append(groups, "Uploaded file")
        proxy <- proxy %>% 
          addPolygons(
            data = user_polys(),
            group = "Uploaded file",
            stroke = TRUE,
            color = "purple",
            weight = 2,
            fillColor = "transparent",
            highlightOptions = highlightOptions(weight = 3,
                                                color = "#fff",
                                                bringToFront = TRUE)
          ) %>% 
          addLayersControl(overlayGroups = groups,
                           position = "topright",
                           options = layersControlOptions(collapsed = FALSE))
      }
      
      ##### map (crime) #####
      for (i in seq_along(colnames(crime_cols()))) {
        c_name <- colnames(crime_cols())[i]
        x <- crime_cols()[[c_name]]
        n <- length(crime_cols())
        k <- NULL
        
        if (ncol(crime_cols()) > 1 && c_name != "geometry" && c_name != "geom") {
          k <- layer_counter() + 1
          layer_counter(k) # set the new layer count
          print(paste("LAYER COUNTER", layer_counter()))
        }
        
        pal <- geoex.palette(c_name, crime, layer_index = layer_counter())
        step <- if (n > 0) 1 / n else 1
        
        # increment progress bar
        incProgress(step, detail = paste("Adding", layer.titles(c_name)))
        
        info_id <- paste0("legend-info-", c_name)
        info_txt <- var.info(c_name)
        
        if (!is.null(pal)){
          groups <- append(groups, layer.titles(c_name))
          opacity <- if (k == 1) 0.5 else 0.2
          
          proxy <- proxy %>% 
            addPolygons(data = crime_cols(), fillColor = ~pal(x), weight = 0.5, color = "black", group = layer.titles(c_name),
                        fillOpacity = opacity, stroke = input$showcounties, highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE)) %>% 
            addLegendNumeric(pal = pal, values = x, fillOpacity = opacity, 
                             orientation = "horizontal", shape = "stadium", 
                             width = 300,   # wider bar
                             height = 18, bins = 5,
                             naLabel = "Not available",
                             title = htmltools::tags$div(style = "display:flex; align-items:center; justify-content:center; gap:6px; width:100%;",
                                                         htmltools::tags$span(
                                                           legend.titles(c_name),
                                                           style = "flex:0 1 auto; text-align:center; font-weight: bold; font-size: 12px;"
                                                         ),
                                                         htmltools::tags$span(
                                                           bs_icon('info-circle-fill'), # circled 'i'
                                                           id    = info_id,
                                                           `data-bs-toggle` = "tooltip",
                                                           `data-bs-placement` = "top",
                                                           title = info_txt,
                                                           style = "cursor:pointer; font-weight:bold; text-align: center;"
                                                         )
                             ),
                             labelStyle = "text-align: center; font-weight: bold;",
                             className  = "my-centered-num-legend",
                             position   = "bottomright") %>% 
            addLayersControl(overlayGroups = groups, 
                             position = "topright",
                             options = layersControlOptions(collapsed = FALSE))
        }
      }
      
      ##### map (WSCR incidence) #####
      #observe({
      if (inc.ready()) {
        geo.inc <- base::merge(county.bounds, filtered.inc(), by.x = "NAME", by.y = "counties") %>% 
          mutate(Age.Adj..Rate.per.100.000 = as.numeric(Age.Adj..Rate.per.100.000)) %>% 
          st_transform(crs = 4326)
        
        print(geo.inc)
        
        #geo.inc[geo.inc$Age.Adj..Rate.per.100.000 == "NR",] <- NA
        
        if (nrow(geo.inc) > 0) {
          k <- layer_counter() + 1
          layer_counter(k)
          print(paste("LAYER COUNTER", layer_counter()))
          
          opacity <- if (k == 1) 0.5 else 0.2
          
          pal <- geoex.palette("Age.Adj..Rate.per.100.000", geo.inc, layer_index = layer_counter())
          val <- sort(geo.inc$Age.Adj..Rate.per.100.000)
          groups <- append(groups, "Cancer incidence")
          proxy <- proxy %>%
            addPolygons(data = geo.inc, fillColor = ~pal(Age.Adj..Rate.per.100.000),
                        popup = ~paste("<b>", NAMELSAD, "</b>", "<br>Site:", Cancer.Site, "<br>Stage:", Stage.At.Diagnosis, "<br>Sex:", Gender,
                                       "<br>Year:", Year, "<br>Age-adjusted incidence rate per 100,000:", Age.Adj..Rate.per.100.000),
                        group = "Cancer incidence", weight = 0.5, stroke = input$showcounties, fillOpacity = opacity, highlightOptions = highlightOptions(color = "black", weight = 3)) %>% 
            addLegendNumeric(pal = pal, values = val, fillOpacity = opacity, 
                             orientation = "horizontal", shape = "stadium", 
                             width = 300,   # wider bar
                             height = 18, bins = 5,
                             naLabel = "Not available",
                             title = htmltools::tags$div(style = "display:flex; align-items:center; justify-content:center; gap:6px; width:100%;",
                                                         htmltools::tags$span(
                                                           paste(unique(geo.inc$Cancer.Site), "cancer (age-adjusted incidence rate per 100,000) in 2018-2022:"),
                                                           style = "flex:0 1 auto; text-align:center; font-weight: bold; font-size: 12px;"
                                                         ),
                                                         htmltools::tags$span(
                                                           bs_icon('info-circle-fill'),
                                                           `data-bs-toggle` = "tooltip",
                                                           `data-bs-placement` = "top",
                                                           title = "These data were derived from the Washington State Cancer Registry. Information on stage- and sex-specific rates are provided where available.",
                                                           style = "cursor:pointer; font-weight:bold; text-align: center;"
                                                         )
                             ),
                             labelStyle = "text-align: center;",
                             className  = "my-centered-num-legend",
                             position   = "bottomright") %>% 
            addLayersControl(overlayGroups = groups,
                             position = "topright",
                             options = layersControlOptions(collapsed = FALSE))
            #addLegend(layerId = "cancerincidence", pal = pal, values = val, title = paste(unique(geo.inc$Cancer.Site), "Cancer Incidence Rate per 100,000:")) 
        }
        
      } else {
        proxy <- proxy %>%
          clearGroup("Cancer incidence") 
      }
      
      ##### map (WSCR mortality) #####
      #observe({
      if (mort.ready()) {
        geo.mort <- base::merge(county.bounds, filtered.mort(), by.x = "NAME", by.y = "counties") %>% 
          mutate(Age.Adj..Rate.per.100.000 = as.numeric(Age.Adj..Rate.per.100.000))
        
        if (nrow(geo.mort) > 0) {
          k <- layer_counter() + 1
          layer_counter(k)
          print(paste("LAYER COUNTER", layer_counter()))
          
          opacity <- if (k == 1) 0.5 else 0.2
          
          pal <- geoex.palette("Age.Adj..Rate.per.100.000", geo.mort, layer_index = layer_counter())
          val <- sort(geo.mort$Age.Adj..Rate.per.100.000)
          groups <- append(groups, "Cancer mortality")
          proxy <- proxy %>%
            addPolygons(data = geo.mort, fillColor = ~pal(Age.Adj..Rate.per.100.000),
                        popup = ~paste("<b>", NAMELSAD, "</b>", "<br>Site:", Cancer.Site, "<br>Stage:", Stage.At.Diagnosis, "<br>Sex:", Gender,
                                       "<br>Year:", Year, "<br>Age-adjusted incidence rate per 100,000:", Age.Adj..Rate.per.100.000),
                        group = "Cancer mortality", weight = 0.5, stroke = input$showcounties, fillOpacity = opacity, highlightOptions = highlightOptions(color = "black", weight = 3)) %>%
            addLegendNumeric(pal = pal, values = val, fillOpacity = opacity, 
                             orientation = "horizontal", shape = "stadium", 
                             width = 300,   # wider bar
                             height = 18, bins = 5,
                             naLabel = "Not available",
                             title = htmltools::tags$div(style = "display:flex; align-items:center; justify-content:center; gap:6px; width:100%;",
                                                         htmltools::tags$span(
                                                           paste(unique(geo.mort$Cancer.Site), "cancer (age-adjusted mortality rate per 100,000) in 2018-2022:"),
                                                           style = "flex:0 1 auto; text-align:center; font-weight: bold; font-size: 12px;"
                                                         ),
                                                         htmltools::tags$span(
                                                           bs_icon('info-circle-fill'),
                                                           `data-bs-toggle` = "tooltip",
                                                           `data-bs-placement` = "top",
                                                           title = "These data were derived from the Washington State Cancer Registry. Information on stage- and sex-specific rates are provided where available.",
                                                           style = "cursor:pointer; font-weight:bold; text-align: center;"
                                                         )
                             ),
                             labelStyle = "text-align: center;",
                             className  = "my-centered-num-legend",
                             position   = "bottomright") %>% 
            addLayersControl(overlayGroups = groups,
                             position = "topright",
                             options = layersControlOptions(collapsed = FALSE))
            #addLegend(layerId = "cancermortality", pal = pal, values = val, title = paste(unique(geo.mort$Cancer.Site), "Cancer Mortality Rate per 100,000:"))
        }
        
      } else {
        proxy <- proxy %>%
          clearGroup("Cancer mortality")
      }
      
      if (length(current_vars) == 0) {
        # remove panel if no variables
        proxy %>% 
          removeControl(layerId = "variable_panel")
      }
      ##### shapefile bounds #####
      if (input$showcounties) {
        html_legend = ''
        proxy <- proxy %>% 
          addPolygons(data = county.bounds, stroke = TRUE, weight = 2, color = "#0f4c5c", fill = FALSE,
                      group = "countybounds") 
      } else {
        proxy <- proxy %>% 
          clearGroup("countybounds")
      }
      
      if (input$showcities) {
        proxy <- proxy %>% 
          addPolygons(data = city.bounds, stroke = TRUE, weight = 2, color = "#e36414", fill = FALSE,
                      group = "citybounds")
      } else {
        proxy <- proxy %>% 
          clearGroup("citybounds")
      }
      
      if (input$showbounds) {
        proxy <- proxy %>% 
          addPolygons(data = tract.bounds, stroke = TRUE, weight = 1, color = "grey", fill = FALSE,
                      group = "tractbounds")
      } else {
        proxy <- proxy %>% 
          clearGroup("tractbounds")
      }
     
      # Sys.sleep(3)
    })  
  }) %>% 
    bindEvent(layer_selection_state(), input$transit, input$alc, input$superfund, input$parks, input$micro, input$cancer, input$clinics, input$ems, input$hospitals, 
                   input$pharmacies, input$wic_clinics, input$wic_retailers, input$fqhc, input$showcities, input$showcounties, input$showbounds, 
                   input$upload) 
}

# -------- CREATE SHINY APP --------
options <- list()

options(shiny.maxRequestSize = 50 * 1024^2)

if (!interactive()) {
  options$shiny.port = 3838
  options$shiny.host = "0.0.0.0"
}

shinyApp(ui = ui, server = server, options = options)
