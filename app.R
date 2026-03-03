# Geoexmap: map-based visualization tool for all things health and environment

# MC :)

library(shiny)
library(shinyjs)

library(htmltools)
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
data <- data %>% 
  mutate(across(where(is.numeric), ~na_if(., 0)))

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
                "Routine checkup in past year" = "Routine.Checkup.in.the.Past.Year",
                "Visited dentist in past year" = "Visited.Dentist.in.Past.Year",
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
                "Per- and polyfluoroalkyl substances (PFAS) in drinking water" = "PFAS_dw",
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
              "Persons exposed to noise LAeq \U2265 45-50 dB (total)" = "N.Noise.More.than.LAeq.45.to.50.db",
              "Persons exposed to noise LAeq \U2265 45-50 dB (percentage)" = "Pct.Noise.More.than.LAeq.45.to.50.db",
              "Persons exposed to noise LAeq \U2265 50-60 dB (total)" = "N.Noise.More.than.LAeq.50.to.60.db",
              "Persons exposed to noise LAeq \U2265 50-60 dB (percentage)" = "Pct.Noise.More.than.LAeq.50.to.60.db",
              "Persons exposed to noise LAeq \U2265 60-70 dB (total)" = "N.Noise.More.than.LAeq.60.to.70.db",
              "Persons exposed to noise LAeq \U2265 60-70 dB (percentage)" = "Pct.Noise.More.than.LAeq.60.to.70.db",
              "Persons exposed to noise LAeq \U2265 70-80 dB (total)" = "N.Noise.More.than.LAeq.70.to.80.db",
              "Persons exposed to noise LAeq \U2265 70-80 dB (percentage)" = "Pct.Noise.More.than.LAeq.70.to.80.db",
              "Persons exposed to noise LAeq \U2265 80-90 dB (total)" = "N.Noise.More.than.LAeq.80.to.90.db",
              "Persons exposed to noise LAeq \U2265 80-90 dB (percentage)" = "Pct.Noise.More.than.LAeq.80.to.90.db",
              "Persons exposed to noise LAeq \U2265 90 dB (total)" = "N.Noise.More.than.LAeq.90.db",
              "Persons exposed to noise LAeq \U2265 90 dB (percentage)" = "Pct.Noise.More.than.LAeq.90.db",
              "Open water" = "pct_Open_Water",
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

# TODO: take out variables according to feedback
foodenv <- c("Population (2010)" = "Pop2010",
             "Occupied Housing Units (2010)" = "OHU2010",
             "Population > 1 mile from supermarket (total)" = "lapop1",
             "Population > 1 mile from supermarket (proportion)" = "lapop1share",
             "Low-income population > 1 mile from supermarket (total)" = "lalowi1",
             "Low-income population > 1 mile from supermarket (proportion)" = "lalowi1share",
             "Children age 0-17 > 1 mile from supermarket (total)"  = "lakids1",
             "Children age 0-17 > 1 mile from supermarket (proportion)" = "lakids1share",
             "Seniors age 65+ > 1 mile from supermarket (total)" = "laseniors1",
             "Seniors age 65+ > 1 mile from supermarket (proportion)" = "laseniors1share",
             "White population > 1 mile from supermarket (total)" = "lawhite1",
             "White population > 1 mile from supermarket (proportion)" = "lawhite1share",
             "Black population > 1 mile from supermarket (total)" = "lablack1",
             "Black population > 1 mile from supermarket (proportion)" = "lablack1share",
             "Asian population > 1 mile from supermarket (total)" = "laasian1",
             "Asian population > 1 mile from supermarket (proportion)" = "laasian1share",
             "Native Hawaiian and Other Pacific Islander population > 1 mile from supermarket (total)" = "lanhopi1",
             "Native Hawaiian and Other Pacific Islander population > 1 mile from supermarket (proportion)" = "lanhopi1share",
             "American Indian and Alaska Native population > 1 mile from supermarket (total)" = "laaian1",
             "American Indian and Alaska Native population > 1 mile from supermarket (proportion)" = "laaian1share",
             "Other/Multiple race population > 1 mile from supermarket (total)" = "laomultir1",
             "Other/Multiple race population > 1 mile from supermarket (proportion)" = "laomultir1share",
             "Hispanic or Latino population > 1 mile from supermarket (total)" = "lahisp1",
             "Hispanic or Latino population > 1 mile from supermarket (proportion)" = "lahisp1share",
             "Housing units without a vehicle > 1 mile from supermarket (total)" = "lahunv1",
             "Housing units without a vehicle > 1 mile from supermarket (proportion)" = "lahunv1share",
             "Housing units receiving SNAP > 1 mile from supermarket (total)" = "lasnap1",
             "Housing units receiving SNAP > 1 mile from supermarket (proportion)" = "lasnap1share",
             "Children age 0-17 (2010)" = "TractKids",
             "Seniors age 65+ (2010)" = "TractSeniors",
             "White population (2010)" = "TractWhite",
             "Black population (2010)" = "TractBlack",
             "Asian population (2010)" = "TractAsian",
             "Native Hawaiian and Other Pacific Islander population (2010)" = "TractNHOPI",
             "American Indian and Alaska Native population (2010)" = "TractAIAN",
             "Other/Multiple race population (2010)" = "TractOMultir",
             "Hispanic or Latino population (2010)" = "TractHispanic",
             "Housing units without a vehicle (2010)" = "TractHUNV",
             "Housing units receiving SNAP (2010)" = "TractSNAP")

# define filters
health_outcomes <- df_vars %>% 
  dplyr::select(c(22:33)) 

health_behaviors <- df_vars %>% 
  dplyr::select(c(18:21)) 

health_prevention <- df_vars %>% 
  dplyr::select(c(11:17)) 

natural_env <- df_vars %>%
  dplyr::select(c(107:108, 131:134, 141:158, 161)) 

air_pol <- df_vars %>% 
  dplyr::select(c(2, 135:139)) 

built_env <- df_vars %>%
  dplyr::select(c(3:4, 109:110, 112:123, 159, 165:180)) 

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
  dplyr::select(c(11:50)) 
food_env_inp <- food_env %>% 
  st_drop_geometry()

#### DEFINE MARKDOWN FOR TIPS ####
health_out_md <- list(
  "Arthritis.among.Adults" = "- **Arthritis**: What are [risk factors for arthritis](https://www.cdc.gov/arthritis/basics/index.html)? What are ways to [treat arthritis](https://www.arthritis.org/treatments) and [manage arthritis](https://www.cdc.gov/arthritis/prevention/index.html)?",
  "Asthma.among.Adults" = "- **Asthma**: What are [risk factors for asthma](https://www.lung.org/lung-health-diseases/lung-disease-lookup/asthma/learn-about-asthma/what-causes-asthma)? What are ways to [treat asthma](https://www.nhlbi.nih.gov/health/asthma/treatment-action-plan#How-is-asthma-treated?) and [manage asthma](https://www.cdc.gov/asthma/control/index.html)?",
  "High.Blood.Pressure.among.Adults" = "- **High blood pressure**: What are [risk factors for high blood pressure](https://www.cdc.gov/high-blood-pressure/risk-factors/index.html)? What are ways to [treat high blood pressure](https://www.nhlbi.nih.gov/health/high-blood-pressure/treatment) and [control high blood pressure](https://www.heart.org/en/health-topics/high-blood-pressure/changes-you-can-make-to-manage-high-blood-pressure)?",
  "Cancer.or.Melanoma.among.Adults" = "- **Cancer**: What are [risk factors for cancer](https://www.cancer.org/cancer/risk-prevention/understanding-cancer-risk.html)? What are ways to [prevent cancer](https://www.cdc.gov/cancer/prevention/index.html)? What are ways to [treat cancer](https://www.cancer.org/cancer/managing-cancer.html) and [live well after cancer treatment](https://www.cancer.org/cancer/survivorship.html)? What are [healthy recipes and nutrition resources](https://www.cookforyourlife.org/) for people affected by cancer? How can you visit the Fred Hutch Cancer Center Survivorship Clinic to get a [Survivorship Care Plan](https://www.fredhutch.org/en/patient-care/services/survivorship/survivorship-clinic.html)?",
  "High.Cholesterol.among.Screened.Adults" = "- **High cholesterol**: What are [risk factors for high cholesterol](https://www.cdc.gov/cholesterol/risk-factors/index.html)? What are ways to [treat high cholesterol](https://www.heart.org/en/health-topics/cholesterol/prevention-and-treatment-of-high-cholesterol-hyperlipidemia) and [manage high cholesterol](https://www.heart.org/en/healthy-living/healthy-lifestyle/lifes-essential-8/how-to-control-cholesterol-fact-sheet)?",
  "COPD.among.Adults" = "- **Chronic obstructive pulmonary disease (COPD)**: What are [risk factors for COPD](https://www.nhlbi.nih.gov/health/copd/causes)? What are ways to [treat COPD](https://www.lung.org/lung-health-diseases/lung-disease-lookup/copd/treating) and [manage COPD](https://www.lung.org/lung-health-diseases/lung-disease-lookup/copd/living-with-copd)?",
  "Coronary.Heart.Disease.among.Adults" = "- **Heart disease**: What are [risk factors for heart disease](https://www.nhlbi.nih.gov/health/coronary-heart-disease/risk-factors)? What are ways to [treat heart disease](https://www.nhlbi.nih.gov/health/coronary-heart-disease/treatment) and [manage heart disease](https://www.nhlbi.nih.gov/health/coronary-heart-disease/living-with)?",
  "Depression.among.Adults" = "- **Depression**: What are [risk factors for depression](https://www.psychiatry.org/patients-families/depression/what-is-depression#section_0)? What are ways to [treat depression](https://www.cdc.gov/tobacco/campaign/tips/diseases/depression-anxiety.html#treatments) and [manage depression](https://adaa.org/understanding-anxiety/depression/tips)? ",
  "Diagnosed.Diabetes.among.Adults" = "- **Diabetes**: What are [risk factors for diabetes](https://www.cdc.gov/diabetes/risk-factors/index.html)? What are ways to [treat diabetes](https://diabetes.org/living-with-diabetes/treatment-care) and [manage diabetes](https://www.cdc.gov/diabetes/living-with/index.html)?",
  "Obesity.among.Adults" = "- **Obesity**: What are [risk factors for obesity](https://www.cdc.gov/obesity/risk-factors/risk-factors.html)? What are ways to [treat obesity](https://diabetes.org/obesity) and [manage your weight](https://www.cdc.gov/diabetes/living-with/index.html)?",
  "All.Teeth.Lost.among.Adults.65.and.Older" = "- **Tooth loss**: What are [risk factors for tooth loss](https://www.cdc.gov/oral-health/about/about-tooth-loss.html#cdc_disease_basics_population-who-is-at-risk)? What are ways to [treat tooth loss](https://www.cdc.gov/oral-health/about/about-tooth-loss.html#cdc_disease_basics_treatment-treatment-and-recovery) and [manage oral health?](https://www.cdc.gov/oral-health/prevention/oral-health-tips-for-adults.html)",
  "Stroke.among.Adults" = "- **Stroke**: What are [risk factors for stroke](https://www.cdc.gov/stroke/risk-factors/index.html)? What are ways to [treat stroke](https://www.cdc.gov/stroke/treatment/index.html) and [rehab after experiencing a stroke](https://www.stroke.org/en/life-after-stroke)?"
) 

cancer_mort_md <- markdown("What are risk factors for cancer? What are ways to prevent cancer? What are ways to treat cancer and live well after cancer treatment? What are healthy recipes and nutrition resources for people affected by cancer? How can you visit the Fred Hutch Cancer Center Survivorship Clinic to get a Survivorship Care Plan?")

health_bh_md <- list(
  "Binge.Drinking.among.Adults" = "- **Binge drinking**: What are ways that can help with [starting to drink less](https://www.cdc.gov/drink-less-be-your-best/getting-started-with-drinking-less/index.html)?",
  "Cigarette.Smoking.among.Adults" = "- **Cigarette smoking**: What are ways to [quit smoking](https://www.cdc.gov/tobacco/campaign/tips/quit-smoking/index.html)? How do I download [QuitBot](https://quitbot.net/), a free smartphone app to help quit smoking?",
  "No.Leisure.time.Physical.Activity.among.Adults" = "- **No leisure-time physical activity**: learn about ways to [get more exercise](https://www.cdc.gov/healthy-weight-growth/physical-activity/getting-started.html)",
  "Short.Sleep.Duration" = "- **Short sleep duration**: learn about ways to [get better sleep](https://www.cdc.gov/sleep/about/index.html)"
)

health_prev_md <- list("Lack.of.Health.Insurance" = "- **Lack of health insurance**: How can I [apply for Apple Health](https://www.wahealthplanfinder.org/us/en/my-account/my-coverage/learnapplehealth.html), which is the name for free or low-cost Medicaid health insurance in Washington state?",
                "Routine.Checkup.in.the.Past.Year" = "- **Routine checkup**: What are ways to [stay up to date on your preventive care](https://www.cdc.gov/chronic-disease/prevention/preventive-care.html)?",
                "Visited.Dentist.in.Past.Year" = "- **Dental care**: What are ways to [maintain dental health](https://www.cdc.gov/oral-health/prevention/oral-health-tips-for-adults.html)?",
                "Taking.Medicine.to.Control.High.Blood.Pressure" = "- **High blood pressure medication use**: What are ways to [better manage taking blood pressure medication](https://www.heart.org/en/health-topics/high-blood-pressure/changes-you-can-make-to-manage-high-blood-pressure/managing-high-blood-pressure-medications)?",
                "Cholesterol.Screening" = "- **Cholesterol screening**: What are ways to [test for cholesterol](https://www.cdc.gov/cholesterol/testing/index.html)?",
                "Mammography.Use.among.Women.50.to.74" = "- **Mammography**: What are ways to get a [mammogram](https://www.komen.org/breast-cancer/screening/), which helps to screen for breast cancer? How can I schedule a mammogram on the [Fred Hutch Mammography Van](https://www.fredhutch.org/en/patient-care/prevention/breast-cancer-screening/mammogram-van.html)?",
                "Colorectal.Cancer.Screening.among.Adults.45.to.75" = "- **Colorectal cancer screening**: What are ways to get [screened for colorectal cancer](https://www.fredhutch.org/en/research/institutes-networks-ircs/population-health-colorectal-cancer-screening-program/resources.html)? How can you use [MyGeneRisk](https://mygenerisk-colon.fredhutch.org/), a free tool to learn about your risk of developing colorectal cancer?"
                )

airpol_md_list <- list("Particulate.Matter.2.5" = "- **Particulate matter <2.5 microns in diameter (PM<sub>2.5</sub>)**: What is [PM<sub>2.5</sub>](https://www.stateofglobalair.org/pollution-sources/pm25)? What are ways to [protect yourself from air pollution](http://www.breatheasy.tips/)? How can you use the [Air Quality Index (AQI)](https://www.breatheasy.tips/#aqi), a free tool to help plan your outdoor activities and learn about unhealthy air pollution levels?",
                       "Nitrogen.dioxide" = "- **Nitrogen dioxide (NO<sub>2</sub>)**: What is [NO<sub>2</sub>](https://www.lung.org/clean-air/outdoors/what-makes-air-unhealthy/nitrogen-dioxide)? What are ways to [protect yourself from air pollution](http://www.breatheasy.tips/)? How can you use the [Air Quality Index (AQI)](https://www.breatheasy.tips/#aqi), a free tool to help plan your outdoor activities and learn about unhealthy air pollution levels?",
                       "Ozone" = "- **Ozone (O<sub>3</sub>)**: What is [O<sub>3</sub>](https://www.lung.org/clean-air/outdoors/what-makes-air-unhealthy/ozone)? What are ways to [protect yourself from air pollution](http://www.breatheasy.tips/)? How can you use the [Air Quality Index (AQI)](https://www.breatheasy.tips/#aqi), a free tool to help plan your outdoor activities and learn about unhealthy air pollution levels?",
                       "Carbon.monoxide" = "- **Carbon monoxide (CO)**: What is [CO](https://www.lung.org/clean-air/indoor-air/indoor-air-pollutants/carbon-monoxide)? What are ways to [protect yourself from air pollution](http://www.breatheasy.tips/)? How can you use the [Air Quality Index (AQI)](https://www.breatheasy.tips/#aqi), a free tool to help plan your outdoor activities and learn about unhealthy air pollution levels?",
                       "Sulfur.dioxide" = "- **Sulfur dioxide (SO<sub>2</sub>)**: What is [SO<sub>2</sub>](https://www.lung.org/clean-air/outdoors/what-makes-air-unhealthy/sulfur-dioxide)? What are ways to [protect yourself from air pollution](http://www.breatheasy.tips/)? How can you use the [Air Quality Index (AQI)](https://www.breatheasy.tips/#aqi), a free tool to help plan your outdoor activities and learn about unhealthy air pollution levels?",
                       "Wildfire.smoke" = "- **Wildfire smoke**: What is [wildfire smoke](https://ecology.wa.gov/air-climate/air-quality/smoke-fire/wildfire-smoke)? What are ways to [protect yourself from air pollution](http://www.breatheasy.tips/)? How can you use the [Air Quality Index (AQI)](https://www.breatheasy.tips/#aqi), a free tool to help plan your outdoor activities and learn about unhealthy air pollution levels? How can you learn about the current [wildfire smoke forecast](https://airqualitymap.ecology.wa.gov/?view=forecast) in your area (select <u>**Smoke Forecast**</u> from the View menu)? How can you [prepare for wildfires](https://doh.wa.gov/emergencies/be-prepared-be-safe/severe-weather-and-natural-disasters/wildfires)?"
                       )

# all natural disaster events have same key--use to avoid redundancy and repeated same tip
nat_dis_key <- list(nat_disaster = "- **Extreme weather events and natural disasters**: What can you do before, during, and after an [extreme weather event or natural disaster](https://doh.wa.gov/emergencies/be-prepared-be-safe/severe-weather-and-natural-disasters)?",
                    temperature = "- **Temperature**: What are ways to help with [heat waves](https://www.cdc.gov/heat-health/about/index.html?CDC_AA_refVal=https%3A%2F%2Fwww.cdc.gov%2Fextreme-heat%2Fabout%2Findex.html)? How can you find [extreme heat cooling centers](https://search.wa211.org/search?location=&query=TH-2600.1900-180&query_type=taxonomy&query_label=Extreme+Heat+Cooling+Centers) in your area? What are ways to help with [cold spells](https://www.cdc.gov/winter-weather/safety/stay-safe-during-after-a-winter-storm-safety.html)?")

nat_md_list <- list("UV.Index" = "- **Ultraviolet radiation (UV)**: What is [UV](https://www.cdc.gov/radiation-health/data-research/facts-stats/ultraviolet-radiation.html)? What are ways to help with [sun safety](https://www.cdc.gov/skin-cancer/sun-safety/index.html)?",
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
                 "Winter.Weather.Risk.Score" = "nat_disaster",
                 "Maximum.temperature" = "temperature",
                 "Minimum.temperature" = "temperature",
                 "Average.temperature" = "temperature",
                 "Precipitation" = "- **Precipitation**: What to do before, during, and after a [flood](https://doh.wa.gov/emergencies/be-prepared-be-safe/severe-weather-and-natural-disasters/floods)?",
                 "Radon" = "- **Radon**: What is [radon and ways to test for radon in your home](https://doh.wa.gov/community-and-environment/contaminants/radon)?",
                 "PFAS_dw" = "- **Per- and polyfluoroalkyl substances (PFAS) in drinking water**: What are [PFAS and ways to reduce your exposure to PFAS](https://doh.wa.gov/community-and-environment/contaminants/pfas)?"
                 )

built_noise_key <- list(noise = "- **Noise**: What is [noise](https://www.who.int/tools/compendium-on-health-and-environment/environmental-noise) that comes from the environment? What are [health effects of noise](https://doh.wa.gov/community-and-environment/noise)?",
                        land = "- **Land use and land cover**: What is [land use and land cover](https://oceanservice.noaa.gov/facts/lclu.html)?")

built_md_list <- list("Walkability" = "- **Neighborhood walkability**: What is [walkability](https://usafacts.org/articles/what-is-walkability-what-does-the-government-spend-on-it/)? What are [health benefits of walking](https://www.heart.org/en/healthy-living/fitness/walking/why-is-walking-the-most-popular-form-of-exercise)?",
                      "Pesticide.Exposure" = "- **Agricultural pesticide use**: What are [pesticides](https://doh.wa.gov/community-and-environment/contaminants/pesticides)? What are ways to reduce pesticide exposure from [foods](https://www.epa.gov/safepestcontrol/pesticides-and-food-healthy-sensible-food-practices) and during [usage](https://icash.public-health.uiowa.edu/wp-content/uploads/2017/02/UO218.pdf)?",
                      "Green.Space" = "- **Green space**: What is [green space](https://www.countyhealthrankings.org/strategies-and-solutions/what-works-for-health/strategies/green-space-parks)? What are [health benefits of green space](https://www.countyhealthrankings.org/strategies-and-solutions/what-works-for-health/strategies/green-space-parks)?",
                      "bluespace" = "- **Blue space**: [Blue space](https://pubmed.ncbi.nlm.nih.gov/32971082/) is any water body such as ponds, lakes, rivers, and oceans. What are [health benefits of blue space](https://www.apa.org/monitor/2020/04/nurtured-nature)?",
                      "Nighttime.Radiance" = "- **Outdoor light at night**: What is [outdoor light at night](), which is also known as light pollution? What are [health effects of outdoor light at night](https://journalofethics.ama-assn.org/article/were-all-healthier-under-starry-sky/2024-10#:~:text=Blue%20wavelengths%20of%20light%20are,to%20many%20kinds%20of%20illness.)?",
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
                      "Pct.Noise.More.than.LAeq.90.db" = "noise",
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
                        - **Food environment/healthy food**: How can you find [local healthy foods](https://www.usdalocalfoodportal.com/) in your area such as farmers markets?
                        ")

crime_md <- markdown("
                     - **Crime**: Where can you learn more information about [crimes in Washington state](https://nibrs.fbi.gov/2024/) from the Federal Bureau of Investigation?
                     ")

soc_md_list <- list("Food.Insecurity" = "- **Food insecurity**: Call 2-1-1 or text '211WAOD' to 898211 for nearby food banks and free meals from the [Washington helpline](https://search.wa211.org/). Call 1-866-HUNGRY for food assistance programs from the [National Hunger Hotline](https://www.hungerfreeamerica.org/en-us/national-hunger-hotline). Find the closest food bank or meal program from [Feeding Washington](https://feedingwashington.org/find-food/). Find other available resources from [Washington Connection](https://www.washingtonconnection.org/home/exploreoptions.go).",
               "Housing.Insecurity" = "- **Housing insecurity**: Call 2-1-1 or text '211WAOD' to 898211 for housing resources from the [Washington helpline](https://search.wa211.org/). Find other available resources, including emergency housing, from the [Washington State Department of Social and Health Services](https://www.dshs.wa.gov/esa/community-services-offices/housing-resources) and [Washington Connection](https://www.washingtonconnection.org/home/exploreoptions.go).",
               "Utility.Services.Threat" = "- **Utility services threat**: Call 2-1-1 or text '211WAOD' to 898211 for help with utilities from the [Washington helpline](https://search.wa211.org/). Find other available resources, including energy assistance programs, from the [Washington Utilities and Transportation Commission](https://www.utc.wa.gov/consumers/energy/energy-assistance-programs) and [Washington Connection](https://www.washingtonconnection.org/home/exploreoptions.go).",
               "Lacking.Reliable.Transportation" = "- **Lack of reliable transportation**: Call 2-1-1 or text '211WAOD' to 898211 for help with transportation from the [Washington helpline](https://search.wa211.org/). Find other available resources from [Washington Connection](https://www.washingtonconnection.org/home/exploreoptions.go).",
               "No.broadband.internet" = "- **No internet**: Call 2-1-1 or text '211WAOD' to 898211 for help with getting internet from the [Washington helpline](https://search.wa211.org/). Find other available resources from [Washington Connection](https://www.washingtonconnection.org/home/exploreoptions.go).",
               "Crowding" = "- **Household crowding**: Call 2-1-1 or text '211WAOD' to 898211 for housing resources from the [Washington helpline](https://search.wa211.org/). Find other available resources, including emergency housing, from the [Washington State Department of Social and Health Services](https://www.dshs.wa.gov/esa/community-services-offices/housing-resources) and [Washington Connection](https://www.washingtonconnection.org/home/exploreoptions.go).",
               "Housing.cost.burden" = "- **Housing cost burden**: Call 2-1-1 or text '211WAOD' to 898211 for housing resources from the [Washington helpline](https://search.wa211.org/). Find other available resources, including emergency housing, from the [Washington State Department of Social and Health Services](https://www.dshs.wa.gov/esa/community-services-offices/housing-resources) and [Washington Connection](https://www.washingtonconnection.org/home/exploreoptions.go).",
               "No.high.school.diploma" = "- **No high school education**: Call 2-1-1 or text '211WAOD' to 898211 for education resources from the [Washington helpline](https://search.wa211.org/). Find other available resources from [Washington Connection](https://www.washingtonconnection.org/home/exploreoptions.go).",
               "Poverty" = "- **People living below 150% of the poverty level**: 150% of the poverty level: Call 2-1-1 or text '211WAOD' to 898211 for financial resources from the [Washington helpline](https://search.wa211.org/). Find other available resources from [Washington Connection](https://www.washingtonconnection.org/home/exploreoptions.go).",
               "Single.parent.households" = "- Single parent households: Find resources for families, including childcare, from the [Washington State Department of Children, Youth, and Families](https://dcyf.wa.gov/services/housing-basic-needs) and [Washington Connection](https://www.washingtonconnection.org/home/exploreoptions.go).",
               "Unemployment" = "- **Unemployment**: Call 2-1-1 or text '211WAOD' to 898211 for employment resources from the [Washington helpline](https://search.wa211.org/). Find other available resources from [Washington Connection](https://www.washingtonconnection.org/home/exploreoptions.go).",
               "Environmental.Justice.Index" = "- **Environmental Justice Index (EJI)*: What is the [Environmental Justice Index](https://www.atsdr.cdc.gov/place-health/php/eji/eji-frequently-asked-questions-faqs.html)?",
               "Social.Vulnerability.Index" = "- **Social Vulnerability Index (SVI)**: What is the [Social Vulnerability Index](https://www.atsdr.cdc.gov/place-health/php/svi/svi-frequently-asked-questions-faqs.html)",
               "Median.HH.Income" = "- **Median household income**: What does a [median household income](https://usafacts.org/answers/what-is-the-income-of-a-us-household/country/united-states/) mean?",
               "HT_Index" = "- **Housing and Transportation (H + T\U00AE) Affordability Index**: Why is it important to consider [transportation costs with affordability](https://cnt.org/tools/housing-and-transportation-affordability-index)?",
               "Racial.Residential.Segregation" = "- **Residential segregation**: What is the [Dissimilarity Index](https://www.khanacademy.org/test-prep/mcat/social-inequality/social-class/v/residential-segregation), which is a measure of residential segregation?",
               "Historic.Redlining.Score" = "- **Redlining**: What is [redlining](https://education.nationalgeographic.org/resource/mapmaker-redlining-united-states/)?",
               "social_capital" = "- **Social capital**: What is [social capital](https://aspe.hhs.gov/sites/default/files/private/pdf/263491/What-is-social-capital.pdf)?",
               "Population.density" = "- **Urbanicity/rurality**: What are resources to improve health and healthcare in [rural communities](https://doh.wa.gov/public-health-provider-resources/rural-health)?"
               )

# -------- UI ELEMENTS --------
categories <- accordion(
  open = FALSE,
  accordion_panel(
    HTML("<b>Sociodemographics</b>"), icon = bs_icon("person-vcard"),
    selectInput('sociodemo', "Select variables",
                sociodemographics,
                selectize = TRUE, multiple = TRUE),
    accordion_panel("Race and Ethnicity", selectInput('race', NULL, racev, selectize = TRUE, multiple = TRUE)),
    accordion_panel("Sex", selectInput('sex', NULL, sexv, selectize = TRUE, multiple = TRUE)),
    accordion_panel("Age", selectInput('age', NULL, agev, selectize = TRUE, multiple = TRUE))
    
  ),
  accordion_panel(
    HTML("<b>Health Outcomes</b>"), icon = bs_icon("heart-pulse"),
    selectInput('outcomes', 
                htmltools::span("Select variables",
                     popover(bs_icon("lightbulb"),
                             "Select one or more health outcomes to see tips.",
                             title = "Tips",
                             placement = "right",
                             id = "outcome_popover")), outcomes, selectize = TRUE, multiple = TRUE),
    # options to filter by cancer site, stage at diagnosis, gender
    accordion_panel("Cancer Incidence",
                    selectInput('incsite', "Cancer Site", choices = c("Please choose a site" = "", unique(wscr.inc$Cancer.Site)), selectize = TRUE, selected = ""),
                    selectInput('incstage', "Stage at Diagnosis", choices = c("Please choose a stage" = "", unique(wscr.inc$Stage.At.Diagnosis)), selectize = TRUE, selected = ""),
                    selectInput('incsex', "Sex", choices = c("Please choose a sex" = "", unique(wscr.inc$Gender)), selectize = TRUE, selected = ""),
                    actionButton('incbutton', "Reset filters")),
    # options to filter by cancer site, gender
    accordion_panel("Cancer Mortality",
                    selectInput('mortsite', "Cancer Site", choices = c("Please choose a site" = "", unique(wscr.mort$Cancer.Site)), selectize = TRUE, selected = ""),
                    selectInput('mortsex', "Sex", choices = c("Please choose a sex" = "", unique(wscr.mort$Gender)), selectize = TRUE, selected = ""),
                    actionButton('mortbutton', "Reset filters"))
  ),
  accordion_panel(
    HTML("<b>Health Behaviors</b>"), icon = bs_icon("person-walking"),
    selectInput('behaviors', htmltools::span("Select variables", 
                                  popover(bs_icon("lightbulb"),
                                          "Select one or more health behaviors to see tips.",
                                          title = "Tips",
                                          placement = "right",
                                          id = "behavior_popover")), behaviors,
                 multiple = TRUE, selectize = TRUE, selected = "")
  ),
  accordion_panel(
    HTML("<b>Prevention</b>"), icon = tags$img(src = "/prevention.png", height = "20.48px", width = "20.48px"),
    selectInput('prevention', htmltools::span("Select variables", 
                                      popover(bs_icon("lightbulb"),
                                              # tags$img(src = "/tips [4].png", height = "32px", width = "32px"),
                                              "Select one or more prevention measures to see tips.",
                                              title = "Tips",
                                              placement = "right",
                                              id = "prevention_popover")), prevention, selectize = TRUE, multiple = TRUE)
  ),
  accordion_panel(
    HTML("<b>Healthcare Access</b>"), icon = bs_icon("building-add"),
    h6("Select features", htmltools::span(popover(bs_icon("lightbulb"),
            "Switch on one or more healthcare access features to see tips.",
            title = "Tips",
            placement = "right",
            id = "healthaccpopover"))),
    input_switch('cancer', "Commission on Cancer (Coc)-accredited programs ", value = FALSE),
    input_switch('clinics', "Clinics", value = FALSE), 
    input_switch('ems', "Emergency medical stations", value = FALSE),
    input_switch('hospitals', "Hospitals", value = FALSE),
    input_switch('pharmacies', "Pharmacies", value = FALSE),
    input_switch('wic_clinics', "Nutrition Program for Women, Infancts, and Childeren (WIC) clinics", value = FALSE),
    input_switch('wic_retailers', "WIC retailers", value = FALSE),
    input_switch('fqhc', "Federally qualified health centers (FQHCs)", value = FALSE)
  ),
  accordion_panel(
    HTML("<b>Natural Environment</b>"), icon = bs_icon("sun"),
    selectInput('naturalenv', htmltools::span("Select variables", 
                                              popover(bs_icon("lightbulb"),
                                                      "Select one or more natural environment measures to see tips.",
                                                      title = "Tips",
                                                      placement = "right",
                                                      id = "natenvpopover")), naturalenv, selectize = TRUE, multiple = TRUE),
    input_switch('microplastics', "Microplastics", value = FALSE),
    div(id = 'micro_div', selectInput('micro', '', choices = c("Please choose a marine setting" = "", unique(microplastics$Marine.Setting)), selectize = TRUE, multiple = TRUE)),
    accordion_panel("Air pollutants", icon = bs_icon("cloud-haze"),
                   selectInput('airpol', htmltools::span("Select variables",
                                                         popover(bs_icon("question-circle"),
                                                                 "Select one or more air pollutants to see tips.",
                                                                 title = "Tips",
                                                                 placement = "right",
                                                                 id = "airpopover")), airpol, selectize = TRUE, multiple = TRUE))
  ),
  accordion_panel(
    HTML("<b>Built Environment</b>"), icon = bs_icon("buildings"),
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
    accordion_panel(
      "Food Environment", icon = bs_icon("basket"),
      selectInput('foodenv', label = htmltools::span("Select variables", 
                                 popover(bs_icon("lightbulb"),
                                         food_env_md,
                                         title = "Tips",
                                         placement = "right")), foodenv, selectize = TRUE, multiple = TRUE)
    )
  ),
  accordion_panel(
    HTML("<b>Social Environment</b>"), icon = tags$img(src = "/social-environment.png", height = "20.48px", width = "20.48px"),
    selectInput('socialenv', htmltools::span("Select variables", 
                                          popover(bs_icon("lightbulb"),
                                                  "Select one or more social environment measures to see tips.",
                                                  title = "Tips",
                                                  placement = "right",
                                                  id = "socenvpopover")), socialenv, selectize = TRUE,  multiple = TRUE),
    accordion_panel('Crime', icon = bs_icon('file-earmark-lock'),
                    selectInput('crime', htmltools::span("Select variables",
                                                         popover(bs_icon("lightbulb"),
                                                                 crime_md,
                                                                 title = "Tips",
                                                                 placement = "right")), crimeenv, selectize = TRUE, multiple = TRUE))
  ),
  accordion_panel(
    HTML("<b>Options</b>"), icon = bs_icon("gear"),
    input_switch("showbounds", "Show tract boundaries", value = TRUE),
    input_switch("showcounties", "Show county boundaries", value = FALSE),
    input_switch("showcities", "Show city boundaries", value = FALSE),
    input_switch("showchart", "Show graph", value = FALSE),
    fileInput("upload", "Upload a shapefile", accept = ".shp")#,
    #downloadButton("download", "Download data")
  )
  
)

# define table categories
table.cats <- accordion(
  open = FALSE,
  accordion_panel(
    "Sociodemographics", icon = bs_icon("person-vcard"),
    selectInput('sociodemo_tab', "Select variables",
                sociodemographics,
                selectize = TRUE, multiple = TRUE),
    accordion_panel("Race and Ethnicity", selectInput('race_tab', NULL, racev, selectize = TRUE, multiple = TRUE)),
    accordion_panel("Sex", selectInput('sex_tab', NULL, sexv, selectize = TRUE, multiple = TRUE)),
    accordion_panel("Age", selectInput('age_tab', NULL, agev, selectize = TRUE, multiple = TRUE))
    
  ),
  accordion_panel(
    "Health Outcomes", icon = bs_icon("heart-pulse"),
    selectInput('outcomes_tab', 
                htmltools::span("Select variables",
                                popover(bs_icon("lightbulb"),
                                        "Select one or more health outcomes to see tips.",
                                        title = "Tips",
                                        placement = "right",
                                        id = "outcome_popover")), outcomes, selectize = TRUE, multiple = TRUE),
    # options to filter by cancer site, stage at diagnosis, gender
    accordion_panel("Cancer Incidence",
                    selectInput('incsite_tab', "Cancer Site", choices = c("Please choose a site" = "", unique(wscr.inc$Cancer.Site)), selectize = TRUE, selected = ""),
                    selectInput('incstage_tab', "Stage at Diagnosis", choices = c("Please choose a stage" = "", unique(wscr.inc$Stage.At.Diagnosis)), selectize = TRUE, selected = ""),
                    selectInput('incsex_tab', "Sex", choices = c("Please choose a sex" = "", unique(wscr.inc$Gender)), selectize = TRUE, selected = ""),
                    actionButton('incbutton_tab', "Reset filters")),
    accordion_panel("Cancer Mortality",
                    selectInput('mortsite_tab', "Cancer Site", choices = c("Please choose a site" = "", unique(wscr.mort$Cancer.Site)), selectize = TRUE, selected = ""),
                    selectInput('mortsex_tab', "Sex", choices = c("Please choose a sex" = "", unique(wscr.mort$Gender)), selectize = TRUE, selected = ""),
                    actionButton('mortbutton_tab', "Reset filters"))
  ),
  accordion_panel(
    "Health Behaviors", icon = bs_icon("person-walking"),
    selectInput('behaviors_tab', htmltools::span("Select variables", 
                                             popover(bs_icon("lightbulb"),
                                                     "Select one or more health behaviors to see tips.",
                                                     title = "Tips",
                                                     placement = "right",
                                                     id = "behavior_popover")), behaviors,
                multiple = TRUE, selectize = TRUE)
  ),
  accordion_panel(
    "Prevention", icon = tags$img(src = "/prevention.png", height = "20.48px", width = "20.48px"),
    selectInput('prevention_tab', htmltools::span("Select variables", 
                                              popover(bs_icon("lightbulb"),
                                                      # tags$img(src = "/tips [4].png", height = "32px", width = "32px"),
                                                      "Select one or more prevention measures to see tips.",
                                                      title = "Tips",
                                                      placement = "right",
                                                      id = "prevention_popover")), prevention, selectize = TRUE, multiple = TRUE)
  ),
  accordion_panel(
    "Natural Environment", icon = bs_icon("sun"),
    selectInput('naturalenv_tab', htmltools::span("Select variables", 
                                              popover(bs_icon("lightbulb"),
                                                      "Select one or more natural environment measures to see tips.",
                                                      title = "Tips",
                                                      placement = "right",
                                                      id = "natenvpopover")), naturalenv, selectize = TRUE, multiple = TRUE),
    accordion_panel("Air pollutants", icon = bs_icon("cloud-haze"),
                    selectInput('airpol_tab', htmltools::span("Select variables",
                                                          popover(bs_icon("question-circle"),
                                                                  "Select one or more air pollutants to see tips.",
                                                                  title = "Tips",
                                                                  placement = "right",
                                                                  id = "airpopover")), airpol, selectize = TRUE, multiple = TRUE))
  ),
  accordion_panel(
    "Built Environment", icon = bs_icon("buildings"),
    selectInput('builtenv_tab', htmltools::span("Select variables", 
                                            popover(bs_icon("lightbulb"),
                                                    "Select one or more built environment measures to see tips.",
                                                    title = "Tips",
                                                    placement = "right",
                                                    id = "builtenvpopover")), builtenv, selectize = TRUE, multiple = TRUE),
   accordion_panel(
      "Food Environment", icon = bs_icon("basket"),
      selectInput('foodenv_tab', label = htmltools::span("Select variables", 
                                                     popover(bs_icon("lightbulb"),
                                                             food_env_md,
                                                             title = "Tips",
                                                             placement = "right")), foodenv, selectize = TRUE, multiple = TRUE)
    )
  ),
  accordion_panel(
    "Social Environment", icon = tags$img(src = "/social-environment.png", height = "20.48px", width = "20.48px"),
    selectInput('socialenv', htmltools::span("Select variables", 
                                             popover(bs_icon("lightbulb"),
                                                     "Select one or more social environment measures to see tips.",
                                                     title = "Tips",
                                                     placement = "right",
                                                     id = "socenvpopover")), socialenv, selectize = TRUE,  multiple = TRUE),
    accordion_panel('Crime', icon = bs_icon('file-earmark-lock'),
                    selectInput('crime_tab', htmltools::span("Select variables",
                                                         popover(bs_icon("lightbulb"),
                                                                 crime_md,
                                                                 title = "Tips",
                                                                 placement = "right")), crimeenv, selectize = TRUE, multiple = TRUE))
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
                    "Parks" = "parks",
                    "Superfund sites" = "superfund") 

# -------- UI LAYOUT --------
ui <- page_navbar(
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(HTML("
    .leaflet-control.my-centered-num-legend {
      background: white;
      border: 1px solid #ccc;
      border-radius: 4px;
      padding: 4px 6px;
    }
    

    /* Keep label centering without touching fill colors */
    .leaflet-control.my-centered-num-legend text {
      text-anchor: middle;
      fill: #333;              /* text color */
    }
  ")),
    tags$script(HTML("
    document.addEventListener('shown.bs.modal', function(){}); // no-op to ensure BS is loaded

    document.addEventListener('DOMContentLoaded', function () {
      var tooltipTriggerList = [].slice.call(
        document.querySelectorAll('[data-bs-toggle=\"tooltip\"]')
      );
      tooltipTriggerList.forEach(function (tooltipTriggerEl) {
        new bootstrap.Tooltip(tooltipTriggerEl);
      });
    });

    // Also re-initialize when Leaflet redraws legends:
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
  "))
  ),
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
              sidebar = sidebar(table.cats, 
                                width = "400px"),
              accordion_panel("Census Tract Data", accordion_panel("2020 Census Tracts", reactableOutput("ct_table"), downloadButton('downloadcttab', "Download .csv")),
                              accordion_panel("2010 Census Tracts", reactableOutput("food_table"))),
              accordion_panel("County Data", 
                              accordion_panel("Crime", reactableOutput("cnty_crime_table"), downloadButton('downloadcntycrime', "Download .csv")),
                              accordion_panel("Cancer Incidence", reactableOutput("cnty_inc_table"), downloadButton('downloadcntyinc', "Download .csv")),
                              accordion_panel("Cancer Mortality", reactableOutput("cnty_mort_table"), downloadButton('downloadcntymort', "Download .csv"))),
              accordion_panel("Standalone Data", selectInput('standalone', "", choices = standalone_tab), reactableOutput("standalone_table"))
              
            )),
  nav_panel("Documentation",
            h2("Version History"),
            h2("Technical Documentation"),
            a("geoexmap_technical_documentation.pdf", target = "_blank", href = "geoexmap_technical_documentation.pdf")),
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
  
  #### DYNAMIC TIP LOGIC ####
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
  
  observeEvent(list(input$naturalenv, input$microplastics), {
    update_popover(
      "natenvpopover",
      content = markdown(nat_popover_md())
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
        return(colorFactor(
          palette = c("#780000", "#fdf0d5"), domain = domain,
          levels = c(TRUE, FALSE)
        ))
      }
      
      return(colorNumeric(palette = base_ramp, domain = domain, na.color = "transparent"))
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
    if(col == "Particulate.Matter.2.5") return(HTML(paste0("PM<sub>2.5</sub> concentrations in 2022 ", "(\U03BC", "g/m<sup>3</sup>)")))
    if(col == "Green.Space") return("Normalized difference vegetation index (NDVI) in July 2024")
    if(col == "Nighttime.Radiance") return(HTML("Light at night (nW/cm<sup>2</sup>/sr)"))
    if(col == "Food.Stamps") return("SNAP benefits in 2023 (%)")
    if(col == "Food.Insecurity") return("Food insecurity in 2023 (%)")
    if(col == "Housing.Insecurity") return("Housing insecurity in 2023 (%)")
    if(col == "Utility.Services.Threat") return("Utility services threat in 2023 (%)")
    if(col == "Lacking.Reliable.Transportation") return("Lack of reliable transportation in 2023 (%)")
    if(col == "Lack.of.Social.and.Emotional.Support") return("Lack of social and emotional support in 2023 (%)")
    if(col == "Lack.of.Health.Insurance") return("No health insurance in 2021 (%)")
    if(col == "Routine.Checkup.in.the.Past.Year") return("Routine checkup in past year in 2023 (%)")
    if(col == "Visited.Dentist.in.Past.Year") return("Visited dentist in past year in 2023 (%)")
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
    if(col == "Cancer.or.Melanoma.among.Adults") return("Cancer prevalence among adults in 2023 (%)")
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
    if(col == "American.Indian.Alaska.Native.NonHispanic") return("Non-Hispanic American Indian and Alaska Native population in 2023 (total)")
    if(col == "Percent.American.Indian.Alaska.Native.NonHispanic") return("Non-Hispanic American Indian and Alaska Native population in 2023 (%)")
    if(col == "Asian.NonHispanic") return("Non-Hispanic Asian population in 2023 (total)")
    if(col == "Percent.Asian.NonHispanic") return("Non-Hispanic Asian population in 2023 (%)")
    if(col == "Native.Hawaiian.Pacific.Islander.NonHispanic") return("Non-Hispanic Native Hawaiian and other Pacific Islander population in 2023 (total)")
    if(col == "Percent.Native.Hawaiian.Pacific.Islander.NonHispanic") return("Non-Hispanic Native Hawaiian and other Pacific Islander population in 2023 (%)")
    if(col == "Other.Race.NonHispanic") return("Non-Hispanic other race population in 2023 (total)")
    if(col == "Percent.Other.Race.NonHispanic") return("Non-Hispanic other race population in 2023 (%)")
    if(col == "Two.or.More.Races.NonHispanic") return("Non-Hispanic two or more races population in 2023 (total)")
    if(col == "Percent.Two.or.More.Races.NonHispanic") return("Non-Hispanic two or more races population in 2023 (%)")
    
    # hispanic or latino subcats
    if(col == "White.Hispanic.or.Latino") return("Hispanic or Latino White (total)")
    if(col == "Percent.White.Hispanic.or.Latino") return("Hispanic or Latino White (%)")
    if(col == "Black.Hispanic.or.Latino") return("Hispanic or Latino Black(total)")
    if(col == "Percent.Black.Hispanic.or.Latino") return("Hispanic or Latino Black (%)")
    if(col == "American.Indian.Alaska.Native.Hispanic.or.Latino") return("Hispanic or Latino American Indian and Alaska Native (total)")
    if(col == "Percent.American.Indian.Alaska.Native.Hispanic.or.Latino") return("Hispanic or Latino American Indian and Alaska Native (%)")
    if(col == "Asian.Hispanic.or.Latino") return("Hispanic or Latino Asian (total)")
    if(col == "Percent.Asian.Hispanic.or.Latino") return("Hispanic or Latino Asian (%)")
    if(col == "Native.Hawaiian.Pacific.Islander.Hispanic.or.Latino") return("Hispanic or Latino Native Hawaiian and other Pacific Islander (total)")
    if(col == "Percent.Native.Hawaiian.Pacific.Islander.Hispanic.or.Latino") return("Hispanic or Latino Native Hawaiian and other Pacific Islander (%)")
    if(col == "Other.Race.Hispanic.or.Latino") return("Hispanic or Latino other race (total)")
    if(col == "Percent.Other.Race.Hispanic.or.Latino") return("Hispanic or Latino other race (%)")
    if(col == "Two.or.More.Races.Hispanic.or.Latino") return("Hispanic or Latino (total)")
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
    
    if(col == "Social.Vulnerability.Index") return("SVI in 2022")
    if(col == "Environmental.Justice.Index") return("EJI in 2019")
    if(col == "Unemployment") return("Unemployment in 2021 (%)")
    
    if(col == "UV.Index") return("UVI in 2024")
    
    if(col == "Radon") return(HTML("Radon gas concentration in 2021 (Bq/m<sup>3</sup>)"))
    
    if(col == "Pesticide.Exposure") return(HTML("Agricultural pesticide use in 2023 (lbs/mi<sup>2</sup>)"))
    
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
    
    if(col == "Walkability") return("Walkability score in 2019")
    
    if(col == "No.broadband.internet") return("No internet in 2021 (%)")
    if(col == "No.high.school.diploma") return("No high school diploma in 2021 (%)")
    if(col == "Single.parent.households") return("Single parent households in 2021 (%)")
    if(col == "Crowding") return("Crowding among households in 2021 (%)")
    if(col == "Poverty") return("Poverty in 2021 (%)")
    if(col == "Housing.cost.burden") return("Housing cost burden in 2021 (%)")
    
    if(col == "Dew.point") return(paste0("Dew point ", "(\U00B0", "F)"))
    if(col == "Maximum.temperature") return(paste0("Maximum temperature ", "(\U00B0", "F)"))
    if(col == "Minimum.temperature") return(paste0("Minimum temperature ", "(\U00B0", "F)"))
    if(col == "Average.temperature") return(paste0("Average temperature ", "(\U00B0", "F)"))
    if(col == "Precipitation") return("Precipitation (in.)")
    
    if(col == "Wildfire.smoke") return(HTML(paste0("Wildfire smoke PM<sub>2.5</sub> in 2020 ", "(\U03BC", "g/m<sup>3</sup>)")))
    if(col == "Nitrogen.dioxide") return(HTML(paste0("Nitrogen dioxide (NO<sub>2</sub>) (ppb)")))
    if(col == "Sulfur.dioxide") return(HTML("Sulfur dioxide (SO<sub>2</sub>) (ppb)"))
    if(col == "Carbon.monoxide") return(HTML("Carbon monoxide (CO) (ppm)"))
    if(col == "Ozone") return(HTML("Ozone (O<sub>3</sub>) (ppb)"))
    
    if(col == "Population.density") return("Population density in 2023 (population per square mile)")
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
    
    if(col == "bluespace") return("Blue space coverage (% in tract)")
    if(col == "social_capital") return("Social capital")
    
    if(col == "PFAS_dw") return("PFAS in drinking water in 2021")
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
    
    if(col == "total_p2") return("Part II offenses")
    if(col == "p2_rate") return("Part II offenses (per 1,000 population)")
    
    # TODO: add legend titles for food environment columns
      
  }
  
  layer.titles <- function(col) {
    if(col == "Particulate.Matter.2.5") return(HTML(paste0("PM<sub>2.5</sub> ", "(\U03BC", "g/m<sup>3</sup>)")))
    if(col == "Green.Space") return("Normalized Difference Vegetation Index (NDVI)")
    if(col == "Nighttime.Radiance") return(HTML("Light at Night (nW/cm<sup>2</sup>/sr)"))
    if(col == "Food.Stamps") return("SNAP benefits  (%)")
    if(col == "Food.Insecurity") return("Food insecurity (%)")
    if(col == "Housing.Insecurity") return("Housing insecurity (%)")
    if(col == "Utility.Services.Threat") return("Utility services threat (%)")
    if(col == "Lacking.Reliable.Transportation") return("Lack of reliable transportation (%)")
    if(col == "Lack.of.Social.and.Emotional.Support") return("Lack of social and emotional support (%)")
    if(col == "Lack.of.Health.Insurance") return("No health insurance (%)")
    if(col == "Routine.Checkup.in.the.Past.Year") return("Routine checkup in past year (%)")
    if(col == "Visited.Dentist.in.Past.Year") return("Visited dentist in past year (%)")
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
    if(col == "Cancer.or.Melanoma.among.Adults") return("Cancer or melanoma among adults (%)")
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
    if(col == "American.Indian.Alaska.Native.NonHispanic") return("Non-Hispanic American Indian and Alaska Native population (total)")
    if(col == "Percent.American.Indian.Alaska.Native.NonHispanic") return("Non-Hispanic American Indian and Alaska Native population (%)")
    if(col == "Asian.NonHispanic") return("Non-Hispanic Asian population (total)")
    if(col == "Percent.Asian.NonHispanic") return("Non-Hispanic Asian population (%)")
    if(col == "Native.Hawaiian.Pacific.Islander.NonHispanic") return("Non-Hispanic Native Hawaiian and other Pacific Islander population (total)")
    if(col == "Percent.Native.Hawaiian.Pacific.Islander.NonHispanic") return("Non-Hispanic Native Hawaiian and other Pacific Islander population (%)")
    if(col == "Other.Race.NonHispanic") return("Non-Hispanic other race population (total)")
    if(col == "Percent.Other.Race.NonHispanic") return("Non-Hispanic other race population (%)")
    if(col == "Two.or.More.Races.NonHispanic") return("Non-Hispanic two or more races population (total)")
    if(col == "Percent.Two.or.More.Races.NonHispanic") return("Non-Hispanic two or more races population (%)")
    
    # hispanic or latino subcats
    if(col == "White.Hispanic.or.Latino") return("Hispanic or Latino White (total)")
    if(col == "Percent.White.Hispanic.or.Latino") return("Hispanic or Latino White (%)")
    if(col == "Black.Hispanic.or.Latino") return("Hispanic or Latino Black(total)")
    if(col == "Percent.Black.Hispanic.or.Latino") return("Hispanic or Latino Black (%)")
    if(col == "American.Indian.Alaska.Native.Hispanic.or.Latino") return("Hispanic or Latino American Indian and Alaska Native (total)")
    if(col == "Percent.American.Indian.Alaska.Native.Hispanic.or.Latino") return("Hispanic or Latino American Indian and Alaska Native (%)")
    if(col == "Asian.Hispanic.or.Latino") return("Hispanic or Latino Asian (total)")
    if(col == "Percent.Asian.Hispanic.or.Latino") return("Hispanic or Latino Asian (%)")
    if(col == "Native.Hawaiian.Pacific.Islander.Hispanic.or.Latino") return("Hispanic or Latino Native Hawaiian and other Pacific Islander (total)")
    if(col == "Percent.Native.Hawaiian.Pacific.Islander.Hispanic.or.Latino") return("Hispanic or Latino Native Hawaiian and other Pacific Islander (%)")
    if(col == "Other.Race.Hispanic.or.Latino") return("Hispanic or Latino other race (total)")
    if(col == "Percent.Other.Race.Hispanic.or.Latino") return("Hispanic or Latino other race (%)")
    if(col == "Two.or.More.Races.Hispanic.or.Latino") return("Hispanic or Latino (total)")
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
    
    if(col == "Social.Vulnerability.Index") return("Social vulnerability index (SVI)")
    if(col == "Environmental.Justice.Index") return("Environmental justice index (EJI)")
    if(col == "Unemployment") return("Unemployment (%)")
    
    if(col == "UV.Index") return("UV index (UVI)")
    
    if(col == "Radon") return(HTML("Radon gas concentration (Bq/m<sup>3</sup>)"))
    
    if(col == "Pesticide.Exposure") return(HTML("Agricultural pesticide use in 2023 (lbs/mi<sup>2</sup>)"))
    
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
    
    if(col == "total_p2") return("Part II offenses")
    if(col == "p2_rate") return("Part II offenses (per 1,000 population)")
    
    # TODO: add legend titles for food environment columns
    
  }
  
  var.info <- function(col) {
    if(col == "Particulate.Matter.2.5") return(HTML(paste0("PM<sub>2.5</sub> concentrations in 2022 ", "(\U03BC", "g/m<sup>3</sup>)")))
    if(col == "Green.Space") return("Normalized difference vegetation index (NDVI) in July 2024")
    if(col == "Nighttime.Radiance") return(HTML("Light at night (nW/cm<sup>2</sup>/sr)"))
    if(col == "Food.Stamps") return("SNAP benefits in 2023 (%)")
    if(col == "Food.Insecurity") return("Food insecurity in 2023 (%)")
    if(col == "Housing.Insecurity") return("Housing insecurity in 2023 (%)")
    if(col == "Utility.Services.Threat") return("Utility services threat in 2023 (%)")
    if(col == "Lacking.Reliable.Transportation") return("Lack of reliable transportation in 2023 (%)")
    if(col == "Lack.of.Social.and.Emotional.Support") return("Lack of social and emotional support in 2023 (%)")
    if(col == "Lack.of.Health.Insurance") return("No health insurance in 2021 (%)")
    if(col == "Routine.Checkup.in.the.Past.Year") return("Routine checkup in past year in 2023 (%)")
    if(col == "Visited.Dentist.in.Past.Year") return("Visited dentist in past year in 2023 (%)")
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
    if(col == "Cancer.or.Melanoma.among.Adults") return("Cancer prevalence among adults in 2023 (%)")
    if(col == "High.Cholesterol.among.Screened.Adults") return("High cholesterol in 2023 (%)")
    if(col == "COPD.among.Adults") return("Chronic obstructive pulmonary disease in 2023 (%)")
    if(col == "Coronary.Heart.Disease.among.Adults") return("Coronary heart disease in 2023 (%)")
    if(col == "Depression.among.Adults") return("Depression in 2023 (%)")
    if(col == "Diagnosed.Diabetes.among.Adults") return("Diabetes in 2023 (%)")
    if(col == "Obesity.among.Adults") return("Obesity in 2023 (%)")
    if(col == "All.Teeth.Lost.among.Adults.65.and.Older") return("All teeth lost in 2023 (%)")
    if(col == "Stroke.among.Adults") return("Stroke in 2023 (%)")
    
    if(col == "Total.Population") return("Total number of people in a census tract using American Community Survey 5-year data (years)")
    if(col == "Hispanic.or.Latino") return("Total number of Hispanic or Latino individuals in 2023")
    if(col == "Percent.Hispanic.or.Latino") return("Percent Hispanic or Latino population in a census tract using American Community Survey 5-year data (2023)")
    if(col == "White.NonHispanic") return("Total number of Non-Hispanic White individuals in a census tract using American Community Survey 5-year data (2023)")
    if(col == "Percent.White.NonHispanic") return("Non-Hispanic White population in 2023 (%)")
    if(col == "Black.NonHispanic") return("Non-Hispanic Black population in 2023 (total)")
    if(col == "Percent.Black.NonHispanic") return("Non-Hispanic Black population in 2023 (%)")
    if(col == "American.Indian.Alaska.Native.NonHispanic") return("Non-Hispanic American Indian and Alaska Native population in 2023 (total)")
    if(col == "Percent.American.Indian.Alaska.Native.NonHispanic") return("Non-Hispanic American Indian and Alaska Native population in 2023 (%)")
    if(col == "Asian.NonHispanic") return("Non-Hispanic Asian population in 2023 (total)")
    if(col == "Percent.Asian.NonHispanic") return("Non-Hispanic Asian population in 2023 (%)")
    if(col == "Native.Hawaiian.Pacific.Islander.NonHispanic") return("Non-Hispanic Native Hawaiian and other Pacific Islander population in 2023 (total)")
    if(col == "Percent.Native.Hawaiian.Pacific.Islander.NonHispanic") return("Non-Hispanic Native Hawaiian and other Pacific Islander population in 2023 (%)")
    if(col == "Other.Race.NonHispanic") return("Non-Hispanic other race population in 2023 (total)")
    if(col == "Percent.Other.Race.NonHispanic") return("Non-Hispanic other race population in 2023 (%)")
    if(col == "Two.or.More.Races.NonHispanic") return("Non-Hispanic two or more races population in 2023 (total)")
    if(col == "Percent.Two.or.More.Races.NonHispanic") return("Non-Hispanic two or more races population in 2023 (%)")
    
    # hispanic or latino subcats
    if(col == "White.Hispanic.or.Latino") return("Hispanic or Latino White (total)")
    if(col == "Percent.White.Hispanic.or.Latino") return("Hispanic or Latino White (%)")
    if(col == "Black.Hispanic.or.Latino") return("Hispanic or Latino Black(total)")
    if(col == "Percent.Black.Hispanic.or.Latino") return("Hispanic or Latino Black (%)")
    if(col == "American.Indian.Alaska.Native.Hispanic.or.Latino") return("Hispanic or Latino American Indian and Alaska Native (total)")
    if(col == "Percent.American.Indian.Alaska.Native.Hispanic.or.Latino") return("Hispanic or Latino American Indian and Alaska Native (%)")
    if(col == "Asian.Hispanic.or.Latino") return("Hispanic or Latino Asian (total)")
    if(col == "Percent.Asian.Hispanic.or.Latino") return("Hispanic or Latino Asian (%)")
    if(col == "Native.Hawaiian.Pacific.Islander.Hispanic.or.Latino") return("Hispanic or Latino Native Hawaiian and other Pacific Islander (total)")
    if(col == "Percent.Native.Hawaiian.Pacific.Islander.Hispanic.or.Latino") return("Hispanic or Latino Native Hawaiian and other Pacific Islander (%)")
    if(col == "Other.Race.Hispanic.or.Latino") return("Hispanic or Latino other race (total)")
    if(col == "Percent.Other.Race.Hispanic.or.Latino") return("Hispanic or Latino other race (%)")
    if(col == "Two.or.More.Races.Hispanic.or.Latino") return("Hispanic or Latino (total)")
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
    
    if(col == "Social.Vulnerability.Index") return("SVI in 2022")
    if(col == "Environmental.Justice.Index") return("EJI in 2019")
    if(col == "Unemployment") return("Unemployment in 2021 (%)")
    
    if(col == "UV.Index") return("UVI in 2024")
    
    if(col == "Radon") return(HTML("Radon gas concentration in 2021 (Bq/m<sup>3</sup>)"))
    
    if(col == "Pesticide.Exposure") return(HTML("Agricultural pesticide use in 2023 (lbs/mi<sup>2</sup>)"))
    
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
    
    if(col == "Walkability") return("Walkability score in 2019")
    
    if(col == "No.broadband.internet") return("No internet in 2021 (%)")
    if(col == "No.high.school.diploma") return("No high school diploma in 2021 (%)")
    if(col == "Single.parent.households") return("Single parent households in 2021 (%)")
    if(col == "Crowding") return("Crowding among households in 2021 (%)")
    if(col == "Poverty") return("Poverty in 2021 (%)")
    if(col == "Housing.cost.burden") return("Housing cost burden in 2021 (%)")
    
    if(col == "Dew.point") return(paste0("Dew point ", "(\U00B0", "F)"))
    if(col == "Maximum.temperature") return(paste0("Maximum temperature ", "(\U00B0", "F)"))
    if(col == "Minimum.temperature") return(paste0("Minimum temperature ", "(\U00B0", "F)"))
    if(col == "Average.temperature") return(paste0("Average temperature ", "(\U00B0", "F)"))
    if(col == "Precipitation") return("Precipitation (in.)")
    
    if(col == "Wildfire.smoke") return(HTML(paste0("Wildfire smoke PM<sub>2.5</sub> in 2020 ", "(\U03BC", "g/m<sup>3</sup>)")))
    if(col == "Nitrogen.dioxide") return(HTML(paste0("Nitrogen dioxide (NO<sub>2</sub>) (ppb)")))
    if(col == "Sulfur.dioxide") return(HTML("Sulfur dioxide (SO<sub>2</sub>) (ppb)"))
    if(col == "Carbon.monoxide") return(HTML("Carbon monoxide (CO) (ppm)"))
    if(col == "Ozone") return(HTML("Ozone (O<sub>3</sub>) (ppb)"))
    
    if(col == "Population.density") return("Population density in 2023 (population per square mile)")
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
    
    if(col == "bluespace") return("Blue space coverage (% in tract)")
    if(col == "social_capital") return("Social capital")
    
    if(col == "PFAS_dw") return("PFAS in drinking water in 2021")
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
    
    if(col == "total_p2") return("Part II offenses")
    if(col == "p2_rate") return("Part II offenses (per 1,000 population)")
    
    # TODO: add legend titles for food environment columns
    
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
    updateSelectInput(session, "foodenv", selected = character(0))
    
    updateSelectInput(session, "crime", selected = character(0))
    
    # reset switches if needed
    update_switch("transit", value = FALSE)
    update_switch("showcounties", value = FALSE)
    update_switch("showcities", value = FALSE)
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
  
  #### REACTIVE VALUES #### 
  # layer ids for inputs to control number of layers 
  layer_ids <- c("outcomes", "sociodemo", "age", "sex", "race", "socialenv", "prevention", "behaviors", "naturalenv",
                 "airpol", "builtenv", "incsite", "incstage", "incsex", "mortsite", "mortsex", "foodenv", "crime")
  
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
                     builtenvironment = input$builtenv),
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
    df <- cbind(health_outcomes, sociodemo, age, sex, race, social_env, health_prevention, air_pol, health_behaviors, natural_env, built_env)
    
    df[, c(input$outcomes, input$sociodemo, input$age, input$sex, input$race, input$socialenv, input$prevention, input$behaviors, input$airpol, input$naturalenv, input$builtenv), drop = FALSE]
  }) %>% 
    bindCache(input$outcomes, input$sociodemo, input$age, input$sex, input$race, input$socialenv, input$prevention, input$behaviors, input$naturalenv, input$airpol, input$builtenv) # reduce work by server
  
  tab_cols <- reactive({
    df <- og.data
    
    df[, c(input$outcomes_tab, input$sociodemo_tab, input$age_tab, input$sex_tab, input$race_tab, input$socialenv_tab, input$prevention_tab, input$behaviors_tab, input$naturalenv_tab, input$airpol_tab, input$builtenv_tab)]
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
           "microplastics" = microplastics, "transit" = transit, "parks" = parks, "superfund" = superfund)
  })
  
  #### OBSERVERS FOR WSCR DATA ####
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
  # output$download <- downloadHandler(
  #   filename = function() {paste0(Sys.Date(), "geoexmap_download.gpkg")},
  #   content = function(file) {
  #     st_write(map_cols(), file)
  #   }
  # )
  
  output$downloadcttab <- downloadHandler(
    filename = function() {"geoexmap_2020_tract_download.zip"},
    content = function(file) {
      tmpdir <- tempdir()
      
      data <- ct_table_cols()
      layer_name <- "geoexmap_2020_tract_download"
      
      shp_path <- file.path(tmpdir, paste0(layer_name, ".shp"))
      
      st_write(obj = data, dsn = shp_path, driver = "ESRI Shapefile", delete_dsn = TRUE)
      
      shp_files <- list.files(tmpdir, pattern = paste0("^", layer_name, "\\."), full.names = TRUE)
      
      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      setwd(tmpdir)
      
      zipfile_tmp <- "geoexmap_2020_tract_download.zip"
      zip(zipfile_tmp, files = basename(shp_files))
      
      file.copy(file.path(tmpdir, zipfile_tmp), file, overwrite = TRUE)
    },
    contentType = "application/zip"
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
  
  #### PLOTLY RENDER ####
  output$chart <- renderPlotly({
    plotly.dat <- map_cols() %>%
      st_drop_geometry()

    if (ncol(plotly.dat) == 1) {
      plot_ly(data = plotly.dat, x = plotly.dat[,1]) %>% 
        layout(
          plot_bgcolor = '#e5ecf6',
          xaxis = list(title = names(plotly.dat)[1]),
          yaxis = list(title = "Frequency")) %>% 
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
      #TODO: add to render UI to choose at least 1 variable
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
      
      # find the matching category and update
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
    validate(need(base::ncol(food_env_cols_tab()) > 1, "Please select a food environment variable."))
    
    reactable(food_env_cols_tab())
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
  
  #### LEGEND CREATION ####
  # legend <- function(values, var, df, layer_index, palette, title, left_label, right_label, bins = 5) {
  #   # validate args
  #   stopifnot(!is.null(values))
  #   stopifnot(!is.null(palette))
  #   stopifnot(!is.null(title))
  #   stopifnot(!is.null(left_label))
  #   stopifnot(!is.null(right_label))
  #   
  #   pal <- geoex.palette(var, df, layer_index)
  #   # accommodate different bin arguments--may be needed?
  #   cuts <- if (length(bins) == 1) pretty(values, n = bins) else bins
  #   n <- length(cuts)
  #   r <- range(values, na.rm = TRUE)
  #   # pretty cut points may be out of the range of `values`
  #   cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
  #   colors <- pal(c(r[1], cuts, r[2]))
  #   
  #   # generate html list object using colors
  #   legend <- tags$ul(class = "legend")
  #   legend$children <- lapply(seq_len(length(colors)), function(color) {
  #     tags$li(
  #       class = "legend-item legend-color",
  #       style = paste0(
  #         "background-color:", colors[color]
  #       ),
  #     )
  #   })
  #   
  #   # add labels to list
  #   legend$children <- tagList(
  #     tags$li(
  #       class = "legend-item legend-label left-label",
  #       as.character(left_label)
  #     ),
  #     legend$children,
  #     tags$li(
  #       class = "legend-item legend-label right-label",
  #       as.character(right_label)
  #     )
  #   )
  #   
  #   # render legend with title
  #   return(
  #     tagList(
  #       tags$span(class = "legend-title", as.character(title)),
  #       legend
  #     )
  #   )
  #   # TODO: consider other legends (PFAS)
  # }
  
  #### MAIN OBSERVER LOGIC ####
  observe({
    layer_counter(0)
    observeEvent(input$legend_info_click, {
      v <- input$legend_info_click$var
      
      showModal(modalDialog(
        title = paste("About", legend.titles(v)),
        tagList(
          p("Data source", v),
          p("Text from lookup table...")
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
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
        clearMarkers() %>% 
        addProviderTiles(providers$CartoDB.Positron)
      
      # update the variable panel
      if (length(current_vars) > 0) {
        panel_html <- create_variable_panel(current_vars)
        proxy <- proxy %>% addControl(
            html = panel_html,
            position = "bottomright",
            layerId = "variable_panel"
          ) 
      }
      # TODO: dynamic labels for tracts
      label = ""
      
      ##### point control flow #####
      if (input$transit) {
        html_legend <- '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-bus-front" viewBox="0 0 16 16">
  <path d="M5 11a1 1 0 1 1-2 0 1 1 0 0 1 2 0m8 0a1 1 0 1 1-2 0 1 1 0 0 1 2 0m-6-1a1 1 0 1 0 0 2h2a1 1 0 1 0 0-2zm1-6c-1.876 0-3.426.109-4.552.226A.5.5 0 0 0 3 4.723v3.554a.5.5 0 0 0 .448.497C4.574 8.891 6.124 9 8 9s3.426-.109 4.552-.226A.5.5 0 0 0 13 8.277V4.723a.5.5 0 0 0-.448-.497A44 44 0 0 0 8 4m0-1c-1.837 0-3.353.107-4.448.22a.5.5 0 1 1-.104-.994A44 44 0 0 1 8 2c1.876 0 3.426.109 4.552.226a.5.5 0 1 1-.104.994A43 43 0 0 0 8 3"/>
  <path d="M15 8a1 1 0 0 0 1-1V5a1 1 0 0 0-1-1V2.64c0-1.188-.845-2.232-2.064-2.372A44 44 0 0 0 8 0C5.9 0 4.208.136 3.064.268 1.845.408 1 1.452 1 2.64V4a1 1 0 0 0-1 1v2a1 1 0 0 0 1 1v3.5c0 .818.393 1.544 1 2v2a.5.5 0 0 0 .5.5h2a.5.5 0 0 0 .5-.5V14h6v1.5a.5.5 0 0 0 .5.5h2a.5.5 0 0 0 .5-.5v-2c.607-.456 1-1.182 1-2zM8 1c2.056 0 3.71.134 4.822.261.676.078 1.178.66 1.178 1.379v8.86a1.5 1.5 0 0 1-1.5 1.5h-9A1.5 1.5 0 0 1 2 11.5V2.64c0-.72.502-1.301 1.178-1.379A43 43 0 0 1 8 1"/>
</svg> Transit stops (zoom in to see stops)<br/>'
        
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
        html_legend <- '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-clipboard-plus-fill" viewBox="0 0 16 16">
          <path d="M6.5 0A1.5 1.5 0 0 0 5 1.5v1A1.5 1.5 0 0 0 6.5 4h3A1.5 1.5 0 0 0 11 2.5v-1A1.5 1.5 0 0 0 9.5 0zm3 1a.5.5 0 0 1 .5.5v1a.5.5 0 0 1-.5.5h-3a.5.5 0 0 1-.5-.5v-1a.5.5 0 0 1 .5-.5z"/>
            <path d="M4 1.5H3a2 2 0 0 0-2 2V14a2 2 0 0 0 2 2h10a2 2 0 0 0 2-2V3.5a2 2 0 0 0-2-2h-1v1A2.5 2.5 0 0 1 9.5 5h-3A2.5 2.5 0 0 1 4 2.5zm4.5 6V9H10a.5.5 0 0 1 0 1H8.5v1.5a.5.5 0 0 1-1 0V10H6a.5.5 0 0 1 0-1h1.5V7.5a.5.5 0 0 1 1 0"/>
              </svg> Clinics <br/>'
        
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
      
      if (input$pharmacies) {
        html_legend <- 'Pharmacies <br/>'
        proxy <- proxy %>% 
          addMarkers(data = pharmacies,
                     popup = ~inFacility,
                     group = "pharmacies") %>% 
          addControl(html = html_legend, position = "topright")
      } else {
        proxy <- proxy %>% 
          clearGroup(group = "pharmacies")
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
      
      if (input$microplastics) {
        # duplicate to avoid error with retrieving cols from reactive dataset
        micro_data <- micro.dat()
        micro_data_conc <- factor(micro_data$Concentration.class.text,
                              levels = c("Very Low", "Low", "Medium", "High"),
                              ordered = TRUE)
        pal <- colorFactor("OrRd", domain = levels(micro_data_conc), ordered = TRUE)
        proxy <- proxy %>% 
          addCircleMarkers(data = micro_data, lng = ~x, lat = ~y, color = ~pal(Concentration.class.text),
                           radius = 4, fillOpacity = 0.8, popup = ~paste("<b>Region:</b>", Region, "<b><br>Sampling method:</b>", Sampling.Method,
                                                                         "<b><br>Date of collection:</b>", Date..MM.DD.YYYY.)) %>% 
          addLegend(pal = pal,
                    values = ~micro_data$Concentration.class.text,
                    title = "Microplastics Concentration")
      }
      
      ##### map (main data) #####
        groups <- character(0)
      
        for (i in seq_along(colnames(map_cols()))) {
          c_name <- colnames(map_cols())[i]
          x <- map_cols()[[c_name]]
          k <- NULL
          
          info_id <- paste0("legend info!", c_name)
          
          if (ncol(map_cols()) > 1 && c_name != "geometry" && c_name != "geom") {
            k <- isolate(layer_counter()) + 1
            layer_counter(k) # set the new layer count
            print(paste("LAYER COUNTER", layer_counter()))
          }
          
          pal <- geoex.palette(c_name, df_vars, layer_index = layer_counter())
          
          
          if (!is.null(pal)){
            opacity <- if (k == 1) 0.5 else 0.2
            
            groups <- append(groups, layer.titles(c_name))
            print(groups)
            
            info_id <- paste0("legend-info-", c_name)
            info_txt <- var.info(c_name)
            
            proxy <- proxy %>% 
              addPolygons(., fillColor = ~pal(x), stroke = FALSE,
                          fillOpacity = opacity, highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
                          group = layer.titles(c_name), label = "") %>% 
              addLegendNumeric(pal = pal, values = x, fillOpacity = opacity, 
                               orientation = "horizontal", shape = "stadium", 
                               width = 300,   # wider bar
                               height = 18, bins = 5,
                               title = htmltools::tags$div(
                                 legend.titles(c_name),
                                 htmltools::tags$span(
                                   "\u24D8",                    # circled 'i'
                                   id    = info_id,
                                   `data-bs-toggle` = "tooltip",
                                   `data-bs-placement` = "top",
                                   title = info_txt,
                                   style = "cursor:pointer; font-weight:bold; text-align: center; font-weight: bold"
                                 )
                               ),
            labelStyle = "text-align: center; font-weight: bold;",
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
      
      ##### map (food environment) #####
      for (i in seq_along(colnames(food_env_cols()))) {
        c_name <- colnames(food_env_cols())[i]
        x <- food_env_cols()[[c_name]]
        k <- NULL
        
        if (ncol(food_env_cols()) > 1 && c_name != "geometry" && c_name != "geom") {
          k <- isolate(layer_counter()) + 1
          layer_counter(k) # set the new layer count
          print(paste("LAYER COUNTER", layer_counter()))
        }

        pal <- geoex.palette(c_name, food, layer_index = layer_counter())
        
        if (!is.null(pal)) {
          groups <- append(groups, layer.titles(c_name))
          opacity <- if (k == 1) 0.5 else 0.2
          proxy <- proxy %>% 
            addPolygons(data = food_env_cols(), fillColor = ~pal(x), stroke = TRUE, weight = 0.25, color = "blue", group = layer.titles(c_name),
                        fillOpacity = opacity, highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE)) %>% 
            addLegendNumeric(pal = pal, values = x, fillOpacity = opacity, 
                             orientation = "horizontal", shape = "stadium", 
                             width = 300,   # wider bar
                             height = 18, bins = 5,
                             title = htmltools::tags$div(
                               legend.titles(c_name),
                               class = "legend-title",
                               style = "text-align: center; width: 100%; font-weight: bold;"
                             ),
                             labelStyle = "text-align: center; font-weight: bold;",
                             className  = "my-centered-num-legend",
                             position   = "bottomright") %>% 
            addLayersControl(overlayGroups = groups, # TODO: fix this functionality (overlay not functioning)
                             position = "topright",
                             options = layersControlOptions(collapsed = FALSE))
          #addLegend(pal = pal, values = x, title = legend.titles(c_name))
        }
      }
      
      ##### map (crime) #####
      #observe({
        for (i in seq_along(colnames(crime_cols()))) {
          c_name <- colnames(crime_cols())[i]
          x <- crime_cols()[[c_name]]
          k <- NULL
          
          if (ncol(crime_cols()) > 1 && c_name != "geometry" && c_name != "geom") {
            k <- layer_counter() + 1
            layer_counter(k) # set the new layer count
            print(paste("LAYER COUNTER", layer_counter()))
          }
          
          pal <- geoex.palette(c_name, crime, layer_index = layer_counter())
          
          if (!is.null(pal)){
            groups <- append(groups, layer.titles(c_name))
            opacity <- if (k == 1) 0.5 else 0.2
            
            proxy <- proxy %>% 
              addPolygons(data = crime_cols(), fillColor = ~pal(x), weight = 0.5, color = "black", group = layer.titles(c_name),
                          fillOpacity = opacity, stroke = input$showcounties, highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE)) %>% 
              #addLegend(colors = pal_colors, labels = pal_labs, title = legend.titles(c)) 
              #addLegend(pal = pal, values = x, title = legend.titles(c_name), na.label = "NA") 
              addLegendNumeric(pal = pal, values = x, fillOpacity = opacity, 
                               orientation = "horizontal", shape = "stadium", 
                               width = 300,   # wider bar
                               height = 18, bins = 5,
                               title = htmltools::tags$div(
                                 legend.titles(c_name),
                                 style = "text-align: center; width: 100%; font-weight: bold;"
                               ),
                               labelStyle = "text-align: center; font-weight: bold;",
                               className  = "my-centered-num-legend",
                               position   = "bottomright") %>% 
              addLayersControl(overlayGroups = groups, # TODO: fix this functionality (overlay not functioning)
                               position = "topright",
                               options = layersControlOptions(collapsed = FALSE))
          }
        }#}) %>% 
        #bindEvent(input$crime)
      
      ##### map (WSCR incidence) #####
      #observe({
      if (inc.ready()) {
        geo.inc <- base::merge(county.bounds, filtered.inc(), by.x = "NAME", by.y = "counties") %>% 
          mutate(Age.Adj..Rate.per.100.000 = as.numeric(Age.Adj..Rate.per.100.000))
        
        if (nrow(geo.inc) > 0) {
          k <- layer_counter() + 1
          layer_counter(k)
          print(paste("LAYER COUNTER", layer_counter()))
          
          opacity <- if (k == 1) 0.5 else 0.2
          
          pal <- geoex.palette("Age.Adj..Rate.per.100.000", geo.inc, layer_index = layer_counter())
          #pal <- colorNumeric("YlOrRd", domain = geo.inc$Age.Adj..Rate.per.100.000)
          val <- sort(geo.inc$Age.Adj..Rate.per.100.000)
          groups <- append(groups, "Cancer incidence")
          proxy <- proxy %>%
            addPolygons(data = geo.inc, fillColor = ~pal(Age.Adj..Rate.per.100.000),
                        popup = ~paste(NAMELSAD, "<br>Site:", Cancer.Site, "<br>Stage:", Stage.At.Diagnosis, "<br>Sex:", Gender,
                                       "<br>Age-Adjusted Rate:", Age.Adj..Rate.per.100.000),
                        group = "Cancer incidence", weight = 0.5, stroke = input$showcounties, fillOpacity = opacity, highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE)) %>% 
            addLegendNumeric(pal = pal, values = val, fillOpacity = opacity, 
                             orientation = "horizontal", shape = "stadium", 
                             width = 300,   # wider bar
                             height = 18, bins = 5,
                             title = htmltools::tags$div(
                               paste(unique(geo.inc$Cancer.Site), "cancer (age-adjusted incidence rate per 100,000 in 2025):"),
                               style = "text-align: center; width: 100%; font-weight: bold;"
                             ),
                             labelStyle = "text-align: center; font-weight: bold;",
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
                        popup = ~paste(NAMELSAD, "<br>Site:", Cancer.Site, "<br>Stage:", Stage.At.Diagnosis, "<br>Sex:", Gender,
                                       "<br>Age-Adjusted Rate:", Age.Adj..Rate.per.100.000),
                        group = "Cancer mortality", weight = 0.5, stroke = input$showcounties, fillOpacity = opacity, highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE)) %>%
            addLegendNumeric(pal = pal, values = val, fillOpacity = opacity, 
                             orientation = "horizontal", shape = "stadium", 
                             width = 300,   # wider bar
                             height = 18, bins = 5,
                             title = htmltools::tags$div(
                               paste(unique(geo.mort$Cancer.Site), "cancer (age-adjusted mortality rate per 100,000 in 2023):"),
                               style = "text-align: center; width: 100%; font-weight: bold;"
                             ),
                             labelStyle = "text-align: center; font-weight: bold;",
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
      
      proxy <- proxy %>% 
        addEasyprint(options = easyprintOptions(filename = "geoexmap_output", hideControlContainer = FALSE))
      
    })  
  }) %>% 
    bindEvent(layer_selection_state(), input$transit, input$alc, input$superfund, input$parks, input$micro, input$cancer, input$clinics, input$ems, input$hospitals, 
                   input$pharmacies, input$wic_clinics, input$wic_retailers, input$fqhc, input$showcities, input$showcounties, input$showbounds, 
                   input$upload) 
}

# -------- CREATE SHINY APP --------
options <- list()

if (!interactive()) {
  options$shiny.port = 3838
  options$shiny.host = "0.0.0.0"
}

shinyApp(ui = ui, server = server, options = options)
