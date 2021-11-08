# Author: M.L.

# input: dataset.Rdata

# Note: In 03 script, the Moran's I tests indicate that the residuals from OLS are
#       spatially clustering. Therefore, spatail models are required.

# end

library(tidyverse)
library(dplyr)
library(spatialreg)
library(rgdal)
library(tigris)
library(spdep)
library(stargazer)

load("00_RData/dataset.Rdata")
variables.in.reg.form = c("Open_Water_capi",
                          "Developed_Open_Space_capi","Developed_Low_Intensity_capi",
                          "Developed_Medium_Intensity_capi",
                          "Developed_High_Intensity_capi","Barren_Land_capi","Deciduous_Forest_capi",
                          "Evergreen_Forest_capi","Mixed_Forest_capi","Shrub_capi",
                          "Grassland_capi"," Pasture_capi"," Cultivated_Crops_capi",
                          "Woody_Wetlands_capi","Emergent_Herbaceous_Wetlands_capi")
reg.form.mort <- function(vari_name){
  #  input a variable name
  func = paste('mortality ~', vari_name, " +
  incidence_proportion + gatherings_restrictions + transport_closing +
  stay_home_restrictions + internal_movement_restrictions + 
  international_movement_restrictions + 
  pop_density +
  age15_44 + age45_64 +
  age65_99 + black_rate + hispanic_rate + male +
  Unemployment_rate_2019 + log_Median_Household_Income_2018 +
  poverty_rate + less_than_high_school +  
  poor_health_rate_2019 + poor_physical_days_2019 + poor_mental_days_2019 +
  smoker_rate_2019 + obesity_rate_2019 + physical_inactivity_2019 +
  exercise_opportunities_rate_2019 +
  hospital_beds_per_1000 + summer_tmmx_mean + winter_tmmx_mean + summer_rmax_mean +
  winter_rmax_mean + pm25_mean") %>% as.formula()
  return(func)
}

sar.reg.mort <- function(variable.number){
  reg.form <- reg.form.mort(variables.in.reg.form[variable.number])
  model <- lagsarlm(reg.form, data = us_shape@data, listw = lw, type = "lag", zero.policy=TRUE,
                    tol.solve = 1e-30)
  return(model)
}

reg.form.pr <- function(vari_name){
  #  input a variable name
  func = paste('incidence_proportion  ~', vari_name, " + 
  gatherings_restrictions + transport_closing +
  stay_home_restrictions + internal_movement_restrictions + 
  international_movement_restrictions + 
  pop_density +
  age15_44 + age45_64 +
  age65_99 + black_rate + hispanic_rate + male +
  Unemployment_rate_2019 + log_Median_Household_Income_2018 +
  poverty_rate + less_than_high_school +  
  poor_health_rate_2019 + poor_physical_days_2019 + poor_mental_days_2019 +
  smoker_rate_2019 + obesity_rate_2019 + physical_inactivity_2019 +
  exercise_opportunities_rate_2019 +
  hospital_beds_per_1000 + summer_tmmx_mean + winter_tmmx_mean + summer_rmax_mean +
  winter_rmax_mean + pm25_mean") %>% as.formula()
  return(func)
}

sar.reg.pr <- function(variable.number){
  reg.form <- reg.form.pr(variables.in.reg.form[variable.number])
  model <- lagsarlm(reg.form, data = us_shape@data, listw = lw, type = "lag", zero.policy=TRUE,
                    tol.solve = 1e-30)
  return(model)
}

tes <- dataset %>%
  dplyr::select("Open_Water_capi",
                "Developed_Open_Space_capi","Developed_Low_Intensity_capi",
                "Developed_Medium_Intensity_capi",
                "Developed_High_Intensity_capi","Barren_Land_capi","Deciduous_Forest_capi",
                "Evergreen_Forest_capi","Mixed_Forest_capi","Shrub_capi",
                "Grassland_capi","Pasture_capi","Cultivated_Crops_capi",
                "Woody_Wetlands_capi","Emergent_Herbaceous_Wetlands_capi",
                incidence_proportion, 
                gatherings_restrictions, transport_closing,
                stay_home_restrictions, internal_movement_restrictions, 
                international_movement_restrictions, pop_density, mortality,
                age15_44, age45_64, age65_99, black_rate, hispanic_rate, male,
                Unemployment_rate_2019, log_Median_Household_Income_2018,
                poverty_rate, less_than_high_school,  
                poor_health_rate_2019, poor_physical_days_2019, poor_mental_days_2019,
                smoker_rate_2019, obesity_rate_2019, physical_inactivity_2019,
                exercise_opportunities_rate_2019,
                hospital_beds_per_1000, summer_tmmx_mean, winter_tmmx_mean, summer_rmax_mean,
                winter_rmax_mean, pm25_mean, key_numeric
  ) %>% na.omit()
us_shape <- readOGR(dsn = "01_Raster\\01_Boundary", layer = "cb_2017_us_county_20m84")
us_shape <- geo_join(us_shape, tes, 'CountyFIPS', 'key_numeric', how = 'inner')
plot(us_shape) # now there are 3081 records
rm(tes)
gc()

nb <- poly2nb(us_shape, queen = T)
lw <- nb2listw(nb, style="W",zero.policy=T) 

sar.mort.1 <- sar.reg.mort(1)
sar.mort.2 <- sar.reg.mort(2)
sar.mort.3 <- sar.reg.mort(3)
sar.mort.4 <- sar.reg.mort(4)
sar.mort.5 <- sar.reg.mort(5)
sar.mort.6 <- sar.reg.mort(6)
sar.mort.7 <- sar.reg.mort(7)
sar.mort.8 <- sar.reg.mort(8)
sar.mort.9 <- sar.reg.mort(9)
sar.mort.10 <- sar.reg.mort(10)
sar.mort.11 <- sar.reg.mort(11)
sar.mort.12 <- sar.reg.mort(12)
sar.mort.13 <- sar.reg.mort(13)
sar.mort.14 <- sar.reg.mort(14)
sar.mort.15 <- sar.reg.mort(15)

stargazer(sar.mort.1, sar.mort.2, sar.mort.3, sar.mort.4, sar.mort.5, 
          sar.mort.6, sar.mort.7, sar.mort.8,sar.mort.9, sar.mort.10, 
          sar.mort.11, sar.mort.12, sar.mort.13, sar.mort.14, 
          sar.mort.15,
          title = "Table XXX: Test",  type = "text", 
          no.space = T,
          covariate.labels = c(
            'Open Water (hm2/cap)', 
            'Open Space Developed Area (hm2/cap)',                   
            "Low Intensity Developed Area (hm2/cap)",
            "Medium Intensity Developed Area (hm2/cap)",
            'High Intensity Developed Area (hm2/cap)',
            'Barren Land (hm2/cap)',
            'Deciduous Forest (hm2/cap)',
            'Evergreen Forest (hm2/cap)',
            'Mixed Forest (hm2/cap)', 'Shrub (hm2/cap)',
            'Grassland (hm2/cap)', 'Pasture (hm2/cap)',
            'Cultivated Crops (hm2/cap)',
            'Woody Wetlands (hm2/cap)',
            'Emergent Herbaceous Wetlands (hm2/cap)',
            "Prevalence Rate (cap/1000)",
            'Gathering Restrictions (days)',
            'Transport Closing (days)', 'Staying Home (days)',
            "Internal MoRe (days)", "International MoRe (days)",
            "Population Density (cap/km2)",'Population 15-44 (%)',
            'Population 45-64 (%)', 
            'Population >= 65 (%)', 'Black People (%)', 
            'Hispanic People (%)',  'Male (%)',
            'Umemployment Rate', 'Median Household Income (logatithm)',
            'Poverty Rate (%)', 'Adults Without High School Diploma (%)',
            'Poor Health Rate (%)',  'Poor Physical Health (days)', "Poor Mental Health (days)",
            'Adult Smoking Rate (%)', 'Obesity Rate (%)',
            'Physical Inactivity Rate (%)', 
            'Having Access To Exercise Opportunities (%)', "Hospital Beds (bed/1000)",
            'Average Temperature In Summer', 
            'Average Temperature In Winter', 
            'Average Relative Humidity In Summer', 'Average Relative Humidity In Winter',
            'PM2.5'
          ),
          iqr = F, out = "03_Results\\SeperatedSAR.MR.html"
) 

sar.pr.1 <- sar.reg.pr(1)
sar.pr.2 <- sar.reg.pr(2)
sar.pr.3 <- sar.reg.pr(3)
sar.pr.4 <- sar.reg.pr(4)
sar.pr.5 <- sar.reg.pr(5)
sar.pr.6 <- sar.reg.pr(6)
sar.pr.7 <- sar.reg.pr(7)
sar.pr.8 <- sar.reg.pr(8)
sar.pr.9 <- sar.reg.pr(9)
sar.pr.10 <- sar.reg.pr(10)
sar.pr.11 <- sar.reg.pr(11)
sar.pr.12 <- sar.reg.pr(12)
sar.pr.13 <- sar.reg.pr(13)
sar.pr.14 <- sar.reg.pr(14)
sar.pr.15 <- sar.reg.pr(15)