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
variables.in.reg.form = c("Open_Water_perc",
                          "Developed_Open_Space_perc","Developed_Low_Intensity_perc",
                          "Developed_Medium_Intensity_perc",
                          "Developed_High_Intensity_perc","Barren_Land_perc","Deciduous_Forest_perc",
                          "Evergreen_Forest_perc","Mixed_Forest_perc","Shrub_perc",
                          "Grassland_perc"," Pasture_perc"," Cultivated_Crops_perc",
                          "Woody_Wetlands_perc","Emergent_Herbaceous_Wetlands_perc")
reg.form.mort <- function(vari_name){
  #  input a variable name
  func = paste('mortality ~', vari_name, " +
  incidence_proportion + gatherings_restrictions + transport_closing +
  stay_home_restrictions + internal_movement_restrictions + 
  international_movement_restrictions + 
  #TestCode pop_density + 
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
} # if still want to use stargazer to summary the sar result, DO NOT apply this function

reg.form.pr <- function(vari_name){
  #  input a variable name
  func = paste('incidence_proportion  ~', vari_name, " + 
  gatherings_restrictions + transport_closing +
  stay_home_restrictions + internal_movement_restrictions + 
  international_movement_restrictions + 
  #TestCode pop_density +
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
} # if still want to use stargazer to summary the sar result, DO NOT apply this function

tes <- dataset %>%
  dplyr::select("Open_Water_perc",
                "Developed_Open_Space_perc","Developed_Low_Intensity_perc",
                "Developed_Medium_Intensity_perc",
                "Developed_High_Intensity_perc","Barren_Land_perc","Deciduous_Forest_perc",
                "Evergreen_Forest_perc","Mixed_Forest_perc","Shrub_perc",
                "Grassland_perc","Pasture_perc","Cultivated_Crops_perc",
                "Woody_Wetlands_perc","Emergent_Herbaceous_Wetlands_perc",
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

GOT_FORMULAS <- T
if(GOT_FORMULAS){
  reg.form.mort.1 <- reg.form.mort(variables.in.reg.form[1])
  reg.form.mort.2 <- reg.form.mort(variables.in.reg.form[2])
  reg.form.mort.3 <- reg.form.mort(variables.in.reg.form[3])
  reg.form.mort.4 <- reg.form.mort(variables.in.reg.form[4])
  reg.form.mort.5 <- reg.form.mort(variables.in.reg.form[5])
  reg.form.mort.6 <- reg.form.mort(variables.in.reg.form[6])
  reg.form.mort.7 <- reg.form.mort(variables.in.reg.form[7])
  reg.form.mort.8 <- reg.form.mort(variables.in.reg.form[8])
  reg.form.mort.9 <- reg.form.mort(variables.in.reg.form[9])
  reg.form.mort.10 <- reg.form.mort(variables.in.reg.form[10])
  reg.form.mort.11 <- reg.form.mort(variables.in.reg.form[11])
  reg.form.mort.12 <- reg.form.mort(variables.in.reg.form[12])
  reg.form.mort.13 <- reg.form.mort(variables.in.reg.form[13])
  reg.form.mort.14 <- reg.form.mort(variables.in.reg.form[14])
  reg.form.mort.15 <- reg.form.mort(variables.in.reg.form[15])
  
  reg.form.pr.1 <- reg.form.pr(variables.in.reg.form[1])
  reg.form.pr.2 <- reg.form.pr(variables.in.reg.form[2])
  reg.form.pr.3 <- reg.form.pr(variables.in.reg.form[3])
  reg.form.pr.4 <- reg.form.pr(variables.in.reg.form[4])
  reg.form.pr.5 <- reg.form.pr(variables.in.reg.form[5])
  reg.form.pr.6 <- reg.form.pr(variables.in.reg.form[6])
  reg.form.pr.7 <- reg.form.pr(variables.in.reg.form[7])
  reg.form.pr.8 <- reg.form.pr(variables.in.reg.form[8])
  reg.form.pr.9 <- reg.form.pr(variables.in.reg.form[9])
  reg.form.pr.10 <- reg.form.pr(variables.in.reg.form[10])
  reg.form.pr.11 <- reg.form.pr(variables.in.reg.form[11])
  reg.form.pr.12 <- reg.form.pr(variables.in.reg.form[12])
  reg.form.pr.13 <- reg.form.pr(variables.in.reg.form[13])
  reg.form.pr.14 <- reg.form.pr(variables.in.reg.form[14])
  reg.form.pr.15 <- reg.form.pr(variables.in.reg.form[15])
}

SAR_LAG <- T # if run this block, then T
if(SAR_LAG) {
  sar.mort.1 <- lagsarlm(reg.form.mort.1, data = us_shape@data, listw = lw, 
                         type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.2 <- lagsarlm(reg.form.mort.2, data = us_shape@data, listw = lw, 
                         type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.3 <- lagsarlm(reg.form.mort.3, data = us_shape@data, listw = lw, 
                         type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.4 <- lagsarlm(reg.form.mort.4, data = us_shape@data, listw = lw, 
                         type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.5 <- lagsarlm(reg.form.mort.5, data = us_shape@data, listw = lw, 
                         type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.6 <- lagsarlm(reg.form.mort.6, data = us_shape@data, listw = lw, 
                         type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.7 <- lagsarlm(reg.form.mort.7, data = us_shape@data, listw = lw, 
                         type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.8 <- lagsarlm(reg.form.mort.8, data = us_shape@data, listw = lw, 
                         type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.9 <- lagsarlm(reg.form.mort.9, data = us_shape@data, listw = lw, 
                         type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.10 <- lagsarlm(reg.form.mort.10, data = us_shape@data, listw = lw, 
                         type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.11 <- lagsarlm(reg.form.mort.11, data = us_shape@data, listw = lw, 
                         type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.12 <- lagsarlm(reg.form.mort.12, data = us_shape@data, listw = lw, 
                         type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.13 <- lagsarlm(reg.form.mort.13, data = us_shape@data, listw = lw, 
                         type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.14 <- lagsarlm(reg.form.mort.14, data = us_shape@data, listw = lw, 
                         type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.15 <- lagsarlm(reg.form.mort.15, data = us_shape@data, listw = lw, 
                         type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  
  
  stargazer(sar.mort.1, sar.mort.2, sar.mort.3, sar.mort.4,
            sar.mort.5, sar.mort.6, sar.mort.7, sar.mort.8,
            sar.mort.9, sar.mort.10, sar.mort.11, sar.mort.12,
            sar.mort.13, sar.mort.14, sar.mort.15,
            title = "Table XXX: Test",  type = "text", 
            no.space = T,
            covariate.labels = c(
              'Open Water (%)', 
              'Open Space Developed Area (%)',                   
              "Low Intensity Developed Area (%)",
              "Medium Intensity Developed Area (%)",
              'High Intensity Developed Area (%)',
              'Barren Land (%)',
              'Deciduous Forest (%)',
              'Evergreen Forest (%)',
              'Mixed Forest (%)', 'Shrub (%)',
              'Grassland (%)', 'Pasture (%)',
              'Cultivated Crops (%)',
              'Woody Wetlands (%)',
              'Emergent Herbaceous Wetlands (%)',
              "Prevalence Rate (cap/1000)",
              'Gathering Restrictions (days)',
              'Transport Closing (days)', 'Staying Home (days)',
              "Internal MoRe (days)", "International MoRe (days)",
              #TestCode "Population Density (cap/km2)",
              'Population 15-44 (%)',
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
  
  sar.pr.1 <- lagsarlm(reg.form.pr.1, data = us_shape@data, listw = lw, 
                         type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.2 <- lagsarlm(reg.form.pr.2, data = us_shape@data, listw = lw, 
                         type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.3 <- lagsarlm(reg.form.pr.3, data = us_shape@data, listw = lw, 
                         type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.4 <- lagsarlm(reg.form.pr.4, data = us_shape@data, listw = lw, 
                         type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.5 <- lagsarlm(reg.form.pr.5, data = us_shape@data, listw = lw, 
                         type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.6 <- lagsarlm(reg.form.pr.6, data = us_shape@data, listw = lw, 
                         type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.7 <- lagsarlm(reg.form.pr.7, data = us_shape@data, listw = lw, 
                         type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.8 <- lagsarlm(reg.form.pr.8, data = us_shape@data, listw = lw, 
                         type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.9 <- lagsarlm(reg.form.pr.9, data = us_shape@data, listw = lw, 
                         type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.10 <- lagsarlm(reg.form.pr.10, data = us_shape@data, listw = lw, 
                          type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.11 <- lagsarlm(reg.form.pr.11, data = us_shape@data, listw = lw, 
                          type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.12 <- lagsarlm(reg.form.pr.12, data = us_shape@data, listw = lw, 
                          type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.13 <- lagsarlm(reg.form.pr.13, data = us_shape@data, listw = lw, 
                          type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.14 <- lagsarlm(reg.form.pr.14, data = us_shape@data, listw = lw, 
                          type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.15 <- lagsarlm(reg.form.pr.15, data = us_shape@data, listw = lw, 
                          type = "lag", zero.policy=TRUE, tol.solve = 1e-30)
  
  stargazer(sar.pr.1, sar.pr.2, sar.pr.3, sar.pr.4,
            sar.pr.5, sar.pr.6, sar.pr.7, sar.pr.8,
            sar.pr.9, sar.pr.10, sar.pr.11, sar.pr.12,
            sar.pr.13, sar.pr.14, sar.pr.15,
            title = "Table XXX: Test",  type = "text", 
            no.space = T,
            covariate.labels = c(
              'Open Water (%)', 
              'Open Space Developed Area (%)',                   
              "Low Intensity Developed Area (%)",
              "Medium Intensity Developed Area (%)",
              'High Intensity Developed Area (%)',
              'Barren Land (%)',
              'Deciduous Forest (%)',
              'Evergreen Forest (%)',
              'Mixed Forest (%)', 'Shrub (%)',
              'Grassland (%)', 'Pasture (%)',
              'Cultivated Crops (%)',
              'Woody Wetlands (%)',
              'Emergent Herbaceous Wetlands (%)',
              "Prevalence Rate (cap/1000)",
              'Gathering Restrictions (days)',
              'Transport Closing (days)', 'Staying Home (days)',
              "Internal MoRe (days)", "International MoRe (days)",
              #TestCode "Population Density (cap/km2)",
              'Population 15-44 (%)',
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
            iqr = F, out = "03_Results\\SeperatedSAR.PR.html"
  ) 
}

SAR_ERROR <- T # if run this block, then T
if(SAR_ERROR) {
  sar.mort.1 <- errorsarlm(reg.form.mort.1, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.2 <- errorsarlm(reg.form.mort.2, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.3 <- errorsarlm(reg.form.mort.3, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.4 <- errorsarlm(reg.form.mort.4, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.5 <- errorsarlm(reg.form.mort.5, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.6 <- errorsarlm(reg.form.mort.6, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.7 <- errorsarlm(reg.form.mort.7, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.8 <- errorsarlm(reg.form.mort.8, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.9 <- errorsarlm(reg.form.mort.9, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.10 <- errorsarlm(reg.form.mort.10, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.11 <- errorsarlm(reg.form.mort.11, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.12 <- errorsarlm(reg.form.mort.12, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.13 <- errorsarlm(reg.form.mort.13, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.14 <- errorsarlm(reg.form.mort.14, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.15 <- errorsarlm(reg.form.mort.15, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  
  
  stargazer(sar.mort.1, sar.mort.2, sar.mort.3, sar.mort.4,
            sar.mort.5, sar.mort.6, sar.mort.7, sar.mort.8,
            sar.mort.9, sar.mort.10, sar.mort.11, sar.mort.12,
            sar.mort.13, sar.mort.14, sar.mort.15,
            title = "Table XXX: Test",  type = "text", 
            no.space = T,
            covariate.labels = c(
              'Open Water (%)', 
              'Open Space Developed Area (%)',                   
              "Low Intensity Developed Area (%)",
              "Medium Intensity Developed Area (%)",
              'High Intensity Developed Area (%)',
              'Barren Land (%)',
              'Deciduous Forest (%)',
              'Evergreen Forest (%)',
              'Mixed Forest (%)', 'Shrub (%)',
              'Grassland (%)', 'Pasture (%)',
              'Cultivated Crops (%)',
              'Woody Wetlands (%)',
              'Emergent Herbaceous Wetlands (%)',
              "Prevalence Rate (cap/1000)",
              'Gathering Restrictions (days)',
              'Transport Closing (days)', 'Staying Home (days)',
              "Internal MoRe (days)", "International MoRe (days)",
              #TestCode "Population Density (cap/km2)",
              'Population 15-44 (%)',
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
            iqr = F, out = "03_Results\\SeperatedSER.MR.html"
  ) 
  
  sar.pr.1 <- errorsarlm(reg.form.pr.1, data = us_shape@data, listw = lw, 
                       zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.2 <- errorsarlm(reg.form.pr.2, data = us_shape@data, listw = lw, 
                       zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.3 <- errorsarlm(reg.form.pr.3, data = us_shape@data, listw = lw, 
                       zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.4 <- errorsarlm(reg.form.pr.4, data = us_shape@data, listw = lw, 
                       zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.5 <- errorsarlm(reg.form.pr.5, data = us_shape@data, listw = lw, 
                       zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.6 <- errorsarlm(reg.form.pr.6, data = us_shape@data, listw = lw, 
                       zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.7 <- errorsarlm(reg.form.pr.7, data = us_shape@data, listw = lw, 
                       zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.8 <- errorsarlm(reg.form.pr.8, data = us_shape@data, listw = lw, 
                       zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.9 <- errorsarlm(reg.form.pr.9, data = us_shape@data, listw = lw, 
                       zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.10 <- errorsarlm(reg.form.pr.10, data = us_shape@data, listw = lw, 
                        zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.11 <- errorsarlm(reg.form.pr.11, data = us_shape@data, listw = lw, 
                        zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.12 <- errorsarlm(reg.form.pr.12, data = us_shape@data, listw = lw, 
                        zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.13 <- errorsarlm(reg.form.pr.13, data = us_shape@data, listw = lw, 
                        zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.14 <- errorsarlm(reg.form.pr.14, data = us_shape@data, listw = lw, 
                        zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.15 <- errorsarlm(reg.form.pr.15, data = us_shape@data, listw = lw, 
                        zero.policy=TRUE, tol.solve = 1e-30)
  
  stargazer(sar.pr.1, sar.pr.2, sar.pr.3, sar.pr.4,
            sar.pr.5, sar.pr.6, sar.pr.7, sar.pr.8,
            sar.pr.9, sar.pr.10, sar.pr.11, sar.pr.12,
            sar.pr.13, sar.pr.14, sar.pr.15,
            title = "Table XXX: Test",  type = "text", 
            no.space = T,
            covariate.labels = c(
              'Open Water (%)', 
              'Open Space Developed Area (%)',                   
              "Low Intensity Developed Area (%)",
              "Medium Intensity Developed Area (%)",
              'High Intensity Developed Area (%)',
              'Barren Land (%)',
              'Deciduous Forest (%)',
              'Evergreen Forest (%)',
              'Mixed Forest (%)', 'Shrub (%)',
              'Grassland (%)', 'Pasture (%)',
              'Cultivated Crops (%)',
              'Woody Wetlands (%)',
              'Emergent Herbaceous Wetlands (%)',
              "Prevalence Rate (cap/1000)",
              'Gathering Restrictions (days)',
              'Transport Closing (days)', 'Staying Home (days)',
              "Internal MoRe (days)", "International MoRe (days)",
              #TestCode "Population Density (cap/km2)",
              'Population 15-44 (%)',
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
            iqr = F, out = "03_Results\\SeperatedSER.PR.html"
  ) 
}

SARAR <- T # if run this block, then T
if(SARAR) {
  sar.mort.1 <- sacsarlm(reg.form.mort.1, data = us_shape@data, listw = lw, 
                           zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.2 <- sacsarlm(reg.form.mort.2, data = us_shape@data, listw = lw, 
                           zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.3 <- sacsarlm(reg.form.mort.3, data = us_shape@data, listw = lw, 
                           zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.4 <- sacsarlm(reg.form.mort.4, data = us_shape@data, listw = lw, 
                           zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.5 <- sacsarlm(reg.form.mort.5, data = us_shape@data, listw = lw, 
                           zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.6 <- sacsarlm(reg.form.mort.6, data = us_shape@data, listw = lw, 
                           zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.7 <- sacsarlm(reg.form.mort.7, data = us_shape@data, listw = lw, 
                           zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.8 <- sacsarlm(reg.form.mort.8, data = us_shape@data, listw = lw, 
                           zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.9 <- sacsarlm(reg.form.mort.9, data = us_shape@data, listw = lw, 
                           zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.10 <- sacsarlm(reg.form.mort.10, data = us_shape@data, listw = lw, 
                            zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.11 <- sacsarlm(reg.form.mort.11, data = us_shape@data, listw = lw, 
                            zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.12 <- sacsarlm(reg.form.mort.12, data = us_shape@data, listw = lw, 
                            zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.13 <- sacsarlm(reg.form.mort.13, data = us_shape@data, listw = lw, 
                            zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.14 <- sacsarlm(reg.form.mort.14, data = us_shape@data, listw = lw, 
                            zero.policy=TRUE, tol.solve = 1e-30)
  sar.mort.15 <- sacsarlm(reg.form.mort.15, data = us_shape@data, listw = lw, 
                            zero.policy=TRUE, tol.solve = 1e-30)
  
  
  stargazer(sar.mort.1, sar.mort.2, sar.mort.3, sar.mort.4,
            sar.mort.5, sar.mort.6, sar.mort.7, sar.mort.8,
            sar.mort.9, sar.mort.10, sar.mort.11, sar.mort.12,
            sar.mort.13, sar.mort.14, sar.mort.15,
            title = "Table XXX: Test",  type = "text", 
            no.space = T,
            covariate.labels = c(
              'Open Water (%)', 
              'Open Space Developed Area (%)',                   
              "Low Intensity Developed Area (%)",
              "Medium Intensity Developed Area (%)",
              'High Intensity Developed Area (%)',
              'Barren Land (%)',
              'Deciduous Forest (%)',
              'Evergreen Forest (%)',
              'Mixed Forest (%)', 'Shrub (%)',
              'Grassland (%)', 'Pasture (%)',
              'Cultivated Crops (%)',
              'Woody Wetlands (%)',
              'Emergent Herbaceous Wetlands (%)',
              "Prevalence Rate (cap/1000)",
              'Gathering Restrictions (days)',
              'Transport Closing (days)', 'Staying Home (days)',
              "Internal MoRe (days)", "International MoRe (days)",
              #TestCode "Population Density (cap/km2)",
              'Population 15-44 (%)',
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
            iqr = F, out = "03_Results\\SeperatedSAC.MR.html"
  ) 
  
  sar.pr.1 <- sacsarlm(reg.form.pr.1, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.2 <- sacsarlm(reg.form.pr.2, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.3 <- sacsarlm(reg.form.pr.3, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.4 <- sacsarlm(reg.form.pr.4, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.5 <- sacsarlm(reg.form.pr.5, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.6 <- sacsarlm(reg.form.pr.6, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.7 <- sacsarlm(reg.form.pr.7, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.8 <- sacsarlm(reg.form.pr.8, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.9 <- sacsarlm(reg.form.pr.9, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.10 <- sacsarlm(reg.form.pr.10, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.11 <- sacsarlm(reg.form.pr.11, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.12 <- sacsarlm(reg.form.pr.12, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.13 <- sacsarlm(reg.form.pr.13, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.14 <- sacsarlm(reg.form.pr.14, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  sar.pr.15 <- sacsarlm(reg.form.pr.15, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  
  stargazer(sar.pr.1, sar.pr.2, sar.pr.3, sar.pr.4,
            sar.pr.5, sar.pr.6, sar.pr.7, sar.pr.8,
            sar.pr.9, sar.pr.10, sar.pr.11, sar.pr.12,
            sar.pr.13, sar.pr.14, sar.pr.15,
            title = "Table XXX: Test",  type = "text", 
            no.space = T,
            covariate.labels = c(
              'Open Water (%)', 
              'Open Space Developed Area (%)',                   
              "Low Intensity Developed Area (%)",
              "Medium Intensity Developed Area (%)",
              'High Intensity Developed Area (%)',
              'Barren Land (%)',
              'Deciduous Forest (%)',
              'Evergreen Forest (%)',
              'Mixed Forest (%)', 'Shrub (%)',
              'Grassland (%)', 'Pasture (%)',
              'Cultivated Crops (%)',
              'Woody Wetlands (%)',
              'Emergent Herbaceous Wetlands (%)',
              "Prevalence Rate (cap/1000)",
              'Gathering Restrictions (days)',
              'Transport Closing (days)', 'Staying Home (days)',
              "Internal MoRe (days)", "International MoRe (days)",
              #TestCode "Population Density (cap/km2)",
              'Population 15-44 (%)',
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
            iqr = F, out = "03_Results\\SeperatedSAC.PR.html"
  ) 
}
