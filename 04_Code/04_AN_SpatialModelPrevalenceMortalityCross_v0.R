# Author: M.L.

# input: dataset.Rdata

# Note: In 03 script, the Moran's I tests indicate that the residuals from OLS are
#       spatially clustering. Therefore, spatial models are required. Furthermore,
#       according to the Lagrange Multiplier diagnostics for spatial dependence in
#       linear models (lm.LMtests), both spatial lag and spatial error dependence 
#       are significant. Therefore, the SAC model should be the best model in this
#       study.

# end

library(tidyverse)
library(dplyr)
library(spatialreg)
library(rgdal)
library(tigris)
library(spdep)
library(stargazer)

load("00_RData/dataset.Rdata")

# to guarantee there are the same dataset in the spatial model and ols model
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
                international_movement_restrictions, 
                #TestCode pop_density, 
                mortality,
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
  #TestCode #pop_density #+ 
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
  reg.form.mort.all <- reg.form.mort(
    paste0("Open_Water_perc", "+",
           "Developed_Open_Space_perc",  "+","Developed_Low_Intensity_perc",  "+",
           "Developed_Medium_Intensity_perc", "+",
           "Developed_High_Intensity_perc",  "+", "Deciduous_Forest_perc",  "+", 
           "Evergreen_Forest_perc", "+", "Mixed_Forest_perc",  "+", "Shrub_perc", "+",
           "Grassland_perc",  "+",
           "Woody_Wetlands_perc", "+", "Emergent_Herbaceous_Wetlands_perc"
    )
  )
  
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
  reg.form.pr.all <- reg.form.mort(
    paste0("Open_Water_perc", "+",
           "Developed_Open_Space_perc",  "+","Developed_Low_Intensity_perc",  "+",
           "Developed_Medium_Intensity_perc", "+",
           "Developed_High_Intensity_perc",  "+", "Deciduous_Forest_perc",  "+", 
           "Evergreen_Forest_perc", "+", "Mixed_Forest_perc",  "+", "Shrub_perc", "+",
           "Grassland_perc",  "+",
           "Woody_Wetlands_perc", "+", "Emergent_Herbaceous_Wetlands_perc"
    )
  )
}

SPATIAL_MODEL_TEST <- T
if(SPATIAL_MODEL_TEST) {
  lmLMtests.mort.1 <- lm.LMtests(model.mort.1, listw = lw, zero.policy = T,
                                 test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.mort.2 <- lm.LMtests(model.mort.2, listw = lw, zero.policy = T,
                                 test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.mort.3 <- lm.LMtests(model.mort.3, listw = lw, zero.policy = T,
                                 test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.mort.4 <- lm.LMtests(model.mort.4, listw = lw, zero.policy = T,
                                 test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.mort.5 <- lm.LMtests(model.mort.5, listw = lw, zero.policy = T,
                                 test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.mort.6 <- lm.LMtests(model.mort.6, listw = lw, zero.policy = T,
                                 test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.mort.7 <- lm.LMtests(model.mort.7, listw = lw, zero.policy = T,
                                 test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.mort.8 <- lm.LMtests(model.mort.8, listw = lw, zero.policy = T,
                                 test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.mort.9 <- lm.LMtests(model.mort.9, listw = lw, zero.policy = T,
                                 test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.mort.10 <- lm.LMtests(model.mort.10, listw = lw, zero.policy = T,
                                 test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.mort.11 <- lm.LMtests(model.mort.11, listw = lw, zero.policy = T,
                                 test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.mort.12 <- lm.LMtests(model.mort.12, listw = lw, zero.policy = T,
                                 test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.mort.13 <- lm.LMtests(model.mort.13, listw = lw, zero.policy = T,
                                 test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.mort.14 <- lm.LMtests(model.mort.14, listw = lw, zero.policy = T,
                                 test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.mort.15 <- lm.LMtests(model.mort.15, listw = lw, zero.policy = T,
                                 test=c("RLMerr", "RLMlag", "SARMA"))
  (lmLMtests.mort.all <- lm.LMtests(model.mort.all, listw = lw, zero.policy = T,
                                  test=c("RLMerr", "RLMlag", "SARMA")))
  
  lmLMtests.pr.1 <- lm.LMtests(model.pr.1, listw = lw, zero.policy = T,
                                 test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.pr.2 <- lm.LMtests(model.pr.2, listw = lw, zero.policy = T,
                                 test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.pr.3 <- lm.LMtests(model.pr.3, listw = lw, zero.policy = T,
                                 test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.pr.4 <- lm.LMtests(model.pr.4, listw = lw, zero.policy = T,
                                 test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.pr.5 <- lm.LMtests(model.pr.5, listw = lw, zero.policy = T,
                                 test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.pr.6 <- lm.LMtests(model.pr.6, listw = lw, zero.policy = T,
                                 test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.pr.7 <- lm.LMtests(model.pr.7, listw = lw, zero.policy = T,
                                 test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.pr.8 <- lm.LMtests(model.pr.8, listw = lw, zero.policy = T,
                                 test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.pr.9 <- lm.LMtests(model.pr.9, listw = lw, zero.policy = T,
                                 test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.pr.10 <- lm.LMtests(model.pr.10, listw = lw, zero.policy = T,
                                  test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.pr.11 <- lm.LMtests(model.pr.11, listw = lw, zero.policy = T,
                                  test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.pr.12 <- lm.LMtests(model.pr.12, listw = lw, zero.policy = T,
                                  test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.pr.13 <- lm.LMtests(model.pr.13, listw = lw, zero.policy = T,
                                  test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.pr.14 <- lm.LMtests(model.pr.14, listw = lw, zero.policy = T,
                                  test=c("RLMerr", "RLMlag", "SARMA"))
  lmLMtests.pr.15 <- lm.LMtests(model.pr.15, listw = lw, zero.policy = T,
                                  test=c("RLMerr", "RLMlag", "SARMA"))
  (lmLMtests.pr.all <- lm.LMtests(model.pr.all, listw = lw, zero.policy = T,
                                test=c("RLMerr", "RLMlag", "SARMA")))
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
  error.mort.1 <- errorsarlm(reg.form.mort.1, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  error.mort.2 <- errorsarlm(reg.form.mort.2, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  error.mort.3 <- errorsarlm(reg.form.mort.3, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  error.mort.4 <- errorsarlm(reg.form.mort.4, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  error.mort.5 <- errorsarlm(reg.form.mort.5, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  error.mort.6 <- errorsarlm(reg.form.mort.6, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  error.mort.7 <- errorsarlm(reg.form.mort.7, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  error.mort.8 <- errorsarlm(reg.form.mort.8, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  error.mort.9 <- errorsarlm(reg.form.mort.9, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  error.mort.10 <- errorsarlm(reg.form.mort.10, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  error.mort.11 <- errorsarlm(reg.form.mort.11, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  error.mort.12 <- errorsarlm(reg.form.mort.12, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  error.mort.13 <- errorsarlm(reg.form.mort.13, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  error.mort.14 <- errorsarlm(reg.form.mort.14, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  error.mort.15 <- errorsarlm(reg.form.mort.15, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  
  
  stargazer(error.mort.1, error.mort.2, error.mort.3, error.mort.4,
            error.mort.5, error.mort.6, error.mort.7, error.mort.8,
            error.mort.9, error.mort.10, error.mort.11, error.mort.12,
            error.mort.13, error.mort.14, error.mort.15,
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
  
  error.pr.1 <- errorsarlm(reg.form.pr.1, data = us_shape@data, listw = lw, 
                       zero.policy=TRUE, tol.solve = 1e-30)
  error.pr.2 <- errorsarlm(reg.form.pr.2, data = us_shape@data, listw = lw, 
                       zero.policy=TRUE, tol.solve = 1e-30)
  error.pr.3 <- errorsarlm(reg.form.pr.3, data = us_shape@data, listw = lw, 
                       zero.policy=TRUE, tol.solve = 1e-30)
  error.pr.4 <- errorsarlm(reg.form.pr.4, data = us_shape@data, listw = lw, 
                       zero.policy=TRUE, tol.solve = 1e-30)
  error.pr.5 <- errorsarlm(reg.form.pr.5, data = us_shape@data, listw = lw, 
                       zero.policy=TRUE, tol.solve = 1e-30)
  error.pr.6 <- errorsarlm(reg.form.pr.6, data = us_shape@data, listw = lw, 
                       zero.policy=TRUE, tol.solve = 1e-30)
  error.pr.7 <- errorsarlm(reg.form.pr.7, data = us_shape@data, listw = lw, 
                       zero.policy=TRUE, tol.solve = 1e-30)
  error.pr.8 <- errorsarlm(reg.form.pr.8, data = us_shape@data, listw = lw, 
                       zero.policy=TRUE, tol.solve = 1e-30)
  error.pr.9 <- errorsarlm(reg.form.pr.9, data = us_shape@data, listw = lw, 
                       zero.policy=TRUE, tol.solve = 1e-30)
  error.pr.10 <- errorsarlm(reg.form.pr.10, data = us_shape@data, listw = lw, 
                        zero.policy=TRUE, tol.solve = 1e-30)
  error.pr.11 <- errorsarlm(reg.form.pr.11, data = us_shape@data, listw = lw, 
                        zero.policy=TRUE, tol.solve = 1e-30)
  error.pr.12 <- errorsarlm(reg.form.pr.12, data = us_shape@data, listw = lw, 
                        zero.policy=TRUE, tol.solve = 1e-30)
  error.pr.13 <- errorsarlm(reg.form.pr.13, data = us_shape@data, listw = lw, 
                        zero.policy=TRUE, tol.solve = 1e-30)
  error.pr.14 <- errorsarlm(reg.form.pr.14, data = us_shape@data, listw = lw, 
                        zero.policy=TRUE, tol.solve = 1e-30)
  error.pr.15 <- errorsarlm(reg.form.pr.15, data = us_shape@data, listw = lw, 
                        zero.policy=TRUE, tol.solve = 1e-30)
  
  stargazer(error.pr.1, error.pr.2, error.pr.3, error.pr.4,
            error.pr.5, error.pr.6, error.pr.7, error.pr.8,
            error.pr.9, error.pr.10, error.pr.11, error.pr.12,
            error.pr.13, error.pr.14, error.pr.15,
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
  sac.mort.1 <- sacsarlm(reg.form.mort.1, data = us_shape@data, listw = lw, 
                           zero.policy=TRUE, tol.solve = 1e-30)
  sac.mort.2 <- sacsarlm(reg.form.mort.2, data = us_shape@data, listw = lw, 
                           zero.policy=TRUE, tol.solve = 1e-30)
  sac.mort.3 <- sacsarlm(reg.form.mort.3, data = us_shape@data, listw = lw, 
                           zero.policy=TRUE, tol.solve = 1e-30)
  sac.mort.4 <- sacsarlm(reg.form.mort.4, data = us_shape@data, listw = lw, 
                           zero.policy=TRUE, tol.solve = 1e-30)
  sac.mort.5 <- sacsarlm(reg.form.mort.5, data = us_shape@data, listw = lw, 
                           zero.policy=TRUE, tol.solve = 1e-30)
  sac.mort.6 <- sacsarlm(reg.form.mort.6, data = us_shape@data, listw = lw, 
                           zero.policy=TRUE, tol.solve = 1e-30)
  sac.mort.7 <- sacsarlm(reg.form.mort.7, data = us_shape@data, listw = lw, 
                           zero.policy=TRUE, tol.solve = 1e-30)
  sac.mort.8 <- sacsarlm(reg.form.mort.8, data = us_shape@data, listw = lw, 
                           zero.policy=TRUE, tol.solve = 1e-30)
  sac.mort.9 <- sacsarlm(reg.form.mort.9, data = us_shape@data, listw = lw, 
                           zero.policy=TRUE, tol.solve = 1e-30)
  sac.mort.10 <- sacsarlm(reg.form.mort.10, data = us_shape@data, listw = lw, 
                            zero.policy=TRUE, tol.solve = 1e-30)
  sac.mort.11 <- sacsarlm(reg.form.mort.11, data = us_shape@data, listw = lw, 
                            zero.policy=TRUE, tol.solve = 1e-30)
  sac.mort.12 <- sacsarlm(reg.form.mort.12, data = us_shape@data, listw = lw, 
                            zero.policy=TRUE, tol.solve = 1e-30)
  sac.mort.13 <- sacsarlm(reg.form.mort.13, data = us_shape@data, listw = lw, 
                            zero.policy=TRUE, tol.solve = 1e-30)
  sac.mort.14 <- sacsarlm(reg.form.mort.14, data = us_shape@data, listw = lw, 
                            zero.policy=TRUE, tol.solve = 1e-30)
  sac.mort.15 <- sacsarlm(reg.form.mort.15, data = us_shape@data, listw = lw, 
                            zero.policy=TRUE, tol.solve = 1e-30)
  sac.mort.all <- sacsarlm(reg.form.mort.all, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  
  
  stargazer(sac.mort.1, sac.mort.2, sac.mort.3, sac.mort.4,
            sac.mort.5, sac.mort.6, sac.mort.7, sac.mort.8,
            sac.mort.9, sac.mort.10, sac.mort.11, sac.mort.12,
            sac.mort.13, sac.mort.14, sac.mort.15,
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
  
  sac.pr.1 <- sacsarlm(reg.form.pr.1, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  sac.pr.2 <- sacsarlm(reg.form.pr.2, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  sac.pr.3 <- sacsarlm(reg.form.pr.3, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  sac.pr.4 <- sacsarlm(reg.form.pr.4, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  sac.pr.5 <- sacsarlm(reg.form.pr.5, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  sac.pr.6 <- sacsarlm(reg.form.pr.6, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  sac.pr.7 <- sacsarlm(reg.form.pr.7, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  sac.pr.8 <- sacsarlm(reg.form.pr.8, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  sac.pr.9 <- sacsarlm(reg.form.pr.9, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
  sac.pr.10 <- sacsarlm(reg.form.pr.10, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  sac.pr.11 <- sacsarlm(reg.form.pr.11, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  sac.pr.12 <- sacsarlm(reg.form.pr.12, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  sac.pr.13 <- sacsarlm(reg.form.pr.13, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  sac.pr.14 <- sacsarlm(reg.form.pr.14, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  sac.pr.15 <- sacsarlm(reg.form.pr.15, data = us_shape@data, listw = lw, 
                          zero.policy=TRUE, tol.solve = 1e-30)
  sac.pr.all <- sacsarlm(reg.form.pr.all, data = us_shape@data, listw = lw, 
                        zero.policy=TRUE, tol.solve = 1e-30)
  
  stargazer(sac.pr.1, sac.pr.2, sac.pr.3, sac.pr.4,
            sac.pr.5, sac.pr.6, sac.pr.7, sac.pr.8,
            sac.pr.9, sac.pr.10, sac.pr.11, sac.pr.12,
            sac.pr.13, sac.pr.14, sac.pr.15,
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

## SAC model impact
impact_summary_death_CS <- summary(impacts(sac.mort.all, listw = lw,
                                                  R = 1000), zstats = TRUE, short = T) 
impact_summary_conf_CS <- summary(impacts(sac.pr.all, listw = lw,
                                           R = 1000), zstats = TRUE, short = T) 
source('04_Code/07_AF_OutputSplmImpactFunction_v1.R')
impact_summary_death_CS_variable_names <- 
  c('Open Water (%)', 'Open Space Developed Area (%)',"Low Intensity Developed Area (%)",
    "Medium Intensity Developed Area (%)",'High Intensity Developed Area (%)',
    'Deciduous Forest (%)','Evergreen Forest (%)','Mixed Forest (%)', 'Shrub (%)',
    'Grassland (%)', 'Woody Wetlands (%)','Emergent Herbaceous Wetlands (%)',
    "Prevalence Rate (cap/1000)",'Gathering Restrictions (days)','Transport Closing (days)',
    'Staying Home (days)',"Internal MoRe (days)", "International MoRe (days)",
    'Population 15-44 (%)','Population 45-64 (%)', 'Population >= 65 (%)', 'Black People (%)', 
    'Hispanic People (%)',  'Male (%)','Umemployment Rate', 'Median Household Income (logatithm)',
    'Poverty Rate (%)', 'Adults Without High School Diploma (%)','Poor Health Rate (%)',  
    'Poor Physical Health (days)', "Poor Mental Health (days)",'Adult Smoking Rate (%)',
    'Obesity Rate (%)','Physical Inactivity Rate (%)','Having Access To Exercise Opportunities (%)', 
    "Hospital Beds (bed/1000)",'Average Temperature In Summer','Average Temperature In Winter', 
    'Average Relative Humidity In Summer', 'Average Relative Humidity In Winter','PM2.5')
impact_summary_conf_CS_variable_names <- 
  c('Open Water (%)', 'Open Space Developed Area (%)',"Low Intensity Developed Area (%)",
    "Medium Intensity Developed Area (%)",'High Intensity Developed Area (%)',
    'Deciduous Forest (%)','Evergreen Forest (%)','Mixed Forest (%)', 'Shrub (%)',
    'Grassland (%)', 'Woody Wetlands (%)','Emergent Herbaceous Wetlands (%)',
    'Gathering Restrictions (days)','Transport Closing (days)',
    'Staying Home (days)',"Internal MoRe (days)", "International MoRe (days)",
    'Population 15-44 (%)','Population 45-64 (%)', 'Population >= 65 (%)', 'Black People (%)', 
    'Hispanic People (%)',  'Male (%)','Umemployment Rate', 'Median Household Income (logatithm)',
    'Poverty Rate (%)', 'Adults Without High School Diploma (%)','Poor Health Rate (%)',  
    'Poor Physical Health (days)', "Poor Mental Health (days)",'Adult Smoking Rate (%)',
    'Obesity Rate (%)','Physical Inactivity Rate (%)','Having Access To Exercise Opportunities (%)', 
    "Hospital Beds (bed/1000)",'Average Temperature In Summer','Average Temperature In Winter', 
    'Average Relative Humidity In Summer', 'Average Relative Humidity In Winter','PM2.5')
(impact.table.sac.death <- 
    output_SPML_model_impacts(impact_summary_death_CS, impact_summary_death_CS_variable_names))
(impact.table.sac.conf <- 
    output_SPML_model_impacts(impact_summary_conf_CS, impact_summary_conf_CS_variable_names))

save.image("Temp/02_SlmResultCross.RData")
