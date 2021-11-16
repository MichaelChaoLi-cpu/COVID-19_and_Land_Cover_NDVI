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
#plot(us_shape) # now there are 3103 records
gc() 

tes <- tes %>%
  dplyr::select(mortality, incidence_proportion, everything()) %>% as.data.frame()
stargazer(tes,
          title = "Table XXX: Test",  type = "text", 
          no.space = T,
          covariate.labels = c(
            "Mortality Rate(cases/1000)", "Prevalence Rate (cases/1000)",
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
          iqr = F, out = "03_Results\\CrossSectionalStatisticSummary.html"
) 

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

reg.form.pr.all <- reg.form.pr(
  paste0("Open_Water_perc", "+",
         "Developed_Open_Space_perc",  "+","Developed_Low_Intensity_perc",  "+",
         "Developed_Medium_Intensity_perc", "+",
         "Developed_High_Intensity_perc",  "+", "Deciduous_Forest_perc",  "+", 
         "Evergreen_Forest_perc", "+", "Mixed_Forest_perc",  "+", "Shrub_perc", "+",
         "Grassland_perc",  "+",
         "Woody_Wetlands_perc", "+", "Emergent_Herbaceous_Wetlands_perc"
  )
)

(lmLMtests.mort.all <- lm.LMtests(lm(reg.form.mort.all, data = us_shape@data),
                                  listw = lw, zero.policy = T,
                                  test=c("RLMerr", "RLMlag", "SARMA")))
(lmLMtests.pr.all <- lm.LMtests(lm(reg.form.pr.all, data = us_shape@data), 
                                model.pr.all, listw = lw, zero.policy = T,
                                test=c("RLMerr", "RLMlag", "SARMA")))
## SAC model impact
sac.mort.all <- sacsarlm(reg.form.mort.all, data = us_shape@data, listw = lw, 
                         zero.policy=TRUE, tol.solve = 1e-30)
sac.pr.all <- sacsarlm(reg.form.pr.all, data = us_shape@data, listw = lw, 
                       zero.policy=TRUE, tol.solve = 1e-30)
summary(sac.mort.all)
1-(sac.mort.all$SSE/(var(us_shape@data$mortality)*(length(us_shape@data$mortality)-1)))
summary(sac.pr.all)
1-(sac.pr.all$SSE/(var(us_shape@data$incidence_proportion)*(length(us_shape@data$incidence_proportion)-1)))

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

impact.table.sac.death %>% 
  xlsx::write.xlsx("03_Results/04_01RE_SACMortalityLandCoverCross.xlsx", sheetName = "Sheet1", 
                   row.names = F)
impact.table.sac.conf %>% 
  xlsx::write.xlsx("03_Results/04_02RE_SACPrevalenceLandCoverCross.xlsx", sheetName = "Sheet1", 
                   row.names = F)

corr.mat <- cor(tes, method = "pearson", use = "complete.obs")
view(corr.mat)
cor.test(us_shape@data$smoker_rate_2019, us_shape@data$log_Median_Household_Income_2018)
cor.test(us_shape@data$pm25_mean, us_shape@data$summer_rmax_mean)
