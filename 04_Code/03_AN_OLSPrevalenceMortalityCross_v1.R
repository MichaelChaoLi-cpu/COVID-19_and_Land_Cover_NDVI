# Author: M.L.

# input: dataset.Rdata

# output: SeperatedOLS.PR.html
# output: SeperatedOLS.MR.html

# SeperatedOLS.PR.html: "" this is the results of OLS regression to examine the 
#                          relationship between land cover and prevalence (PR) of COVID-19.
#                          The standard errors of the parameters have been fixed.

# SeperatedOLS.MR.html: "" this is the results of OLS regression to examine the 
#                          relationship between land cover and mortality (MR) of COVID-19.
#                          The standard errors of the parameters have been fixed.
# 

# Note: This script also includes the GWR regression. However, most local parameters
#       are not significant. So, we DO NOT apply this method in the analysis.

# end

library(tidyverse)
library(dplyr)
library(lmtest)
library(olsrr)
library(sandwich)
library(spdep)
library(spgwr)
library(GWmodel)
library(stargazer)
library(parallel)
library(tmap)

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

ols.reg.mort <- function(variable.number){
  reg.form <- reg.form.mort(variables.in.reg.form[variable.number])
  model <- lm(reg.form,
                data = dataset)
  return(model)
}

rse.mort.bptest <- function(model){
  test <- lmtest::bptest(model)
  if(test$p.value < 0.1){
    rse <- vcovHAC(model)
    rse.2 <- sqrt(diag(rse))
  } else {
    rse.2 <- NULL
  }
  return(rse.2)
}

model.mort.1 <- ols.reg.mort(1)
rse.mort.1 <- rse.mort.bptest(ols.reg.mort(1))

model.mort.2 <- ols.reg.mort(2) 
rse.mort.2 <- rse.mort.bptest(ols.reg.mort(2))

model.mort.3 <- ols.reg.mort(3) 
rse.mort.3 <- rse.mort.bptest(ols.reg.mort(3))

model.mort.4 <- ols.reg.mort(4) 
rse.mort.4 <- rse.mort.bptest(ols.reg.mort(4))

model.mort.5 <- ols.reg.mort(5) 
rse.mort.5 <- rse.mort.bptest(ols.reg.mort(5))

model.mort.6 <- ols.reg.mort(6) 
rse.mort.6 <- rse.mort.bptest(ols.reg.mort(6))

model.mort.7 <- ols.reg.mort(7) 
rse.mort.7 <- rse.mort.bptest(ols.reg.mort(7))

model.mort.8 <- ols.reg.mort(8) 
rse.mort.8 <- rse.mort.bptest(ols.reg.mort(8))

model.mort.9 <- ols.reg.mort(9) 
rse.mort.9 <- rse.mort.bptest(ols.reg.mort(9))

model.mort.10 <- ols.reg.mort(10) 
rse.mort.10 <- rse.mort.bptest(ols.reg.mort(10))

model.mort.11 <- ols.reg.mort(11) 
rse.mort.11 <- rse.mort.bptest(ols.reg.mort(11))

model.mort.12 <- ols.reg.mort(12) 
rse.mort.12 <- rse.mort.bptest(ols.reg.mort(12))

model.mort.13 <- ols.reg.mort(13) 
rse.mort.13 <- rse.mort.bptest(ols.reg.mort(13))

model.mort.14 <- ols.reg.mort(14) 
rse.mort.14 <- rse.mort.bptest(ols.reg.mort(14))

model.mort.15 <- ols.reg.mort(15) 
rse.mort.15 <- rse.mort.bptest(ols.reg.mort(15))


stargazer(model.mort.1, model.mort.2, model.mort.3, model.mort.4, model.mort.5, 
          model.mort.6, model.mort.7, model.mort.8,model.mort.9, model.mort.10, 
          model.mort.11, model.mort.12, model.mort.13, model.mort.14, 
          model.mort.15,
          title = "Table XXX: Test",  type = "text", 
          se = list(rse.mort.1, rse.mort.2, rse.mort.3, rse.mort.4, rse.mort.5, 
                    rse.mort.6, rse.mort.7, rse.mort.8,rse.mort.9, rse.mort.10, 
                    rse.mort.11, rse.mort.12, rse.mort.13, rse.mort.14, 
                    rse.mort.15),
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
          iqr = F, out = "03_Results\\SeperatedOLS.MR.html"
) 

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

ols.reg.pr <- function(variable.number){
  reg.form.pr <- reg.form.pr(variables.in.reg.form[variable.number])
  model <- lm(reg.form.pr,
              data = dataset)
  return(model)
}

rse.pr.bptest <- function(model){
  test <- lmtest::bptest(model)
  if(test$p.value < 0.1){
    rse <- vcovHAC(model)
    rse.2 <- sqrt(diag(rse))
  } else {
    rse.2 <- NULL
  }
  return(rse.2)
}

model.pr.1 <- ols.reg.pr(1) 
rse.pr.1 <- rse.pr.bptest(ols.reg.pr(1))

model.pr.2 <- ols.reg.pr(2) 
rse.pr.2 <- rse.pr.bptest(ols.reg.pr(2))

model.pr.3 <- ols.reg.pr(3) 
rse.pr.3 <- rse.pr.bptest(ols.reg.pr(3))

model.pr.4 <- ols.reg.pr(4) 
rse.pr.4 <- rse.pr.bptest(ols.reg.pr(4))

model.pr.5 <- ols.reg.pr(5) 
rse.pr.5 <- rse.pr.bptest(ols.reg.pr(5))

model.pr.6 <- ols.reg.pr(6) 
rse.pr.6 <- rse.pr.bptest(ols.reg.pr(6))

model.pr.7 <- ols.reg.pr(7) 
rse.pr.7 <- rse.pr.bptest(ols.reg.pr(7))

model.pr.8 <- ols.reg.pr(8) 
rse.pr.8 <- rse.pr.bptest(ols.reg.pr(8))

model.pr.9 <- ols.reg.pr(9) 
rse.pr.9 <- rse.pr.bptest(ols.reg.pr(9))

model.pr.10 <- ols.reg.pr(10) 
rse.pr.10 <- rse.pr.bptest(ols.reg.pr(10))

model.pr.11 <- ols.reg.pr(11) 
rse.pr.11 <- rse.pr.bptest(ols.reg.pr(11))

model.pr.12 <- ols.reg.pr(12) 
rse.pr.12 <- rse.pr.bptest(ols.reg.pr(12))

model.pr.13 <- ols.reg.pr(13) 
rse.pr.13 <- rse.pr.bptest(ols.reg.pr(13))

model.pr.14 <- ols.reg.pr(14) 
rse.pr.14 <- rse.pr.bptest(ols.reg.pr(14))

model.pr.15 <- ols.reg.pr(15) 
rse.pr.15 <- rse.pr.bptest(ols.reg.pr(15))


stargazer(model.pr.1, model.pr.2, model.pr.3, model.pr.4, model.pr.5, model.pr.6,
          model.pr.7, model.pr.8, model.pr.9, model.pr.10, model.pr.11,
          model.pr.12, model.pr.13, model.pr.14, model.pr.15, 
          title = "Table XXX: Test",  type = "text", 
          se = list(rse.pr.1, rse.pr.2, rse.pr.3, rse.pr.4, rse.pr.5, rse.pr.6,
                    rse.pr.7, rse.pr.8, rse.pr.9, rse.pr.10, rse.pr.11,
                    rse.pr.12, rse.pr.13, rse.pr.14, rse.pr.15),
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
          iqr = F, out = "03_Results\\SeperatedOLS.PR.html"
)

# save.image("00_Rdata\\Regression.Rdata")

suitable <- F
if(suitable)
{
  # Moran I Test
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
  plot(us_shape)
  rm(tes)
  gc()
  
  nb <- poly2nb(us_shape, queen = T)
  lw <- nb2listw(nb, style="W",zero.policy=T) 
  
  ols.reg.moran.pr <- function(variable.number){
    reg.form <- reg.form.pr(variables.in.reg.form[variable.number])
    model <- lm(reg.form,
                data = us_shape@data)
    return(model)
  }
  
  i <- 1
  while (i < 16) {
    model.1 <- ols.reg.moran.pr(i)
    test <- lm.morantest(model.1, lw, zero.policy = T)
    print(test)
    i = i + 1
  }
  gc()
  
  
  # GWR
  GWRbandwidth.1 <- bw.gwr(reg.form.mort(variables.in.reg.form[1]), data = us_shape, 
                         adaptive = F)
  gwr.model.1 <- gwr.basic(reg.form.mort(variables.in.reg.form[1]), data = us_shape,
                         bw = GWRbandwidth.1, adaptive = F)
  GWRbandwidth.2 <- bw.gwr(reg.form.mort(variables.in.reg.form[2]), data = us_shape, 
                           adaptive = F)
  gwr.model.2 <- gwr.basic(reg.form.mort(variables.in.reg.form[2]), data = us_shape,
                           bw = GWRbandwidth.2, adaptive = F)
  GWRbandwidth.3 <- bw.gwr(reg.form.mort(variables.in.reg.form[3]), data = us_shape, 
                           adaptive = F)
  gwr.model.3 <- gwr.basic(reg.form.mort(variables.in.reg.form[3]), data = us_shape,
                           bw = GWRbandwidth.3, adaptive = F)
  GWRbandwidth.4 <- bw.gwr(reg.form.mort(variables.in.reg.form[4]), data = us_shape, 
                           adaptive = F)
  gwr.model.4 <- gwr.basic(reg.form.mort(variables.in.reg.form[4]), data = us_shape,
                           bw = GWRbandwidth.4, adaptive = F)
  GWRbandwidth.5 <- bw.gwr(reg.form.mort(variables.in.reg.form[5]), data = us_shape, 
                           adaptive = F)
  gwr.model.5 <- gwr.basic(reg.form.mort(variables.in.reg.form[5]), data = us_shape,
                           bw = GWRbandwidth.5, adaptive = F)
  GWRbandwidth.6 <- bw.gwr(reg.form.mort(variables.in.reg.form[6]), data = us_shape, 
                           adaptive = F)
  gwr.model.6 <- gwr.basic(reg.form.mort(variables.in.reg.form[6]), data = us_shape,
                           bw = GWRbandwidth.6, adaptive = F)
  GWRbandwidth.7 <- bw.gwr(reg.form.mort(variables.in.reg.form[7]), data = us_shape, 
                           adaptive = F)
  gwr.model.7 <- gwr.basic(reg.form.mort(variables.in.reg.form[7]), data = us_shape,
                           bw = GWRbandwidth.7, adaptive = F)
  GWRbandwidth.8 <- bw.gwr(reg.form.mort(variables.in.reg.form[8]), data = us_shape, 
                           adaptive = F)
  gwr.model.8 <- gwr.basic(reg.form.mort(variables.in.reg.form[8]), data = us_shape,
                           bw = GWRbandwidth.8, adaptive = F)
  GWRbandwidth.9 <- bw.gwr(reg.form.mort(variables.in.reg.form[9]), data = us_shape, 
                           adaptive = F)
  gwr.model.9 <- gwr.basic(reg.form.mort(variables.in.reg.form[9]), data = us_shape,
                           bw = GWRbandwidth.9, adaptive = F)
  GWRbandwidth.10 <- bw.gwr(reg.form.mort(variables.in.reg.form[10]), data = us_shape, 
                           adaptive = F)
  gwr.model.10 <- gwr.basic(reg.form.mort(variables.in.reg.form[10]), data = us_shape,
                           bw = GWRbandwidth.10, adaptive = F)
  GWRbandwidth.11 <- bw.gwr(reg.form.mort(variables.in.reg.form[11]), data = us_shape, 
                           adaptive = F)
  gwr.model.11 <- gwr.basic(reg.form.mort(variables.in.reg.form[11]), data = us_shape,
                           bw = GWRbandwidth.11, adaptive = F)
  GWRbandwidth.12 <- bw.gwr(reg.form.mort(variables.in.reg.form[12]), data = us_shape, 
                           adaptive = F)
  gwr.model.12 <- gwr.basic(reg.form.mort(variables.in.reg.form[12]), data = us_shape,
                           bw = GWRbandwidth.12, adaptive = F)
  GWRbandwidth.13 <- bw.gwr(reg.form.mort(variables.in.reg.form[13]), data = us_shape, 
                           adaptive = F)
  gwr.model.13 <- gwr.basic(reg.form.mort(variables.in.reg.form[13]), data = us_shape,
                           bw = GWRbandwidth.13, adaptive = F)
  GWRbandwidth.14 <- bw.gwr(reg.form.mort(variables.in.reg.form[14]), data = us_shape, 
                           adaptive = F)
  gwr.model.14 <- gwr.basic(reg.form.mort(variables.in.reg.form[14]), data = us_shape,
                           bw = GWRbandwidth.14, adaptive = F)
  GWRbandwidth.15 <- bw.gwr(reg.form.mort(variables.in.reg.form[15]), data = us_shape, 
                           adaptive = F)
  gwr.model.15 <- gwr.basic(reg.form.mort(variables.in.reg.form[15]), data = us_shape,
                           bw = GWRbandwidth.15, adaptive = F)
  
  (abs(gwr.model.1$SDF$Open_Water_capi_TV) > 1.645) %>% summary()
  (abs(gwr.model.2$SDF$Developed_Open_Space_capi_TV ) > 1.645) %>% summary()
  (abs(gwr.model.3$SDF$Developed_Low_Intensity_capi_TV ) > 1.645) %>% summary()
  (abs(gwr.model.4$SDF$Developed_Medium_Intensity_capi_TV ) > 1.645) %>% summary()
  (abs(gwr.model.5$SDF$Developed_High_Intensity_capi_TV ) > 1.645) %>% summary()
  (abs(gwr.model.6$SDF$Barren_Land_capi_TV) > 1.645) %>% summary()
  (abs(gwr.model.7$SDF$Deciduous_Forest_capi_TV) > 1.645) %>% summary()
  (abs(gwr.model.8$SDF$Evergreen_Forest_capi_TV) > 1.645) %>% summary()
  (abs(gwr.model.9$SDF$Mixed_Forest_capi_TV) > 1.645) %>% summary()
  (abs(gwr.model.10$SDF$Shrub_capi_TV) > 1.645) %>% summary()
  (abs(gwr.model.11$SDF$Grassland_capi_TV) > 1.645) %>% summary()
  (abs(gwr.model.12$SDF$Pasture_capi_TV) > 1.645) %>% summary()
  (abs(gwr.model.13$SDF$Cultivated_Crops_capi_TV) > 1.645) %>% summary()
  (abs(gwr.model.14$SDF$Woody_Wetlands_capi_TV) > 1.645) %>% summary()
  (abs(gwr.model.15$SDF$Emergent_Herbaceous_Wetlands_capi_TV) > 1.645) %>% summary()
  
  tm_shape(gwr.model.1$SDF) +
    tm_polygons(col = 'Open_Water_capi', pal = "-RdYlGn")
  tm_shape(gwr.model.2$SDF) +
    tm_polygons(col = 'Developed_Open_Space_capi', pal = "-RdYlGn")
  tm_shape(gwr.model.3$SDF) +
    tm_polygons(col = 'Developed_Low_Intensity_capi', pal = "-RdYlGn")
  tm_shape(gwr.model.4$SDF) +
    tm_polygons(col = 'Developed_Medium_Intensity_capi', pal = "-RdYlGn")
  tm_shape(gwr.model.5$SDF) +
    tm_polygons(col = 'Developed_High_Intensity_capi', pal = "-RdYlGn")
  tm_shape(gwr.model.6$SDF) +
    tm_polygons(col = 'Barren_Land_capi', pal = "-RdYlGn")
  tm_shape(gwr.model.7$SDF) +
    tm_polygons(col = 'Deciduous_Forest_capi', pal = "-RdYlGn")
  tm_shape(gwr.model.8$SDF) +
    tm_polygons(col = 'Evergreen_Forest_capi', pal = "-RdYlGn")
  tm_shape(gwr.model.9$SDF) +
    tm_polygons(col = 'Mixed_Forest_capi', pal = "-RdYlGn")
  tm_shape(gwr.model.10$SDF) +
    tm_polygons(col = 'Shrub_capi', pal = "-RdYlGn")
  tm_shape(gwr.model.11$SDF) +
    tm_polygons(col = 'Grassland_capi', pal = "-RdYlGn")
  tm_shape(gwr.model.12$SDF) +
    tm_polygons(col = 'Pasture_capi', pal = "-RdYlGn")
  tm_shape(gwr.model.13$SDF) +
    tm_polygons(col = 'Cultivated_Crops_capi', pal = "-RdYlGn")
  tm_shape(gwr.model.14$SDF) +
    tm_polygons(col = 'Woody_Wetlands_capi', pal = "-RdYlGn")
  tm_shape(gwr.model.15$SDF) +
    tm_polygons(col = 'Emergent_Herbaceous_Wetlands_capi', pal = "-RdYlGn")
}
