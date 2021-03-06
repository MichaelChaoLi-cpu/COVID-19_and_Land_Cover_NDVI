# Author: M.L.

# output: dataset.Rdata

# dataset.Rdata: "incidence_proportion" incidence_proportion = confirmed / population * 1000.
# dataset.Rdata: "mortality" mortality = deaths / population * 1000.

# v2: we update the land cover. v1's data are the data in 2016, but v2's data are 
#     the average values from 2001 to 2019

# end

library(readr)
library(tidyverse)
library("dplyr")
library(haven)
library(sp)
library(rgdal)
library(tigris)
library(spdep)
library(spData)
library(sf)
library(Matrix)
library(spatialreg)
library("Hmisc")
library("COVID19")
library(lubridate)
library(foreign)
library(olsrr)
library("readxl")


deat.conf.pop <- COVID19::covid19(country = "USA", level = 3) %>% filter(date == ymd("2021-11-01"))
deat.conf.pop <- deat.conf.pop %>% filter(administrative_area_level_2 != "Puerto Rico")
deat.conf.pop <- deat.conf.pop %>% select(confirmed, deaths, population, key_local)
deat.conf.pop <- deat.conf.pop %>% rename(key_numeric = key_local)
deat.conf.pop$key_numeric <- deat.conf.pop$key_numeric %>% as.numeric()

deat.conf.pop <- deat.conf.pop %>%
  mutate(
    CFR = deaths / confirmed * 100, # percentage
    incidence_proportion = confirmed / population * 1000, # infection per 1000
    mortality = deaths / population * 1000 # mortality per 1000
         )

restrictions <- COVID19::covid19(country = "USA", level = 3) %>% 
  filter(date < ymd("2021-10-15")) %>%
  filter(administrative_area_level_2 != "Puerto Rico")

restrictions <- restrictions %>%
  dplyr::select(id, gatherings_restrictions, transport_closing, stay_home_restrictions,
                internal_movement_restrictions, international_movement_restrictions) %>%
  mutate(
    gatherings_restrictions = ifelse(gatherings_restrictions > 0, 1, 0),
    transport_closing = ifelse(transport_closing > 0, 1, 0),
    stay_home_restrictions = ifelse(stay_home_restrictions > 0, 1, 0),
    internal_movement_restrictions = ifelse(internal_movement_restrictions > 0, 1, 0),
    international_movement_restrictions = ifelse(international_movement_restrictions > 0, 1, 0)
  )
restrictions <-
  aggregate(restrictions[,c(2:6)] ,by = list(restrictions$id), FUN = 'sum', na.rm = T)
restrictions <- restrictions %>% mutate(id = Group.1)
restrictions <- restrictions %>%
  dplyr::select(-Group.1)
covid.data <- merge(deat.conf.pop, restrictions)
rm(deat.conf.pop)
rm(restrictions)


###------------land cover https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover---
LC_2001 <- foreign::read.dbf(file = '01_Raster/02_LandCoverTable/LC_2001.dbf', as.is = T)
LC_2004 <- foreign::read.dbf(file = '01_Raster/02_LandCoverTable/LC_2004.dbf', as.is = T)
LC_2006 <- foreign::read.dbf(file = '01_Raster/02_LandCoverTable/LC_2006.dbf', as.is = T)
LC_2008 <- foreign::read.dbf(file = '01_Raster/02_LandCoverTable/LC_2008.dbf', as.is = T)
LC_2011 <- foreign::read.dbf(file = '01_Raster/02_LandCoverTable/LC_2011.dbf', as.is = T)
LC_2013 <- foreign::read.dbf(file = '01_Raster/02_LandCoverTable/LC_2013.dbf', as.is = T)
LC_2016 <- foreign::read.dbf(file = '01_Raster/02_LandCoverTable/LC_2016.dbf', as.is = T)
LC_2019 <- foreign::read.dbf(file = '01_Raster/02_LandCoverTable/LC_2019.dbf', as.is = T)
LC_total <- rbind(LC_2001, LC_2004, LC_2006, LC_2008, LC_2011, LC_2013, LC_2016, LC_2019)
rm(LC_2001, LC_2004, LC_2006, LC_2008, LC_2011, LC_2013, LC_2016, LC_2019)
LC_total <- LC_total %>% as.data.frame()
LC_total <- LC_total %>% aggregate(by = list(LC_total$GEOID), FUN=mean)

LC_total <- LC_total %>%
  rename(
    Unknown = VALUE_0,
    Open_Water = VALUE_11,
    Perenial_Ice = VALUE_12,
    Developed_Open_Space = VALUE_21,
    Developed_Low_Intensity = VALUE_22,
    Developed_Medium_Intensity = VALUE_23,
    Developed_High_Intensity = VALUE_24,
    Barren_Land = VALUE_31,
    Deciduous_Forest = VALUE_41,
    Evergreen_Forest = VALUE_42,
    Mixed_Forest = VALUE_43,
    Shrub = VALUE_52,
    Grassland = VALUE_71,
    Pasture = VALUE_81,
    Cultivated_Crops = VALUE_82,
    Woody_Wetlands = VALUE_90,
    Emergent_Herbaceous_Wetlands = VALUE_95
  )
LC_total <- LC_total %>% dplyr::select(-GEOID) %>% rename(GEOID = Group.1)
LC_total$GEOID <- LC_total$GEOID %>% as.numeric()
LC_total <- LC_total %>% rename(key_numeric = GEOID)

dataset <- merge(covid.data, LC_total)
dataset <- dataset %>%
  rowwise() %>%
  mutate(
    TotalArea = Unknown + Open_Water + Perenial_Ice + Developed_Open_Space + 
      Developed_Low_Intensity + Developed_Medium_Intensity + 
      Developed_High_Intensity + Barren_Land + Deciduous_Forest + 
      Evergreen_Forest + Mixed_Forest + Shrub + Grassland + Pasture + 
      Cultivated_Crops + Woody_Wetlands + Emergent_Herbaceous_Wetlands
  )
switch <- T
if(switch == T){
  dataset <- dataset %>%
    mutate(
      Unknown_perc = Unknown / TotalArea * 100,
      Open_Water_perc = Open_Water / TotalArea * 100,
      Perenial_Ice_perc = Perenial_Ice / TotalArea * 100,
      Developed_Open_Space_perc = Developed_Open_Space / TotalArea * 100,
      Developed_Low_Intensity_perc = Developed_Low_Intensity / TotalArea * 100,
      Developed_Medium_Intensity_perc = Developed_Medium_Intensity / TotalArea * 100,
      Developed_High_Intensity_perc = Developed_High_Intensity / TotalArea * 100,
      Barren_Land_perc = Barren_Land / TotalArea * 100,
      Deciduous_Forest_perc = Deciduous_Forest / TotalArea * 100,
      Evergreen_Forest_perc = Evergreen_Forest / TotalArea * 100,
      Mixed_Forest_perc = Mixed_Forest / TotalArea * 100,
      Shrub_perc = Shrub / TotalArea * 100,
      Grassland_perc = Grassland / TotalArea * 100,
      Pasture_perc = Pasture / TotalArea * 100,
      Cultivated_Crops_perc = Cultivated_Crops / TotalArea * 100,
      Woody_Wetlands_perc = Woody_Wetlands / TotalArea * 100,
      Emergent_Herbaceous_Wetlands_perc = Emergent_Herbaceous_Wetlands / TotalArea * 100,
      Green_rate = Deciduous_Forest_perc + Evergreen_Forest_perc + Mixed_Forest_perc + 
        Shrub_perc + Grassland_perc + Pasture_perc + Cultivated_Crops_perc,
      Blue_rate = Open_Water_perc +
        Woody_Wetlands_perc + Emergent_Herbaceous_Wetlands_perc,
      Grey_rate = Developed_Open_Space_perc + Developed_Low_Intensity_perc +
        Developed_Medium_Intensity_perc + Developed_High_Intensity_perc,
      Other_rate = Barren_Land_perc + Perenial_Ice_perc
    )
  switch <- F
} ###  percentage

dataset <- dataset %>%
  mutate(
    Unknown_capi = Unknown / population / 10000,
    Open_Water_capi = Open_Water / population / 10000,
    Perenial_Ice_capi = Perenial_Ice / population / 10000,
    Developed_Open_Space_capi = Developed_Open_Space / population / 10000,
    Developed_Low_Intensity_capi = Developed_Low_Intensity / population / 10000,
    Developed_Medium_Intensity_capi = Developed_Medium_Intensity / population / 10000,
    Developed_High_Intensity_capi = Developed_High_Intensity / population / 10000,
    Barren_Land_capi = Barren_Land / population / 10000,
    Deciduous_Forest_capi = Deciduous_Forest / population / 10000,
    Evergreen_Forest_capi = Evergreen_Forest / population / 10000,
    Mixed_Forest_capi = Mixed_Forest / population / 10000,
    Shrub_capi = Shrub / population / 10000,
    Grassland_capi = Grassland / population / 10000,
    Pasture_capi = Pasture / population / 10000,
    Cultivated_Crops_capi = Cultivated_Crops / population / 10000,
    Woody_Wetlands_capi = Woody_Wetlands / population / 10000,
    Emergent_Herbaceous_Wetlands_capi = Emergent_Herbaceous_Wetlands / population / 10000
  ) 
###------------land cover https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover---

# Median Income and Unemployment  https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/
income <- read_excel(path = '02_RawData\\Unemployment.xls', sheet = 1)
income$FIPStxt <- as.numeric(income$FIPStxt)
income <- income %>%
  select(FIPStxt, Unemployment_rate_2019, Median_Household_Income_2018) %>%
  mutate(log_Median_Household_Income_2018 = log(Median_Household_Income_2018)) %>%
  rename(
    key_numeric = FIPStxt
  )

dataset <- left_join(dataset, income)
rm(income)
# Median Income and Unemployment https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/

# Poverty Rate  https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/
poverty <- read.csv(file = '02_RawData\\PovertyEstimates.csv')
poverty$FIPStxt <- as.numeric(poverty$FIPStxt)
poverty <- poverty %>%
  select(FIPStxt, PCTPOVALL_2018) %>%
  rename(
    key_numeric = FIPStxt,
    poverty_rate = PCTPOVALL_2018
  )
dataset <- left_join(dataset, poverty)
rm(poverty)
# Poverty Rate  https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/

# Education https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/
education <- read.csv(file = '02_RawData\\Education.csv')
education <- education %>%
  select(FIPS.Code, Percent.of.adults.with.less.than.a.high.school.diploma..2014.18) %>%
  rename(
    key_numeric = FIPS.Code,
    less_than_high_school = Percent.of.adults.with.less.than.a.high.school.diploma..2014.18
  )
dataset <- left_join(dataset, education)
rm(education)
# Education https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/

# Age Group, Sex, Race  https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-detail.html
age_sex_race <- read.csv(file = '02_RawData\\cc-est2019-alldata.csv')
age_sex_race <- age_sex_race %>%
  subset(YEAR == 12) %>%
  mutate(CountyFIPS = STATE * 1000 + COUNTY) %>%
  select(-YEAR, -SUMLEV, -STATE, -COUNTY, -STNAME)

age <- age_sex_race %>%
  select(CountyFIPS, AGEGRP, TOT_POP)  %>%
  pivot_wider(names_from = AGEGRP, values_from = TOT_POP) %>%
  mutate(
    age0_14 = (`1` + `2` + `3`) / `0` * 100,
    age15_44 = (`4` + `5` + `6` + `7` + `8` + `9`) / `0` * 100,
    age45_64 = (`10` + `11` + `12` + `13`) / `0` * 100,
    age65_99 = (`14` + `15` + `16` + `17` + `18`) / `0` * 100
  ) %>%
  select(CountyFIPS, age0_14:age65_99)
sex <- age_sex_race %>%
  subset(AGEGRP == 0) %>%
  select(CountyFIPS, TOT_POP, TOT_MALE, TOT_FEMALE) %>%
  mutate(
    male = TOT_MALE / TOT_POP * 100
  ) %>%
  select(CountyFIPS, male)
race <- age_sex_race %>%
  subset(AGEGRP == 0) %>%
  select(-AGEGRP) %>%
  mutate(
    black_rate = (BA_MALE + BA_FEMALE) / TOT_POP * 100,
    hispanic_rate = (H_MALE + H_FEMALE) / TOT_POP * 100
  ) %>%
  select(CountyFIPS, black_rate, hispanic_rate)
rm(age_sex_race)
age <- age %>%
  rename(
    key_numeric = CountyFIPS
  )
race <- race %>%
  rename(
    key_numeric = CountyFIPS
  )
sex <- sex %>%
  rename(
    key_numeric = CountyFIPS
  )


dataset <- left_join(dataset, age)
rm(age)
dataset <- left_join(dataset, race)
rm(race)
dataset <- left_join(dataset, sex)
rm(sex)
# Age Group, Sex, Race  https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-detail.html

# obese, smokers, pysical inactivity, access to excercise https://www.countyhealthrankings.org/explore-health-rankings/rankings-data-documentation
obese_smoker_PIA_ATE <- read_excel(path = '02_RawData\\obese_what_inactive.xls', sheet = 1)
obese_smoker_PIA_ATE$CountyFIPS <- as.numeric(obese_smoker_PIA_ATE$CountyFIPS)
health_variable <- obese_smoker_PIA_ATE %>%
  rename(
    poor_health_rate_2019 = `Poor or fair health`,
    poor_physical_days_2019 = `Poor physical health days`,
    poor_mental_days_2019 = `Poor mental health days`,
    smoker_rate_2019 = `Adult smoking`,
    obesity_rate_2019 = `Adult obesity`,
    physical_inactivity_2019 = `Physical inactivity`,
    exercise_opportunities_rate_2019 = `Access to exercise opportunities`
  )
rm(obese_smoker_PIA_ATE)
health_variable <- health_variable %>%
  rename(
    key_numeric = CountyFIPS
  )
dataset <- left_join(dataset, health_variable)
rm(health_variable)
# obese, smokers, pysical inactivity, access to excercise https://www.countyhealthrankings.org/explore-health-rankings/rankings-data-documentation

# hospital bed https://hifld-geoplatform.opendata.arcgis.com/datasets/hospitals/data
hospital_bed <- read.csv(file = '02_RawData\\hospitalBed.csv')
hospital_bed$COUNTYFIPS <- as.numeric(hospital_bed$COUNTYFIPS) 
hospital_bed$BEDS <- as.numeric(hospital_bed$BEDS)
hospital_bed <- hospital_bed %>%
  select(COUNTYFIPS, BEDS) %>%
  rename(
    CountyFIPS = COUNTYFIPS,
    hospital_beds = BEDS
  )
hospital_bed <- hospital_bed %>%
  subset(hospital_beds > 0) 
hospital_bed <- aggregate(hospital_bed[c('hospital_beds')], by = list(hospital_bed$CountyFIPS), FUN = sum)
hospital_bed <- hospital_bed %>%
  rename(
    key_numeric = Group.1
  )
dataset <- left_join(dataset, hospital_bed)
dataset$hospital_beds_per_1000 <- dataset$hospital_beds/dataset$population*1000
#https://www.kff.org/health-costs/press-release/the-u-s-has-fewer-physicians-and-hospital-beds-per-capita-than-italy-and-other-countries-overwhelmed-by-covid-19/
# 2.8 beds per 1000
rm(hospital_bed)
dataset <- dataset %>%
  mutate(hospital_beds_per_1000 = ifelse(is.na(hospital_beds_per_1000), 0, hospital_beds_per_1000))
# hospital bed https://hifld-geoplatform.opendata.arcgis.com/datasets/hospitals/data


# Weather Data https://www.northwestknowledge.net/metdata/data/
weather <- read.csv(file = '02_RawData\\temp_seasonal_county.csv')
weather_summer_tmmx <- weather %>% 
  dplyr::select(fips, year, summer_tmmx) %>%
  pivot_wider(
    names_from = 'year',
    values_from = 'summer_tmmx',
    names_prefix = "summer_tmmx_"
  ) 
weather_summer_tmmx <- weather_summer_tmmx %>%
  mutate(summer_tmmx_mean = apply(
    select(weather_summer_tmmx,-fips), 1, mean)
  ) %>%
  select(fips, summer_tmmx_mean)

weather_winter_tmmx <- weather %>% 
  dplyr::select(fips, year, winter_tmmx) %>%
  pivot_wider(
    names_from = 'year',
    values_from = 'winter_tmmx',
    names_prefix = "winter_tmmx_"
  )
weather_winter_tmmx <- weather_winter_tmmx %>%
  mutate(winter_tmmx_mean = apply(
    select(weather_winter_tmmx,-fips), 1, mean)
  ) %>%
  select(fips, winter_tmmx_mean)

weather_summer_rmax <- weather %>% 
  dplyr::select(fips, year, summer_rmax) %>%
  pivot_wider(
    names_from = 'year',
    values_from = 'summer_rmax',
    names_prefix = "summer_rmax_"
  )
weather_summer_rmax <- weather_summer_rmax %>%
  mutate(summer_rmax_mean = apply(
    select(weather_summer_rmax,-fips), 1, mean)
  ) %>%
  select(fips, summer_rmax_mean)

weather_winter_rmax <- weather %>% 
  dplyr::select(fips, year, winter_rmax) %>%
  pivot_wider(
    names_from = 'year',
    values_from = 'winter_rmax',
    names_prefix = "winter_rmax_"
  )
weather_winter_rmax <- weather_winter_rmax %>%
  mutate(winter_rmax_mean = apply(
    select(weather_winter_rmax,-fips), 1, mean)
  ) %>%
  select(fips, winter_rmax_mean)

weather <- left_join(weather_summer_tmmx, weather_winter_tmmx, by = "fips")
rm(weather_summer_tmmx)
rm(weather_winter_tmmx)
weather <- left_join(weather, weather_summer_rmax, by = "fips")
rm(weather_summer_rmax)
weather <- left_join(weather, weather_winter_rmax, by = "fips")
rm(weather_winter_rmax)
weather <- weather %>%
  rename(
    key_numeric = fips
  )

dataset <- left_join(dataset, weather)
rm(weather)
# Weather Data https://www.northwestknowledge.net/metdata/data/

# PM2.5 extract data from the https://github.com/MichaelChaoLi-cpu/On-road_Transportation_PM2.5/blob/main/Data/dataset.csv
pm2.5 <- read.csv(file = '02_RawData\\county_pm25.csv') %>% dplyr::select(-X)
pm2.5 <- pm2.5 %>% 
  pivot_wider(
    names_from = 'year', 
    values_from = 'pm25_ori',
    names_prefix = "pm25"
  ) %>%
  rename(
    key_numeric = GEOID
  )
pm2.5 <- pm2.5 %>%
  mutate(pm25_mean = rowMeans(dplyr::select(pm2.5,-key_numeric), na.rm = T)
  ) %>%
  select(key_numeric, pm25_mean)
dataset <- left_join(dataset, pm2.5)
rm(pm2.5)
# PM2.5 https://github.com/MichaelChaoLi-cpu/On-road_Transportation_PM2.5/blob/main/Data/dataset.csv

rm(covid.data)
rm(LC_total)
dataset <- dataset %>%
  mutate(pop_density = population / TotalArea * 1000000)
save.image("00_RData\\dataset.Rdata")
