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

options(tigris_use_cache = TRUE)
setwd("C:\\Users\\li.chao.987@s.kyushu-u.ac.jp\\OneDrive - Kyushu University\\05_Article\\")
# County Boundary https://www2.census.gov/geo/tiger/GENZ2017/shp/
# Death Rate in Every County to nov 25th     https://covid.cdc.gov/covid-data-tracker/#county-map
Confirmed_Facts <- read.csv(file = '02_RawData\\COVID_1125\\covid_confirmed_usafacts.csv')
head(Confirmed_Facts)
Confirmed_Facts_Selected <- Confirmed_Facts %>%
  select("锘縞ountyFIPS", X11.25.20)
Confirmed_Facts_Selected <- Confirmed_Facts_Selected %>%
  rename(
    CountyFIPS = 锘縞ountyFIPS,
    corfirmed_cases = X11.25.20
  )
Confirmed_Facts_Selected <- Confirmed_Facts_Selected %>%
  subset(CountyFIPS != 0)
head(Confirmed_Facts_Selected)


Populations <- read.csv(file = '02_RawData\\COVID_1125\\covid_county_population_usafacts.csv')
head(Populations)
Populations <- Populations %>%
  rename(
    CountyFIPS = 锘縞ountyFIPS
  )
Populations <- Populations %>%
  subset(CountyFIPS != 0)


Death_Facts <- read.csv(file = '02_RawData\\COVID_1125\\covid_deaths_usafacts.csv')
head(Death_Facts)


Death_Facts <- Death_Facts %>%
  rename(
    CountyFIPS = 锘縞ountyFIPS
  )
Death_Facts_Selected <- Death_Facts %>%
  select(CountyFIPS, X11.25.20)
Death_Facts_Selected <- Death_Facts_Selected %>%
  rename(
    death_cases = X11.25.20
  )
Death_Facts_Selected <- Death_Facts_Selected %>%
  subset(CountyFIPS != 0)

rm(Death_Facts)
rm(Confirmed_Facts)

Pop_ComCas_Death <- Populations %>%
  merge(Confirmed_Facts_Selected,
        by.x = "CountyFIPS", by.y = "CountyFIPS") %>%
  merge(Death_Facts_Selected,
        by.x = "CountyFIPS", by.y = "CountyFIPS")

rm(Confirmed_Facts_Selected)
rm(Death_Facts_Selected)
rm(Populations)

Pop_ComCas_Death$death_cases <- as.numeric(Pop_ComCas_Death$death_cases)
Pop_ComCas_Death <- Pop_ComCas_Death %>%
  mutate(
    death_rate = death_cases / corfirmed_cases * 100,
    log_death_rate = log(death_rate + 1)
    )
Pop_ComCas_Death <- Pop_ComCas_Death %>%
  mutate(
    infect_rate = corfirmed_cases / population * 100,
    log_infect_rate = log(infect_rate + 1)
  )


Pop_ComCas_Death <- Pop_ComCas_Death %>%
  mutate(
    StateFIPS = CountyFIPS %/% 1000
  )


Pop_ComCas_Death <- Pop_ComCas_Death %>%
  rename(
    County.Name = County.Name
  )
Pop_ComCas_Death <- Pop_ComCas_Death %>%
  select(County.Name, StateFIPS, everything()) %>%
  rename(
    infect_cases = corfirmed_cases
  )
# Death Rate in Every County  https://covid.cdc.gov/covid-data-tracker/#county-map


library(foreign)
library(shapefiles)
library(dplyr)
# Land Cover data   https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover
setwd(
  "C:\\Users\\li.chao.987@s.kyushu-u.ac.jp\\OneDrive - Kyushu University\\05_Article\\01_Raster\\02_LandCoverTable\\"
  )
LC30_2016 <- read.csv(file = 'MainLand_LC30_Area.csv')
LC30_2016 <- LC30_2016 %>%
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
a <- LC30_2016 %>% as.tibble() %>%
  rowwise() %>%
  mutate(
    TotalArea = Unknown + Open_Water + Perenial_Ice + Developed_Open_Space + 
      Developed_Low_Intensity + Developed_Medium_Intensity + 
      Developed_High_Intensity + Barren_Land + Deciduous_Forest + 
      Evergreen_Forest + Mixed_Forest + Shrub + Grassland + Pasture + 
      Cultivated_Crops + Woody_Wetlands + Emergent_Herbaceous_Wetlands
    ) 

rm(LC30_2016)
colnames(a)
a <- a %>%
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
      Shrub_perc + Grassland_perc + Pasture_perc + Cultivated_Crops_perc ,
    Blue_rate = Open_Water_perc +
      Woody_Wetlands_perc + Emergent_Herbaceous_Wetlands_perc,
    Grey_rate = Developed_Open_Space_perc + Developed_Low_Intensity_perc +
      Developed_Medium_Intensity_perc + Developed_High_Intensity_perc,
    Other_rate = Barren_Land_perc + Perenial_Ice_perc
  )
a <- a %>%
  rename(
    CountyFIPS = GEOID
  )
a <- a %>%
  mutate(
    Developed_perc = Developed_Open_Space_perc + Developed_Low_Intensity_perc +
      Developed_Medium_Intensity_perc + Developed_High_Intensity_perc
  )
a <- a %>%
  mutate(
    Open_Space_inDeveloped_perc = Developed_Open_Space_perc / Developed_perc * 100,
    Low_Intensity_inDeveloped_perc = Developed_Low_Intensity_perc / Developed_perc * 100,
    Medium_Intensity_inDeveloped_perc = Developed_Medium_Intensity_perc / Developed_perc * 100,
    High_Intensity_inDeveloped_perc = Developed_High_Intensity_perc / Developed_perc * 100
  )
#Land Cover Rate  https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover


setwd("C:\\Users\\li.chao.987@s.kyushu-u.ac.jp\\OneDrive - Kyushu University\\05_Article\\")
library("readxl")
# Median Income and Unemployment  https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/
income <- read_excel(path = '02_RawData\\Unemployment.xls', sheet = 1)
income$FIPStxt <- as.numeric(income$FIPStxt)
income <- income %>%
  select(FIPStxt, Unemployment_rate_2019, Median_Household_Income_2018) %>%
  mutate(log_Median_Household_Income_2018 = log(Median_Household_Income_2018)) %>%
  rename(
    CountyFIPS = FIPStxt
  )

# Median Income and Unemployment https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/

# Poverty Rate  https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/
poverty <- read.csv(file = '02_RawData\\PovertyEstimates.csv')
poverty$FIPStxt <- as.numeric(poverty$FIPStxt)
poverty <- poverty %>%
  select(FIPStxt, PCTPOVALL_2018) %>%
  rename(
    CountyFIPS = FIPStxt,
    poverty_rate = PCTPOVALL_2018
  )
# Poverty Rate  https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/

# Education https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/
education <- read.csv(file = '02_RawData\\Education.csv')
education <- education %>%
  select(FIPS.Code, Percent.of.adults.with.less.than.a.high.school.diploma..2014.18) %>%
  rename(
    CountyFIPS = FIPS.Code,
    less_than_high_school = Percent.of.adults.with.less.than.a.high.school.diploma..2014.18
  )

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
    CountyFIPS = Group.1
  )
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
    CountyFIPS = fips
  )
# Weather Data https://www.northwestknowledge.net/metdata/data/

# PM2.5 https://github.com/wxwx1993/PM_COVID/tree/master/Data
pm2.5 <- read.csv(file = '02_RawData\\county_pm25.csv')
pm2.5 <- pm2.5 %>% 
  pivot_wider(
    names_from = 'year', 
    values_from = 'pm25',
    names_prefix = "pm25"
    ) %>%
  rename(
    CountyFIPS = fips
  )
pm2.5 <- pm2.5 %>%
  mutate(pm25_mean = apply(
    select(pm2.5,-CountyFIPS), 1, mean)
  ) %>%
  select(CountyFIPS, pm25_mean)
# PM2.5 https://github.com/wxwx1993/PM_COVID/tree/master/Data

# median house value & percentage of house owner
house_median <- read.csv(file = '02_RawData\\census_county_interpolated.csv') %>%
  dplyr::select(fips, year, medianhousevalue) %>%
  pivot_wider(names_from = 'year', values_from = 'medianhousevalue') 
house_median <- house_median %>%
  mutate(median_house_value = apply(
    select(house_median, -fips), 1, mean)) %>%
  select(fips, median_house_value) %>%
  mutate(log_median_house_value = log(median_house_value)) %>%
  rename(
    CountyFIPS = fips,
  )

house_owner <- read.csv(file = '02_RawData\\census_county_interpolated.csv') %>%
  dplyr::select(fips, year, pct_owner_occ) %>%
  pivot_wider(names_from = 'year', values_from = 'pct_owner_occ')
house_owner <- house_owner %>%
  mutate(mean_house_owner = apply(
    select(house_owner, -fips), 1, mean)) %>%
  select(fips, mean_house_owner) %>%
  mutate(
    mean_house_owner_perc = mean_house_owner * 100
      ) %>%
  select(fips, mean_house_owner_perc) %>%
  rename(
    CountyFIPS = fips,
  )

house <- left_join(house_median, house_owner, by = "CountyFIPS")
rm(house_median)
rm(house_owner)
# median house value & percentage of house owner

# ToMerge
LC_POP_CAS_DE <- left_join(a, Pop_ComCas_Death, by = "CountyFIPS")
rm(a)
rm(Pop_ComCas_Death)
LC_POP_CAS_DE <- LC_POP_CAS_DE %>%
  mutate(
    pop_density = population / TotalArea * 1000000
  )
LC_POP_CAS_DE <- LC_POP_CAS_DE %>%
  mutate(
    TotalArea_developed = Developed_Open_Space / 1000000 + 
      Developed_Low_Intensity / 1000000 + 
      Developed_Medium_Intensity / 1000000 + 
      Developed_High_Intensity / 1000000,
    pop_density_developed = population / TotalArea_developed
  )
LC_POP_CAS_DE <- left_join(LC_POP_CAS_DE, income, by = "CountyFIPS")
rm(income)
LC_POP_CAS_DE <- left_join(LC_POP_CAS_DE, poverty, by = "CountyFIPS")
rm(poverty)
LC_POP_CAS_DE <- left_join(LC_POP_CAS_DE, education, by = "CountyFIPS")
rm(education)
LC_POP_CAS_DE <- left_join(LC_POP_CAS_DE, age, by = "CountyFIPS")
rm(age)
LC_POP_CAS_DE <- left_join(LC_POP_CAS_DE, sex, by = "CountyFIPS")
rm(sex)
LC_POP_CAS_DE <- left_join(LC_POP_CAS_DE, race, by = "CountyFIPS")
rm(race)
LC_POP_CAS_DE <- left_join(LC_POP_CAS_DE, health_variable, by = "CountyFIPS")
rm(health_variable)
LC_POP_CAS_DE <- left_join(LC_POP_CAS_DE, hospital_bed, by = "CountyFIPS")
rm(hospital_bed)
LC_POP_CAS_DE <- left_join(LC_POP_CAS_DE, weather, by = "CountyFIPS")
rm(weather)
LC_POP_CAS_DE <- left_join(LC_POP_CAS_DE, pm2.5, by = "CountyFIPS")
rm(pm2.5)
LC_POP_CAS_DE <- left_join(LC_POP_CAS_DE, house, by = "CountyFIPS")
rm(house)
#view(LC_POP_CAS_DE)
# ToMerge

form1 = death_rate  ~ 
  Developed_Low_Intensity_perc +
  Developed_Medium_Intensity_perc + Developed_High_Intensity_perc +
  Open_Water_perc + Woody_Wetlands_perc + Emergent_Herbaceous_Wetlands_perc + 
  Deciduous_Forest_perc + Evergreen_Forest_perc + Mixed_Forest_perc + 
  Shrub_perc + Grassland_perc + Pasture_perc + Cultivated_Crops_perc + 
  Barren_Land_perc +
  pop_density + 
  male + age15_44 + age45_64 + age65_99 + black_rate + hispanic_rate +
  log_Median_Household_Income_2018 + log_median_house_value + mean_house_owner_perc +
  Unemployment_rate_2019 + poverty_rate + less_than_high_school +
  poor_health_rate_2019 +  poor_physical_days_2019 + poor_mental_days_2019 +
  smoker_rate_2019 + obesity_rate_2019 +
  physical_inactivity_2019 + exercise_opportunities_rate_2019 + 
  hospital_beds +
  pm25_mean +
  summer_tmmx_mean + winter_tmmx_mean + 
  summer_rmax_mean + winter_rmax_mean
form2 = death_rate ~ Low_Intensity_inDeveloped_perc +
  Medium_Intensity_inDeveloped_perc + High_Intensity_inDeveloped_perc +
  pop_density_developed + 
  male + age15_44 + age45_64 + age65_99 + black_rate + hispanic_rate +
  log_Median_Household_Income_2018 + log_median_house_value + mean_house_owner_perc +
  Unemployment_rate_2019 + poverty_rate + less_than_high_school +
  poor_health_rate_2019 +  poor_physical_days_2019 + poor_mental_days_2019 +
  smoker_rate_2019 + obesity_rate_2019 +
  smoker_rate_2019 + obesity_rate_2019 +
  physical_inactivity_2019 + exercise_opportunities_rate_2019 + 
  hospital_beds +
  pm25_mean +
  summer_tmmx_mean + winter_tmmx_mean + 
  summer_rmax_mean + winter_rmax_mean
form3 = death_rate ~ 
  Green_rate + Grey_rate + Other_rate +
  pop_density + 
  male + age15_44 + age45_64 + age65_99 + black_rate + hispanic_rate +
  log_Median_Household_Income_2018 + log_median_house_value + mean_house_owner_perc +
  Unemployment_rate_2019 + poverty_rate + less_than_high_school +
  poor_health_rate_2019 +  poor_physical_days_2019 + poor_mental_days_2019 +
  smoker_rate_2019 + obesity_rate_2019 +
  smoker_rate_2019 + obesity_rate_2019 +
  physical_inactivity_2019 + exercise_opportunities_rate_2019 + 
  hospital_beds +
  pm25_mean +
  summer_tmmx_mean + winter_tmmx_mean + 
  summer_rmax_mean + winter_rmax_mean



# SAR
setwd('C:\\Users\\li.chao.987@s.kyushu-u.ac.jp\\OneDrive - Kyushu University\\05_Article\\01_Raster\\cb_2017_us_county_20m\\')
shape_usa <- readOGR(dsn = ".", layer = "cb_2017_us_county_20m")
shape_usa@data <- shape_usa@data %>%
  dplyr::select(CountyFIPS)
shape_usa@data$CountyFIPS <- shape_usa@data$CountyFIPS %>% as.integer()
LC_POP_CAS_DE_na <- LC_POP_CAS_DE %>% na.omit()
LC_POP_CAS_DE_na <- LC_POP_CAS_DE_na %>%
  filter(CountyFIPS != 51690,
         CountyFIPS != 51590,
         CountyFIPS != 51890,
         CountyFIPS != 51131,
         CountyFIPS != 37055,
         CountyFIPS != 37139,
         CountyFIPS != 51840)
shape_usa <- geo_join(shape_usa, LC_POP_CAS_DE_na, 'CountyFIPS', 'CountyFIPS', how = 'inner')
queen.nb = poly2nb(shape_usa)
moran.test(shape_usa$death_rate, nb2listw(queen.nb, zero.policy = T), zero.policy = T)



library(broom)
Model_1 <- 
  lm(formula = form1,
     data = shape_usa@data
  )


Model_2 <-
  lm(formula = form2,
     data = shape_usa@data
  )

Model_3 <- 
  lm(formula = form3,
     data = shape_usa@data
  )




# heteroscedasticity test
lmtest::bptest(Model_1)
lmtest::bptest(Model_2)
lmtest::bptest(Model_3)
# heteroscedasticity test



# Spatial Test
W = nb2listw(queen.nb, zero.policy = T)
moran.lm <- lm.morantest(Model_1 , W, alternative="two.sided", zero.policy = T)
print(moran.lm)
LM_1 <- lm.LMtests(Model_1, W, test="all", zero.policy = T)

LM_2 <- lm.LMtests(Model_2, W, test="all", zero.policy = T)
LM_3 <- lm.LMtests(Model_3, W, test="all", zero.policy = T)
print(LM_3)
# Spatial Test


Model_SAR_1 <- lagsarlm(formula = form1,
                        data = shape_usa@data, W, zero.policy = T)
Model_SAR_2 <- lagsarlm(formula = form2,
                        data = shape_usa@data, W, zero.policy = T)
Model_SAR_3 <- lagsarlm(formula = form3,
                        data = shape_usa@data, W, zero.policy = T)

impacts_1 <- summary(spatialreg::impacts.sarlm(Model_SAR_1, listw = W, R=500, zero.policy = T), zstats = T)
impacts_2 <- summary(spatialreg::impacts.sarlm(Model_SAR_2, listw = W, R=500, zero.policy = T), zstats = T)
impacts_3 <- summary(spatialreg::impacts.sarlm(Model_SAR_3, listw = W, R=500, zero.policy = T), zstats = T)
setwd("C:\\Users\\li.chao.987@s.kyushu-u.ac.jp\\OneDrive - Kyushu University\\05_Article\\")

data.frame(impacts_1$res) %>%
  write.csv("05_RegressionResult\\impacts_1_CO.csv")

data.frame(impacts_1$pzmat) %>%
  write.csv("05_RegressionResult\\impacts_1_pv.csv")

data.frame(impacts_2$res) %>%
  write.csv("05_RegressionResult\\impacts_2_CO.csv")

data.frame(impacts_2$pzmat) %>%
  write.csv("05_RegressionResult\\impacts_2_pv.csv")

data.frame(impacts_3$res) %>%
  write.csv("05_RegressionResult\\impacts_3_CO.csv")

data.frame(impacts_3$pzmat) %>%
  write.csv("05_RegressionResult\\impacts_3_pv.csv")

Model_1 <- 
  lm(formula = form1,
     data = LC_POP_CAS_DE
  )


Model_2 <-
  lm(formula = form2,
     data = LC_POP_CAS_DE
  )

Model_3 <- 
  lm(formula = form3,
     data = LC_POP_CAS_DE
  )

# correlation
my_data <- LC_POP_CAS_DE %>% 
  dplyr::select(death_rate, Green_rate, Grey_rate,  Other_rate,  
                Low_Intensity_inDeveloped_perc, Medium_Intensity_inDeveloped_perc, 
                Open_Water_perc, Woody_Wetlands_perc, Emergent_Herbaceous_Wetlands_perc, 
                Deciduous_Forest_perc, Evergreen_Forest_perc, Mixed_Forest_perc, 
                Shrub_perc, Grassland_perc, Pasture_perc, Cultivated_Crops_perc, 
                Barren_Land_perc,
                High_Intensity_inDeveloped_perc, Developed_Low_Intensity_perc, 
                Developed_Medium_Intensity_perc, Developed_High_Intensity_perc, 
                pop_density, pop_density_developed, male, age15_44, age45_64, age65_99, black_rate, 
                hispanic_rate, log_Median_Household_Income_2018, 
                log_median_house_value, mean_house_owner_perc, 
                Unemployment_rate_2019, poverty_rate, 
                less_than_high_school, poor_health_rate_2019,  
                poor_physical_days_2019, poor_mental_days_2019, 
                smoker_rate_2019, obesity_rate_2019, 
                physical_inactivity_2019, exercise_opportunities_rate_2019, hospital_beds, 
                pm25_mean, summer_tmmx_mean, winter_tmmx_mean, 
                summer_rmax_mean, winter_rmax_mean) %>% na.omit() %>% as.data.frame()
correlat <- rcorr(as.matrix(my_data))

data.frame(correlat$r) %>%
  write.csv("05_RegressionResult\\corr_r.csv")
data.frame(correlat$P) %>%
  write.csv("05_RegressionResult\\corr_p.csv")

# correlation





library(stargazer)
library(rmarkdown)
library(rsq)
library(broom)


stargazer(Model_3, Model_1, Model_2, title = "Table XXX: Linear Regression Results",  type = "text", 
          no.space = T,
          covariate.labels = c('Green Space (%)',
                               'Grey Space (%)', 'Other Space (%)',
                                "Low Intensity Developed Area (%)", "Medium Intensity Developed Area (%)",
                               'High Intensity Developed Area (%)', 'Open Water (%)',
                               'Woody Wetlands (%)', 'Emergent Herbaceous Wetlands (%)',
                               'Deciduous Forest (%)', 'Evergreen Forest (%)',
                               'Mixed Forest (%)', 'Shrub (%)', 'Grassland (%)',
                               'Pasture (%)', 'Cultivated Crops (%)',
                               'Barren Land (%)',
                               'Population Density', 'Low Intensity Developed Area In Developed Area (%)', 
                               'Medium Intensity Developed Area In Developed Area (%)', 
                               'High Intensity Developed Area In Developed Area (%)',
                               'Population Density In Developed Area', 'Percentage Of Male', 
                               'Percentage Of Population 15-44', 'Percentage Of Population 45-64', 
                               'Percentage Of Population >= 65', 'Percentage Of Black People', 
                               'Percentage Of Hispanic People', 
                               'Natural Logarithm Of Median Household Income In 2018', 
                               'Natural Logarithm Of Median House Value', 'Percentage Of Owner-Occupied Housing', 
                               'Unemployment Rate In 2019', 'Poverty Rate In 2018', 
                               'Percentage Of The Adults With Less Than High School Diploma In 2018', 
                               'Percentage Of Population With Poo Or Fair Health In 2019', 
                               'Poor Physical Health Days In 2019', 'Poor Mental Health Days In 2019', 
                               'Adult Smoking Rate In 2019', 'Population With Obesity Rate',
                               'Physical Inactivity Rate In 2019', 
                               'Having Access To Exercise Opportunities In 2019', 
                               'Numbers Of Hospital Beds Per Unit Population', 
                               'Mean Of PM2.5 Value', 'Mean Of Daily Temperature In Summer', 
                               'Mean Of Daily Temperature In Winter', 
                               'Mean Of Relative Humidity In Summer', 'Mean Of Relative Humidity In Winter'
                               ),
          dep.var.labels = "County-level COVID-19 CFR (%)",
          column.labels = c("Model I","Model II","Model III"),
          out = "05_RegressionResult\\Regression.html")

total_death = sum(LC_POP_CAS_DE$death_cases, na.rm = TRUE)
total_infect = sum(LC_POP_CAS_DE$infect_cases, na.rm = TRUE)
total_CFR = total_death / total_infect 
rm(total_death)
rm(total_infect)

library(magrittr)
library(tidyverse)

# CFRR table output Model I
COEF <- round(coef(Model_1), 3)
CFRR <- round(coef(Model_1) / total_CFR, 2)
CI <- round(confint(Model_1) / total_CFR, 2)
P <- round(coef(summary(Model_1))[,4], 3)
colnames(CI) <- c("Lower", "Higher")
table1 <- as.data.frame(cbind(COEF, Vari, CFRR, CI, P))
table1$a <- "("; table1$b <- "-"; table1$c <- ")"
table1 <- table1[,c("COEF","CFRR","a","Lower","b","Higher","c", "P")]
table1 <- unite(table1, "95%CI", c(a, Lower, b, Higher, c), sep = "", remove=T)
table1[,1] <- gsub("\\(", " (", table1[,1])
table1 <- table1 %>% as.tibble()
table1 %>% write.csv(file = "05_RegressionResult\\CFRR_Model_1.csv")
# CFRR table output Model I

# CFRR table output Model II
COEF <- round(coef(Model_2), 3)
CFRR <- round(coef(Model_2) / total_CFR, 2)
CI <- round(confint(Model_2) / total_CFR, 2)
P <- round(coef(summary(Model_2))[,4], 3)
colnames(CI) <- c("Lower", "Higher")
table2 <- as.data.frame(cbind(COEF, CFRR, CI, P))
table2$a <- "("; table2$b <- "-"; table2$c <- ")"
table2 <- table2[,c("COEF", "CFRR","a","Lower","b","Higher","c", "P")]
table2 <- unite(table2, "95%CI", c(a, Lower, b, Higher, c), sep = "", remove=T)
table2[,1] <- gsub("\\(", " (", table2[,1])
table2 <- table2 %>% as.tibble()
table2 %>% write.csv(file = "05_RegressionResult\\CFRR_Model_2.csv")
# CFRR table output Model II

# CFRR table output Model III
COEF <- round(coef(Model_3), 3)
CFRR <- round(coef(Model_3) / total_CFR, 2)
CI <- round(confint(Model_3) / total_CFR, 2)
P <- round(coef(summary(Model_3))[,4], 3)
colnames(CI) <- c("Lower", "Higher")
table3 <- as.data.frame(cbind(COEF, CFRR, CI, P))
table3$a <- "("; table3$b <- "-"; table3$c <- ")"
table3 <- table3[,c("COEF", "CFRR","a","Lower","b","Higher","c", "P")]
table3 <- unite(table3, "95%CI", c(a, Lower, b, Higher, c), sep = "", remove=T)
table3[,1] <- gsub("\\(", " (", table3[,1])
table3 <- table3 %>% as.tibble()
table3 %>% write.csv(file = "05_RegressionResult\\CFRR_Model_3.csv")
# CFRR table output Model III


Model_1 %>% 
  tidy() %>% 
  mutate(
    significant = ifelse(p.value > 0.1, FALSE, TRUE),
    CFRR = estimate / total_CFR
  ) %>% 
  write.csv(file = "05_RegressionResult\\Regression_Model_1.csv")

Model_2 %>% 
  tidy() %>% 
  mutate(
    significant = ifelse(p.value > 0.1, FALSE, TRUE),
    CFRR = estimate / total_CFR
  ) %>% 
  write.csv(file = "05_RegressionResult\\Regression_Model_2.csv")

Model_3 %>% 
  tidy() %>% 
  mutate(
    significant = ifelse(p.value > 0.1, FALSE, TRUE),
    CFRR = estimate / total_CFR
  ) %>% 
  write.csv(file = "05_RegressionResult\\Regression_Model_3.csv")


summ <- LC_POP_CAS_DE %>%
  na.omit() %>%
  select(Green_rate, Grey_rate,  Other_rate,  
         Low_Intensity_inDeveloped_perc, Medium_Intensity_inDeveloped_perc, 
         Open_Water_perc, Woody_Wetlands_perc, Emergent_Herbaceous_Wetlands_perc, 
         Deciduous_Forest_perc, Evergreen_Forest_perc, Mixed_Forest_perc, 
         Shrub_perc, Grassland_perc, Pasture_perc, Cultivated_Crops_perc, 
         Barren_Land_perc,
         High_Intensity_inDeveloped_perc, Developed_Low_Intensity_perc, 
         Developed_Medium_Intensity_perc, Developed_High_Intensity_perc, 
         pop_density, pop_density_developed, male, age15_44, age45_64, age65_99, black_rate, 
         hispanic_rate, log_Median_Household_Income_2018, 
         log_median_house_value, mean_house_owner_perc, 
         Unemployment_rate_2019, poverty_rate, 
         less_than_high_school, poor_health_rate_2019,  
         poor_physical_days_2019, poor_mental_days_2019, 
         smoker_rate_2019, obesity_rate_2019, 
         physical_inactivity_2019, exercise_opportunities_rate_2019, hospital_beds, 
         pm25_mean, summer_tmmx_mean, winter_tmmx_mean, 
         summer_rmax_mean, winter_rmax_mean) %>% as.data.frame()

stargazer(summ, title = "Table XXX: Data Statistic Summary",  type = "text", 
          no.space = T,
          covariate.labels = c('Green Space (%)',
                               'Grey Space (%)', 'Other Space (%)',
                               "Low Intensity Developed Area (%)", "Medium Intensity Developed Area (%)",
                               'High Intensity Developed Area (%)', 'Open Water (%)',
                               'Woody Wetlands (%)', 'Emergent Herbaceous Wetlands (%)',
                               'Deciduous Forest (%)', 'Evergreen Forest (%)',
                               'Mixed Forest (%)', 'Shrub (%)', 'Grassland (%)',
                               'Pasture (%)', 'Cultivated Crops (%)',
                               'Barren Land (%)',
                               'Low Intensity Developed Area In Developed Area (%)', 
                               'Medium Intensity Developed Area In Developed Area (%)', 
                               'High Intensity Developed Area In Developed Area (%)',
                               'Population Density', 
                               'Population Density In Developed Area', 'Percentage Of Male', 
                               'Percentage Of Population 15-44', 'Percentage Of Population 45-64', 
                               'Percentage Of Population >= 65', 'Percentage Of Black People', 
                               'Percentage Of Hispanic People', 
                               'Natural Logarithm Of Median Household Income In 2018', 
                               'Natural Logarithm Of Median House Value', 'Percentage Of Owner-Occupied Housing', 
                               'Unemployment Rate In 2019', 'Poverty Rate In 2018', 
                               'Percentage Of The Adults With Less Than High School Diploma In 2018', 
                               'Percentage Of Population With Poo Or Fair Health In 2019', 
                               'Poor Physical Health Days In 2019', 'Poor Mental Health Days In 2019', 
                               'Adult Smoking Rate In 2019', 'Population With Obesity Rate',
                               'Physical Inactivity Rate In 2019', 
                               'Having Access To Exercise Opportunities In 2019', 
                               'Numbers Of Hospital Beds Per Unit Population', 
                               'Mean Of PM2.5 Value', 'Mean Of Daily Temperature In Summer', 
                               'Mean Of Daily Temperature In Winter', 
                               'Mean Of Relative Humidity In Summer', 'Mean Of Relative Humidity In Winter'
          ),
          iqr = F, out = "05_RegressionResult\\summary_table.html"
  ) 


