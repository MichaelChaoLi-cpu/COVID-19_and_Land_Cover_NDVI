# Author: M.L.

# input: panel_NDVI_mortality_prevalence.csv

# panel_NDVI_mortality_prevalence.csv: "stringency_index" this is prevention score, 0 (nothing) - 100 (utmost strict)
# panel_NDVI_mortality_prevalence.csv: "date" this is the time index
# panel_NDVI_mortality_prevalence.csv: "GEOID" this is the identity index.
# panel_NDVI_mortality_prevalence.csv: "NDVI_perc" this is the percentage of NDVI (-100% - 100%) quarterly.
# panel_NDVI_mortality_prevalence.csv: "tem_c" this is the temperature (C degree) quarterly.
# panel_NDVI_mortality_prevalence.csv: "NLT" this is the index of Nighttime Light quarterly.
# panel_NDVI_mortality_prevalence.csv: "confirmed_per1000" this is the prevalence index (confirmed case/1000) quarterly.
# panel_NDVI_mortality_prevalence.csv: "deaths_per1000" this is the mortality index (mortality/1000) quarterly.

# output: 

# end

library(plm)
library(tidyverse)
library(dplyr)
library(splm)
library(spatialreg)

source('04_Code/07_AF_OutputSplmImpactFunction_v1.R')

# data processing
merge_df.M <- read.csv("02_RawData/panel_NDVI_mortality_prevalence_monthly.csv")
merge_df.M$date <- ymd(merge_df.M$date)
merge_df.M <- merge_df.M %>% na.omit() %>% 
  dplyr::filter(GEOID != 15003, GEOID != 53055,
                GEOID != 2220, GEOID != 15007,
                GEOID != 15001, GEOID != 25019,
                !((GEOID > 14999)&(GEOID < 15999)),
                !((GEOID > 1999)&(GEOID < 2999)),
                GEOID < 57000) # drop the data not on the CONUS
merge_df.M$cut <- merge_df.M$date %>% as.factor() %>% as.numeric()


merge_df.M.lag <- merge_df.M %>%
  dplyr::select(GEOID, confirmed_per1000, deaths_per1000, cut)
merge_df.M.lag$cut <- merge_df.M.lag$cut + 1
colnames(merge_df.M.lag) <- c("GEOID", "confirmed_per1000_lag",
                              "deaths_per1000_lag", "cut")
merge_df.M.lag <- left_join(merge_df.M, merge_df.M.lag, by = c("GEOID", "cut"))
merge_df.M.lag <- merge_df.M.lag %>%
  filter(cut > 1)

#test code
merge_df.M.lag.pd <- pdata.frame(merge_df.M.lag, index = c("GEOID", "date"))
confirmed.formula = 
  confirmed_per1000 ~ stringency_index + NDVI_perc + tem_c + 
  NTL + confirmed_per1000_lag
death.formula = 
  deaths_per1000 ~ confirmed_per1000 + stringency_index + NDVI_perc + 
  tem_c + NTL + confirmed_per1000_lag + deaths_per1000_lag

test.ols.death <- plm(death.formula,
                      data = merge_df.M.lag.pd, model = "pooling") 
summary(test.ols.death)
test.fe.death <- plm(death.formula, 
                     data = merge_df.M.lag.pd, model = "within")
summary(test.fe.death)
test.re.death <- plm(death.formula,
                     data = merge_df.M.lag.pd, model = "random")
summary(test.re.death)
pFtest(test.fe.death, test.ols.death)
phtest(test.fe.death, test.re.death)

test.ols.confirmed <- plm(confirmed.formula,
                          data = merge_df.M.lag.pd, model = "pooling") 
summary(test.ols.confirmed)
test.fe.confirmed <- plm(confirmed.formula,
                         data = merge_df.M.lag.pd, model = "within")
summary(test.fe.confirmed)
test.re.confirmed <- plm(confirmed.formula,
                         data = merge_df.M.lag.pd, model = "random", random.method = "amemiya")
summary(test.re.confirmed)

pFtest(test.fe.confirmed, test.ols.confirmed)
phtest(test.fe.confirmed, test.re.confirmed)

# according to the test, fixed effects model are selected
panel.fem.confirmed <- test.fe.confirmed
panel.fem.deaths <- test.fe.death
rm(test.ols.confirmed, test.ols.death, test.re.confirmed, test.re.death)
rm(test.fe.confirmed, test.fe.death)
summary(panel.fem.confirmed)
summary(panel.fem.deaths)
gc()

# build spatial data set
us_shape <- readOGR(dsn = "01_Raster\\01_Boundary", layer = "cb_2017_us_county_20m84")
us_shape@data <- us_shape@data %>%
  dplyr::select(GEOID)
us_shape@data$GEOID <- us_shape@data$GEOID %>% as.numeric() 
shape_usa_county <- geo_join(us_shape, unique(dplyr::select(na.omit(merge_df.M.lag), GEOID)),
                             'GEOID', 'GEOID', how = 'inner')
shape_usa_county <- arrange(shape_usa_county, GEOID)
shape_usa_county@data$GEOID <- shape_usa_county@data$GEOID %>% as.factor()
queen.nb = poly2nb(shape_usa_county, row.names = shape_usa_county$GEOID)
W = nb2mat(queen.nb, zero.policy = T)
listW = mat2listw(W, style = "W")

## splm
slmtest(confirmed.formula, data = merge_df.M.lag, 
        index = c('GEOID', 'date'), model = 'within', listw = listW, test="rlml")
slmtest(confirmed.formula, data = merge_df.M.lag,
        index = c('GEOID', 'date'), model = 'within', listw = listW, test="rlme")
gc()

merge_df.M.lag <- merge_df.M.lag %>% arrange(GEOID, cut)
merge_df.M.lag$GEOID <- merge_df.M.lag$GEOID %>% as.factor()
merge_df.M.lag$cut <- merge_df.M.lag$cut %>% as.factor() 
merge_df.M.lag <- merge_df.M.lag %>% dplyr::select(GEOID, cut, everything()) 
# Note: When we use splm the first colnumn must be the index and second one should be time index.

confirmed.formula 
fem.sdm.conf <- spml(confirmed.formula,
                     index = c('GEOID', 'cut'),
                     data = merge_df.M.lag, listw = mat2listw(W, style = "W"),
                     model = 'within', spatial.error = "kkp", lag = T)

slmtest(death.formula, data = merge_df.M.lag,
        index = c('GEOID', 'date'), model = 'within', listw = listW, test="rlml")
slmtest(death.formula, data = merge_df.M.lag,
        index = c('GEOID', 'date'), model = 'within', listw = listW, test="rlme")
gc()

death.formula
fem.sdm.death <- spml(death.formula,
                      index = c('GEOID', 'cut'), data = merge_df.M.lag,  listw = listW,
                      model = 'within', spatial.error = "kkp", lag = T)


summary(fem.sdm.death)
summary(fem.sdm.conf)

impact_summary_death <- summary(spdep::impacts(fem.sdm.death, listw = listW,
                                               time = 6, R = 1000), zstats = TRUE, short = T) 
#note: here must use spdep::impacts otherwise error
impact_summary_conf <- summary(spdep::impacts(fem.sdm.conf, listw = listW,
                                              time = 6, R = 500), zstats = TRUE, short = T) 
#note: here must use spdep::impacts otherwise error

variable_name_death <- c("Prevalence (confirmed/1000 cap)", "Preventation Stringency", 
                         "NDVI (%)", "Temperature",
                         "NTL", "Time Lag of Prevalence",  "Time Lag of Mortality")
variable_name_conf <- c( "Preventation Stringency", "NDVI (%)", "Temperature",
                         "NTL", "Time Lag of Prevalence")
(impact.table.sdm.death <- output_SPML_model_impacts(impact_summary_death, variable_name_death))
(impact.table.sdm.conf <- output_SPML_model_impacts(impact_summary_conf, variable_name_conf))

save.image("Temp/01_SpmlResultMonthly.RData")
