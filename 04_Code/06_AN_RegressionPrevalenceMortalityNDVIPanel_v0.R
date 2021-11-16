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
library(lubridate)
library(rgdal)
library(tigris)
library(spdplyr)
library(spdep)
library(stargazer)

source('04_Code/07_AF_OutputSplmImpactFunction_v1.R')

# data processing
merge_df.Q <- read.csv("02_RawData/panel_NDVI_mortality_prevalence.csv")
merge_df.Q$date <- ymd(merge_df.Q$date)
merge_df.Q <- merge_df.Q %>% na.omit() %>% 
  dplyr::filter(GEOID != 15003, GEOID != 53055,
                GEOID != 2220, GEOID != 15007,
                GEOID != 15001, GEOID != 25019,
                !((GEOID > 14999)&(GEOID < 15999)),
                !((GEOID > 1999)&(GEOID < 2999)),
                GEOID < 57000) # drop the data not on the CONUS
merge_df.Q$cut <- merge_df.Q$date %>% as.factor() %>% as.numeric()


merge_df.Q.lag <- merge_df.Q %>%
  dplyr::select(GEOID, confirmed_per1000, deaths_per1000, cut)
merge_df.Q.lag$cut <- merge_df.Q.lag$cut + 1
colnames(merge_df.Q.lag) <- c("GEOID", "confirmed_per1000_lag",
                              "deaths_per1000_lag", "cut")
merge_df.Q.lag <- left_join(merge_df.Q, merge_df.Q.lag, by = c("GEOID", "cut"))
merge_df.Q.lag <- merge_df.Q.lag %>%
  filter(cut > 1)

#test code
merge_df.Q.lag.pd <- pdata.frame(merge_df.Q.lag, index = c("GEOID", "date"))
confirmed.formula = 
  confirmed_per1000 ~ stringency_index + NDVI_perc + tem_c + 
  NTL + confirmed_per1000_lag
death.formula = 
  deaths_per1000 ~ confirmed_per1000 + stringency_index + NDVI_perc + 
  tem_c + NTL + confirmed_per1000_lag + deaths_per1000_lag

test.ols.death <- plm(death.formula,
                      data = merge_df.Q.lag.pd, model = "pooling") 
summary(test.ols.death)
test.fe.death <- plm(death.formula, 
                     data = merge_df.Q.lag.pd, model = "within")
summary(test.fe.death)
test.re.death <- plm(death.formula,
                     data = merge_df.Q.lag.pd, model = "random")
summary(test.re.death)
pFtest(test.fe.death, test.ols.death)
phtest(test.fe.death, test.re.death)

test.ols.confirmed <- plm(confirmed.formula,
                          data = merge_df.Q.lag.pd, model = "pooling") 
summary(test.ols.confirmed)
test.fe.confirmed <- plm(confirmed.formula,
                         data = merge_df.Q.lag.pd, model = "within")
summary(test.fe.confirmed)
test.re.confirmed <- plm(confirmed.formula,
                         data = merge_df.Q.lag.pd, model = "random", random.method = "amemiya")
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
shape_usa_county <- geo_join(us_shape, unique(dplyr::select(na.omit(merge_df.Q.lag), GEOID)),
                             'GEOID', 'GEOID', how = 'inner')
shape_usa_county <- arrange(shape_usa_county, GEOID)
shape_usa_county@data$GEOID <- shape_usa_county@data$GEOID %>% as.factor()
queen.nb = poly2nb(shape_usa_county, row.names = shape_usa_county$GEOID)
W = nb2mat(queen.nb, zero.policy = T)
listW = mat2listw(W, style = "W")

## splm
slmtest(confirmed.formula, data = merge_df.Q.lag, 
        index = c('GEOID', 'date'), model = 'within', listw = listW, test="rlml")
slmtest(confirmed.formula, data = merge_df.Q.lag,
        index = c('GEOID', 'date'), model = 'within', listw = listW, test="rlme")
gc()

merge_df.Q.lag <- merge_df.Q.lag %>% arrange(GEOID, cut)
merge_df.Q.lag$GEOID <- merge_df.Q.lag$GEOID %>% as.factor()
merge_df.Q.lag$cut <- merge_df.Q.lag$cut %>% as.factor() 
merge_df.Q.lag <- merge_df.Q.lag %>% dplyr::select(GEOID, cut, everything()) 
# Note: When we use splm the first colnumn must be the index and second one should be time index.

confirmed.formula 
fem.sdm.conf <- spml(confirmed.formula,
                     index = c('GEOID', 'cut'),
                     data = merge_df.Q.lag, listw = mat2listw(W, style = "W"),
                     model = 'within', spatial.error = "kkp", lag = T)

slmtest(death.formula, data = merge_df.Q.lag,
        index = c('GEOID', 'date'), model = 'within', listw = listW, test="rlml")
slmtest(death.formula, data = merge_df.Q.lag,
        index = c('GEOID', 'date'), model = 'within', listw = listW, test="rlme")
gc()

death.formula
fem.sdm.death <- spml(death.formula,
                      index = c('GEOID', 'cut'), data = merge_df.Q.lag,  listw = listW,
                      model = 'within', spatial.error = "kkp", lag = T)

merge_df.Q.lag.mean <- aggregate(merge_df.Q.lag %>% dplyr::select(confirmed_per1000, deaths_per1000),
                                 by = list(merge_df.Q.lag$GEOID), FUN = mean)
merge_df.Q.lag.mean <- rbind(merge_df.Q.lag.mean, merge_df.Q.lag.mean, merge_df.Q.lag.mean,
                             merge_df.Q.lag.mean, merge_df.Q.lag.mean, merge_df.Q.lag.mean)

summary(fem.sdm.death)
summary(fem.sdm.conf)

# fem.sdm.death
y <- merge_df.Q.lag$deaths_per1000 - merge_df.Q.lag.mean$deaths_per1000
ss.death <- (y - mean(y))^2
1-sum(fem.sdm.death$residuals^2)/sum(ss.death)

# fem.sdm.conf
y <- merge_df.Q.lag$confirmed_per1000 - merge_df.Q.lag.mean$confirmed_per1000
ss.death <- (y - mean(y))^2
1-sum(fem.sdm.conf$residuals^2)/sum(ss.death)

impact_summary_death <- summary(spdep::impacts(fem.sdm.death, listw = listW,
                                time = 6, R = 1000), zstats = TRUE, short = T) 
#note: here must use spdep::impacts otherwise error
impact_summary_conf <- summary(spdep::impacts(fem.sdm.conf, listw = listW,
                              time = 6, R = 500), zstats = TRUE, short = T) 
#note: here must use spdep::impacts otherwise error

variable_name_death <- c("Prevalence", "Preventation Stringency", 
                         "NDVI (%)", "Temperature",
                         "NTL", "Time Lag of Prevalence",  "Time Lag of Mortality")
variable_name_conf <- c( "Preventation Stringency", "NDVI (%)", "Temperature",
                         "NTL", "Time Lag of Prevalence")
(impact.table.sdm.death <- output_SPML_model_impacts(impact_summary_death, variable_name_death))
(impact.table.sdm.conf <- output_SPML_model_impacts(impact_summary_conf, variable_name_conf))

impact.table.sdm.death %>% 
  xlsx::write.xlsx("03_Results/04_03RE_SACMortalityLandCoverPanel.xlsx", sheetName = "Sheet1", 
                   row.names = F)
impact.table.sdm.conf %>% 
  xlsx::write.xlsx("03_Results/04_04RE_SACPrevalenceLandCoverPanel.xlsx", sheetName = "Sheet1", 
                   row.names = F)

#save.image("Temp/01_SpmlResultQuaterly.RData")

PSS <- merge_df.Q %>% 
  dplyr::select(cut, deaths_per1000, confirmed_per1000, NDVI_perc, tem_c, NTL)
stargazer(PSS %>% filter(cut == 1) %>% dplyr::select(-cut),
          PSS %>% filter(cut == 2) %>% dplyr::select(-cut),
          PSS %>% filter(cut == 3) %>% dplyr::select(-cut),
          PSS %>% filter(cut == 4) %>% dplyr::select(-cut),
          PSS %>% filter(cut == 5) %>% dplyr::select(-cut),
          PSS %>% filter(cut == 6) %>% dplyr::select(-cut),
          PSS %>% filter(cut == 7) %>% dplyr::select(-cut),
          title = "Table XXX: Test",  type = "text", 
          no.space = T,
          covariate.labels = c(
            "Mortality Rate(cases/1000)", "Prevalence Rate (cases/1000)",
            "NDVI (%)", "Temperature (C)", "NTL Index"
            ),
          iqr = F, out = "03_Results\\PanelStatisticSummary.html"
) 
