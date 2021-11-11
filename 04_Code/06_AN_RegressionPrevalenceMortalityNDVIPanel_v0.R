# Author: M.L.

# input: panel_NDVI_mortality_prevalence.csv

# output: 

# end

library(plm)
library(tidyverse)
library(dplyr)
library(splm)
library(spatialreg)

# the function to output spml model impacts.
output_SPML_model_impacts <- function(impact_summary, variable_name){
  col1 <- variable_name
  col2 <- impact_summary$res$direct %>% round(6) %>% as.character()
  col3 <- impact_summary$res$indirect %>% round(6) %>% as.character()
  col4 <- impact_summary$res$total %>% round(6) %>% as.character()
  col5 <- impact_summary$pzmat[,1] %>% as.vector()
  col6 <- impact_summary$pzmat[,2] %>% as.vector()
  col7 <- impact_summary$pzmat[,3] %>% as.vector()
  
  output <- cbind(col1, col2, col3, col4, col5, col6, col7) %>% as.data.frame()
  output <- output %>%
    mutate(
      col2 = ifelse(col5 < 0.01, paste0(col2, "*"), col2),
      col2 = ifelse(col5 < 0.05, paste0(col2, "*"), col2),
      col2 = ifelse(col5 < 0.1, paste0(col2, "*"), col2),
      col3 = ifelse(col6 < 0.01, paste0(col3, "*"), col3),
      col3 = ifelse(col6 < 0.05, paste0(col3, "*"), col3),
      col3 = ifelse(col6 < 0.1, paste0(col3, "*"), col3),
      col4 = ifelse(col7 < 0.01, paste0(col4, "*"), col4),
      col4 = ifelse(col7 < 0.05, paste0(col4, "*"), col4),
      col4 = ifelse(col7 < 0.1, paste0(col4, "*"), col4)
    )
  output <- output[,1:4]
  
  output$num <- c(1:nrow(output)) 
  output <- rbind(output, output)
  output <- output %>% arrange(num)
  
  col1.se <- c("", "", "","", "", "","")
  col2.se <- paste0("(",impact_summary$semat[,1] %>% as.vector() %>% round(6) %>% as.character(),")")
  col3.se <- paste0("(",impact_summary$semat[,2] %>% as.vector() %>% round(6) %>% as.character(),")")
  col4.se <- paste0("(",impact_summary$semat[,3] %>% as.vector() %>% round(6) %>% as.character(),")")
  
  output.se <- cbind(col1.se, col2.se, col3.se, col4.se) %>% as.data.frame()
  output.se$num <- c(1:nrow(output.se)) 
  output[seq(2,nrow(output),2),] <- output.se 
  output <- output %>% dplyr::select(-num)
  colnames(output) <- c("X", "Direct Impacts", "Indirect Impacts", "Total Impacts")
  output <- as.data.frame(output)
  return(output)
}


# data processing
merge_df.Q <- read.csv("02_RawData/panel_NDVI_mortality_prevalence.csv")
merge_df.Q <- merge_df.Q %>% na.omit() %>% 
  dplyr::filter(GEOID != 15003, GEOID != 53055,
                GEOID != 2220, GEOID != 15007,
                GEOID != 15001, GEOID != 25019,
                !((GEOID > 14999)&(GEOID < 15999)),
                !((GEOID > 1999)&(GEOID < 2999)),
                GEOID < 57000) # drop the data not on the CONUS
merge_df.Q$cut <- merge_df.Q$date %>% as.factor() %>% as.numeric()


#test code
merge_df.Q.pd <- pdata.frame(merge_df.Q, index = c("GEOID", "date"))
death.formula = 
  deaths_per1000 ~ confirmed_per1000 + stringency_index + NDVI_perc + 
  tem_c + NTL + lag(confirmed_per1000) + lag(deaths_per1000)
confirmed.formula = 
  confirmed_per1000 ~ stringency_index + NDVI_perc + tem_c + 
  NTL + lag(confirmed_per1000)

test.ols.death <- plm(death.formula,
                      data = merge_df.Q.pd, model = "pooling") 
summary(test.ols.death)
test.fe.death <- plm(death.formula, 
                     data = merge_df.Q.pd, model = "within")
summary(test.fe.death)
test.re.death <- plm(death.formula,
                     data = merge_df.Q.pd, model = "random")
summary(test.re.death)
pFtest(test.fe.death, test.ols.death)
phtest(test.fe.death, test.re.death)

test.ols.confirmed <- plm(confirmed.formula,
                          data = merge_df.Q.pd, model = "pooling") 
summary(test.ols.confirmed)
test.fe.confirmed <- plm(confirmed.formula,
                         data = merge_df.Q.pd, model = "within")
summary(test.fe.confirmed)
test.re.confirmed <- plm(confirmed.formula,
                         data = merge_df.Q.pd, model = "random", random.method = "amemiya")
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
shape_usa_county <- geo_join(us_shape, unique(dplyr::select(na.omit(merge_df.Q), GEOID)),
                             'GEOID', 'GEOID', how = 'inner')
queen.nb = poly2nb(shape_usa_county, row.names = shape_usa_county$GEOID)
W = nb2mat(queen.nb, zero.policy = TRUE)
listW = mat2listw(W, style = "W")

merge_df.Q.lag <- merge_df.Q %>%
  dplyr::select(GEOID, confirmed_per1000, deaths_per1000, cut)
merge_df.Q.lag$cut <- merge_df.Q.lag$cut + 1
colnames(merge_df.Q.lag) <- c("GEOID", "confirmed_per1000_lag",
                              "deaths_per1000_lag", "cut")
merge_df.Q.lag <- left_join(merge_df.Q, merge_df.Q.lag, by = c("GEOID", "cut"))
merge_df.Q.lag <- merge_df.Q.lag %>%
  filter(cut > 1)

confirmed.formula = 
  confirmed_per1000 ~ stringency_index + NDVI_perc + tem_c + 
  NTL + confirmed_per1000_lag
slmtest(confirmed.formula, data = merge_df.Q.lag, 
        index = c('GEOID', 'date'), model = 'within', listw = listW, test="rlml")
slmtest(confirmed.formula, data = merge_df.Q.lag,
        index = c('GEOID', 'date'), model = 'within', listw = listW, test="rlme")
gc()

confirmed.formula 
fem.sdm.conf <- spml(confirmed.formula,
                     index = c('GEOID', 'date'),
                     data = merge_df.Q.lag,  listw = listW, model = 'within',
                     spatial.error = "kkp", lag = T)

death.formula = 
  deaths_per1000 ~ confirmed_per1000 + stringency_index + NDVI_perc + 
  tem_c + NTL + confirmed_per1000_lag + deaths_per1000_lag
slmtest(death.formula, data = merge_df.Q.lag,
        index = c('GEOID', 'date'), model = 'within', listw = listW, test="rlml")
slmtest(death.formula, data = merge_df.Q.lag,
        index = c('GEOID', 'date'), model = 'within', listw = listW, test="rlme")
gc()

death.formula
fem.sdm.death <- spml(death.formula,
                      index = c('GEOID', 'date'), data = merge_df.Q.lag,  listw = listW,
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
                         "NTL") #, "Time Lag of Prevalence",  "Time Lag of Mortality")
impact.table.sdm.death <- output_SPML_model_impacts(impact_summary_death, variable_name_death)
impact.table.sdm.conf <- output_SPML_model_impacts(impact_summary_conf, variable_name_conf)
