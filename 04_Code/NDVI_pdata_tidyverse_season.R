library("COVID19")
library(tidyverse)
library("dplyr")
library(lubridate)
library(plm)
library(stringr)
library(rgdal)
library(tigris)
library(spdep)
library(splm)

deat.conf.pop <- COVID19::covid19(country = "USA", level = 3)
id_pop <- dplyr::select(deat.conf.pop, id, population) %>% unique()

sub.Dataset.Q <- function(dataset, input_date, id_pop){
  base_month = ymd(input_date) %m-% months(3)
  dataset.output <- dataset %>% filter(date == ymd(input_date)) %>%
    dplyr::select(id, confirmed, deaths, population) %>%
    filter(!is.na(confirmed)) %>%
    mutate(deaths = ifelse(is.na(deaths), 0, deaths)) %>%
    rename(ori_con = confirmed,
           ori_deaths = deaths)
  if (input_date != "2020-04-01") {
    base <- dataset %>% filter(date == ymd(base_month)) %>%
      dplyr::select(id, confirmed, deaths) %>%
      filter(!is.na(confirmed)) %>%
      mutate(deaths = ifelse(is.na(deaths), 0, deaths))
    dataset.output <- left_join(dataset.output, base)  %>%
      mutate(confirmed = ifelse(is.na(confirmed), 0, confirmed),
             deaths = ifelse(is.na(deaths), 0, deaths)) 
    dataset.output <- dataset.output %>%
      mutate(o.con = (ori_con - confirmed),
             o.deaths = (ori_deaths - deaths)) 
    #      mutate(o.con = (ori_con - confirmed)/population,
    #             o.deaths = (ori_deaths - deaths)/population) 
    stringency <- dataset %>% filter(date > ymd(base_month)) %>%
      filter(date < ymd(input_date)) %>%
      dplyr::select(id, stringency_index)
    stringency <- stringency$stringency_index %>% 
      aggregate(by = list(stringency$id), mean)
    stringency <- stringency %>% rename(id = Group.1)
    dataset.output <- left_join(dataset.output, stringency) %>%
      dplyr::select(id, o.con, o.deaths, x)
    
  } else {
    dataset.output <- dataset.output %>%
      mutate(o.con = ori_con,
             o.deaths = ori_deaths)
    #      mutate(o.con = ori_con/population,
    #             o.deaths = ori_deaths/population)
    stringency <- dataset %>% filter(date < ymd(input_date)) %>%
      dplyr::select(id, stringency_index)
    stringency <- stringency$stringency_index %>% 
      aggregate(by = list(stringency$id), mean, na.action = na.omit)
    stringency <- stringency %>% rename(id = Group.1)
    dataset.output <- left_join(dataset.output, stringency) %>%
      dplyr::select(id, o.con, o.deaths, x)
  }
  dataset.output$date <- ymd(base_month)
  colnames(dataset.output) <- c("id", "confirmed", "deaths", 
                                "stringency_index", "date")
  dataset.output <- left_join(id_pop, dataset.output, by = "id")
  dataset.output <- dataset.output %>% mutate(
    confirmed = ifelse(is.na(confirmed), 0, confirmed),
    deaths = ifelse(is.na(deaths), 0, deaths),
    stringency_index = ifelse(is.na(stringency_index), 0, stringency_index),
    date = base_month,
  )
  return(dataset.output)
}

deat.conf.pop.2020q1 <- sub.Dataset.Q(deat.conf.pop, "2020-04-01", id_pop = id_pop)
deat.conf.pop.2020q2 <- sub.Dataset.Q(deat.conf.pop, "2020-07-01", id_pop = id_pop)
deat.conf.pop.2020q3 <- sub.Dataset.Q(deat.conf.pop, "2020-10-01", id_pop = id_pop)
deat.conf.pop.2020q4 <- sub.Dataset.Q(deat.conf.pop, "2021-01-01", id_pop = id_pop)
deat.conf.pop.2021q1 <- sub.Dataset.Q(deat.conf.pop, "2021-04-01", id_pop = id_pop)
deat.conf.pop.2021q2 <- sub.Dataset.Q(deat.conf.pop, "2021-07-01", id_pop = id_pop)
deat.conf.pop.2021q3 <- sub.Dataset.Q(deat.conf.pop, "2021-10-01", id_pop = id_pop)

merge_df.Q <- rbind(deat.conf.pop.2020q1, deat.conf.pop.2020q2, deat.conf.pop.2020q3,
                    deat.conf.pop.2020q4, deat.conf.pop.2021q1, deat.conf.pop.2021q2,
                    deat.conf.pop.2021q3)
rm(deat.conf.pop.2020q1, deat.conf.pop.2020q2, deat.conf.pop.2020q3,
   deat.conf.pop.2020q4, deat.conf.pop.2021q1, deat.conf.pop.2021q2,
   deat.conf.pop.2021q3)
gc()

dataset.id <- deat.conf.pop %>% filter(date == ymd("2021-08-01"))
dataset.id <- dataset.id %>% dplyr::select(id, key_local, population) %>% as.data.frame()
merge_df.Q <- left_join(merge_df.Q, dataset.id)
rm(dataset.id)

merge_df.Q <- merge_df.Q %>% as.data.frame()
merge_df.Q <- merge_df.Q %>% rename(GEOID = key_local)
merge_df.Q$GEOID <- merge_df.Q$GEOID %>% as.numeric()
merge_df.Q <- merge_df.Q %>% 
  mutate(stringency_index = ifelse(is.na(stringency_index), 0,stringency_index))

NDVI.temper.panel <- read.csv("02_RawData\\panel_mod.csv")
NDVI.temper.panel <- NDVI.temper.panel %>%
  mutate(D2020_001 = D2020_001 + D2020_032 + D2020_061,
         D2020_092 = D2020_092 + D2020_122 + D2020_153,
         D2020_183 = D2020_183 + D2020_214 + D2020_245,
         D2020_275 = D2020_275 + D2020_306 + D2020_336,
         D2021_001 = D2021_001 + D2021_032 + D2021_060,
         D2021_091 = D2021_091 + D2021_121 + D2021_152,
         D2021_182 = D2021_182 + D2021_213 + D2021_244) %>%
  dplyr::select(GEOID, D2020_001, D2020_092, D2020_183, D2020_275,
                D2021_001, D2021_091, D2021_182, type)
NDVI.panel <- NDVI.temper.panel %>%
  filter(type == "NDVI") %>%
  dplyr::select(-type)
NDVI.panel <- NDVI.panel %>%
  pivot_longer(cols = D2020_001:D2021_182, names_to = "date", values_to = "NDVI")
NDVI.panel <- NDVI.panel %>%
  mutate(year = str_sub(date, 2, 5),
         day = str_sub(date, 7, 9)) 
NDVI.panel$day <- NDVI.panel$day %>% as.numeric()
NDVI.panel$date <- as.Date((NDVI.panel$day - 1), origin = paste0(NDVI.panel$year,"-01-01"))
NDVI.panel <- NDVI.panel %>% dplyr::select("GEOID", "date", "NDVI")
NDVI.panel$NDVI <- NDVI.panel$NDVI / 10000
merge_df.Q <- left_join(merge_df.Q, NDVI.panel, by = c("GEOID", "date"))
rm(NDVI.panel)

DayTem.panel <- NDVI.temper.panel %>%
  filter(type == "DayTem") %>%
  dplyr::select(-type)
DayTem.panel <- DayTem.panel %>%
  pivot_longer(cols = D2020_001:D2021_182, names_to = "date", values_to = "DayTem")
NigTem.panel <- NDVI.temper.panel %>%
  filter(type == "NigTem") %>%
  dplyr::select(-type)
NigTem.panel <- NigTem.panel %>%
  pivot_longer(cols = D2020_001:D2021_182, names_to = "date", values_to = "NigTem")
Tem.panel <- left_join(DayTem.panel, NigTem.panel, by = c("GEOID", "date"))
rm(DayTem.panel, NigTem.panel)
Tem.panel$tem <- (Tem.panel$DayTem + Tem.panel$NigTem)/2 
Tem.panel <- Tem.panel %>% 
  dplyr::select(-DayTem, -NigTem)
Tem.panel <- Tem.panel %>% 
  mutate(year = str_sub(date, 2, 5),
         day = str_sub(date, 7, 9)) 
Tem.panel$day <- Tem.panel$day %>% as.numeric()
Tem.panel$date <- as.Date((Tem.panel$day - 1), origin = paste0(Tem.panel$year,"-01-01"))
Tem.panel <- Tem.panel %>% dplyr::select("GEOID", "date", "tem")
merge_df.Q <- left_join(merge_df.Q, Tem.panel, by = c("GEOID", "date"))
rm(Tem.panel)
rm(NDVI.temper.panel)

merge_df.Q$confirmed_perc <- merge_df.Q$confirmed / merge_df.Q$population * 100
merge_df.Q$deaths_perc <- merge_df.Q$deaths / merge_df.Q$population * 100
merge_df.Q$NDVI_perc <- merge_df.Q$NDVI * 100
merge_df.Q$tem_c <- merge_df.Q$tem * 0.02 - 273.16 

merge_df.Q <- merge_df.Q %>% na.omit() %>% 
  dplyr::filter(GEOID != 15003, GEOID != 53055,
                GEOID != 2220, GEOID != 15007,
                GEOID != 15001, GEOID != 25019,
                !((GEOID > 14999)&(GEOID < 15999)),
                !((GEOID > 1999)&(GEOID < 2999)),
                GEOID < 57000)
merge_df.Q$cut <- merge_df.Q$date %>% as.numeric() %>% as.factor() %>% as.numeric()

#test code
merge_df.Q.pd <- pdata.frame(merge_df.Q, index = c("GEOID", "date"))
death.formula = deaths_perc ~ confirmed_perc + stringency_index + NDVI_perc + tem_c + lag(confirmed_perc) + lag(deaths_perc)
confirmed.formula = confirmed_perc ~ stringency_index + NDVI_perc + tem_c + lag(confirmed_perc)

test.ols.death <- plm(death.formula,
                      data = merge_df.Q.pd, model = "pooling") 
summary(test.ols.death)
test.fe.death <- plm(death.formula, 
                     data = merge_df.Q.pd, model = "within")
summary(test.fe.death)
test.re.death <- plm(death.formula,
                     data = merge_df.Q.pd, model = "random", random.method = "amemiya")
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
slmtest(confirmed.formula, data = merge_df.Q,
        index = c('GEOID', 'date'), model = 'within', listw = listW, test="rlml")
slmtest(confirmed.formula, data = merge_df.Q,
        index = c('GEOID', 'date'), model = 'within', listw = listW, test="rlme")
gc()

confirmed.formula
fem.sdm.conf <- spml(confirmed_perc ~ stringency_index + NDVI_perc + tem_c, index = c('GEOID', 'date'),
                 data = merge_df.Q,  listw = mat2listw(W), model = 'within',
                 spatial.error = 'kkp', lag = T)

slmtest(death.formula, data = merge_df.Q,
        index = c('GEOID', 'date'), model = 'within', listw = listW, test="rlml")
slmtest(death.formula, data = merge_df.Q,
        index = c('GEOID', 'date'), model = 'within', listw = listW, test="rlme")
gc()

death.formula
fem.sdm.death <- spml(deaths_perc ~ confirmed_perc + stringency_index + NDVI_perc + tem_c,
                      index = c('GEOID', 'date'), data = merge_df.Q,  listw = mat2listw(W),
                      model = 'within', spatial.error = 'kkp', lag = T)
