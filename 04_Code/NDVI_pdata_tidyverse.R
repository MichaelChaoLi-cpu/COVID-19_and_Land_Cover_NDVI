library("COVID19")
library(tidyverse)
library("dplyr")
library(lubridate)
library(plm)
library(stringr)

deat.conf.pop <- COVID19::covid19(country = "USA", level = 3)

sub.Dataset <- function(dataset, input_date){
  base_month = ymd(input_date) %m-% months(1)
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
  return(dataset.output)
}

deat.conf.pop.202003 <- sub.Dataset(deat.conf.pop, "2020-04-01")
deat.conf.pop.202004 <- sub.Dataset(deat.conf.pop, "2020-05-01")
deat.conf.pop.202005 <- sub.Dataset(deat.conf.pop, "2020-06-01")
deat.conf.pop.202006 <- sub.Dataset(deat.conf.pop, "2020-07-01")
deat.conf.pop.202007 <- sub.Dataset(deat.conf.pop, "2020-08-01")
deat.conf.pop.202008 <- sub.Dataset(deat.conf.pop, "2020-09-01")
deat.conf.pop.202009 <- sub.Dataset(deat.conf.pop, "2020-10-01")
deat.conf.pop.202010 <- sub.Dataset(deat.conf.pop, "2020-11-01")
deat.conf.pop.202011 <- sub.Dataset(deat.conf.pop, "2020-12-01")
deat.conf.pop.202012 <- sub.Dataset(deat.conf.pop, "2021-01-01")
deat.conf.pop.202101 <- sub.Dataset(deat.conf.pop, "2021-02-01")
deat.conf.pop.202102 <- sub.Dataset(deat.conf.pop, "2021-03-01")
deat.conf.pop.202103 <- sub.Dataset(deat.conf.pop, "2021-04-01")
deat.conf.pop.202104 <- sub.Dataset(deat.conf.pop, "2021-05-01")
deat.conf.pop.202105 <- sub.Dataset(deat.conf.pop, "2021-06-01")
deat.conf.pop.202106 <- sub.Dataset(deat.conf.pop, "2021-07-01")
deat.conf.pop.202107 <- sub.Dataset(deat.conf.pop, "2021-08-01")
deat.conf.pop.202108 <- sub.Dataset(deat.conf.pop, "2021-09-01")
deat.conf.pop.202109 <- sub.Dataset(deat.conf.pop, "2021-10-01")

merge_df <- rbind(deat.conf.pop.202003, deat.conf.pop.202004, deat.conf.pop.202005,
                  deat.conf.pop.202006, deat.conf.pop.202007, deat.conf.pop.202008,
                  deat.conf.pop.202009, deat.conf.pop.202010, deat.conf.pop.202011,
                  deat.conf.pop.202012, deat.conf.pop.202101, deat.conf.pop.202102,
                  deat.conf.pop.202103, deat.conf.pop.202104, deat.conf.pop.202105,
                  deat.conf.pop.202106, deat.conf.pop.202107, deat.conf.pop.202108,
                  deat.conf.pop.202109)
rm(deat.conf.pop.202003, deat.conf.pop.202004, deat.conf.pop.202005,
   deat.conf.pop.202006, deat.conf.pop.202007, deat.conf.pop.202008,
   deat.conf.pop.202009, deat.conf.pop.202010, deat.conf.pop.202011,
   deat.conf.pop.202012, deat.conf.pop.202101, deat.conf.pop.202102,
   deat.conf.pop.202103, deat.conf.pop.202104, deat.conf.pop.202105,
   deat.conf.pop.202106, deat.conf.pop.202107, deat.conf.pop.202108,
   deat.conf.pop.202109)
gc()

dataset.id <- deat.conf.pop %>% filter(date == ymd("2021-08-01"))
dataset.id <- dataset.id %>% dplyr::select(id, key_local, population) %>% as.data.frame()
merge_df <- left_join(merge_df, dataset.id)
rm(dataset.id)
merge_df <- merge_df %>% filter(confirmed > -1) %>%
  filter(deaths > -1)
merge_df <- merge_df %>% as.data.frame()
merge_df <- merge_df %>% rename(GEOID = key_local)
merge_df$GEOID <- merge_df$GEOID %>% as.numeric()
save.image("00_RData\\panel_monthly_dcp.Rdata")

NDVI.temper.panel <- read.csv("02_RawData\\panel_mod.csv")
NDVI.panel <- NDVI.temper.panel %>%
  filter(type == "NDVI") %>%
  dplyr::select(-X, -type)
NDVI.panel <- NDVI.panel %>%
  pivot_longer(cols = D2020_001:D2021_244, names_to = "date", values_to = "NDVI")
NDVI.panel <- NDVI.panel %>%
  mutate(year = str_sub(date, 2, 5),
         day = str_sub(date, 7, 9)) 
NDVI.panel$day <- NDVI.panel$day %>% as.numeric()
NDVI.panel$date <- as.Date((NDVI.panel$day - 1), origin = paste0(NDVI.panel$year,"-01-01"))
NDVI.panel <- NDVI.panel %>% dplyr::select("GEOID", "date", "NDVI")
NDVI.panel$NDVI <- NDVI.panel$NDVI / 10000
merge_df <- left_join(merge_df, NDVI.panel, by = c("GEOID", "date"))
rm(NDVI.panel)

DayTem.panel <- NDVI.temper.panel %>%
  filter(type == "DayTem") %>%
  dplyr::select(-X, -type)
DayTem.panel <- DayTem.panel %>%
  pivot_longer(cols = D2020_001:D2021_244, names_to = "date", values_to = "DayTem")
NigTem.panel <- NDVI.temper.panel %>%
  filter(type == "NigTem") %>%
  dplyr::select(-X, -type)
NigTem.panel <- NigTem.panel %>%
  pivot_longer(cols = D2020_001:D2021_244, names_to = "date", values_to = "NigTem")
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
#Tem.panel$NDVI <- Tem.panel$NDVI / 10000
merge_df <- left_join(merge_df, Tem.panel, by = c("GEOID", "date"))
rm(Tem.panel)
rm(NDVI.temper.panel)



#test code
merge_df.pd <- pdata.frame(merge_df, index = c("GEOID", "date"))

test.ols <- plm(deaths_perc ~ confirmed_perc + stringency_index + NDVI + tem + lag(deaths_perc) + lag(confirmed_perc),
                data = merge_df.pd, model = "pooling") 
summary(test.ols)
test.fe <- plm(deaths_perc ~ confirmed_perc + stringency_index + NDVI + tem + lag(deaths_perc), 
               data = merge_df.pd, model = "within")
summary(test.fe)
test.re <- plm(deaths_perc ~ confirmed_perc + stringency_index + NDVI + tem + lag(deaths_perc), 
               data = merge_df.pd, model = "random")
summary(test.re)

pFtest(test.fe, test.ols)
phtest(test.fe, test.re)
