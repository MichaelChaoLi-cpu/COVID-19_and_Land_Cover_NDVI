# Author: M.L.

# output: panel_NDVI_mortality_prevalence_monthly.csv

# panel_NDVI_mortality_prevalence_monthly.csv: "stringency_index" this is prevention score, 0 (nothing) - 100 (utmost strict)
# panel_NDVI_mortality_prevalence_monthly.csv: "date" this is the time index
# panel_NDVI_mortality_prevalence_monthly.csv: "GEOID" this is the identity index.
# panel_NDVI_mortality_prevalence_monthly.csv: "NDVI_perc" this is the percentage of NDVI (-100% - 100%)
# panel_NDVI_mortality_prevalence_monthly.csv: "tem_c" this is the temperature (C degree)
# panel_NDVI_mortality_prevalence_monthly.csv: "NLT" this is the index of Nighttime Light.
# panel_NDVI_mortality_prevalence_monthly.csv: "confirmed_per1000" this is the prevalence index (confirmed case/1000).
# panel_NDVI_mortality_prevalence_monthly.csv: "deaths_per1000" this is the mortality index (mortality/1000).

# end

library("COVID19")
library(tidyverse)
library("dplyr")
library(lubridate)
library(stringr)
library(rgdal)
library(tigris)
library(spdep)
library(splm)

deat.conf.pop <- COVID19::covid19(country = "USA", level = 3)

test <- deat.conf.pop %>% 
  dplyr::select("id", "date", "confirmed", "deaths") %>%
  as.data.frame()
test <- plm::pdata.frame(test, index = c("id", "date"))
test$confirmed.dif <- test$confirmed - plm::lag(test$confirmed)
test$deaths.dif <- test$deaths - plm::lag(test$deaths)
test <- test %>% 
  mutate(confirmed.dif = ifelse(confirmed.dif < 0, 0, confirmed.dif),
         deaths.dif = ifelse(deaths.dif < 0, 0, deaths.dif)) %>% 
  dplyr::select(-"confirmed", -"deaths")
test <- lapply(test, function(x){attr(x, c("id", "date")) <- NULL; x}) %>% as.data.frame()
test$id <- test$id %>% as.character()
test$date <- test$date %>% as.character() %>% ymd()

deat.conf.pop <- left_join(deat.conf.pop, test, by = c("id", "date"))
rm(test)

sub.Dataset.M <- function(dataset, input_date){
  dataset.id <- dataset %>% filter(date == ymd("2021-08-01")) %>%
    dplyr::select(id, population)
  input_date <- ymd(input_date)
  base_month = input_date %m-% months(1)
  dataset.output <- dataset %>% 
    dplyr::select(id, date, population, confirmed.dif, deaths.dif) %>%
    filter(ymd(date) < input_date,
           ymd(date) >= base_month) %>%
    as.data.frame()
  dataset.output <- dataset.output %>% 
    group_by(id) %>% 
    summarise(confirmed = sum(confirmed.dif, na.rm = T),
              deaths = sum(deaths.dif, na.rm = T))
  dataset.output <- left_join(dataset.id, dataset.output, by = "id")
  dataset.output <- dataset.output %>% 
    mutate(
      confirmed = ifelse(is.na(confirmed), 0, confirmed),
      deaths = ifelse(is.na(deaths), 0, deaths)
    )
  stringency <- dataset %>% filter(date > ymd(base_month)) %>%
    filter(date < ymd(input_date)) %>%
    dplyr::select(id, stringency_index)
  stringency <- stringency$stringency_index %>% 
    aggregate(by = list(stringency$id), mean)
  stringency <- stringency %>% rename(id = Group.1)
  dataset.output <- left_join(dataset.output, stringency, by = "id") 
  dataset.output$date <- ymd(base_month)
  colnames(dataset.output) <- c("id", "population", "confirmed", "deaths", 
                                "stringency_index", "date")
  dataset.output <- dataset.output %>% 
    mutate(
      stringency_index = ifelse(is.na(stringency_index), 0, stringency_index)
    )
  return(dataset.output)
}

deat.conf.pop.202003 <- sub.Dataset.M(deat.conf.pop, "2020-04-01")
deat.conf.pop.202004 <- sub.Dataset.M(deat.conf.pop, "2020-05-01")
deat.conf.pop.202005 <- sub.Dataset.M(deat.conf.pop, "2020-06-01")
deat.conf.pop.202006 <- sub.Dataset.M(deat.conf.pop, "2020-07-01")
deat.conf.pop.202007 <- sub.Dataset.M(deat.conf.pop, "2020-08-01")
deat.conf.pop.202008 <- sub.Dataset.M(deat.conf.pop, "2020-09-01")
deat.conf.pop.202009 <- sub.Dataset.M(deat.conf.pop, "2020-10-01")
deat.conf.pop.202010 <- sub.Dataset.M(deat.conf.pop, "2020-11-01")
deat.conf.pop.202011 <- sub.Dataset.M(deat.conf.pop, "2020-12-01")
deat.conf.pop.202012 <- sub.Dataset.M(deat.conf.pop, "2021-01-01")
deat.conf.pop.202101 <- sub.Dataset.M(deat.conf.pop, "2021-02-01")
deat.conf.pop.202102 <- sub.Dataset.M(deat.conf.pop, "2021-03-01")
deat.conf.pop.202103 <- sub.Dataset.M(deat.conf.pop, "2021-04-01")
deat.conf.pop.202104 <- sub.Dataset.M(deat.conf.pop, "2021-05-01")
deat.conf.pop.202105 <- sub.Dataset.M(deat.conf.pop, "2021-06-01")
deat.conf.pop.202106 <- sub.Dataset.M(deat.conf.pop, "2021-07-01")
deat.conf.pop.202107 <- sub.Dataset.M(deat.conf.pop, "2021-08-01")
deat.conf.pop.202108 <- sub.Dataset.M(deat.conf.pop, "2021-09-01")
deat.conf.pop.202109 <- sub.Dataset.M(deat.conf.pop, "2021-10-01")
deat.conf.pop.202110 <- sub.Dataset.M(deat.conf.pop, "2021-11-01")


merge_df.M <- rbind(
  deat.conf.pop.202003, deat.conf.pop.202004, deat.conf.pop.202005,
  deat.conf.pop.202006, deat.conf.pop.202007, deat.conf.pop.202008,
  deat.conf.pop.202009, deat.conf.pop.202010, deat.conf.pop.202011,
  deat.conf.pop.202012, deat.conf.pop.202101, deat.conf.pop.202102,
  deat.conf.pop.202103, deat.conf.pop.202104, deat.conf.pop.202105,
  deat.conf.pop.202106, deat.conf.pop.202107, deat.conf.pop.202108,
  deat.conf.pop.202109, deat.conf.pop.202110
  )
rm(
  deat.conf.pop.202003, deat.conf.pop.202004, deat.conf.pop.202005,
  deat.conf.pop.202006, deat.conf.pop.202007, deat.conf.pop.202008,
  deat.conf.pop.202009, deat.conf.pop.202010, deat.conf.pop.202011,
  deat.conf.pop.202012, deat.conf.pop.202101, deat.conf.pop.202102,
  deat.conf.pop.202103, deat.conf.pop.202104, deat.conf.pop.202105,
  deat.conf.pop.202106, deat.conf.pop.202107, deat.conf.pop.202108,
  deat.conf.pop.202109, deat.conf.pop.202110
  )
gc()

dataset.id <- deat.conf.pop %>% filter(date == ymd("2021-08-01"))
dataset.id <- dataset.id %>% dplyr::select(id, key_local) %>% as.data.frame()
merge_df.M <- left_join(merge_df.M, dataset.id)
rm(dataset.id)

merge_df.M <- merge_df.M %>% as.data.frame()
merge_df.M <- merge_df.M %>% rename(GEOID = key_local)
merge_df.M$GEOID <- merge_df.M$GEOID %>% as.numeric()
merge_df.M <- merge_df.M %>% 
  mutate(stringency_index = ifelse(is.na(stringency_index), 0,stringency_index))

NDVI.temper..NTL.panel <- read.csv("02_RawData\\panel_mod.csv")
NDVI.temper..NTL.panel <- NDVI.temper..NTL.panel %>%
  dplyr::select(-X)

NDVI.panel <- NDVI.temper..NTL.panel %>%
  filter(type == "NDVI") %>%
  dplyr::select(-type)
NDVI.panel <- NDVI.panel %>%
  pivot_longer(cols = D2020_001:D2021_244, names_to = "date", values_to = "NDVI")
NDVI.panel <- NDVI.panel %>%
  mutate(year = str_sub(date, 2, 5),
         day = str_sub(date, 7, 9)) 
NDVI.panel$day <- NDVI.panel$day %>% as.numeric()
NDVI.panel$date <- as.Date((NDVI.panel$day - 1), origin = paste0(NDVI.panel$year,"-01-01"))
NDVI.panel <- NDVI.panel %>% dplyr::select("GEOID", "date", "NDVI")
NDVI.panel$NDVI <- NDVI.panel$NDVI / 10000
merge_df.M <- left_join(merge_df.M, NDVI.panel, by = c("GEOID", "date"))
rm(NDVI.panel)

DayTem.panel <- NDVI.temper..NTL.panel %>%
  filter(type == "DayTem") %>%
  dplyr::select(-type)
DayTem.panel <- DayTem.panel %>%
  pivot_longer(cols = D2020_001:D2021_244, names_to = "date", values_to = "DayTem")
NigTem.panel <- NDVI.temper..NTL.panel %>%
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
merge_df.M <- left_join(merge_df.M, Tem.panel, by = c("GEOID", "date"))
rm(Tem.panel)

NTL.panel <- NDVI.temper..NTL.panel %>%
  filter(type == "NTL") %>%
  dplyr::select(-type)
NTL.panel <- NTL.panel %>%
  pivot_longer(cols = D2020_001:D2021_244, names_to = "date", values_to = "NTL")
NTL.panel <- NTL.panel %>%
  mutate(year = str_sub(date, 2, 5),
         day = str_sub(date, 7, 9)) 
NTL.panel$day <- NTL.panel$day %>% as.numeric()
NTL.panel$date <- as.Date((NTL.panel$day - 1), origin = paste0(NTL.panel$year,"-01-01"))
NTL.panel <- NTL.panel %>% dplyr::select("GEOID", "date", "NTL")
merge_df.M <- left_join(merge_df.M, NTL.panel, by = c("GEOID", "date"))
rm(NTL.panel)

rm(NDVI.temper..NTL.panel)

merge_df.M$confirmed_per1000 <- merge_df.M$confirmed / merge_df.M$population * 1000
merge_df.M$deaths_per1000 <- merge_df.M$deaths / merge_df.M$population * 1000
merge_df.M$NDVI_perc <- merge_df.M$NDVI * 100
merge_df.M$tem_c <- merge_df.M$tem * 0.02 - 273.16 

merge_df.M %>% 
  write.csv("02_RawData\\panel_NDVI_mortality_prevalence_monthly.csv")
