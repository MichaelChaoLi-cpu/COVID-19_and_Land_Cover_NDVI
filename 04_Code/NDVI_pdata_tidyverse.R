library("COVID19")
library(tidyverse)
library("dplyr")
library(lubridate)
library(plm)

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
      mutate(o.con = (ori_con - confirmed)/population,
             o.deaths = (ori_deaths - deaths)/population) 
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
      mutate(o.con = ori_con/population,
             o.deaths = ori_deaths/population)
    stringency <- dataset %>% filter(date < ymd(input_date)) %>%
      dplyr::select(id, stringency_index)
    stringency <- stringency$stringency_index %>% 
      aggregate(by = list(stringency$id), mean, na.action = na.omit)
    stringency <- stringency %>% rename(id = Group.1)
    dataset.output <- left_join(dataset.output, stringency) %>%
      dplyr::select(id, o.con, o.deaths, x)
  }
  dataset.output$date <- ymd(base_month)
  colnames(dataset.output) <- c("id", "confirmed_perc", "deaths_perc", 
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

save.image("00_RData\\panel_monthly_dcp.Rdata")



#test code
merge_df.pd <- pdata.frame(merge_df, index = c("id", "date"))

test.ols <- plm(deaths_perc ~ confirmed_perc + stringency_index, data = merge_df.pd, model = "pooling") 
summary(test.ols)
test.fe <- plm(deaths_perc ~ confirmed_perc + stringency_index, data = merge_df.pd, model = "within")
summary(test.fe)
test.re <- plm(deaths_perc ~ confirmed_perc + stringency_index, data = merge_df.pd, model = "random")
summary(test.re)

pFtest(test.fe, test.ols)
phtest(test.fe, test.re)
