# Author: M.L.

# output: panel_mod.csv

# panel_mod.csv: "NDVI" this is raw data, which should be multiply by the factor (0.0001). MOD13A3 1 km monthly NDVI 
# panel_mod.csv: "DayTem" this is raw data, which should be multiply by the factor (0.02 K). MOD11C3 0.05 monthly
# panel_mod.csv: "NightTem" this is raw data, which should be multiply by the factor (0.02 K). MOD11C3 0.05 monthly
# 

# end


library(tidyverse)
library(dplyr)
library(readxl)
library(stringr)
library(raster)
library(gdalUtils)
library(rgdal)

us_shape <- readOGR(dsn = "01_Raster\\01_Boundary", layer = "cb_2017_us_county_20m84")
proj_us <- crs(us_shape)

#get the name of NDVI.tif
filelist <- list.files("01_Raster\\03_NDVI")
filelist.tif <- c()
for (filename in filelist){
  if (str_sub(filename, -3, -1) == "tif") {
    filelist.tif <- append(filelist.tif, filename)
  }
}
rm(filename, filelist)

raster.dates <- c()
for (filename in filelist.tif){
  raster.dates <- append(raster.dates, str_sub(filename, 14, 21))
}
raster.dates <- raster.dates %>% unique()
rm(filename)


tif.list <- list()
for (raster.date in raster.dates){
  tif.name.used <- c()
  for (tif.name in filelist.tif){
    if (raster.date == str_sub(tif.name, 14, 21)){
      tif.name.used <- append(tif.name.used, tif.name)
    }
  }
  tif.A <- raster(paste0("01_Raster\\03_NDVI\\", tif.name.used[1]))
  tif.B <- raster(paste0("01_Raster\\03_NDVI\\", tif.name.used[2]))
  tif <- mean(tif.A, tif.B, na.rm = T)
  tif.list <- append(tif.list, tif)
  rm(tif.A, tif.B)
}

NDVI.data <- us_shape@data %>% dplyr::select(GEOID)
i = 1
while (i < (length(raster.dates) + 1)){
  extract.data <- raster::extract(tif.list[[i]], us_shape, fun = mean, na.rm = T)
  extract.data <- as.data.frame(extract.data)
  colnames(extract.data) <- paste0("D", raster.dates[i])
  NDVI.data <- cbind(NDVI.data, extract.data)
  i <- i + 1
}
rm(tif.list)
gc()

#get the name of DayTemperature.tif
filelist <- list.files("01_Raster\\04_DayTemperature")
filelist.tif <- c()
for (filename in filelist){
  if (str_sub(filename, -3, -1) == "tif") {
    filelist.tif <- append(filelist.tif, filename)
  }
}
rm(filename, filelist)

tif.list <- list()
for (raster.date in raster.dates){
  tif.name.used <- c()
  for (tif.name in filelist.tif){
    if (raster.date == str_sub(tif.name, 21, 28)){
      tif.name.used <- append(tif.name.used, tif.name)
    }
  }
  tif.A <- raster(paste0("01_Raster\\04_DayTemperature\\", tif.name.used[1]))
  tif.B <- raster(paste0("01_Raster\\04_DayTemperature\\", tif.name.used[2]))
  tif <- mean(tif.A, tif.B, na.rm = T)
  tif.list <- append(tif.list, tif)
  rm(tif.A, tif.B)
}

DayTemperature.data <- us_shape@data %>% dplyr::select(GEOID)
i = 1
while (i < (length(raster.dates) + 1)){
  extract.data <- raster::extract(tif.list[[i]], us_shape, fun = mean, na.rm = T)
  extract.data <- as.data.frame(extract.data)
  colnames(extract.data) <- paste0("D", raster.dates[i])
  DayTemperature.data <- cbind(DayTemperature.data, extract.data)
  i <- i + 1
}
rm(tif.list)
gc()


#get the name of NightTemperature.tif
filelist <- list.files("01_Raster\\05_NightTemperature")
filelist.tif <- c()
for (filename in filelist){
  if (str_sub(filename, -3, -1) == "tif") {
    filelist.tif <- append(filelist.tif, filename)
  }
}
rm(filename, filelist)

tif.list <- list()
for (raster.date in raster.dates){
  tif.name.used <- c()
  for (tif.name in filelist.tif){
    if (raster.date == str_sub(tif.name, 23, 30)){
      tif.name.used <- append(tif.name.used, tif.name)
    }
  }
  tif.A <- raster(paste0("01_Raster\\05_NightTemperature\\", tif.name.used[1]))
  tif.B <- raster(paste0("01_Raster\\05_NightTemperature\\", tif.name.used[2]))
  tif <- mean(tif.A, tif.B, na.rm = T)
  tif.list <- append(tif.list, tif)
  rm(tif.A, tif.B)
}

NightTemperature.data <- us_shape@data %>% dplyr::select(GEOID)
i = 1
while (i < (length(raster.dates) + 1)){
  extract.data <- raster::extract(tif.list[[i]], us_shape, fun = mean, na.rm = T)
  extract.data <- as.data.frame(extract.data)
  colnames(extract.data) <- paste0("D", raster.dates[i])
  NightTemperature.data <- cbind(NightTemperature.data, extract.data)
  i <- i + 1
}
rm(tif.list)
gc()

NDVI.data$type <- "NDVI"
NightTemperature.data$type <- "NigTem"
DayTemperature.data$type <- "DayTem"

panel_mod <- rbind(NDVI.data, NightTemperature.data, DayTemperature.data)
#panel_mod %>% write.csv("02_RawData\\panel_mod.csv")

# to get the NTL 
filelist <- list.files("D:/05_Article/NTLoutput")
month.code <- str_sub(filelist, 42, 48)

NTL.data <- us_shape@data %>% dplyr::select(GEOID)

for (raster.date in month.code){
  tif.list <- list()
  tif.name.used <- c()
  for (tif.name in filelist){
    if (raster.date == str_sub(tif.name, 42, 48)){
      tif.name.used <- append(tif.name.used, tif.name)
    }
  }
  for (tif.name.used.single in tif.name.used) {
    tif <- raster(paste0("D:/05_Article/NTLoutput/", tif.name.used.single))
    tif.list <- append(tif.list, tif)
  }
  merged.tif <- do.call(merge, tif.list)
  rm(tif.list)
  rm(tif)
  extract.data <- raster::extract(merged.tif, us_shape, fun = mean, na.rm = T)
  extract.data <- as.data.frame(extract.data)
  colnames(extract.data) <- paste0("D", str_sub(raster.date, 1, 4), "_", str_sub(raster.date, 5, 7)) 
  NTL.data <- cbind(NTL.data, extract.data)
}
