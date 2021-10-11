library(tidyverse)
library(dplyr)
library(readxl)
library(stringr)
library(raster)
library(gdalUtils)
library(rgdal)

setwd("C:\\Users\\li.chao.987@s.kyushu-u.ac.jp\\OneDrive - Kyushu University\\05_Article\\01_Raster\\cb_2017_us_county_20m\\")
us_shape <- readOGR(dsn = ".", layer = "cb_2017_us_county_20m84")
proj_us <- us_shape@proj4string
  
setwd("D:\\05_Article\\NDVI\\HEGOUT")
filelist <- list.files("D:\\05_Article\\NDVI\\HEGOUT")
filelist.tif <- c()
for (filename in filelist){
  if (str_sub(filename, -3, -1) == "tif") {
    filelist.tif <- append(filelist.tif, filename)
  }
}
rm(filelist)

raster.date <- c()
for (filename in filelist.tif){
  raster.date <- append(raster.date, str_sub(filename, 9, 16))
}
raster.date <- raster.date %>% unique()

# sub by raster.date
single.date <- raster.date[2]
filelist.sub <- c()
for (filename in filelist.tif){
  if (str_sub(filename, 9, 16) == single.date) {
    filelist.sub <- append(filelist.sub, filename)
  }
}

'################
for (raster.name in filelist.sub){
  try({
    import.tif <- raster(raster.name)
    NAvalue(import.tif) <- -3000
    outname <- paste("toGIS\\", raster.name, sep = "")
    a <- (import.tif %>% summary())
    if (!is.na(a[1])){
      writeRaster(import.tif, filename = outname,
                  format="GTiff", overwrite=TRUE)
    }
  })
}
'################

fail.number <- 1
first.raster <- T
raster.number <- 1
for (raster.name in filelist.sub){
  if(first.raster == T){
    import.tif <- try(raster(raster.name))
    if(!inherits(import.tif, "try-error"))
    {
      NAvalue(import.tif) <- -3000
      merged.raster <- import.tif
      first.raster <- F
    }
    raster.number <- raster.number + 1
  } else {
    import.tif <- try(raster(raster.name))
    if(!inherits(import.tif, "try-error"))
    {
      NAvalue(import.tif) <- -3000
      merged.raster <- mosaic(merged.raster, import.tif, fun = mean, tolerance = 0.5)
    } else {
      cat("This raster fail:", raster.name, "\n")
      fail.number <- fail.number + 1
    }
    raster.number <- raster.number + 1
  }
}
cat("Raster fail number:", fail.number, "\n")

plot(merged.raster)
import.tif <- raster(filelist.sub[4])
plot(import.tif)
