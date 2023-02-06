# Author: M.L.

# input: dataset.Rdata

# output: prevalence.png
# output: mortality.png

# prevalence.png: "" Illustrating the spatial distribution of the total prevalence
# mortality.png: "" Illustrating the spatial distribution of the total mortality


# end


library(wesanderson)
library("viridisLite")
library("viridis") 
library(RColorBrewer)
library(tmap)
library(ggplot2)
library(ggthemes)
library(leaflet)
library(spData)
library(ceramic)
library(sf)
library(tmaptools)
library(grid)
library(rgdal)
library(tigris)
library(spdplyr)

load("00_RData/dataset.Rdata")

# to guarantee there are the same dataset in the spatial model and ols model
tes <- dataset %>%
  dplyr::select("Open_Water_perc",
                "Developed_Open_Space_perc","Developed_Low_Intensity_perc",
                "Developed_Medium_Intensity_perc",
                "Developed_High_Intensity_perc","Barren_Land_perc","Deciduous_Forest_perc",
                "Evergreen_Forest_perc","Mixed_Forest_perc","Shrub_perc",
                "Grassland_perc","Pasture_perc","Cultivated_Crops_perc",
                "Woody_Wetlands_perc","Emergent_Herbaceous_Wetlands_perc",
                incidence_proportion, 
                gatherings_restrictions, transport_closing,
                stay_home_restrictions, internal_movement_restrictions, 
                international_movement_restrictions, 
                #TestCode pop_density, 
                mortality,
                age15_44, age45_64, age65_99, black_rate, hispanic_rate, male,
                Unemployment_rate_2019, log_Median_Household_Income_2018,
                poverty_rate, less_than_high_school,  
                poor_health_rate_2019, poor_physical_days_2019, poor_mental_days_2019,
                smoker_rate_2019, obesity_rate_2019, physical_inactivity_2019,
                exercise_opportunities_rate_2019,
                hospital_beds_per_1000, summer_tmmx_mean, winter_tmmx_mean, summer_rmax_mean,
                winter_rmax_mean, pm25_mean, key_numeric
  ) %>% na.omit()
us_shape <- readOGR(dsn = "01_Raster\\01_Boundary", layer = "cb_2017_us_county_20m84")
us_shape <- geo_join(us_shape, tes, 'CountyFIPS', 'key_numeric', how = 'left')
#us_shape_back <- readOGR(dsn = "01_Raster\\01_Boundary", layer = "cb_2017_us_county_20m84")
us_shape <- us_shape %>% filter(STATEFP != '02', STATEFP != '72', STATEFP != '15')
us_shape$col <- 1
#plot(us_shape) # now there are 3103 records
gc() 

# tm set
title_size = .0001
legend_title_size = 1
margin = 0.005

tmap_mode('plot')
prj <- get_projection(us_shape,  guess.longlat = FALSE)
# tm set

brk_ols_res = c(0,  10,  20,  30, 60)
labels_ols = c("0 ~ 10%", "10% ~ 20%", "20% ~ 30%", "30% +")
(EHW <-
  tm_shape(us_shape) +
  tm_polygons(col = 'Emergent_Herbaceous_Wetlands_perc', pal = "-RdYlGn", 
              auto.palette.mapping = FALSE,
              title = "Emergent Herbaceous Wetland Ratio:", border.alpha = 0, 
              breaks = brk_ols_res, labels	= labels_ols, 
              colorNA = 'grey') +
    tm_shape(us_shape) +
    tm_polygons(lwd = 0.01, alpha = .25) +
    tm_grid(projection = prj, alpha = .25)+ 
    tm_layout(
      inner.margins = c(0.1, margin, margin, margin),
      title.size = title_size, 
      legend.position = c("left", "bottom"),
      main.title.position = c("center", "bottom"),
      legend.title.size = legend_title_size,
      legend.text.size = legend_title_size * 0.75
    ) + 
    tm_scale_bar()
    )
tmap_save(EHW, "06_Figure/EHW_SpatialDistribution.png", dpi = 300)

brk_ols_res = c(0,  50,  100,  150, 200, 250, 300)
labels_ols = c("0 ~ 50", "50 ~ 100", "100 ~ 150", "150 ~ 200", "200 ~ 250", "250 ~ 300")
(Prevalence <-
    tm_shape(us_shape) +
    tm_polygons(col = 'incidence_proportion', pal = "-RdYlGn", 
                auto.palette.mapping = FALSE,
                title = "Prevalence (cases/1000):", border.alpha = 0, 
                breaks = brk_ols_res, labels	= labels_ols, 
                colorNA = 'grey') +
    tm_shape(us_shape) +
    tm_polygons(lwd = 0.01, alpha = .25) +
    tm_grid(projection = prj, alpha = .25)+ 
    tm_layout(
      inner.margins = c(0.1, margin, margin, margin),
      title.size = title_size, 
      legend.position = c("left", "bottom"),
      main.title.position = c("center", "bottom"),
      legend.title.size = legend_title_size,
      legend.text.size = legend_title_size * 0.75
    ) + 
    tm_scale_bar()
)
tmap_save(Prevalence, "06_Figure/prevalence.png", dpi = 300)

brk_ols_res = c(0,  1,  2,  3, 4, 5, 12)
labels_ols = c("0 ~ 1", "1 ~ 2", "2 ~ 3", "3 ~ 4", "4 ~ 5", "5 +")
(Prevalence <-
    tm_shape(us_shape) +
    tm_polygons(col = 'mortality', pal = "-RdYlGn", 
                auto.palette.mapping = FALSE,
                title = "Mortality (cases/1000):", border.alpha = 0, 
                breaks = brk_ols_res, labels	= labels_ols, 
                colorNA = 'grey') +
    tm_shape(us_shape) +
    tm_polygons(lwd = 0.01, alpha = .25) +
    tm_grid(projection = prj, alpha = .25)+ 
    tm_layout(
      inner.margins = c(0.1, margin, margin, margin),
      title.size = title_size, 
      legend.position = c("left", "bottom"),
      main.title.position = c("center", "bottom"),
      legend.title.size = legend_title_size,
      legend.text.size = legend_title_size * 0.75
    ) + 
    tm_scale_bar()
)
tmap_save(Prevalence, "06_Figure/mortality.png", dpi = 300)
