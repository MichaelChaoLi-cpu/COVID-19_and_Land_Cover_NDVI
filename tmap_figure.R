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
library(OpenStreetMap)
library(tmaptools)
library(grid)
library(rgdal)
library(tigris)

setwd("C:\\Users\\li.chao.987@s.kyushu-u.ac.jp\\OneDrive - Kyushu University\\05_Article\\01_Raster\\cb_2017_us_county_20m\\")
states <- readOGR(dsn = ".", layer = "cb_2017_us_county_20m")
statesCOVID <- geo_join(states, LC_POP_CAS_DE, 'CountyFIPS', 'CountyFIPS', how = 'inner')
setwd("C:\\Users\\li.chao.987@s.kyushu-u.ac.jp\\OneDrive - Kyushu University\\05_Article\\")

# tm set
title_size = .0001
legend_title_size = 1
margin = 0
brk_ols_res = c(-2, -1.5, -1.0, -0.5, 0, 0.5, 1, 1.5, 2)
tmap_mode('plot')
prj <- get_projection(statesCOVID,  guess.longlat = FALSE)
# tm set

a_CFR <-
  tm_shape(statesCOVID) +
  tm_polygons(col = 'log_death_rate', pal = "-RdYlGn", auto.palette.mapping = FALSE,
              title = "Natural Logarithm of COVID-19\nCase Fatality Rate (CFR):", border.alpha = 0) +
  tm_shape(statesCOVID) +
  tm_polygons(lwd = 0.01, alpha = .25) +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    #legend.bg.color = "white",
    #main.title = "Figure 1: Natural Logarithm of COVID-19 Case Fatality Rate (CFR)",
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) + 
  tm_scale_bar()

tmap_save(a_CFR, "04_Figure\\geo\\CFR1.png", dpi = 300)

a_Green_rate <-
  tm_shape(statesCOVID) +
  tm_polygons(col = 'Green_rate', pal = "-RdYlGn", auto.palette.mapping = FALSE,
              title = "Percentage of Green Space\nin Each County:", border.alpha = 0) +
  tm_shape(statesCOVID) +
  tm_polygons(lwd = 0.01, alpha = .25) +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    #legend.bg.color = "white",
    #main.title = "Figure 2: Percentage of Green Space in Each County",
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) + 
  tm_scale_bar()

tmap_save(a_Green_rate, "04_Figure\\geo\\Green_rate.png", dpi = 300)

a_Medium_Intensity_inDeveloped_perc <-
  tm_shape(statesCOVID) +
  tm_polygons(col = 'Medium_Intensity_inDeveloped_perc', pal = "-RdYlGn", auto.palette.mapping = FALSE,
              breaks = c(0, 10, 20, 30, 40, 60),
              title = "Percentage of Medium Intensity Developed\nArea in Developed Area:", border.alpha = 0) +
  tm_shape(statesCOVID) +
  tm_polygons(lwd = 0.01, alpha = .25) +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    #legend.bg.color = "white",
    #main.title = "Figure 3: Percentage of Medium Intensity in Developed Area",
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) + 
  tm_scale_bar()

tmap_save(a_Medium_Intensity_inDeveloped_perc, "04_Figure\\geo\\Medium_Intensity_inDeveloped_perc.png", dpi = 300)

a_High_Intensity_inDeveloped_perc <-
  tm_shape(statesCOVID) +
  tm_polygons(col = 'High_Intensity_inDeveloped_perc', pal = "-RdYlGn", auto.palette.mapping = FALSE,
              breaks = c(0, 5, 10, 15, 60),
              title = "Percentage of High Intensity Developed\nArea in Developed Area:", border.alpha = 0) +
  tm_shape(statesCOVID) +
  tm_polygons(lwd = 0.01, alpha = .25) +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    #legend.bg.color = "white",
    #main.title = "Figure 4: Percentage of High Intensity in Developed Area",
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) + 
  tm_scale_bar()

tmap_save(a_High_Intensity_inDeveloped_perc, "04_Figure\\geo\\High_Intensity_inDeveloped_perc.png", dpi = 300)
