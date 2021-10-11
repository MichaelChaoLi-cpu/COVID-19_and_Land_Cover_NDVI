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

# tm set
title_size = .0001
legend_title_size = 1
margin = 0

tmap_mode('plot')
prj <- get_projection(us_shape,  guess.longlat = FALSE)
# tm set

brk_ols_res = c(0, 1, 2 , 3, 4, 5, 6)
labels_ols = c("0 ~ 1%", "1% ~ 2%", "2% ~ 3%", "3% ~ 4%", "4% ~ 5%", "5% +")
a_CFR <-
  tm_shape(us_shape) +
  tm_polygons(col = 'CFR', pal = "-RdYlGn", auto.palette.mapping = FALSE,
              title = "The COVID-19 Case Fatality Rate (CFR):", border.alpha = 0, 
              breaks = brk_ols_res, labels	= labels_ols) +
  tm_shape(us_shape) +
  tm_polygons(lwd = 0.01, alpha = .25) +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    #legend.bg.color = "white",
    #main.title = "Figure 1: The COVID-19 Case Fatality Rate (CFR)",
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) + 
  tm_scale_bar()
a_CFR
tmap_save(a_CFR, "04_Figure\\geo\\CFR1.png", dpi = 300)

a_open_water <-
  tm_shape(gwr.model$SDF) +
  tm_polygons(col = 'Open_Water_capi', pal = "Reds", auto.palette.mapping = FALSE,
              title = "The Coefficient of Open Water per Capita:", border.alpha = 0) +
  tm_shape(us_shape) +
  tm_polygons(lwd = 0.01, alpha = .25) +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    #legend.bg.color = "white",
    #main.title = "Figure 1: The COVID-19 Case Fatality Rate (CFR)",
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) + 
  tm_scale_bar()
a_open_water
tmap_save(a_open_water, "04_Figure\\geo\\coef_open_water.png", dpi = 300)

a_deciduous <-
  tm_shape(gwr.model$SDF) +
  tm_polygons(col = 'Deciduous_Forest_capi', pal = "Reds", auto.palette.mapping = FALSE,
              title = "The Coefficient of Deciduous Forest\nper Capita:", border.alpha = 0) +
  tm_shape(us_shape) +
  tm_polygons(lwd = 0.01, alpha = .25) +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    #legend.bg.color = "white",
    #main.title = "Figure 1: The COVID-19 Case Fatality Rate (CFR)",
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) + 
  tm_scale_bar()
a_deciduous
tmap_save(a_deciduous, "04_Figure\\geo\\coef_deciduous.png", dpi = 300)

a_income <-
  tm_shape(gwr.model$SDF) +
  tm_polygons(col = 'log_Median_Household_Income_2018', pal = "RdYlGn", auto.palette.mapping = FALSE,
              title = "The Coefficient of Median\nHousehold Income:", border.alpha = 0) +
  tm_shape(us_shape) +
  tm_polygons(lwd = 0.01, alpha = .25) +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    #legend.bg.color = "white",
    #main.title = "Figure 1: The COVID-19 Case Fatality Rate (CFR)",
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) + 
  tm_scale_bar()
a_income
tmap_save(a_income, "04_Figure\\geo\\coef_income.png", dpi = 300)

gwr.model$SDF@data$cfrr_ow <- gwr.model$SDF@data$Open_Water_capi / model$model$CFR * 100
brk_ols_res = c(-120, -50, -40, -30, -20, -10, 0)
labels_ols = c("< -50", "-50 ~ -40", "-40 ~ -30", "-30 ~ -20",
               "-20 ~ -10", "-10 ~ 0")
a_open_water_CFRR <-
  tm_shape(gwr.model$SDF) +
  tm_polygons(col = 'cfrr_ow', pal = "Blues", auto.palette.mapping = FALSE,
              title = "CFRRs of Open Water per Capita (%):", border.alpha = 0,
              breaks = brk_ols_res, labels	= labels_ols) +
  tm_shape(us_shape) +
  tm_polygons(lwd = 0.01, alpha = .25) +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    #legend.bg.color = "white",
    #main.title = "Figure 1: The COVID-19 Case Fatality Rate (CFR)",
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) + 
  tm_scale_bar()
a_open_water_CFRR
tmap_save(a_open_water_CFRR, "04_Figure\\geo\\CFRR_open_water.png", dpi = 300)

gwr.model$SDF@data$cfrr_decforest <- gwr.model$SDF@data$Deciduous_Forest_capi / model$model$CFR * 100
brk_ols_res = c(-350, -50, -40, -30, -20, -10, 0)
labels_ols = c("< -50", "-50 ~ -40", "-40 ~ -30", "-30 ~ -20",
               "-20 ~ -10", "-10 ~ 0")
a_Deciduous_Forest_CFRR <-
  tm_shape(gwr.model$SDF) +
  tm_polygons(col = 'cfrr_decforest', pal = "Blues", auto.palette.mapping = FALSE,
              title = "CFRRs of Deciduous Forest per Capita (%):", border.alpha = 0,
              breaks = brk_ols_res, labels	= labels_ols) +
  tm_shape(us_shape) +
  tm_polygons(lwd = 0.01, alpha = .25) +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    #legend.bg.color = "white",
    #main.title = "Figure 1: The COVID-19 Case Fatality Rate (CFR)",
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) + 
  tm_scale_bar()
a_Deciduous_Forest_CFRR
tmap_save(a_Deciduous_Forest_CFRR, "04_Figure\\geo\\CFRR_Deciduous_Forest.png", dpi = 300)

us_shape@data$mv_ow <- coef(summary(model))[2,1]/coef(summary(model))[21,1] * exp(us_shape@data$log_Median_Household_Income_2018)
hist(us_shape@data$mv_ow)
a_open_water_mv <-
  tm_shape(us_shape) +
  tm_polygons(col = 'mv_ow', pal = "-RdYlGn", auto.palette.mapping = FALSE,
              title = "MV of Open Water (USD/hm2):", border.alpha = 0
              ) +
  tm_shape(us_shape) +
  tm_polygons(lwd = 0.01, alpha = .25) +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    #legend.bg.color = "white",
    #main.title = "Figure 1: The COVID-19 Case Fatality Rate (CFR)",
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) + 
  tm_scale_bar()
a_open_water_mv
tmap_save(a_open_water_mv, "04_Figure\\geo\\mv_open_water.png", dpi = 300)

us_shape@data$mv_ow <- coef(summary(model))[2,1]/coef(summary(model))[21,1] * exp(us_shape@data$log_Median_Household_Income_2018)
hist(us_shape@data$mv_ow)
a_open_water_mv <-
  tm_shape(us_shape) +
  tm_polygons(col = 'mv_ow', pal = "-RdYlGn", auto.palette.mapping = FALSE,
              title = "MV of Open Water (USD/hm2):", border.alpha = 0
  ) +
  tm_shape(us_shape) +
  tm_polygons(lwd = 0.01, alpha = .25) +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    #legend.bg.color = "white",
    #main.title = "Figure 1: The COVID-19 Case Fatality Rate (CFR)",
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) + 
  tm_scale_bar()
a_open_water_mv
tmap_save(a_open_water_mv, "04_Figure\\geo\\mv_open_water.png", dpi = 300)

us_shape@data$mv_df <- coef(summary(model))[7,1]/coef(summary(model))[21,1] * exp(us_shape@data$log_Median_Household_Income_2018)
hist(us_shape@data$mv_df)
a_decuous_forest_mv <-
  tm_shape(us_shape) +
  tm_polygons(col = 'mv_df', pal = "-RdYlGn", auto.palette.mapping = FALSE,
              title = "MV of Deciduous Forest (USD/hm2):", border.alpha = 0
  ) +
  tm_shape(us_shape) +
  tm_polygons(lwd = 0.01, alpha = .25) +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    #legend.bg.color = "white",
    #main.title = "Figure 1: The COVID-19 Case Fatality Rate (CFR)",
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) + 
  tm_scale_bar()
a_decuous_forest_mv
tmap_save(a_decuous_forest_mv, "04_Figure\\geo\\mv_dec_fore.png", dpi = 300)
