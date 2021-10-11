library("tidyverse")
library("sp")
library("raster")
library("dplyr")
library("sf")
library("stringr")
library("ggplot2")
library("grid") 
library("pBrackets") 
library("gridExtra")
library("lme4")
library(ggspatial)
library(ggsn)

setwd("C:\\Users\\li.chao.987@s.kyushu-u.ac.jp\\OneDrive - Kyushu University\\05_Article\\")
us <- map_data('state')

#Plot prevalence of COVID-19
states <- sf::st_read(".\\01_Raster\\cb_2017_us_county_20m\\cb_2017_us_county_20m.shp")

statesCOVID <- left_join(LC_POP_CAS_DE, states, by = "CountyFIPS")


# FIgure I
g1 <- statesCOVID %>% ggplot(aes(geometry = geometry)) +
  xlim(-125, -65) +
  ylim(25, 50) +
  geom_sf(mapping = aes(fill = log_death_rate), color = 'grey', size = 0.001) +
  scale_fill_gradient2(expression(paste("# Figure I: Natural Logarithm of COVID-19 Case Fatality Rate (CFR)")), 
                       low = "#1e90ff",
                       mid = "#ffffba",
                       high = "#8b0000",
                       midpoint = 1.5,
                       breaks = c(0, 0.5, 1, 1.5, 2, 2.5),
                       labels = c("0", "0.5", "1", "1.5", "2", "2.5+"), 
                       limits = c(0, 3),
                       na.value = "white") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12 * 2, hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        line = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal", 
        legend.text = element_text(angle = 60,  size = 12 * 2),
        legend.text.align = 0.75,
        legend.title = element_text(size = 18 * 2),
        legend.key.width = unit(75 * 2, "points"),
        panel.grid.major = element_line(colour = "transparent"))



png(".\\04_Figure\\geo\\county_ln_death_rate.jpeg", height = 1024 * 0.6 * 2, width = 1024 * 2, pointsize = 40)
g1
dev.off()


# Figure II 
g2 <- statesCOVID %>% ggplot(aes(geometry = geometry)) +
  xlim(-125, -65) +
  ylim(25, 50) +
  geom_sf(mapping = aes(fill = Green_rate), color = 'grey', size = 0.001) +
  scale_fill_gradient2(expression(paste("# Figure II: Percentage of Green Space in Every County")), 
                       low = "#1e90ff",
                       mid = "#ffffba",
                       high = "#8b0000",
                       midpoint = 50,
                       breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90),
                       labels = c("0", "10", "20", "30", "40", "50", "60", "70", "80", "90+"), 
                       limits = c(0, 100),
                       na.value = "white") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12 * 2, hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        line = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal", 
        legend.text = element_text(angle = 60,  size = 12 * 2),
        legend.text.align = 0.75,
        legend.title = element_text(size = 18 * 2),
        legend.key.width = unit(75 * 2, "points"),
        panel.grid.major = element_line(colour = "transparent"))

png(".\\04_Figure\\geo\\county_green_rate.jpeg", height = 1024 * 0.6 * 2, width = 1024 * 2, pointsize = 40)
g2
dev.off()


# Figure III
g5 <- statesCOVID %>% ggplot(aes(geometry = geometry)) +
  xlim(-125, -65) +
  ylim(25, 50) +
  geom_sf(mapping = aes(fill = Medium_Intensity_inDeveloped_perc), color = 'grey', size = 0.001) +
  scale_fill_gradient2(expression(paste("# Figure III: Percentage of Medium Intensity Developed Area in Developed Area")), 
                       low = "#1e90ff",
                       mid = "#ffffba",
                       high = "#8b0000",
                       midpoint = 25,
                       breaks = c(0, 10, 20, 30, 40),
                       labels = c("0", "10", "20", "30", "40+"), 
                       limits = c(0, 50),
                       na.value = "white") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12 * 2, hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        line = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal", 
        legend.text = element_text(angle = 60,  size = 12 * 2),
        legend.text.align = 0.75,
        legend.title = element_text(size = 18 * 2),
        legend.key.width = unit(75 * 2, "points"),
        panel.grid.major = element_line(colour = "transparent"))

png(".\\04_Figure\\geo\\county_medium_intensity_area.jpeg", height = 1024 * 0.6 * 2, width = 1024 * 2, pointsize = 40)
g5
dev.off()


# Figure IV
g6 <- statesCOVID %>% ggplot(aes(geometry = geometry)) +
  xlim(-125, -65) +
  ylim(25, 50) +
  geom_sf(mapping = aes(fill =  High_Intensity_inDeveloped_perc), color = 'grey', size = 0.001) +
  scale_fill_gradient2(expression(paste("# Figure IV: Percentage of High Intensity Developed Area in Developed Area")), 
                       low = "#1e90ff",
                       mid = "#ffffba",
                       high = "#8b0000",
                       midpoint = 10,
                       breaks = c(0, 5, 10, 15),
                       labels = c("0", "5", "10", "15+"), 
                       limits = c(0, 30),
                       na.value = "white") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12 * 2, hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        line = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal", 
        legend.text = element_text(angle = 60,  size = 12 * 2),
        legend.text.align = 0.75,
        legend.title = element_text(size = 18 * 2),
        legend.key.width = unit(75 * 2, "points"),
        panel.grid.major = element_line(colour = "transparent"))

png(".\\04_Figure\\geo\\county_high_intensity_area.jpeg", height = 1024 * 0.6 * 2, width = 1024 * 2, pointsize = 40)
g6
dev.off()

library("pheatmap")
library("graphics")
library("ellipse")


# Figure S1
cor_matrix <- LC_POP_CAS_DE %>%
  na.omit() %>%
  select(death_rate, Green_rate, Grey_rate,  Other_rate,  
         Low_Intensity_inDeveloped_perc, Medium_Intensity_inDeveloped_perc, 
         Open_Water_perc, Woody_Wetlands_perc, Emergent_Herbaceous_Wetlands_perc, 
         Deciduous_Forest_perc, Evergreen_Forest_perc, Mixed_Forest_perc, 
         Shrub_perc, Grassland_perc, Pasture_perc, Cultivated_Crops_perc, 
         Barren_Land_perc,
         High_Intensity_inDeveloped_perc, Developed_Low_Intensity_perc, 
         Developed_Medium_Intensity_perc, Developed_High_Intensity_perc, 
         pop_density, pop_density_developed, male, age15_44, age45_64, age65_99, black_rate, 
         hispanic_rate, log_Median_Household_Income_2018, 
         log_median_house_value, mean_house_owner_perc, 
         Unemployment_rate_2019, poverty_rate, 
         less_than_high_school, poor_health_rate_2019,  
         poor_physical_days_2019, poor_mental_days_2019, 
         smoker_rate_2019, obesity_rate_2019, 
         physical_inactivity_2019, exercise_opportunities_rate_2019, hospital_beds, 
         pm25_mean, summer_tmmx_mean, winter_tmmx_mean, 
         summer_rmax_mean, winter_rmax_mean) %>% as.data.frame() %>%
  cor(use = "complete.obs")
labels_row = c('CFR', 'Green Space (%)',
               'Grey Space (%)', 'Other Space (%)',
               "Low Intensity Developed Area (%)", "Medium Intensity Developed Area (%)",
               'High Intensity Developed Area (%)', 'Open Water (%)',
               'Woody Wetlands (%)', 'Emergent Herbaceous Wetlands (%)',
               'Deciduous Forest (%)', 'Evergreen Forest (%)',
               'Mixed Forest (%)', 'Shrub (%)', 'Grassland (%)',
               'Pasture (%)', 'Cultivated Crops (%)',
               'Barren Land (%)',
               'Low Intensity Developed Area In Developed Area (%)', 
               'Medium Intensity Developed Area In Developed Area (%)', 
               'High Intensity Developed Area In Developed Area (%)',
               'Population Density', 
               'Population Density In Developed Area', 'Percentage Of Male', 
               'Percentage Of Population 15-44', 'Percentage Of Population 45-64', 
               'Percentage Of Population >= 65', 'Percentage Of Black People', 
               'Percentage Of Hispanic People', 
               'Natural Logarithm Of Median Household Income', 
               'Natural Logarithm Of Median House Value', 'Percentage Of Owner-Occupied Housing', 
               'Unemployment Rate', 'Poverty Rate', 
               'Percentage Of The Adults With Less Than High School Diploma', 
               'Percentage Of Population With Poor Or Fair Health', 
               'Poor Physical Health Days', 'Poor Mental Health Days', 
               'Adult Smoking Rate', 'Population With Obesity Rate',
               'Physical Inactivity Rate', 
               'Having Access To Exercise Opportunities', 
               'Numbers Of Hospital Beds Per Unit Population', 
               'Mean Of PM2.5 Value', 'Mean Of Daily Temperature In Summer', 
               'Mean Of Daily Temperature In Winter', 
               'Mean Of Relative Humidity In Summer', 'Mean Of Relative Humidity In Winter'
)
g7 <- pheatmap(cor_matrix, cluster_rows = F, cluster_cols = F, display_numbers = T, fontsize = 8, 
               labels_row = labels_row, labels_col = "", main = "Figure S1: Correlation among Variables")
png(filename = ".\\04_Figure\\Correlation_Variable.png", width = 6000, height = 4000, res = 400, type = "cairo")
g7
dev.off();
