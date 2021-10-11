library(grid)
library(ggplot2)
#Plot
setwd(
  "C:\\Users\\li.chao.987@s.kyushu-u.ac.jp\\OneDrive - Kyushu University\\05_Article\\04_Figure\\"
)

plot1 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = Developed_High_Intensity_perc, y = death_rate)) +
  geom_point() +
  geom_smooth()

plot2 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = Developed_Medium_Intensity_perc, y = death_rate)) +
  geom_point() +
  geom_smooth()

plot3 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = Developed_Low_Intensity_perc, y = death_rate)) +
  geom_point() +
  geom_smooth() 

plot4 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = Developed_Open_Space_perc, y = death_rate)) +
  geom_point() +
  geom_smooth()

grid.newpage()
pushViewport(viewport(layout = grid.layout(2,2)))
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}

print(plot1, vp = vplayout(1,1)) 
print(plot2, vp = vplayout(1,2)) 
print(plot3, vp = vplayout(2,1))
print(plot4, vp = vplayout(2,2))

png(filename = "corr_DR_Deve.png", width = 1600, height = 1200, pointsize = 40)
LC_POP_CAS_DE %>%
  select(
    death_rate, Developed_Open_Space_perc, Developed_Low_Intensity_perc, 
    Developed_Medium_Intensity_perc, Developed_High_Intensity_perc
  ) %>%
  pairs()
dev.off();

png(filename = "corr_DR_Blue.png", width = 1600, height = 1200, pointsize = 40)
LC_POP_CAS_DE %>%
  select(
    death_rate, Open_Water_perc, Woody_Wetlands_perc, 
    Emergent_Herbaceous_Wetlands_perc
  ) %>%
  pairs()
dev.off();

png(filename = "corr_DR_Green.png", width = 1600, height = 1200, pointsize = 40)
LC_POP_CAS_DE %>%
  select(
    death_rate, Deciduous_Forest_perc, Evergreen_Forest_perc, 
    Mixed_Forest_perc, Shrub_perc, Grassland_perc, Pasture_perc
  ) %>%
  pairs()
dev.off();

png(filename = "corr_DR_Human.png", width = 1600, height = 1200, pointsize = 40)
LC_POP_CAS_DE %>%
  select(
    death_rate, Barren_Land_perc, Cultivated_Crops_perc
  ) %>%
  pairs()
dev.off();



#plot infect Rate
png(filename = "corr_IR_Deve.png", width = 1600, height = 1200, pointsize = 40)
LC_POP_CAS_DE %>%
  select(
    infect_rate, Developed_Open_Space_perc, Developed_Low_Intensity_perc, 
    Developed_Medium_Intensity_perc, Developed_High_Intensity_perc
  ) %>%
  pairs()
dev.off();

png(filename = "corr_IR_Blue.png", width = 1600, height = 1200, pointsize = 40)
LC_POP_CAS_DE %>%
  select(
    infect_rate, Open_Water_perc, Woody_Wetlands_perc, 
    Emergent_Herbaceous_Wetlands_perc
  ) %>%
  pairs()
dev.off();

png(filename = "corr_IR_Green.png", width = 1600, height = 1200, pointsize = 40)
LC_POP_CAS_DE %>%
  select(
    infect_rate, Deciduous_Forest_perc, Evergreen_Forest_perc, 
    Mixed_Forest_perc, Shrub_perc, Grassland_perc, Pasture_perc
  ) %>%
  pairs()
dev.off();

png(filename = "corr_IR_Human.png", width = 1600, height = 1200, pointsize = 40)
LC_POP_CAS_DE %>%
  select(
    infect_rate, Barren_Land_perc, Cultivated_Crops_perc
  ) %>%
  pairs()
dev.off();

png(filename = "DR_Tensity_Developed.png", width = 800, height = 600, pointsize = 40)
plot1 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = Open_Space_inDeveloped_perc, y = death_rate)) +
  geom_point() +
  geom_smooth()

plot2 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = Low_Intensity_inDeveloped_perc, y = death_rate)) +
  geom_point() +
  geom_smooth()

plot3 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = Medium_Intensity_inDeveloped_perc, y = death_rate)) +
  geom_point() +
  geom_smooth() 

plot4 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = High_Intensity_inDeveloped_perc, y = death_rate)) +
  geom_point() +
  geom_smooth()

grid.newpage()
pushViewport(viewport(layout = grid.layout(2,2)))
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}

print(plot1, vp = vplayout(1,1)) 
print(plot2, vp = vplayout(1,2)) 
print(plot3, vp = vplayout(2,1))
print(plot4, vp = vplayout(2,2))
dev.off();

png(filename = "IR_Density_Developed.png", width = 800, height = 600, pointsize = 40)
plot1 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = Open_Space_inDeveloped_perc, y = infect_rate)) +
  geom_point() +
  geom_smooth()

plot2 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = Low_Intensity_inDeveloped_perc, y = infect_rate)) +
  geom_point() +
  geom_smooth()

plot3 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = Medium_Intensity_inDeveloped_perc, y = infect_rate)) +
  geom_point() +
  geom_smooth() 

plot4 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = High_Intensity_inDeveloped_perc, y = infect_rate)) +
  geom_point() +
  geom_smooth()

grid.newpage()
pushViewport(viewport(layout = grid.layout(2,2)))
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}

print(plot1, vp = vplayout(1,1)) 
print(plot2, vp = vplayout(1,2)) 
print(plot3, vp = vplayout(2,1))
print(plot4, vp = vplayout(2,2))
dev.off();

library("pheatmap")
library("ellipse")

cor_matrix <- LC_POP_CAS_DE %>%
  select(
    death_rate, infect_rate, Open_Water_perc, Perenial_Ice_perc, 
    Developed_Open_Space_perc, Developed_Low_Intensity_perc, 
    Developed_Medium_Intensity_perc, Developed_High_Intensity_perc, 
    Barren_Land_perc, Deciduous_Forest_perc, Evergreen_Forest_perc, 
    Mixed_Forest_perc, Shrub_perc, Grassland_perc, Pasture_perc, 
    Cultivated_Crops_perc, Woody_Wetlands_perc, 
    Emergent_Herbaceous_Wetlands_perc, pop_density, 
    pop_density_developed
  ) %>%
  cor(use = "complete.obs")
png(filename = "Correlation_Variable.png", width = 4000, height = 3000, res = 400, type = "cairo")
pheatmap(cor_matrix, cluster_rows = F, cluster_cols = F, display_numbers = T)
dev.off();

cor_matrix <- LC_POP_CAS_DE %>%
  select(
    death_rate, infect_rate, Open_Space_inDeveloped_perc : High_Intensity_inDeveloped_perc
  ) %>%
  cor(use = "complete.obs")
png(filename = "Correlation_Variable_Deve.png", width = 4000, height = 3000, res = 400, type = "cairo")
pheatmap(cor_matrix, cluster_rows = F, cluster_cols = F, display_numbers = T)
dev.off();

png(filename = "PopDensity_IR_DR.png", width = 800, height = 600, pointsize = 40)
plot1 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = pop_density, y = infect_rate)) +
  geom_point() +
  geom_smooth()

plot2 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = pop_density_developed, y = infect_rate)) +
  geom_point() +
  geom_smooth()

plot3 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = pop_density, y = death_rate)) +
  geom_point() +
  geom_smooth() 

plot4 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = pop_density_developed, y = death_rate)) +
  geom_point() +
  geom_smooth()

grid.newpage()
pushViewport(viewport(layout = grid.layout(2,2)))
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}

print(plot1, vp = vplayout(1,1)) 
print(plot2, vp = vplayout(1,2)) 
print(plot3, vp = vplayout(2,1))
print(plot4, vp = vplayout(2,2))
dev.off();

png(filename = "Water_DR.png", width = 800, height = 600, pointsize = 40)
plot1 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = Open_Water_perc, y = death_rate)) +
  geom_point() +
  geom_smooth()

plot2 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = Woody_Wetlands_perc, y = death_rate)) +
  geom_point() +
  geom_smooth()

plot3 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = Emergent_Herbaceous_Wetlands_perc, y = death_rate)) +
  geom_point() +
  geom_smooth() 

grid.newpage()
pushViewport(viewport(layout = grid.layout(2,2)))
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}

print(plot1, vp = vplayout(1,1)) 
print(plot2, vp = vplayout(1,2)) 
print(plot3, vp = vplayout(2,1))
dev.off();

png(filename = "Forest_DR.png", width = 800, height = 600, pointsize = 40)
plot1 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = Deciduous_Forest_perc, y = death_rate)) +
  geom_point() +
  geom_smooth()

plot2 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = Evergreen_Forest_perc, y = death_rate)) +
  geom_point() +
  geom_smooth()

plot3 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = Mixed_Forest_perc, y = death_rate)) +
  geom_point() +
  geom_smooth() 

plot4 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = Shrub_perc, y = death_rate)) +
  geom_point() +
  geom_smooth()

grid.newpage()
pushViewport(viewport(layout = grid.layout(2,2)))
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}

print(plot1, vp = vplayout(1,1)) 
print(plot2, vp = vplayout(1,2)) 
print(plot3, vp = vplayout(2,1))
print(plot4, vp = vplayout(2,2))
dev.off();

png(filename = "Grassland_DR.png", width = 800, height = 600, pointsize = 40)
plot1 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = Grassland_perc, y = death_rate)) +
  geom_point() +
  geom_smooth()

plot2 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = Pasture_perc, y = death_rate)) +
  geom_point() +
  geom_smooth()

plot3 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = Cultivated_Crops_perc, y = death_rate)) +
  geom_point() +
  geom_smooth() 

plot4 <- ggplot(data = LC_POP_CAS_DE, mapping = aes(x = Barren_Land_perc, y = death_rate)) +
  geom_point() +
  geom_smooth()

grid.newpage()
pushViewport(viewport(layout = grid.layout(2,2)))
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}

print(plot1, vp = vplayout(1,1)) 
print(plot2, vp = vplayout(1,2)) 
print(plot3, vp = vplayout(2,1))
print(plot4, vp = vplayout(2,2))
dev.off();

cor_matrix <- LC_POP_CAS_DE %>%
  select(
    death_rate,
    Developed_Open_Space_perc, Developed_Low_Intensity_perc, 
    Developed_Medium_Intensity_perc, Developed_High_Intensity_perc, 
    Open_Water_perc, Woody_Wetlands_perc, Emergent_Herbaceous_Wetlands_perc,
    Deciduous_Forest_perc, Evergreen_Forest_perc, Mixed_Forest_perc, 
    Shrub_perc, Grassland_perc, Pasture_perc, Cultivated_Crops_perc,
    Barren_Land_perc,
    pop_density, 
    male, age15_44, age45_64, age65_99, black_rate, hispanic_rate,
    log_Median_Household_Income_2018, log_median_house_value, mean_house_owner_perc,
    Unemployment_rate_2019, poverty_rate, less_than_high_school,      
    poor_health_rate_2019,  smoker_rate_2019, obesity_rate_2019,
    physical_inactivity_2019, exercise_opportunities_rate_2019, 
    hospital_beds,
    pm25_mean
  ) %>%
  cor(use = "complete.obs")
png(filename = "04_Figure\\Correlation_Variable.png", width = 4000, height = 3000, res = 400, type = "cairo")
pheatmap(cor_matrix, cluster_rows = F, cluster_cols = F, display_numbers = T, fontsize = 8)
dev.off();
