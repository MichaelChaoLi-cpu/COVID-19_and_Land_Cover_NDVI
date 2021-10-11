library(ggpubr)
library(gtable)
plot1 <- ggscatter(model$model, x = "Open_Water_capi", y = "CFR", size = 1, cor.coef.size = 3,
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Open Water", ylab = "COVID-19 CFR")

plot3 <- ggscatter(model$model, x = "Developed_Open_Space_capi", y = "CFR",size = 1, cor.coef.size = 3,
                   add = "reg.line", conf.int = TRUE,
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "Open Space\nDeveloped Area", ylab = "COVID-19 CFR")
plot4 <- ggscatter(model$model, x = "Developed_Low_Intensity_capi", y = "CFR",size = 1, cor.coef.size = 3,
                   add = "reg.line", conf.int = TRUE,
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "Low Intensity\nDeveloped Area", ylab = "COVID-19 CFR")
plot5 <- ggscatter(model$model, x = "Developed_Medium_Intensity_capi", y = "CFR",size = 1, cor.coef.size = 3,
                   add = "reg.line", conf.int = TRUE,
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "Medium Intensity\nDeveloped Area", ylab = "COVID-19 CFR")
plot6 <- ggscatter(model$model, x = "Developed_Medium_Intensity_capi", y = "CFR",size = 1, cor.coef.size = 3,
                   add = "reg.line", conf.int = TRUE,
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "High Intensity\nDeveloped Area", ylab = "COVID-19 CFR")
plot8 <- ggscatter(model$model, x = "Deciduous_Forest_capi", y = "CFR",size = 1, cor.coef.size = 3,
                   add = "reg.line", conf.int = TRUE,
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "Deciduous Forest", ylab = "COVID-19 CFR")
plot10 <- ggscatter(model$model, x = "Mixed_Forest_capi", y = "CFR",size = 1, cor.coef.size = 3,
                    add = "reg.line", conf.int = TRUE,
                    cor.coef = TRUE, cor.method = "pearson",
                    xlab = "Mixed Forest", ylab = "COVID-19 CFR")
plot11 <- ggscatter(model$model, x = "Grassland_capi", y = "CFR",size = 1, cor.coef.size = 3,
                   add = "reg.line", conf.int = TRUE,
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "Grassland", ylab = "COVID-19 CFR")
plot12 <- ggscatter(model$model, x = "Cultivated_Crops_capi", y = "CFR",size = 1, cor.coef.size = 3,
                   add = "reg.line", conf.int = TRUE,
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "Cultivated Crops", ylab = "COVID-19 CFR")
plot13 <- ggscatter(model$model, x = "Emergent_Herbaceous_Wetlands_capi", y = "CFR",size = 1, cor.coef.size = 3,
                   add = "reg.line", conf.int = TRUE,
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "Emergent Herbaceous\nWetlands", ylab = "COVID-19 CFR")
plot14 <- ggscatter(model$model, x = "gatherings_restrictions", y = "CFR",size = 1, cor.coef.size = 3,
                    add = "reg.line", conf.int = TRUE,
                    cor.coef = TRUE, cor.method = "pearson",
                    xlab = "Gathering Restrictions", ylab = "COVID-19 CFR")
plot15 <- ggscatter(model$model, x = "transport_closing", y = "CFR",size = 1, cor.coef.size = 3,
                    add = "reg.line", conf.int = TRUE,
                    cor.coef = TRUE, cor.method = "pearson",
                    xlab = "Transport Closing", ylab = "COVID-19 CFR")
plot16 <- ggscatter(model$model, x = "stay_home_restrictions", y = "CFR",size = 1, cor.coef.size = 3,
                    add = "reg.line", conf.int = TRUE,
                    cor.coef = TRUE, cor.method = "pearson",
                    xlab = "Staying Home", ylab = "COVID-19 CFR")
plot17 <- ggscatter(model$model, x = "age45_64", y = "CFR",size = 1, cor.coef.size = 3,
                    add = "reg.line", conf.int = TRUE,
                    cor.coef = TRUE, cor.method = "pearson",
                    xlab = "Population 45-64", ylab = "COVID-19 CFR")
plot18 <- ggscatter(model$model, x = "age65_99", y = "CFR",size = 1, cor.coef.size = 3,
                    add = "reg.line", conf.int = TRUE,
                    cor.coef = TRUE, cor.method = "pearson",
                    xlab = "Population >= 65", ylab = "COVID-19 CFR")
plot19 <- ggscatter(model$model, x = "black_rate", y = "CFR",size = 1, cor.coef.size = 3,
                    add = "reg.line", conf.int = TRUE,
                    cor.coef = TRUE, cor.method = "pearson",
                    xlab = "Black People", ylab = "COVID-19 CFR")
plot20 <- ggscatter(model$model, x = "hispanic_rate", y = "CFR",size = 1, cor.coef.size = 3,
                    add = "reg.line", conf.int = TRUE,
                    cor.coef = TRUE, cor.method = "pearson",
                    xlab = "Hispanic People", ylab = "COVID-19 CFR")
plot21 <- ggscatter(model$model, x = "Unemployment_rate_2019", y = "CFR",size = 1, cor.coef.size = 3,
                    add = "reg.line", conf.int = TRUE,
                    cor.coef = TRUE, cor.method = "pearson",
                    xlab = "Umemployment Rate", ylab = "COVID-19 CFR")
plot22 <- ggscatter(model$model, x = "log_Median_Household_Income_2018", y = "CFR",size = 1, cor.coef.size = 3,
                    add = "reg.line", conf.int = TRUE,
                    cor.coef = TRUE, cor.method = "pearson",
                    xlab = "Median Household Income", ylab = "COVID-19 CFR")
plot23 <- ggscatter(model$model, x = "poverty_rate", y = "CFR",size = 1, cor.coef.size = 3,
                    add = "reg.line", conf.int = TRUE,
                    cor.coef = TRUE, cor.method = "pearson",
                    xlab = "Poverty Rate", ylab = "COVID-19 CFR")
plot24 <- ggscatter(model$model, x = "less_than_high_school", y = "CFR",size = 1, cor.coef.size = 3,
                    add = "reg.line", conf.int = TRUE,
                    cor.coef = TRUE, cor.method = "pearson",
                    xlab = "Adults Without High\nSchool Diploma", ylab = "COVID-19 CFR")
plot25 <- ggscatter(model$model, x = "incidence_proportion", y = "CFR", size = 1, cor.coef.size = 3,
                   add = "reg.line", conf.int = TRUE,
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "Incidence Rate", ylab = "COVID-19 CFR")
plot26 <- ggscatter(model$model, x = "smoker_rate_2019", y = "CFR",size = 1, cor.coef.size = 3,
                   add = "reg.line", conf.int = TRUE,
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "Adult Smoking Rate", ylab = "COVID-19 CFR")
plot27 <- ggscatter(model$model, x = "obesity_rate_2019", y = "CFR",size = 1, cor.coef.size = 3,
                   add = "reg.line", conf.int = TRUE,
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "Obesity Rate", ylab = "COVID-19 CFR")


####################



library(gtable)
library(gridExtra)
library(grid)
setwd("C:\\Users\\li.chao.987@s.kyushu-u.ac.jp\\OneDrive - Kyushu University\\05_Article\\")
jpeg(file="04_Figure\\geo\\cor_line1.jpeg", width = 210, height = 297, units = "mm", quality = 200, res = 200)
grid.arrange(plot1, plot3, plot4,
             plot5, plot6, plot8,
             plot10, plot11, plot12,
             plot13, plot14,
             plot15, plot16, plot17, plot18,
             plot19, plot20, plot21, plot22,
             plot23, plot24, plot25, plot26,
             plot27,
             nrow = 6)
dev.off()


# panel 2
plot1 <- ggscatter(model$model, x = "incidence_proportion", y = "CFR", size = 1, cor.coef.size = 3,
                   add = "reg.line", conf.int = TRUE,
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "Incidence Rate", ylab = "COVID-19 CFR")
plot2 <- ggscatter(model$model, x = "smoker_rate_2019", y = "CFR",size = 1, cor.coef.size = 3,
                   add = "reg.line", conf.int = TRUE,
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "Adult Smoking Rate", ylab = "COVID-19 CFR")
plot3 <- ggscatter(model$model, x = "obesity_rate_2019", y = "CFR",size = 1, cor.coef.size = 3,
                   add = "reg.line", conf.int = TRUE,
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "Obesity Rate", ylab = "COVID-19 CFR")
plot4 <- ggscatter(model$model, x = "physical_inactivity_2019", y = "CFR",size = 1, cor.coef.size = 3,
                   add = "reg.line", conf.int = TRUE,
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "Physical Inactivity\nRate", ylab = "COVID-19 CFR")
plot5 <- ggscatter(model$model, x = "exercise_opportunities_rate_2019", y = "CFR",size = 1, cor.coef.size = 3,
                   add = "reg.line", conf.int = TRUE,
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "Having Access To\nExercise\nOpportunities", ylab = "COVID-19 CFR")
plot6 <- ggscatter(model$model, x = "summer_tmmx_mean", y = "CFR",size = 1, cor.coef.size = 3,
                   add = "reg.line", conf.int = TRUE,
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "Average\nTemperature\nIn Summer", ylab = "COVID-19 CFR")
plot7 <- ggscatter(model$model, x = "winter_tmmx_mean", y = "CFR",size = 1, cor.coef.size = 3,
                   add = "reg.line", conf.int = TRUE,
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "Average\nTemperature\nIn Winter", ylab = "COVID-19 CFR")
plot8 <- ggscatter(model$model, x = "summer_rmax_mean", y = "CFR",size = 1, cor.coef.size = 3,
                   add = "reg.line", conf.int = TRUE,
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "Average\nRelative Humidity\nIn Summer", ylab = "COVID-19 CFR")
plot9 <- ggscatter(model$model, x = "winter_rmax_mean", y = "CFR",size = 1, cor.coef.size = 3,
                   add = "reg.line", conf.int = TRUE,
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "Average\nRelative Humidity\nIn Winter", ylab = "COVID-19 CFR")

jpeg(file="04_Figure\\geo\\cor_line2.jpeg", width = 210, height = 100, units = "mm", quality = 200, res = 200)
grid.arrange( plot4,
             plot5, plot6, plot7, plot8,
             plot9, 
             nrow = 2, ncol = 4)
dev.off()

