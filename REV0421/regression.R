library(lmtest)
library(olsrr)
library(sandwich)
library(spdep)
library(spgwr)
library(GWmodel)
library(stargazer)

#---------------variables selection--------
setwd("C:\\Users\\li.chao.987@s.kyushu-u.ac.jp\\OneDrive - Kyushu University\\05_Article\\")
capi.form = CFR ~ 
  Open_Water_capi + Perenial_Ice_capi +                  
  Developed_Open_Space_capi + Developed_Low_Intensity_capi + Developed_Medium_Intensity_capi +   
  Developed_High_Intensity_capi + Barren_Land_capi + Deciduous_Forest_capi +              
  Evergreen_Forest_capi + Mixed_Forest_capi + Shrub_capi +                         
  Grassland_capi +  Pasture_capi +  Cultivated_Crops_capi +              
  Woody_Wetlands_capi + Emergent_Herbaceous_Wetlands_capi + 
  incidence_proportion + gatherings_restrictions + transport_closing +
  stay_home_restrictions + internal_movement_restrictions + 
  international_movement_restrictions + 
  age15_44 + age45_64 +
  age65_99 + black_rate + hispanic_rate + male +
  Unemployment_rate_2019 + log_Median_Household_Income_2018 +
  poverty_rate + less_than_high_school +  
  poor_health_rate_2019 + poor_physical_days_2019 + poor_mental_days_2019 +
  smoker_rate_2019 + obesity_rate_2019 + physical_inactivity_2019 +
  exercise_opportunities_rate_2019 +
  hospital_beds + summer_tmmx_mean + winter_tmmx_mean + summer_rmax_mean +
  winter_rmax_mean + pm25_mean

model1 <- lm(capi.form ,
            data = dataset)
summary(model1)
lmtest::bptest(model1)
ols_step_backward_p(model1)
ols_coll_diag(model1)
rse.1.1 <- vcovHAC(model1)
rse.2.1 <- sqrt(diag(rse.1.1))
coeftest(model1, vcov = rse.1.1)

#---------------variables selection--------

capi.form.1 = CFR ~   Open_Water_capi  +                  
  Developed_Open_Space_capi + Developed_Low_Intensity_capi + Developed_Medium_Intensity_capi +   
  Developed_High_Intensity_capi +  Deciduous_Forest_capi +              
  Mixed_Forest_capi +                         
  Grassland_capi +  Cultivated_Crops_capi + 
  Emergent_Herbaceous_Wetlands_capi + 
  gatherings_restrictions + transport_closing +
  stay_home_restrictions + 
  age45_64 +
  age65_99 + black_rate + hispanic_rate + male + 
  Unemployment_rate_2019 + log_Median_Household_Income_2018 +
  poverty_rate + less_than_high_school + 
  incidence_proportion +
  smoker_rate_2019 + obesity_rate_2019 + physical_inactivity_2019 +
  exercise_opportunities_rate_2019  +
  summer_tmmx_mean + winter_tmmx_mean + summer_rmax_mean +
  winter_rmax_mean

model <- lm(capi.form.1,
            data = dataset)
summary(model)
ols_coll_diag(model)
ols_step_backward_p(model)
bptest(model)
rse.1 <- vcovHAC(model)
rse.2 <- sqrt(diag(rse.1))
coeftest(model, vcov = rse.1)




stargazer(model$model,
          title = "Table XXX: Data Statistic Summary",  type = "text", 
          no.space = T,
          covariate.labels = c(
            'CFR (%)', 'Open Water (hm2/cap)',
             'Open Space Developed Area (hm2/cap)',                   
            "Low Intensity Developed Area (hm2/cap)",
            "Medium Intensity Developed Area (hm2/cap)",
            'High Intensity Developed Area (hm2/cap)',
            'Deciduous Forest (hm2/cap)',
            'Mixed Forest (hm2/cap)', 'Grassland (hm2/cap)',
            'Cultivated Crops (hm2/cap)',
            'Emergent Herbaceous Wetlands (hm2/cap)',
            'Gathering Restrictions (days)',
            'Transport Closing (days)', 'Staying Home (days)',
            'Population 45-64 (%)', 
            'Population >= 65 (%)', 'Black People (%)', 
            'Hispanic People (%)',  'Male (%)',
            'Umemployment Rate', 'Median Household Income (logatithm)',
            'Poverty Rate (%)', 'Adults Without High School Diploma (%)',
            'Incidence Rate (%)',  'Adult Smoking Rate (%)', 'Obesity Rate (%)',
            'Physical Inactivity Rate (%)', 
            'Having Access To Exercise Opportunities (%)', 'Average Temperature In Summer', 
            'Average Temperature In Winter', 
            'Average Relative Humidity In Summer', 'Average Relative Humidity In Winter'
          ),
          iqr = F, out = "05_RegressionResult\\summary_table.html"
) 


setwd("C:\\Users\\li.chao.987@s.kyushu-u.ac.jp\\OneDrive - Kyushu University\\05_Article\\01_Raster\\cb_2017_us_county_20m\\")
tes <- dataset %>%
  dplyr::select(CFR,  Open_Water_capi,                  
                  Developed_Open_Space_capi, Developed_Low_Intensity_capi, Developed_Medium_Intensity_capi,   
                  Developed_High_Intensity_capi,  Deciduous_Forest_capi,              
                  Mixed_Forest_capi,                         
                  Grassland_capi,  Cultivated_Crops_capi,              
                  Emergent_Herbaceous_Wetlands_capi, 
                  gatherings_restrictions, transport_closing,
                  stay_home_restrictions, 
                  age45_64,
                  age65_99, black_rate, hispanic_rate, male, 
                  Unemployment_rate_2019, log_Median_Household_Income_2018,
                  poverty_rate, less_than_high_school, 
                  incidence_proportion,
                  smoker_rate_2019, obesity_rate_2019, physical_inactivity_2019,
                  exercise_opportunities_rate_2019 ,
                  summer_tmmx_mean, winter_tmmx_mean, summer_rmax_mean,
                  winter_rmax_mean
  ) %>% na.omit()
us_shape <- readOGR(dsn = ".", layer = "cb_2017_us_county_20m84")
us_shape <- geo_join(us_shape, tes, 'CountyFIPS', 'key_numeric', how = 'inner')
plot(us_shape)
rm(tes)
model.moran <- lm(capi.form.1,
            data = us_shape)
summary(model.moran)
nb <- poly2nb(us_shape, queen = T)
lw <- nb2listw(nb, style="W",zero.policy=T) 
lm.morantest(model.moran, lw, zero.policy=T)

GWRbandwidth <- gwr.sel(capi.form.1, data = us_shape)
gwr.model = gwr(capi.form.1, data = us_shape,
                   bandwidth  = GWRbandwidth) 
ss.total <- sum((us_shape@data$CFR - mean(us_shape@data$CFR))^2)
ss.res <- sum((us_shape@data$CFR - gwr.model$SDF@data$pred)^2)
r2.ols <- 1 - ss.res/ss.total

save.image("03_RProject\\REV0421\\result.Rdata")

setwd("C:\\Users\\li.chao.987@s.kyushu-u.ac.jp\\OneDrive - Kyushu University\\05_Article\\")
stargazer(model,
          title = "Table XXX: Data Statistic Summary",  type = "text", 
          se = list(rse.2),
          no.space = T,
          star.cutoffs = c( .05, .01, .001),
          covariate.labels = c(
            'Open Water (hm2/cap)',
            'Open Space Developed Area (hm2/cap)',                   
            "Low Intensity Developed Area (hm2/cap)",
            "Medium Intensity Developed Area (hm2/cap)",
            'High Intensity Developed Area (hm2/cap)',
            'Deciduous Forest (hm2/cap)',
            'Mixed Forest (hm2/cap)', 'Grassland (hm2/cap)',
            'Cultivated Crops (hm2/cap)',
            'Emergent Herbaceous Wetlands (hm2/cap)',
            'Gathering Restrictions (days)',
            'Transport Closing (days)', 'Staying Home (days)',
            'Population 45-64 (%)', 
            'Population >= 65 (%)', 'Black People (%)', 
            'Hispanic People (%)',  'Male (%)',
            'Umemployment Rate', 'Median Household Income (logatithm)',
            'Poverty Rate (%)', 'Adults Without High School Diploma (%)',
            'Incidence Rate (%)',  'Adult Smoking Rate (%)', 'Obesity Rate (%)',
            'Physical Inactivity Rate (%)', 
            'Having Access To Exercise Opportunities (%)', 'Average Temperature In Summer', 
            'Average Temperature In Winter', 
            'Average Relative Humidity In Summer', 'Average Relative Humidity In Winter'
          ),
          iqr = F, out = "05_RegressionResult\\Regression.html"
) 

stargazer(model1,
          title = "Table XXX: OLS Result with All Variables",  type = "text", 
          se = list(rse.2.1),
          no.space = T,
          star.cutoffs = c( .05, .01, .001),
          iqr = F, out = "06_RegressionResult\\Regression_full.html"
) 


# CFRR table output Model OLS
total_CFR <- mean(model$model$CFR)
COEF <- round(coef(model), 3)
CFRR <- round(coef(model) / total_CFR * 100, 2)
CI_up <- round((coef(model) + rse.2) / total_CFR  * 100, 3)
CI_low <- round((coef(model) - rse.2) / total_CFR  * 100, 3)
CI <- cbind(CI_low, CI_up)
P <- round(coef(summary(model))[,4], 3)
vari = variable.names(model)
colnames(CI) <- c("Lower", "Higher")
table1 <- as.data.frame(cbind(vari, COEF, CFRR, CI, P))
table1$a <- "("; table1$b <- "-"; table1$c <- ")"
table1 <- table1[,c("vari","COEF","CFRR","a","Lower","b","Higher","c", "P")]
table1 <- unite(table1, "95%CI", c(a, Lower, b, Higher, c), sep = "", remove=T)
table1[,1] <- gsub("\\(", " (", table1[,1])
table1 <- table1 %>% as.tibble()
table1 %>% write.csv(file = "06_RegressionResult\\CFRR_model.csv")
# CFRR table output Model OLS

mv_s <- coef(summary(model))[2,1]/coef(summary(model))[21,1]
model$model$mv_ow <- exp(model$model$log_Median_Household_Income_2018) * mv_s
summary(model$model$mv_ow)

mv_s <- coef(summary(model))[7,1]/coef(summary(model))[21,1]
model$model$mv_df <- exp(model$model$log_Median_Household_Income_2018) * mv_s
summary(model$model$mv_df)
