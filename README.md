# COVID-19 and Land Cover as well as NDVI (DP05)
This is the research on the relationship between COVID-19 and Land Cover as well as monthly NDVI in the USA

## Data  
### Used in 01_DW_MortalityPrevalenceLandCoverCross_v1.R  
COVID19 Package: confirmed cases, deaths, population, restrictions information.  
[MainLand_LC30_Area.dbf](01_Raster/02_LandCoverTable/MainLand_LC30_Area.dbf): downlaoded from <https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover>. This data set is about 2016 land cover, a tif file, which should be extracted by geoprocessiong. The data are about percentage in the counties.  
[Unemployment.xls](02_RawData/Unemployment.xls): downlaoded from <https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/>.  
[PovertyEstimates.csv](02_RawData/PovertyEstimates.csv): downloaded from <https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/>. 
[Education.csv](02_RawData/Education.csv): downloaded from <https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/>.  
cc-est2019-alldata.csv (Not uploaded): downloaded from <https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-detail.html>. This data set is about county-level age group, sex, and race.  
(obese_what_inactive.xls)[02_RawData/obese_what_inactive.xls]: downloaded from <https://www.countyhealthrankings.org/explore-health-rankings/rankings-data-documentation>.  
(hospitalBed.csv)[02_RawData/hospitalBed.csv]: downloaded from <https://hifld-geoplatform.opendata.arcgis.com/datasets/hospitals/data>.  
(temp_seasonal_county.csv)[02_RawData/temp_seasonal_county.csv]: downloaded from <https://www.northwestknowledge.net/metdata/data/>. This data set is about temperature, and humidity.  
(county_pm25.csv)[02_RawData/county_pm25.csv]: downloaded from <https://github.com/wxwx1993/PM_COVID/tree/master/Data>.  
  
## Code  
[01_DW_MortalityPrevalenceLandCoverCross_v1.R](04_Code/01_DW_MortalityPrevalenceLandCoverCross_v1.R): This script is to wash the data to get the cross-sectional dataset in the analysis. The result of this script is the [dataset](00_RData/dateset.Rdata), including the variable of the county-level prevalence and mortality (capita/1000) of the COVID-19 in the U.S, due by 1st Nov. This dataset also contain the county-level land cover data (hm/capi), weather, income, population, etc.  
[02_DW_NDVITemperatureQuarterly_v1.R](04_Code/02_DW_NDVITemperatureQuarterly_v1.R): This script is to wash the data to get the monthly panel dataset of the NDVI and Temperature data in the analysis. The result of this script is the (panel_mod.csv)[02_RawData/panel_mod.csv] due by the third quarter in the 2021.
[03_AN_OLSPrevalenceMortalityCross_v1.R](04_Code/03_AN_OLSPrevalenceMortalityCross_v1): This script perform OLS on the [dataset](00_RData/dateset.Rdata) to detect the relationship between land cover and mortality as well as prevalence of COVID-19. The estimated parameters in the OLS results are fixed based on the bp tests. The residuals from the OLS are spatially clustering, according to Moran's I test. Therefore, in this scripts, the GWR models are also included. However, the local parameters from the GWR are scarcely signifcant. Obviously, the GWR is not the ideal model to solve this spatial spillover. This script output two results, [SeperatedOLS.MR.html](03_Results/SeperatedOLS.MR.html) and [SeperatedOLS.PR.html](03_Results/SeperatedOLS.PR.html), which are the OLS results of the associations of land cover with mortality and prevalence, respectively.  
[04_AN_SpatialModelPrevalenceMortalityCross_v0.R](04_Code/04_AN_SpatialModelPrevalenceMortalityCross_v0.R): This script tries to use other spatial models to trackle the spillover of COVID-19. The first one is SAR.    
[05_DW_MortalityPrevalenceQuarterlyPanel_v1.R](04_Code/05_DW_MortalityPrevalenceQuarterlyPanel_v1.R): This script make the data set, [panel_NDVI_mortality_prevalence.csv](02_RawData/panel_NDVI_mortality_prevalence.csv), which includes "stringency_index", "NDVI_perc", "tem_c", "NLT", "confirmed_per1000", and "deaths_per1000". With this data set, we could perform the panel regressions and even spatial panel regressions. "NDVI"" represents greenery. "tem_c" represents temperature. "NLT" represents nighttime light. The counties should be richer, whose nighttime light are brighter.  
  
## Workflow  
**WF.A: 01 -> 03 -> 04 -> END**  
**WF.A.01.03**: This step provides the data to perform OLS. According to the results of OLS, the residuals are spatially clustering. Though GWR is considered, the poor results of GWR make us reject it.  
**WF.A.03.04**: This step makes us try to use other spatial models.  
  
**WF.B: 02 -> 05 -> END**  
**WF.B.02.05**: This step help us get the panel data set to detect whether greenery is able to prevent COVID-19.  

