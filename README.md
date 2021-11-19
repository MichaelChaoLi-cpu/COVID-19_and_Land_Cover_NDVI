# Natural Land Cover Improves the COVID-19 Health Outcomes (DP05)
This is the research on the relationship between COVID-19 and Land Cover as well as monthly NDVI in the USA

## Author
Chao Li, Shunsuke Managi

## Manuscript
[Natural Land Cover Improves COVID-19 Health Outcome](07_Manuscript/Natural Land Cover Improves COVID-19 Health Outcome.pdf)  

## Data  
### Used in 01_DW_MortalityPrevalenceLandCoverCross_v1.R  
COVID19 Package: confirmed cases, deaths, population, restrictions information.  
[LC_2001.dbf](01_Raster/02_LandCoverTable/LC_2001.dbf) - [LC_2019.dbf](01_Raster/02_LandCoverTable/LC_2019.dbf): downlaoded from <https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover>. This data set is about 2001 - 2019 land cover, eight tif files, which should be extracted by geoprocessiong. The data are about percentage in the counties.  
[Unemployment.xls](02_RawData/Unemployment.xls): downlaoded from <https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/>.  
[PovertyEstimates.csv](02_RawData/PovertyEstimates.csv): downloaded from <https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/>. 
[Education.csv](02_RawData/Education.csv): downloaded from <https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/>.  
cc-est2019-alldata.csv (Not uploaded): downloaded from <https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-detail.html>. This data set is about county-level age group, sex, and race.  
(obese_what_inactive.xls)[02_RawData/obese_what_inactive.xls]: downloaded from <https://www.countyhealthrankings.org/explore-health-rankings/rankings-data-documentation>.  
(hospitalBed.csv)[02_RawData/hospitalBed.csv]: downloaded from <https://hifld-geoplatform.opendata.arcgis.com/datasets/hospitals/data>.  
(temp_seasonal_county.csv)[02_RawData/temp_seasonal_county.csv]: downloaded from <https://www.northwestknowledge.net/metdata/data/>. This data set is about temperature, and humidity.  
(county_pm25.csv)[02_RawData/county_pm25.csv]: downloaded from <https://github.com/MichaelChaoLi-cpu/On-road_Transportation_PM2.5/blob/main/Data/dataset.csv>.  
### Used in 02_DW_NDVITemperatureQuarterly_v1.R
"NDVI" this is raw data, which should be multiply by the factor (0.0001). MOD13A3 1 km monthly NDVI   
"DayTem" this is raw data, which should be multiply by the factor (0.02 K). MOD11C3 0.05 monthly  
"NightTem" this is raw data, which should be multiply by the factor (0.02 K). MOD11C3 0.05 monthly  
"NTL" this is nightlight data. VNP46A3 15 arc second  
  
### Used in 05_DW_MortalityPrevalenceQuarterlyPanel_v1.R
COVID19 Package: confirmed cases, deaths, population, restrictions information. 
[panel_mod.csv](02_RawData/panel_mod.csv): "NDVI", "DayTem", "NightTem", "NTL" from [02_DW_NDVITemperatureQuarterly_v1.R](04_Code/02_DW_NDVITemperatureQuarterly_v1.R)  
  
## Code  
[01_DW_MortalityPrevalenceLandCoverCross_v1.R](04_Code/01_DW_MortalityPrevalenceLandCoverCross_v1.R): This script is to wash the data to get the cross-sectional dataset in the analysis. The result of this script is the [dataset](00_RData/dateset.Rdata), including the variable of the county-level prevalence and mortality (capita/1000) of the COVID-19 in the U.S, due by 1st Nov. This dataset also contain the county-level land cover data (hm/capi), weather, income, population, etc.  
[02_DW_NDVITemperatureQuarterly_v1.R](04_Code/02_DW_NDVITemperatureQuarterly_v1.R): This script is to wash the data to get the monthly panel dataset of the NDVI and Temperature data in the analysis. The result of this script is the (panel_mod.csv)[02_RawData/panel_mod.csv] due by the third quarter in the 2021.
[03_AN_OLSPrevalenceMortalityCross_v1.R](04_Code/03_AN_OLSPrevalenceMortalityCross_v1): This script perform OLS on the [dataset](00_RData/dateset.Rdata) to detect the relationship between land cover and mortality as well as prevalence of COVID-19. The estimated parameters in the OLS results are fixed based on the bp tests. The residuals from the OLS are spatially clustering, according to Moran's I test. Therefore, in this scripts, the GWR models are also included. However, the local parameters from the GWR are scarcely signifcant. Obviously, the GWR is not the ideal model to solve this spatial spillover. This script output two results, [SeperatedOLS.MR.html](03_Results/SeperatedOLS.MR.html) and [SeperatedOLS.PR.html](03_Results/SeperatedOLS.PR.html), which are the OLS results of the associations of land cover with mortality and prevalence, respectively.  
[04_AN_SpatialModelPrevalenceMortalityCross_v1.R](04_Code/04_AN_SpatialModelPrevalenceMortalityCross_v1.R): This script tries to use other spatial models to trackle the spillover of COVID-19. The first one is SAR.    
[05_DW_MortalityPrevalenceQuarterlyPanel_v1.R](04_Code/05_DW_MortalityPrevalenceQuarterlyPanel_v1.R): This script make the data set, [panel_NDVI_mortality_prevalence.csv](02_RawData/panel_NDVI_mortality_prevalence.csv), which includes "stringency_index", "NDVI_perc", "tem_c", "NLT", "confirmed_per1000", and "deaths_per1000". With this **quarterly panel data set**, we could perform the panel regressions and even spatial panel regressions. "NDVI"" represents greenery. "tem_c" represents temperature. "NLT" represents nighttime light. The counties should be richer, whose nighttime light are brighter.  
[06_AN_RegressionPrevalenceMortalityNDVIPanel_v0.R](04_Code/06_AN_RegressionPrevalenceMortalityNDVIPanel_v0.R): This script mainly perform panel model, including "plm" and "splm". Currently, we use both spatial lag and spatial error in the main model. The basic data is quarterly, [panel_NDVI_mortality_prevalence.csv](02_RawData/panel_NDVI_mortality_prevalence.csv), including "tem_c", "NTL", "NDVI_perc", etc.  
[07_AF_OutputSplmImpactFunction_v1.R](04_Code/07_AF_OutputSplmImpactFunction_v1.R): this script is the function to output spml model impacts, which has been stored in the AssistantFunction repo <https://github.com/MichaelChaoLi-cpu/AssistantFuctions>.  
[08_DW_MortalityPrevalenceMonthlyPanel_v1.R](04_Code/08_DW_MortalityPrevalenceMonthlyPanel_v1.R): This script make the data set, [panel_NDVI_mortality_prevalence_monthly.csv](02_RawData/panel_NDVI_mortality_prevalence_monthly.csv), which includes "stringency_index", "NDVI_perc", "tem_c", "NLT", "confirmed_per1000", and "deaths_per1000". With this **monthly panel data set**, we could perform the panel regressions and even spatial panel regressions.   
[09_AN_PrevalenceMortalityNDVIMonthlyPanel_v0.R](04_Code/09_AN_PrevalenceMortalityNDVIMonthlyPanel_v0.R): This script mainly perform panel model, including "plm" and "splm", based on the monthly panel data set, [panel_NDVI_mortality_prevalence_monthly.csv](02_RawData/panel_NDVI_mortality_prevalence_monthly.csv). However, they are out of memory. Therefore, aborted!
[10_VI_DistributionStatistics_v0.R](04_Code/10_VI_DistributionStatistics_v0.R): This script mainly make the figure in the article.[prevalence.png](06_Figure/prevalence.png) and [mortality.png](06_Figure/mortality.png) are to show the spatial distribution of total prevalence and mortality.
  
## Workflow  
**WF.A: 01 -> 03 -> 04 -> END**  
**WF.A.01.03**: This step provides the data to perform OLS. According to the results of OLS, the residuals are spatially clustering. Though GWR is considered, the poor results of GWR make us reject it.  
**WF.A.03.04**: This step makes us try to use other spatial models.  
  
**WF.B: 02 -> 05 -> 06 -> END**  
**WF.B.02.05**: This step help us get the **quarterly panel data** set to detect whether greenery is able to prevent COVID-19.  
**WF.B.05.06**: This step performs the panel regressions including ordinary (plm) and spatial (splm) based on the **quarterly panel data**.  
  
**WF.B: 02 -> 08 -> 09 -> END**  
Out of memory! Aborted!  

## Contact Us:
- Email: Prof. Shunsuke Managi <managi@doc.kyushu-u.ac.jp>  
- Email: Chao Li <chaoli0394@gmail.com>  
  
## Term of Use:
Authors/funders retain copyright (where applicable) of code on this Github repo. 
This GitHub repo and its contents herein, including data, link to data source, and analysis code that are intended solely for reproducing the results in the manuscript "Natural Land Cover Improves the COVID-19 Health Outcomes" The analyses rely upon publicly available data from multiple sources, that are often updated without advance notice. We hereby disclaim any and all representations and warranties with respect to the site, including accuracy, fitness for use, and merchantability. By using this site, its content, information, and software you agree to assume all risks associated with your use or transfer of information and/or software. You agree to hold the authors harmless from any claims relating to the use of this site.