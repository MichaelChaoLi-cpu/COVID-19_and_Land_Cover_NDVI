# COVID-19 and Land Cover as well as NDVI
This is the research on the relationship between COVID-19 and Land Cover as well as monthly NDVI in the USA

## Data  
### Used in 01_DW_MortalityPrevalenceLandCoverCross_v1.R  
COVID19 Package: confirmed cases, deaths, population, restrictions information.  
[MainLand_LC30_Area.dbf](01_Raster/02_LandCoverTable/MainLand_LC30_Area.dbf): downlaoded from <https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover>. This data set is about 2016 land cover, a tif file, which should be extracted by geoprocessiong.  
[Unemployment.xls](02_RawData/Unemployment.xls): downlaoded from <https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/>.  
[PovertyEstimates.csv](02_RawData/PovertyEstimates.csv): downloaded from <https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/>. 
[Education.csv](02_RawData/Education.csv): downloaded from <https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/>.  
cc-est2019-alldata.csv (Not uploaded): downloaded from <https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-detail.html>. This data set is about county-level age group, sex, and race.  
(obese_what_inactive.xls)['02_RawData/obese_what_inactive.xls']: downloaded from <https://www.countyhealthrankings.org/explore-health-rankings/rankings-data-documentation>.  
(hospitalBed.csv)[02_RawData/hospitalBed.csv]: downloaded from <https://hifld-geoplatform.opendata.arcgis.com/datasets/hospitals/data>.  
(temp_seasonal_county.csv)[02_RawData/temp_seasonal_county.csv]: downloaded from <https://www.northwestknowledge.net/metdata/data/>. This data set is about temperature, and humidity.  
(county_pm25.csv)[02_RawData/county_pm25.csv]: downloaded from <https://github.com/wxwx1993/PM_COVID/tree/master/Data>.  
  
## Code  
[01_DW_MortalityPrevalenceLandCoverCross_v1.R](04_Code/01_DW_MortalityPrevalenceLandCoverCross_v1.R): This script is to wash the data to get the cross-sectional dataset in the analysis. The result of this script is the (dataset)[00RData/dateset.Rdata], including the variable of the county-level prevalence and mortality (capita/1000) of the COVID-19 in the U.S, due by 1st Nov. This dataset also contain the county-level land cover data (hm/capi), weather, income, population, etc.  
  
## Workflow  
WF.A: 01 -> END  
