# Management Procedure  

## 1 Rules of commits to GitHub
We update the working projects on specific time. There are three times per day, normally 12:00, 15:00, 17:30. Every commit should follow this:  
A: aim; revision; results. Here A(12:00), B(15:00), C(17:30) is the time. If need temporally additional commit, the first character should be used "T", then ".time:"   
A.12:  B.15:  C.18:  
  
## 2 Requirements of Code File  
These management rules are only controlling the research project, not including package developing project or others.  
  
### 2.1 Naming the Coding Scripts  
Temporarily, we normally consider three types of scripts are used in the analysis project: data washing (DW), analysis (AN), visualization (VI). All Rscripts should be ordered from 01. Version should add at the ending of the name, form v1. Working version is v0. The aim of the code should be including in the name with five words using camel methods. All the element should be connected by "_".  
e.g.: 01_DW_MortalityPrevalanceLandCoverCross_V1.R  
      02_DW_MortalityPrevelanceNDVIQuartarPanel_V0.R  
  
After the Rscripts cteated, the file should be mentioned in the readme with the necessary information and link.  
  
### 2.2 Code File for DW  
At the beginning of code, anything to output including datasets, rasters, and so forth, must be disclosed.  
For example: \# output: A.RData; B.csv; C.tif (they need to be seperated by the semicolon)  
Then, all the data should be explained \# B.csv: "death" the unit is count/1000. (before the colon is the dataset name: "name in the quotation mark is the variable" the sentence at the end of the line is the explanation.)  
Every full line should leave a blank row.  
\{   
\# Author: M.L.    
  
\# output: A.RData; B.csv; C.tif  
  
\# B.csv: "death" the unit is count/1000.  
\# B.csv: "confirmed" the unit is count/1000.

\# C.tif: "NDVI" factor is 0.0001, resolution is 1 km, monthly

\# end  
\}  
Mandatory lines: in every RData only one dataset can be included. Therefore, we do not recommend use this to store dataset.  
  
### 2.3 Code File for AN  
Here we need to claim both input and output. The input datasets are from other Rscripts (DW).  
\{   
\# Author: M.L.    
  
\# input: B.csv  
  
\# B.csv: "death" the unit is count/1000.  
\# B.csv: "confirmed" the unit is count/1000.  
  
\# output: A.RData; C.tif  
  
\# C.tif: "NDVI" factor is 0.0001, resolution is 1 km, monthly  
  
\# analysis: "Model" this model is OLS.    
  
\# end  
\}  

### 2.4 Workflow  
We claim clearly how to use the code. The workflows are written as follows:  
e.g.: WF.A: 01 -> 02 -> 04 -> 08 -> END  
WF.A.01.02: this is the description from 01...R to 02...R  
WF.A.08.END: the description of the result.  
  
## BreakPoint Recordings  
