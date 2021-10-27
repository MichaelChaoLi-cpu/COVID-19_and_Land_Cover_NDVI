# -*- coding: utf-8 -*-
"""
Convert h4 to GeoTiff 

For DP_05 Nighttime Light from VNP46A3

Band: AllAngle_Composite_Snow_Free_Num 

Resolution: 15 arc sec ()

Created on Wed Oct 27 15:23:59 2021

@author: NASA
"""

from osgeo import gdal
import os

## List input raster files
os.chdir('D:\\05_Article\\NTL')
rasterFiles = os.listdir(os.getcwd())
#print(rasterFiles)

loop = 0

while (loop <  len(rasterFiles)):
    #Get File Name Prefix
    rasterFilePre = rasterFiles[loop][:-3]
    
    fileExtension = "_BBOX.tif"
    
    ## Open HDF file
    hdflayer = gdal.Open(rasterFiles[loop], gdal.GA_ReadOnly)
    
    #print (hdflayer.GetSubDatasets())
    
    # Open raster layer
    #hdflayer.GetSubDatasets()[0][0] - for first layer
    #hdflayer.GetSubDatasets()[1][0] - for second layer ...etc
    subhdflayer = hdflayer.GetSubDatasets()[5][0]
    rlayer = gdal.Open(subhdflayer, gdal.GA_ReadOnly)
    #outputName = rlayer.GetMetadata_Dict()['long_name']
    
    #Subset the Long Name
    outputName = subhdflayer[97:]
    
    outputNameNoSpace = outputName.strip().replace(" ","_").replace("/","_")
    outputNameFinal = outputNameNoSpace + rasterFilePre + fileExtension
    print(outputNameFinal)
    
    outputFolder = "D:\\05_Article\\NTLoutput\\"
    
    outputRaster = outputFolder + outputNameFinal
    
    #collect bounding box coordinates
    HorizontalTileNumber = int(rlayer.GetMetadata_Dict()["HorizontalTileNumber"])
    VerticalTileNumber = int(rlayer.GetMetadata_Dict()["VerticalTileNumber"])
        
    WestBoundCoord = (10*HorizontalTileNumber) - 180
    NorthBoundCoord = 90-(10*VerticalTileNumber)
    EastBoundCoord = WestBoundCoord + 10
    SouthBoundCoord = NorthBoundCoord - 10
    
    EPSG = "-a_srs EPSG:4326" #WGS84
    
    translateOptionText = EPSG+" -a_ullr " + str(WestBoundCoord) + " " + str(NorthBoundCoord) + " " + str(EastBoundCoord) + " " + str(SouthBoundCoord)
    
    translateoptions = gdal.TranslateOptions(gdal.ParseCommandLine(translateOptionText))
    gdal.Translate(outputRaster,rlayer, options=translateoptions)
    
    #Display image in QGIS (run it within QGIS python Console) - remove comment to display
    #iface.addRasterLayer(outputRaster, outputNameFinal)
    
    loop = loop + 1