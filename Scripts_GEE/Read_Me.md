## Scripts_GEE

These scripts are used for extracting rasters with the random forest classification algorithm. 

To have the scripts fully function you'll need to import all the assests per year and compare your imports to the ones that have been used. You will find all assets per year in the folder /Scripts_GEE/Imports_GEE/Shapes. (For comparing the imports you can find the screenshots of all the imports in /Scripts_GEE/Imports_GEE/XXXX_Imports.jpg)


## Comments: 

The rasters are classified as follows: Class 0 Water, Class 1 Dryland, Class 2 Mudflat, Class 3 Mangrove, Class 4 Woodland.  

Note that afterward exporting the 2 TIFS per year have been merged in QGIS resulting in 1 Raster per year (XXXX_Merge.tif) Prior to the automatic post-classification correction (in R) a manual correction of some areas has been conducted with the Serval Plugin in QGIS. This was only applied in the year 2010 and 2023 since there have been significant misclasifications due to cloud cover and haze.  

After manually correcting them the rasters have been saved and transfered to RStudio to apply the automatic post classification correction. The scripts can be found in the folder /Scripts_R/.

The classification rasters and the manually corrected versions as well as the RGB composites used for the classification have been uploaded in this folder: 

XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

