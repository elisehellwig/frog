# frog

This repository is the code repository for a maximum entropy species distribution model for the Sierra Nevada Yellow-legged frog (*Rana sierrae*). The frog name is abbreviated as *Rasi* in the code and data for ease of coding.

All data is stored in frogData/data. Documentation for the data can be found in frogData/documentation.

## Files

### Preprocessing 

__MergeStreams.R__ This script take the California Rasi data and the Nevada Rasi data (data in frogData/data/cathy/rasi_streams/) and merges them together into one object. It also standardizes variable names accross the two data sets and adds a couple of variables including 'state' and 'length' (arc length of each stream reach in km). It then saves the data to processed/RasiStreamLines.RDS. 
* Input files: cathy/rasi_streams/NHDflowline_Streams_Final_withRASI.shp, frogData/data/cathy/rasi_streams/NHDflowlineGB_Streams_Final_withRASI.shp
* Output files: processed/RasiStreamLines.RDS

__ProcessDEM.R__ This script imports a 1/3 arc-second DEM from the USGS website (https://viewer.nationalmap.gov/basic/, with bounding box -121.8, -120, 39.1, 40.5), merges it to create one local DEM, and saves it to localDEM.grd. Then the script calculates slope and aspect from the DEM and saves it to streamGeography.grd.
* Input files: *DEM/n40w120/floatn40w120_13.flt*, *DEM/n40w121/floatn40w121_13.flt*, *DEM/n40w122/floatn40w122_13.flt*, *DEM/n41w120/floatn41w120_13.flt*, *DEM/n41w121/floatn41w121_13.flt*, DEM/n41w122/floatn41w122_13.flt
* Output files: processed/localDEM.grd, streamGeography.grd

__ProcessSoils.R__ This script imports two soil datasets provided by previous people and merges it into one object. It then extracts soil type for each of the stream reaches. In case of a stream going through multiple soil types it takes the most common soil type (mode). There are still about 50 NAs in the soil data. I think we may need to go back to SSURGO to get the rest (or a CA specific dataset).
* Input files: 
* Output files:


__CleanWHR.R__ This script imports the CWHR data layer (frogData/data/frog_model/data/output/ChwrMerge.shp) and uses it to extract WHR and cover type. 
* Input files: 
* Output files:

