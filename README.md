# frog: MaxEnt Modeling in Frogs

This repository is the code repository for a maximum entropy species distribution model for the Sierra Nevada Yellow-legged frog (*Rana sierrae*). The frog name is abbreviated as *Rasi* in the code and data for ease of coding. 

All data is stored in frogData/data. Documentation for the data can be found in frogData/documentation.

## System Requirements

__R Packages__

 * sp
 * raster
 * rgdal
 * rgeos
 * ggplot2 

## Files

### Preprocessing 

__1_MergeStreams.R__ This script take the California Rasi data and the Nevada Rasi data and merges them together into one object. It also standardizes variable names accross the two data sets and adds a couple of variables including 'state' and 'length' (arc length of each stream reach in km). The script also removes a variable (rasichk) that seems to just be notes. It then saves the data to RasiStreamLines.RDS. 
 
 * Input files: 

 	* raw/rasi/NHDflowline_Streams_Final_withRASI.shp
 	* raw/rasi/NHDflowlineGB_Streams_Final_withRASI.shp

 * Output files: processed/RasiStreamLines.RDS


__2_ProcessSoils.R__ This script import a soil dataset provided by previous people and extracts soil type for each of the stream reaches. In case of a stream going through multiple soil types it takes the most common soil type (mode). There are some streams that do not have values in the provided dataset. Those values are saved in missingSoilsKey.csv and were taken from SoilWeb (https://casoilresource.lawr.ucdavis.edu/gmap/).
	
 * Input files: 

	* frog_model/data/output/MergedSoilsClipFinal.shp
	* processed/RasiStreamLines1.RDS
	* processed/missingSoilsKey.csv

 * Output files: processed/RasiStreamLines2.RDS



__3_CleanWHR.R__ This script imports three data layers from the CWHRVg and uses it to extract Wildlife habitat type (whrtype). After it imports the layers, it merges them so they are easier to work with. The script then extracts several variables, including whrtype, whrsize, whrdensity and covertype. Because of the number of NAs in the other variables, only whrtype will be used. Values from whrtype are then aggregated based on the TypeAggregationScheme file.

 * Input files: 

 	* frogData/data/frog_model/data/datasets/cwhr_4Megan/CWHRVg.gdb (all layers)
 	* processed/RasiStreamLines2.RDS
 	* processed/TypeAggregationScheme.csv
	
 * Output files: processed/RasiStreamLines3.RDS



__4_ ProcessDEM.R__ This script imports a 1/3 arc-second DEM from the USGS website (https://viewer.nationalmap.gov/basic/, [-121.8, -120, 39.1, 40.5]), merges it to create one local DEM, and saves it to localDEM.grd. Then the script calculates slope and aspect from the DEM and saves it to streamGeography.grd.

 * Input files: 

 	* DEM/n40w120/floatn40w120_13.flt 
 	* DEM/n40w121/floatn40w121_13.flt 
 	* DEM/n40w122/floatn40w122_13.flt 
 	* DEM/n41w120/floatn41w120_13.flt 
 	* DEM/n41w121/floatn41w121_13.flt 
 	* DEM/n41w122/floatn41w122_13.flt

 * Output files: 

 	* processed/localDEM.grd
 	* processed/streamGeography.grd


