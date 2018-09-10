# frog: MaxEnt Modeling in Frogs

This repository is the code repository for a maximum entropy species distribution model for the Sierra Nevada Yellow-legged frog (*Rana sierrae*). The frog name is abbreviated as *Rasi* in the code and data for ease of coding. 

All data is stored in frogData/data. Documentation for the data can be found in frogData/documentation.

__Required R Packages:__ dismo, DMwR, ggplot2, raster, rgdal, rgeos, sp

*Note: In order to run the maxent model in the 'dismo' package, you will also need to load the 'rJava' package.*

## Files

### Preprocessing 

__1_MergeStreams.R__ This script take the California Rasi data and the Nevada Rasi data and merges them together into one object. It also standardizes variable names accross the two data sets and adds a couple of variables including 'state' and 'length' (arc length of each stream reach in km). The script also removes the variable rasichk (seems to just be notes) and comid12 (duplicate of comid). It then saves the data to RasiStreamLines.RDS. 
 
 * Input files: 

 	* raw/rasi/NHDflowline_Streams_Final_withRASI.shp
 	* raw/rasi/NHDflowlineGB_Streams_Final_withRASI.shp

 * Output files: 
 	* processed/RasiStreamLines1.RDS
 	* processed/shapefiles/RasiStreamLines1.shp


__2_ProcessSoils.R__ This script import a soil dataset provided by previous people and extracts soil type for each of the stream reaches. In case of a stream going through multiple soil types it takes the most common soil type (mode). There are some streams that do not have values in the provided dataset. Those values are saved in missingSoilsKey.csv and were taken from SoilWeb (https://casoilresource.lawr.ucdavis.edu/gmap/). 
	
 * Input files: 

	* raw/soils/MergedSoilsClipFinal.shp
	* processed/RasiStreamLines1.RDS
	* processed/missingSoilsKey.csv

 * Output files: processed/RasiStreamLines2.RDS



__3_CleanWHR.R__ This script imports three data layers from the CWHRVg shapefile and Area_Lidar_1acre shapefile (Tahoe National Forest) and uses them to extract Wildlife habitat type (whrtype). After it imports the layers, it merges them so they are easier to work with. The script then extracts several variables, including whrtype, whrsize, whrdensity, whrlifeform and covertype. Because of the number of NAs, covertype is not used but its categories are used to inform the collapsing of the whrtype variable. Values from whrtype are then aggregated based on the TypeAggregationScheme file. The WHR polygons are also merge to create polygons for each of the national forests (Lassen, Plumas, and Tahoe), which are saved as forestpolygons. 

 * Input files: 

 	* raw/CWHRVg.gdb (all layers)
 	* raw/TNFWHR/Area_Lidar_1acre.shp
 	* processed/RasiStreamLines2.RDS
 	* processed/TypeAggregationScheme.csv
	
 * Output files:
 	* processed/RasiStreamLines3.RDS
 	* results/forestpolygons.RDS



__4_ProcessDEM.R__ *NOTE: This script only needs to be run once, not everytime data gets updated upstream.* This script imports a 1/3 arc-second DEM from the USGS website (https://viewer.nationalmap.gov/basic/, [-121.8, -120, 39.1, 40.5]), merges it to create one local DEM, and saves it to localDEM.grd. Then the script calculates slope and aspect from the DEM and saves it to streamGeography.grd. Finally the script extracts slope and aspect variables using the rasi spatiallinesdataframe and saves that to extractedDEMvalues.RDS

 * Input files: 

 	* processed/RasiStreamLines3.RDS
 	* raw/DEM/n40w120/floatn40w120_13.flt 
 	* raw/DEM/n40w121/floatn40w121_13.flt 
 	* raw/DEM/USGS_NED_13_n40w122_GridFloat/usgs_ned_13_n40w122_gridfloat.flt
 	* raw/DEM/n41w120/floatn41w120_13.flt 
 	* raw/DEM/n41w121/floatn41w121_13.flt 
 	* raw/DEM/n41w122/floatn41w122_13.flt

 * Output files: 

 	* processed/localDEM.grd
 	* processed/streamGeography.grd
 	* processed/extractedDEMvalues.RDS

__5_CombineData.R__ This script takes the extracted DEM/geography data, collapses it and adds it to the rasi spatiallinesdataframe. Aspect is then converted to a circular data type so that binary variables (like North/South) are easier to extract. It also adds rocktype (a collapsed version of geology variable ptype), x (longitude), y (latitude). Then it removes rows with NAs, and fills in missing values for whrdensity and whrsize. Finally the script removes columns we don't need and renames all columns to have sensible short names. 

 * Input files:

 	* processed/extractedDEMvalues.RDS
 	* processed/RasiStreamLines3.RDS
 	* processed/rockclassification.csv


 * Output files: 
 	* processed/RasiStreamLinesFinal.RDS
 	* processed/shapefiles/RasiStreamLinesFinal.shp
 	* processed/RasiStreamDF.csv

__6_SelectVariables.R__ This script selects the final variables and observations that will be used in the maxent modeling. First it removes all observations with a maximum elevation of less than 1524m (5000ft), so as to remove any observations of *Rana boylii*. It uses the aspect variable to create four binary direction variables (south, west, southwest, and southeast). It collapses the WHR categorical variables so that they create fewer binary variables and converts any categorical variables into a series binary variables. The script then transforms the number of waterbodies and number of meadows to deal with high leverage points and removes 3 exessively high leverage points that are resistant to transformation. Finally the observation ID is removed for the file that will be used in the modeling.

	* Input files: processed/RasiStreamDF.csv

	* Output files: 
		* processed/RasiModelDF.csv
		* processed/RasiResultsDF.csv (contains observation ID)


### Modeling 

__1_VariableSelction.R__ *Note: Not necessary to run.* This script uses randomForest to attempt to parse out which variables are the most predictive of frog presence, without the assumptions of Maximum Entropy. 

 * Input files: processed/RasiStreamDF.csv

__2_Crossvalidation.R__ *Note: Not necessary to run.* This script runs a number of models, some with more variables, some with less, to try and get a sense of how best to fit and score the MaxEnt models when doing variable selection and model evaluation. The last model run has some variable selection as a test run.
 
 * processed/RasiStreamDF.csv

__3_ResponseCurves.R__ This 


