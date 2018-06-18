# frog

This repository is the code repository for a maximum entropy species distribution model for the Sierra Nevada Yellow-legged frog (*Rana sierrae*). 

## Files

### Preprocessing 

* __MergeStreams.R__ This script take the California Rasi data and the Nevada Rasi data and merges them together into one object. It also standardizes variable names accross the two data sets and adds a couple of variables including 'state' and 'length' (arc length of each stream reach in km). It then saves the data to processed/RasiStreamLines.RDS. 
* __ProcessDEM.R__ This script imports a 1/3 arc-second DEM from the USGS website (https://viewer.nationalmap.gov/basic/, with bounding box -121.8, -120, 39.1, 40.5) and uses it to calculate slop and aspect. It then saves all this data to processed/streamGeography.grd.
* __ProcessSoils.R__ This script imports two soil datasets provided by previous people and merges it into one object. It then extracts soil type for each of the stream reaches. In case of a stream going through multiple soil types it takes the most common soil type (mode). There are still about 50 NAs in the soil data. I think we may need to go back to SSURGO to get the rest (or a CA specific dataset).

