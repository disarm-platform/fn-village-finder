# fn-village-finder

## Parameters

JSON object containing:

- `Max_Number_Of_People_In_Cluster` {string} 
- `Min_Number_Of_People_In_Cluster` {string} 
- `Maximum_Area_Size_Of_Cluster_Km2` {string}
- `User_Input_Villages_DF` {array} Optional table of locations of known villages. Array with at least 2 columns labeled 'Longitude' and 'Latitude' in decimal degrees
- `filename_raster_pop` {JSON object} with 2 fields. 'filename' and 'url' relating to the population raster file.


## Constraints



## Response
GeoJSON object of points for each predicted village location
	

