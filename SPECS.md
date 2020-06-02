# fn-village-finder

## Parameters

JSON object containing:

- `Max_Number_Of_People_In_Cluster` {string} 
- `Min_Number_Of_People_In_Cluster` {string} 
- `Maximum_Area_Size_Of_Cluster_Km2` {string}
- `filename_raster_pop` {JSON object} with 2 fields. 'filename' and 'url' relating to the population raster file.


## Constraints
* Will be slow for large raster files

## Response
GeoJSON object of points for each predicted village location
	
## Example input
An example JSON input can be found [here](https://raw.githubusercontent.com/disarm-platform/fn-village-finder/master/fn-village-finder/function/test_req.json)
