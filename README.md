# fn-village-finder
OpenFaas version of the village finder algorithm which identifies centroids of clusters of populated places based on gridded population data. A populated site is an area that meets certain size and population criteria and can represent a village, a neighborhood of a crowded city or a large but sparsely populated rural area.

The user specifies the following 3 parameters to define the type of population sites queried:

* maximum area size, above which a region cannot be considered as a unique location;
* upper population threshold, above which a location should be counted as a unique location;
* lower population threshold, below which a region smaller than the maximum area size should not be counted as a populated location.

The algorithm works iteratively. First, any 1km grid cells of the Worldpop raster that adhere to the three parameters are identified and the centroids are kept. The gridded population data, minus those grid cells identified in the first round, are then aggregated by a factor of 2 and any aggregated areas that adhere to the parameters are identified. The centroid of the most populated cell in the aggregated area is then assigned as the village location for that aggregated area. The process continues until all aggregated areas have an assigned centroid or until all thresholds are met.

### OpenFaaS location
http://faas.srv.disarm.io/function/fn-village-finder

### Algo writer
Francois Rerolle  - email: francois.rerolle@ucsf.edu

### Running locally
echo $(cat "function/test_req.json") | Rscript main.R
