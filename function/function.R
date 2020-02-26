library(downloader)
library(velox)
library(tidyverse)
library(raster)
library(rgeos)

###########################################
### Algorithm to find populated cluster ###
###########################################

function(params){

    # Get data
    # Download
    tempfile <- tempfile(fileext = ".RData")
    #if(!file.exists(filename_raster_pop$filename)){
      download(params$filename_raster_pop$url,
               tempfile)
    #}
    
    # load into environment
    load(tempfile)
    
    # Set params
    Initial_Maximum_Population_Raster <- maxValue(Population_Raster_Vx_Original_v$as.RasterLayer())
    
    # Compute minimum aggreagating factor so that some cells in aggregated raster are closer to qualify as a populated cluster (i.e Pop > Number_Of_People_In_Cluster). Start halt way through thought for data vizualization (otherwise in dense urban areas coordinates of suggested villages are too squarely-aligned)
    Initial_Aggregation_Factor <- ceiling(sqrt(params$Max_Number_Of_People_In_Cluster/Initial_Maximum_Population_Raster)/2)
    
    # Work on a copy of velox object
    Population_Raster_Vx <- Population_Raster_Vx_Original_v$copy()
    
    # First aggregation of population raster
    if(Initial_Aggregation_Factor > 1) {
      Population_Raster_Vx$aggregate(factor = Initial_Aggregation_Factor, aggtype = 'sum')
    }
    Population_Raster_Vx_High_Resolution <- Population_Raster_Vx$copy() # Keep a coyp for extraction later
    
    #### Now aggregate until some cell qualify as populated cluster, 
    # remove populated cluster found and repeat until grid cells are too large to qualify as populated cluster
    # Pre-loop settings
    Area_Size_Population_Raster_Cell <- minValue(raster::area(Population_Raster_Vx$as.RasterLayer()))
    Populated_Cluster_DF <- as.data.frame(NULL)
    
    while (Area_Size_Population_Raster_Cell < params$Maximum_Area_Size_Of_Cluster_Km2){
      # Extract all raster cell that qualify as a populated cluster (i.e Pop > Number_Of_People_In_Cluster)
      Cells_Of_Populated_Cluster <- which(Population_Raster_Vx$as.RasterLayer()[] > params$Max_Number_Of_People_In_Cluster)
      
      # If some cells have been found:
      if (length(Cells_Of_Populated_Cluster) > 0){
        # Coordinates
        Coordinates_Of_Populated_Cluster_DF <- xyFromCell(Population_Raster_Vx$as.RasterLayer(), cell = Cells_Of_Populated_Cluster)
        # Area
        Resolution_Of_Populated_Cluster_DF <- rep(Population_Raster_Vx$res[1], length(Cells_Of_Populated_Cluster))
        # Population
        Population_Of_Populated_Cluster_DF <- Population_Raster_Vx$as.RasterLayer()[Cells_Of_Populated_Cluster]
        
        # Store
        Populated_Cluster_DF <- rbind(Populated_Cluster_DF,
                                      cbind(Coordinates_Of_Populated_Cluster_DF,
                                            Resolution_Of_Populated_Cluster_DF,
                                            Population_Of_Populated_Cluster_DF,
                                            Area = rep(Area_Size_Population_Raster_Cell, length(Cells_Of_Populated_Cluster))))
        
        # Remove people from identified populated cluster in order to look for other, more aggregated ones
        # Population_Raster_Vx <- velox(raster::subs(x = Population_Raster_Vx$as.RasterLayer(),
        #                                            y = as.data.frame(cbind(Population_Raster_Vx$as.RasterLayer()[Cells_Of_Populated_Cluster], 
        #                                                                    replacement = rep(0,length(Cells_Of_Populated_Cluster)))),
        #                                            by = 1,
        #                                            which = 2,
        #                                            subsWithNA = FALSE)
        # )
        Population_Raster_Vx = Population_Raster_Vx$as.RasterLayer()
        Population_Raster_Vx[Cells_Of_Populated_Cluster] = 0
        Population_Raster_Vx <- velox(Population_Raster_Vx)
      }
      
      # Re-aggregate by a factor 2 and compute area of cells to consider going through loop one more time
      Population_Raster_Vx_Last <- Population_Raster_Vx$copy() # Save second to last aggregation for identification of populated cluster based on Min_Number_Of_People below
      Population_Raster_Vx$aggregate(factor = 2, aggtype = 'sum')
      Area_Size_Population_Raster_Cell <- minValue(raster::area(Population_Raster_Vx$as.RasterLayer()))
    }
    
    ### Last, record as populated cluster any remaining pixel with more than Min_Number_Of_People_In_Cluster
    Cells_Of_Populated_Cluster <- which(Population_Raster_Vx_Last$as.RasterLayer()[] > params$Min_Number_Of_People_In_Cluster)
    
    if (length(Cells_Of_Populated_Cluster) > 0){
      # Coordinates
      Coordinates_Of_Populated_Cluster_DF <- xyFromCell(Population_Raster_Vx_Last$as.RasterLayer(), cell = Cells_Of_Populated_Cluster)
      # Area
      Resolution_Of_Populated_Cluster_DF <- rep(Population_Raster_Vx_Last$res[1], length(Cells_Of_Populated_Cluster))
      # Population
      Population_Of_Populated_Cluster_DF <- Population_Raster_Vx_Last$as.RasterLayer()[Cells_Of_Populated_Cluster]
      
      # Store
      Populated_Cluster_DF <- rbind(Populated_Cluster_DF,
                                    cbind(Coordinates_Of_Populated_Cluster_DF,
                                          Resolution_Of_Populated_Cluster_DF,
                                          Population_Of_Populated_Cluster_DF,
                                          Area = rep(Area_Size_Population_Raster_Cell, length(Cells_Of_Populated_Cluster))))
    }
    
    
    ### Create squared polygons data frame from coordinates and area
    Populated_Cluster_SP <- SpatialPoints(Populated_Cluster_DF) # Converts to spatial points (uses x and y for coordinates)
    Populated_Cluster_SP$Width_Of_Populated_Cluster <- Populated_Cluster_DF$Resolution_Of_Populated_Cluster_DF/2  ## Widths of buffers needed to produce desired areas  
    
    Populated_Cluster_PP <- gBuffer(spgeom = Populated_Cluster_SP, byid = T, width = Populated_Cluster_SP$Width_Of_Populated_Cluster, quadsegs = 1, capStyle = "SQUARE")
    
    ### Substract smaller spatial polygons from larger polygons to remove intersection
    # For each larger populated cluster, aggregate all smaller cluster and remove intersection
    for (i in 2:length(unique(Populated_Cluster_SP$Width_Of_Populated_Cluster))){ 
      Size_Of_Larger_PP_From_Which_To_Substract <- unique(Populated_Cluster_SP$Width_Of_Populated_Cluster)[i] # Size of larger PP from which to substract all smaller PP, aggregated
      Smaller_PP_To_Substract <- Populated_Cluster_PP[which(Populated_Cluster_SP$Width_Of_Populated_Cluster < Size_Of_Larger_PP_From_Which_To_Substract),] # Restriction to PP smaller than larger PP from which to substract all smaller PP, aggregated
      Smaller_PP_To_Substract_Aggregated <- gUnionCascaded(spgeom = Smaller_PP_To_Substract) # Aggregate of all smaller PP to substract from larger PP
      
      Larger_PP_From_Which_To_Substract <- Populated_Cluster_PP[which(Populated_Cluster_SP$Width_Of_Populated_Cluster >= Size_Of_Larger_PP_From_Which_To_Substract),] # Restriction to PP smaller than larger PP from which to substract all smaller PP, aggregated
      Larger_PP_From_Which_To_Substract_Differenced_Out <- gDifference(Larger_PP_From_Which_To_Substract, Smaller_PP_To_Substract_Aggregated, byid = T) # Remove from larger cluster all smaller clusters, aggregated
      Larger_PP_From_Which_To_Substract_Differenced_Out$Width_Of_Populated_Cluster <- Larger_PP_From_Which_To_Substract$Width_Of_Populated_Cluster # Add width
      
      Populated_Cluster_PP <- bind(Smaller_PP_To_Substract, Larger_PP_From_Which_To_Substract_Differenced_Out) # Bind together small clusters and large clusters with intersection removed
    }
    
    Populated_Cluster_PP$Population_Of_Populated_Cluster <- Populated_Cluster_DF$Population_Of_Populated_Cluster_DF  ## Population of populated clusters
    Populated_Cluster_PP$Area_Of_Populated_Cluster_km2 <- round(Populated_Cluster_DF$Area, digits = 0)
    
    ### For each populated cluster, suggest one GPS coordinates of 1 village
    User_Input_Villages_Max_Population_In_Populated_Cluster_DF <- as.tibble(NULL)
    Populated_Cluster_PP_To_Fill <- Populated_Cluster_PP[]
    
    if(length(params$User_Input_Villages_DF)>0){
      # First use user-inputted list of villages with known coordinates
      # Convert village coordinates as spatial points
      User_Input_Villages_SP <- User_Input_Villages_DF
      coordinates(User_Input_Villages_SP) <- ~ Longitude + Latitude
      proj4string(User_Input_Villages_SP) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
      proj4string(Populated_Cluster_PP) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0') # Two geometries need to be on the same projection in over()
      
      # Extract population at user-inputted villages
      User_Input_Villages_SP$Population_High_Resolution <- Population_Raster_Vx_High_Resolution$extract_points(User_Input_Villages_SP)
      
      # Label Populated clusters
      Populated_Cluster_PP$Label_Number <- 1:length(Populated_Cluster_PP)
      
      #  Extract Populated Cluster ID number in which villages lie in
      User_Input_Villages_SP$Populated_Cluster_Number<- over(User_Input_Villages_SP, Populated_Cluster_PP)[,c("Label_Number")]
      
      User_Input_Villages_Max_Population_In_Populated_Cluster_DF <- User_Input_Villages_DF[unlist(c((as.tibble(User_Input_Villages_SP@data) 
                                                                                                     %>% mutate(Village_Number = row_number())
                                                                                                     %>% filter(!is.na(Populated_Cluster_Number))
                                                                                                     %>% group_by(Populated_Cluster_Number)
                                                                                                     %>% slice(which.max(Population_High_Resolution))
                                                                                                     %>% ungroup(Populated_Cluster_Number)
                                                                                                     %>% dplyr::select(Village_Number)
      ))),]
      
      # Identify Populated cluster with no user-inputted village, therefore requiring filling with a suggested village from algo
      Populated_Cluster_PP_To_Fill <- Populated_Cluster_PP[is.na(over(Populated_Cluster_PP, User_Input_Villages_SP))[,1],]
    }
    
    # For remaining populated cluster with no user-inputted village, extract coordinates of most populated pixel in original high-resolution raster to suggest village locations
    Population_Raster_Vx_High_Resolution_Raster <- Population_Raster_Vx_High_Resolution$as.RasterLayer() # Rasterize
    Population_Raster_Vx_High_Resolution_Raster$CellNumber <- 1:(Population_Raster_Vx_High_Resolution$dim[1]*Population_Raster_Vx_High_Resolution$dim[2]) # Add cell numbers for extraxction later
    Population_Raster_Vx_High_Resolution_Raster_Vx <- velox(Population_Raster_Vx_High_Resolution_Raster) # Re-transform to velox
    Cells_Within_Populated_Clusters <- Population_Raster_Vx_High_Resolution_Raster_Vx$extract(Populated_Cluster_PP_To_Fill) # Extract
    Cells_Of_Suggested_Villages <- t(sapply(Cells_Within_Populated_Clusters, function(i) i[which.max(i[,1]), ] )) # Find cell with max population
    Coordinates_Of_Suggested_Villages  <- SpatialPoints(xyFromCell(Population_Raster_Vx_High_Resolution$as.RasterLayer(), Cells_Of_Suggested_Villages[,2])) # Find coordinates from cell
    
    # Return
    return(list(Populated_Cluster_PP = geojson_list(Populated_Cluster_PP),
           Coordinates_Of_Suggested_Villages = geojson_list(Coordinates_Of_Suggested_Villages)))
}