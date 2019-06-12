## Define functions
Function_Importing_Population_Raster <- function(Country, Folder_to_import){
  foldername <- paste0(Folder_to_import, "/", Country)
  filename_import <- list.files(foldername)
  filename_import <- filename_import[grep(pattern = ".tif", x = filename_import)]
  Population_Raster <- raster(paste0(foldername, "/", filename_import))
  return(Population_Raster)
}

Function_Converting_NA_Into_0 <- function(Raster){
  Raster[which(is.na(Raster[]))] <- 0
  return(Raster)
}

Function_Converting_Raster_Into_Velox <- function(Raster){
  Velox <- velox(Raster) # Transform to velox object
  return(Velox)
}

Function_Exporting_Velox_For_Shinny_App <- function(Country, Folder_to_save){
  
  ## Import Population raster 
  Population_Raster <- Function_Importing_Population_Raster(Country = Country, Folder_to_import = Folder_to_save)
  
  ## Action
  # Change NA to 0 so that pixels near water bodies don't get NA-ed in aggregation via the sum function
  Population_Raster <- Function_Converting_NA_Into_0(Population_Raster)
  
  # Transform raster into velox format for computation efficiency
  Population_Raster_Vx_Original <- Function_Converting_Raster_Into_Velox(Population_Raster)
  
  ## Save 
  save(file=paste0(Folder_to_save, "/", Country, "/Population_Raster_Vx_Original.RData"), Population_Raster_Vx_Original) # Save an Rdata file to conserve data cleaning/management features
  
}
