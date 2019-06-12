#### This code prepares population rasters into velox object and saves a Rdata object for computational speed

## Clear environment
rm(list= ls())

## Set working directory
setwd("/Users/francoisrerolle/Desktop/UCSF/Adaptive-Sampling/Village-Function/Shinny-App/Village-Function-App")

## Load packages
library(raster)
library(velox)

## Source code with functions
source("Code/Functions_Code.R")

## Prepare velox raster population for countries in App
Function_Exporting_Velox_For_Shinny_App(Country = "Laos", Folder_to_save = "/Users/francoisrerolle/Desktop/UCSF/Adaptive-Sampling/Village-Function/Shinny-App/Village-Function-App/Data")
Function_Exporting_Velox_For_Shinny_App(Country = "Philipines", Folder_to_save = "/Users/francoisrerolle/Desktop/UCSF/Adaptive-Sampling/Village-Function/Shinny-App/Village-Function-App/Data")
Function_Exporting_Velox_For_Shinny_App(Country = "Eswatini", Folder_to_save = "/Users/francoisrerolle/Desktop/UCSF/Adaptive-Sampling/Village-Function/Shinny-App/Village-Function-App/Data")
