library(raster)
library(velox)

# Get country codes
ccodes <- ccodes()

# Load pop raster and create country level
# velox objects
AFR_Worldpop_2015_ppp <- raster("/Users/sturrockh/Dropbox/Random/AFR_PPP_2015_adj_v2.tif")
ASIA_Worldpop_2015_ppp <- raster("/Users/sturrockh/Downloads/Asia_1km_Population/Asia_PPP_2015_adj_v2.tif")
LA_Worldpop_2015_ppp <- raster("/Users/sturrockh/Downloads/Latin_America_and_the_Caribbean_1km_Population/LAC_PPP_2015_adj_v2.tif")

# Problem with China
codes <- ccodes$ISO3[ccodes$continent %in% c("South America", "North America")]
codes <- codes[-which(codes %in% c("CAN", "USA"))]
#codes <- codes[-which(codes %in% c("CHN", "CCK"))]

for(country in codes){
      # Crop to country
      country_adm0 <- raster::getData("GADM", level=0, country=country)
      Population_Raster_Vx_Original <- crop(LA_Worldpop_2015_ppp, country_adm0)
      Population_Raster_Vx_Original <- mask(Population_Raster_Vx_Original, country_adm0)
      Population_Raster_Vx_Original_v <- velox(Population_Raster_Vx_Original)
      save(Population_Raster_Vx_Original_v, 
           file=paste0("/Users/hughsturrock/Dropbox/Worldpop/", country, "_worldpop_2015_ppp.RData"))
}