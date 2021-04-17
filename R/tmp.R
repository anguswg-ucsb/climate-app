library(sf)
library(raster)
library(tidyverse)
library(elevatr)
library(whitebox)
library(AOI)
library(nhdplusTools)
library(dataRetrieval)

# AOI Santa Barbara
aoi = AOI::aoi_get(county = "Santa Barbara", state = "CA") %>%
  AOI::aoi_buffer(.5)

# Bounding box
bb <- aoi %>%
  st_transform(5070) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf() %>%
  st_transform(4269)

# Whitebox tools
catch <- nhdplusTools::get_nhdplus(comid = 17596123, realization = "catchment")

elev <- elevatr::get_elev_raster(catch, z = 14)
elev2 <- crop(elev, catch)

writeRaster(elev2, file = "data/sb-stream-elev.tif", overwrite = TRUE)

# Read in elevation raster
r <- raster("data/sb-stream-elev.tif")

# Slope
wbt_slope("data/sb-stream-elev.tif", "data/sb-stream-slope.tif")
slope <-  raster("data/sb-stream-slope.tif")

mapview::mapview(breach_depress)

# HAND

# Hydrological corrected surface (breach depressions)
wbt_breach_depressions('data/sb-stream-elev.tif', 'data/sb-stream-breach.tif')
breach_depress = raster('data/sb-stream-breach.tif')

# HAND raster
wbt_elevation_above_stream('../data/mission-creek-area-breach-depress.tif', '../data/mission-creek-area-streams-rast.tif', '../data/mission-creek-area-HAND.tif')
hand = raster('../data/mission-creek-area-HAND.tif')

# correct local datum offsets
hand2 = hand + 3.69
hand2[streams_rast == 1] = 0

writeRaster(hand2, '../data/mission-creek-area-HAND-offset.tif', overwrite = TRUE)

# 2017 Impact Assessment:


flood_func = function(x){
  ifelse(x < 10.02, x, NA)
}

hand_r = calc(hand2, flood_func)


# Estimate the impacts
flood_depth = extract(hand_r, basin_buildings)
cols = ifelse(!is.na(flood_depth), 'darkred', 'black')
sum(cols == 'darkred')
mapview::mapview(slope)







