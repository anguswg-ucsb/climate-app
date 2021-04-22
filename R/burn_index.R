
library(tidyverse)
library(AOI)
library(climateR)
library(leaflet)
library(sf)
library(raster)
library(dygraphs)


aoi <- aoi_get(state = "CA") %>%
  st_transform(4326)
aoi2 <- aoi_get(state = "CA", county = "Santa Barbara")

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = aoi) %>%
  addPolygons(data = aoi2) %>%
  leafem::addMouseCoordinates()

lng = -120.08
lat = 34.75
pt <- data.frame(lat, lng)
pt <- sf::st_as_sf(pt,
                   coords = c("lng", "lat"),
                   crs = 4326)
buffer <- pt %>%
  st_transform(5070) %>%
  st_buffer(10000) %>%
  st_transform(4326)

bb = buffer %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_transform(4326) %>%
  st_as_sf()

r <- climateR::getGridMET(AOI = bb,
                          "burn_index",
                          startDate = "2008-08-01",
                          endDate = "2008-08-03")
names(r) <- "burn_index"

r2 <- plot(r[['burn_index']][[2]])
plot(r2)

agg <- aggregate_gridmet(bb, start_date = "2001-08-01", end_date = "2010-11-01")



ts <- timeseries_data(clim)
make_timeseries(ts, 'tmax_mean')

mapview::mapview(bb)

clim <- climateR::getGridMET(AOI = pt, "tmin", startDate = "1980-08-01", endDate = "2010-11-01")
clim <- climateR::getGridMET(AOI = bb, "tmin", startDate = "1980-08-01", endDate = "2010-11-01")
layer <- clim$gridmet_burn_index[[1]]
plot(layer)

agg <- aggregate_maca(bb, "2008-08-01", "2025-11-01", as_sf = FALSE)

agg2 <- agg %>%
  select(date, tmax)

ts <- timeseries_data(agg)
ts <- ts %>% select(date, "tmax")

plot <- make_timeseries(ts, "tmax")
all_of()
# --- Fire & Burn index ---

fires <- readr::read_csv("data/fire_incidents.csv") %>%
  rename(start = incident_dateonly_created,
         end = incident_dateonly_extinguished,
         lon = incident_longitude,
         lat = incident_latitude)=

fires <- jsonlite::fromJSON("https://www.fire.ca.gov/umbraco/api/IncidentApi/List?inactive=true") %>%
  janitor::clean_names() %>%
  rename(start = started_date_only,
         end = extinguished_date_only)

fire_geoJSON <- jsonlite::fromJSON("https://www.fire.ca.gov/umbraco/api/IncidentApi/GeoJsonList?inactive=true")
#
big_fires <- fires %>%
  filter(acres_burned >= 500)
#
# # burn index timeseries
burn_index <- climateR::getGridMET(aoi, "burn_index", start = 2002-01-01, end = 2020-01-01)
#
burn <- tidy_stack(burn_index, as_sf = FALSE)
# group_by(date) %>%
# mutate(mean = mean(burn_index))

burn_pts <- timeseries_data(burn)
make_timeseries(burn_pts)
#
burn_area <- sf::read_sf("data/Public_NIFS_Perimeters.shp")
big_area <- burn_area %>%
  arrange(-GISAcres) %>%
  slice(n = 1:30)
#
