
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

mapview::mapview(bb)

 clim <- climateR::getGridMET(AOI = bb, "burn_index", startDate = "2008-08-01", endDate = "2008-11-01")

layer <- clim$gridmet_burn_index[[1]]
plot(layer)
agg <- aggregate_gridmet(bb, "2008-08-01", "2008-11-01", as_sf = TRUE)

 plot(agg)


 # Load packages
 packs <- list("tidyverse", "raster", "sf")
 lapply(packs, require, character.only = T)


 agg2 <- agg %>%
   filter(date == "2010-01-01") %>%
   dplyr::select(tmax)



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

# How to get Time series data from tidy prediction data --- skeleton outline:
# Click map point -> create buffer polygon around point --> filter prediction data on bbox of polygon -->
# --> get mean (or some way of showing daily data from all the predicted points) --> TIME SERIES/PLOT

# using aoi2 as an example of the tidy stacked points we will have from prediction data
# plan - filter prediction data by the bounding box of clicked polygon, use daily mean of parameter to average all of the points in the area
# grab a single day from one of the points and plot mean timeseries

# Map click
lng = -120.08
lat = 34.75

pt <- data.frame(lat, lng)
pt <- sf::st_as_sf(pt,
                   coords = c("lng", "lat"),
                   crs = 4326)

# Takes map click --> creates extent polygon --> gets climate raster for prediction --> outputs point raster
click_to_AOI <- function(pt) {
  buffer <- pt %>%
    st_transform(5070) %>%
    st_buffer(4000) %>%
    st_transform(4326)

  bb = buffer %>%
    st_bbox() %>%
    st_as_sfc() %>%
    st_transform(4326) %>%
    st_as_sf()
}

bb <- click_to_AOI(pt)

# map click --> get AOI climate raster
click_to_raster <- function(bb, param, start_date = NULL, end_date = NULL) {
  r <- climateR::getGridMET(AOI = bb,
                               as.character(param),
                               startDate = as.character(start_date),
                               endDate = as.character(end_date))
  #tidy_stack(clim, as_sf = TRUE)

  # missing step for final:
  # 1. aggregate_gridmet() replaces getGridMET
  # 2. Machine learn on aggregated climate rasters
}
# new_raster <- raster::overlay(clim$tmax[[1]], clim$tmax[[2]], fun = mean)
# new_raster <- raster::calc(clim$tmax, mean)

# climate raster stack
clim <- click_to_raster(bb, "tmax", "1980-01-01", '1980-01-07')

# climate points from raster (this will be the outputted prediction raster in final)
pt_raster <- tidy_stack(clim, as_sf = TRUE)

m <- pt_raster %>%
  st_drop_geometry()
DT::datatable(m)

predictRaster <- function(pt_rast) {
  # get empty raster, set values to 0
  empty_raster <- raster::projectExtent(object = clim$tmax[[1]],
                                crs = clim$tmax@crs) %>%
    raster::setValues(empty_raster, value = 0)

  # Convert points to sp
  agg <- as(pt_rast, "Spatial")

  # rastorize prediction points into empty raster grid --- currently rasterizes "last" layer in stack
  r <- rasterize(agg, empty_raster, field = "tmax")
}

prediction <- predictRaster(pt_rast = pt_raster)
vals <- values(prediction)
pal = colorNumeric("Ylo", domain = vals, n = 50)
pal2 <- colorNumeric("viridis", domain = today$cases, n = 50)
leaflet() %>%
  addProviderTiles(providers$OpenStreetMap, group = "Topographic") %>%
  addProviderTiles(providers$Esri.WorldShadedRelief, group = "Relief") %>%
  addLayersControl(options = layersControlOptions(collapsed = FALSE),
                   baseGroups = c('Topographic', "Relief")) %>%
  addScaleBar("bottomleft") %>%
  addRasterImage(x = prediction) %>%
  addLegend("bottomright", pal = pal, values = vals,
            title = "Temp",
            labFormat = labelFormat(prefix = ""),
            opacity = 1
  )
addMeasure(
  position = "bottomleft",
  primaryLengthUnit = "feet",
  primaryAreaUnit = "sqmiles",
  activeColor = "red",
  completedColor = "green" ) %>%
  leafem::addMouseCoordinates()

# get mean temperature of all cells on a day (raster layer) --- we need a different time series method but this is a generic outline for testing
timeseries_data <- function(pt_rast) {
  agg <- pt_raster %>%
    st_drop_geometry() %>%
    group_by(date) %>%
    mutate(mean = mean(tmax)) %>%
    slice(n = 1)

  agg$date <- as.Date(agg$date)
  agg <- data.frame(agg)
  rownames(agg) <- agg$date
  agg
}

ts <- timeseries_data(pt_rast = pt_raster)

make_timeseries <- function(xts_df) {
  dygraph(data = dplyr::select(xts_df, mean)) %>%
    dyHighlight(highlightCircleSize = 4,
                highlightSeriesBackgroundAlpha = .4) %>%
    dyOptions(colors = c("darkred"),
              fillGraph = TRUE)
}

make_timeseries(ts)





# Other method for prediction raster from points
predictRaster2 <- function(rast, param, pt_rast) {
  # Convert points to sp
  agg <- as(pt_raster, "Spatial")

  # Resolution
  res <- raster::res(rast$param[[1]])

  # exent
  rextent <- rast$param@extent

  # Generate empty raster layer and rasterize points
  r <- raster(crs = crs(agg),
              vals = 0,
              resolution = res,
              ext = rextent) %>%
    rasterize(agg, .)

}

clim <- climateR::getGridMET(AOI = aoi2, "tmax", startDate = "2010-01-01", endDate = "2010-06-03")
r2<- tidy_stack(clim, as_sf = FALSE)

poly_bb <- st_bbox(bb)
poly_ts <- r2 %>%
  filter(lon >= poly_bb[1],
         lon <= poly_bb[3],
         lat >= poly_bb[2],
         lat <= poly_bb[4]) %>%
  group_by(date) %>%
  mutate(mean = mean(tmax)) %>%
  slice(n =1)

poly_ts$date <- as.Date(poly_ts$date)
poly_ts <- data.frame(poly_ts)
rownames(poly_ts) <- poly_ts$date

# plot timeseries
dygraph(data = dplyr::select(poly_ts, mean)) %>%
  dyHighlight(highlightCircleSize = 4,
              highlightSeriesBackgroundAlpha = .4) %>%
  dyOptions(colors = c("darkred"),
            fillGraph = TRUE)



r2_sf <- tidy_stack(clim, as_sf = TRUE)

wth <- st_filter(r2_sf, bb)


poly_bb <- st_bbox(bb)

poly_ts <- r2 %>%
  filter(lon >= poly_bb[1], lon <= poly_bb[3],
         lat >= poly_bb[2], lat <= poly_bb[4])

wth2 <- wth %>%
  group_by(date) %>%
  mutate(mean = mean(tmax)) %>%
 slice( n =1)

wth2$date <- as.Date(wth2$date)
wth2 <- data.frame(wth2)

rownames(wth2) <- wth2$date


dygraph(data = dplyr::select(wth2, mean)) %>%
          dyHighlight(highlightCircleSize = 4,
                    highlightSeriesBackgroundAlpha = .4) %>%
              dyOptions(colors = c("darkred"),
                  fillGraph = TRUE)














