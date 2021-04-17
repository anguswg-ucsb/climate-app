basemap <- function() {
  # pal = colorNumeric("inferno", reverse= TRUE, domain = today$size, n = 50)
  # pal2 <- colorNumeric("inferno", reverse = TRUE, domain = today$cases, n = 50)

  leaflet() %>%
    addProviderTiles(providers$OpenStreetMap, group = "Topographic") %>%
    addProviderTiles(providers$Esri.WorldShadedRelief, group = "Relief") %>%
    addLayersControl(options = layersControlOptions(collapsed = FALSE),
                     baseGroups = c('Topographic', "Relief")) %>%
    addScaleBar("bottomleft") %>%
    setView(-95,40,4) %>%
    addMeasure(
      position = "bottomleft",
      primaryLengthUnit = "feet",
      primaryAreaUnit = "sqmiles",
      activeColor = "red",
      completedColor = "green" ) %>%
    leafem::addMouseCoordinates()
}

second_map <- function() {
  # pal = colorNumeric("inferno", reverse= TRUE, domain = today$size, n = 50)
  # pal2 <- colorNumeric("inferno", reverse = TRUE, domain = today$cases, n = 50)
  leaflet() %>%
    addProviderTiles(providers$Esri.WorldImagery) %>%
    addScaleBar("bottomleft") %>%
    setView(-95,40,4) %>%
    addMiniMap(toggleDisplay = TRUE, minimized = FALSE) %>%
    addMeasure(
      position = "bottomleft",
      primaryLengthUnit = "feet",
      primaryAreaUnit = "sqmiles",
      activeColor = "red",
      completedColor = "green" ) %>%
    leafem::addMouseCoordinates()
}

zoom_to_catch = function(map, df, catchment){
  # Filter the counties to the input FIP code
  shp = filter(df, comid == catchment)
  # Build a buffered bounding box to center the map on:
  bounds = shp %>%
    # make bounding box
    st_bbox() %>%
    # Make spatial
    st_as_sfc() %>%
    # Buffer to .1 degree
    st_buffer(.1) %>%
    # make new bounding box
    st_bbox() %>%
    # extract coordinates as vector
    as.vector()
  # Clear all current shapes (remember county centroids are currently markers!)
  clearShapes(map) %>%
    # Add the county shape making the outline color red and the fill an opaque white
    addPolygons(data = shp,
                color = "red",
                fillColor  = "grey",
                fillOpacity = .3) %>%
    # Fly the leaflet map to the buffered boundary
    flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4])
}


tidy_raster <- function(raster) {
  rtable <- raster %>%
    raster::rasterToPoints() %>%
    tibble::as_tibble() %>%
    dplyr::relocate(x, y) %>%
    setNames(
      .,
      c("lon",
        "lat",
        stringr::str_sub(colnames(.)[-(1:2)], start = 2L))
    ) %>%
    tidyr::pivot_longer(
      cols = c(tidyselect::everything(), -(1:2)),
      names_to = "date"
    ) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::relocate(lon, lat, value)

  rtable
}

tidy_stack <- function(raster_list, as_sf = FALSE) {
  param_names <- names(raster_list)
  tidy_stacks <- lapply(X = raster_list, FUN = tidy_raster)

  p <- progressr::progressor(along = param_names)
  tidy_data <-
    lapply(X = param_names,
           FUN = function(rname) {
             p(paste0("Transforming ", rname, "..."))
             setNames(
               tidy_stacks[[rname]],
               c("lon", "lat", rname, "date")
             )
           }
    ) %>%
    purrr::reduce(dplyr::left_join, by = c("date", "lon", "lat")) %>%
    dplyr::relocate(lon, lat, date)

  if (as_sf) {
    tidy_data <-
      tidy_data %>%
      sf::st_as_sf(coords = c("lon", "lat")) %>%
      sf::st_set_crs(4326)
  }

  tidy_data
}

# Aggregate all gridMET climate parameters into tibble for ML
aggregate_gridmet <- function(aoi, start_date, end_date = NULL, as_sf = FALSE) {
  p <- progressr::progressor(steps = 10L)

  p("Getting climate data...")

  climate_data <- climateR::getGridMET(
    AOI       = aoi,
    param     = climateR::param_meta$gridmet$common.name[-c(11, 12, 14)],
    startDate = start_date,
    endDate   = end_date
  )

  fmoist100 <- climateR::getGridMET(
    AOI = aoi,
    param = "fmoist_100",
    startDate = start_date,
    endDate   = end_date
  )

  fmoist1000 <- climateR::getGridMET(
    AOI = aoi,
    param = "fmoist_1000",
    startDate = start_date,
    endDate   = end_date
  )

  p("Tidying climate data...")

  tidy_clim <- tidy_stack(c(climate_data, fmoist100, fmoist1000),
                          as_sf = as_sf)

  p("Tidied!")

  tidy_clim
}

#
# lat = 35.6643
# lng = -96.91935
# pt <- data.frame(lat, lng)
# pt <- sf::st_as_sf(pt,
#                    coords = c("lng", "lat"),
#                    crs = 4326)
# buffer <- pt %>%
#   st_transform(5070) %>%
#   st_buffer(.05)
#
#
# bb <- bb_poly(buffer) %>%
#   st_sf() %>%
#   st_transform(4326)
# class(bb)
#
# library(raster)
# bb = buffer %>%
#   st_bbox()
# bb_pts <- data.frame(x = c(bb[1], bb[3]), y = c(bb[2], bb[4]))
#
# lat = 35.6643
# lng = -96.91935
# pt <- data.frame(lat, lng)
# pt <- sf::st_as_sf(pt,
#                    coords = c("lng", "lat"),
#                    crs = 4326)
# buffer <- pt %>%
#   st_transform(5070) %>%
#   st_buffer(30000)
#
# bb = buffer %>%
#   st_bbox() %>%
#   st_as_sfc() %>%
#   st_transform(4326) %>%
#   st_as_sf()
#
# rain <- climateR::getGridMET(AOI = bb, "prcp", startDate = "2010-01-01")
#
# mapview::mapview(bb)
#
# library(AOI)
# aoi <- aoi_get(state = "Oklahoma")
# plot(bb)
# class(bb)
# rain <- climateR::getGridMET(AOI = bb, "prcp", startDate = "2010-01-01")
# plot(rain$prcp)
# plot(aoi_get(state = "Oklahoma")$geometry, add = TRUE)
# plot(rain$prcp)
# r1 <- raster(rain$prcp)
# mapview::mapview(bb)
#
# values(stack(rain))
#
# leaflet::leaflet() %>%
#   addProviderTiles(providers$OpenStreetMap) %>%
#   addPolygons(data = bb, color = "red")
#
# runoff <- climateR::getTerraClim(AOI = pt, param = "q",
#                                  startDate = "1993-01-01",
#                                  endDate = "2015-01-01")
#
# runoff$date <- paste0(runoff$date, "-01")
# runoff$date <- as.Date(runoff$date)
# rownames(runoff) <- runoff$date
# runoff <- select(runoff, q)
#
# dygraph(data = runoff)
#


