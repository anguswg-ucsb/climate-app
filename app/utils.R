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
    # addMiniMap(toggleDisplay = TRUE, minimized = FALSE) %>%
    addMeasure(
      position = "bottomleft",
      primaryLengthUnit = "feet",
      primaryAreaUnit = "sqmiles",
      activeColor = "red",
      completedColor = "green" ) %>%
    leafem::addMouseCoordinates()
}
# second_map <- function() {
#   # pal = colorNumeric("inferno", reverse= TRUE, domain = today$size, n = 50)
#   # pal2 <- colorNumeric("inferno", reverse = TRUE, domain = today$cases, n = 50)
#   leaflet() %>%
#     addProviderTiles(providers$Esri.WorldImagery) %>%
#     addScaleBar("bottomleft") %>%
#     setView(-95,40,4) %>%
#     # addMiniMap(toggleDisplay = TRUE, minimized = FALSE) %>%
#     addMeasure(
#       position = "bottomleft",
#       primaryLengthUnit = "feet",
#       primaryAreaUnit = "sqmiles",
#       activeColor = "red",
#       completedColor = "green" ) %>%
#     addRasterImage(x = rasterData())
#     leafem::addMouseCoordinates()
# }


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





# bb <- bb_poly(buffer) %>%
#   st_sf() %>%
#   st_transform(4326)



# bb = buffer %>%
#   st_bbox()
# bb_pts <- data.frame(x = c(bb[1], bb[3]), y = c(bb[2], bb[4]))

lat = 35.6643
lng = -96.91935
pt <- data.frame(lat, lng)
pt <- sf::st_as_sf(pt,
                   coords = c("lng", "lat"),
                   crs = 4326)
buffer <- pt %>%
  st_transform(5070) %>%
  st_buffer(30000) %>%
  st_transform(4326)

bb = buffer %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_transform(4326) %>%
  st_as_sf()

rain <- climateR::getGridMET(AOI = bb, "tmax", startDate = "2010-01-01")

plot(rain$tmax)
# values(rain$tmax)

mapview::mapView(rain$tmax, layer.name = "raster")
#
# pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r),
#                     na.color = "transparent")
#
#
#
#
# remotes::install_github("mikejohnson51/AOI") # suggested!
# remotes::install_github("mikejohnson51/climateR")
#
# aoi <- aoi_get(state = "Oklahoma")
# plot(bb)
# class(bb)
# rain <- climateR::getGridMET(AOI = bb, "tmax", startDate = "2010-01-01")
#
# mapview::mapView(bb)
#
# tmax <- rain$tmax[[1]]
#
#
# leaflet::leaflet() %>%
#   addProviderTiles(providers$OpenStreetMap) %>%
#   leaflet::addRasterImage(x = tmax)
#   addPolygons(data = polylist$tmax, fillColor = ~polylist$tmax$values, color = "red") %>%
#   leaflet::add (data = rain$prcp[[1]])
#
#
# runoff <- climateR::getTerraClim(AOI = pt, param = "q",
#                                  startDate = "1993-01-01",
#                                  endDate = "2015-01-01")
# polylist = lapply(as.list(rain), rasterToPolygons)
#
# polylist$tmax
#
# mapview::mapview(polylist$tmax)
#
# runoff$date <- paste0(runoff$date, "-01")
# runoff$date <- as.Date(runoff$date)
# rownames(runoff) <- runoff$date
# runoff <- select(runoff, q)
#
# dygraph(data = runoff)
#
#
#
