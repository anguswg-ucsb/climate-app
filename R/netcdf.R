library(tidyverse)
library(RNetCDF)
library(tidync)
library(ncdf4)
nc <- RNetCDF::open.nc("https://thredds.ufokn.org/thredds/dodsC/hand/03/031001hand.nc")

catolgue = "http://thredds.northwestknowledge.net:8080/thredds/dodsC/"
cdm = "agg_met_pr_1979_CurrentYear_CONUS.nc"
url = paste0(catolgue, cdm, "#fillmismatch")
nc   = RNetCDF::open.nc(url)
X    = RNetCDF::var.get.nc(nc, "lon")
Y    = RNetCDF::var.get.nc(nc, "lat")
time = RNetCDF::var.get.nc(nc, "day")
AOI     = aoi_get(state = "CA")
(extent = st_bbox(AOI))
(crs    = st_crs(AOI)$proj4string)

x <- c(1:4, 0:5, 11)
which.min(x)
xmin = which.min(abs(sort(X) - extent$xmin)) - 1

(xmax = which.min(abs(sort(X) - extent$xmax)) - 1)

(ymin = which.min(abs(sort(Y) - extent$ymin)) - 1)

(ymax = which.min(abs(sort(Y) - extent$ymax)) - 1)
time = as.Date("2018-01-19") - as.Date("1979-01-01")
(time = as.numeric(time))

url = paste0(
  catolgue,
  cdm,
  "?precipitation_amount",
  "[", time, ":1:", time, "]",
  "[", ymin, ":1:", ymax, "]",
  "[", xmin, ":1:", xmax, "]",
  "#fillmismatch")

(nc    = RNetCDF::open.nc(url))
print.nc(nc)
