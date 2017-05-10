# rm(list = ls())
#
# library(sf)
# library(ggplot2)
# library(dplyr)
# #
#
# ices_url <- "http://gis.ices.dk/shapefiles/ICES_areas.zip"
# eco_url <- "http://gis.ices.dk/shapefiles/ICES_ecoregions.zip"
# # eu_url <- "http://biogeo.ucdavis.edu/data/gadm2.8/gadm28.shp.zip"
#
# tmp_path <- tempdir()
# eu_file <- "~/git/ices-dk/fisheryO/inst/extdata/EU_Country.zip"
# unzip(eu_file, exdir = tmp_path)
#
# get_map <- function(URL) {
#   tmp_file <- tempfile(fileext = ".zip")
#   download.file(url = URL,
#                 destfile = tmp_file,
#                 mode = "wb", quiet = TRUE)
#   unzip(tmp_file, exdir = tmp_path)
# }
#
# lapply(list(ices_url,
#             eco_url), get_map)
#
# ices_areas <- sf::st_read(dsn = tmp_path, layer = "ICES_Areas_20160601_dense", quiet = FALSE)
# eu_areas <- sf::st_read(dsn = tmp_path, layer = "EU_Country", quiet = FALSE)
# eco_areas <- sf::st_read(dsn = tmp_path, layer = "ICES_ecoregions_20150113_no_land", quiet = FALSE)
#
# # Establish the CRS, let's use the Lambert Azimuthal Equal Area, centered at 52 N and 10 E
# eu_areas <- sf::st_transform(eu_areas, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
#
# colnames(ices_areas)
# colnames(eu_areas)
# colnames(eco_areas)
#
# st_bbox(ices_areas)
#
# eu_areas <- eu_areas %>%
#   filter(CONTINENT == "Europe")
#
# eco_areas <- eco_areas %>%
#   filter(Ecoregion == "Greater North Sea")
#
# ices_areas <- ices_areas %>%
#   filter(SubArea == "4")
#
# ggplot() +
#   geom_sf(data = eu_areas) +
#   geom_sf(data = ices_areas, fill = "yellow", alpha = 0.2) +
#   geom_sf(data = eco_areas) +
#   coord_sf +
#   theme_bw()
#
