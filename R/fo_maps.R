rm(list = ls())

library(sf)
library(ggplot2)
library(dplyr)
#

ices_url <- "http://gis.ices.dk/shapefiles/ICES_areas.zip"
eco_url <- "http://gis.ices.dk/shapefiles/ICES_ecoregions.zip"
eu_url <- "http://biogeo.ucdavis.edu/data/gadm2.8/gadm28.shp.zip"

tmp_path <- tempdir()

get_map <- function(URL) {
  tmp_file <- tempfile(fileext = ".zip")
  download.file(url = URL,
                destfile = tmp_file,
                mode = "wb", quiet = TRUE)
  unzip(tmp_file, exdir = tmp_path)
}

lapply(list(ices_url,
            eco_url,
            eu_url), get_map)


ices_areas <- sf::st_read(dsn = tmp_path, layer = "ICES_Areas_20160601_dense", quiet = FALSE)
eu_areas <- sf::st_read(dsn = tmp_path, layer = "gadm28", quiet = FALSE)
eco_areas <- sf::st_read(dsn = tmp_path, layer = "ICES_ecoregions_20150113_no_land", quiet = FALSE)

library(dplyr)
str(eu_areas)
eu_areas <- eu_areas %>%
  filter(CONTINENT == "Europe")

  ggplot(eu_areas) +
    geom_sf()

  # If not supplied, coord_sf() will take the CRS from the first layer
  # and automatically transform all other layers to use that CRS. This
  # ensures that all data will correctly line up
  nc_3857 <- sf::st_transform(nc, "+init=epsg:3857")
  ggplot() +
    geom_sf(data = nc) +
    geom_sf(data = nc_3857, colour = "red", fill = NA)

  # You can also use layers with x and y aesthetics: these are
  # assumed to already be in the common CRS.
  ggplot(nc) +
    geom_sf() +
    annotate("point", x = -80, y = 35, colour = "red", size = 4)

  # Thanks to the power of sf, ageom_sf nicely handles varying projections
  # setting the aspect ratio correctly.
  library(maps)
  world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
  ggplot() + geom_sf(data = world1)

  world2 <- sf::st_transform(
    world1,
    "+proj=laea +y_0=0 +lon_0=155 +lat_0=-90 +ellps=WGS84 +no_defs"
  )
  ggplot() + geom_sf(data = world2)
# }
