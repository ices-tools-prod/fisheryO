# rm(list = ls())
# #
# # devtools::install_github("edzer/sfr")
# # devtools::install_github("tidyverse/ggplot2")
# library(sf)
# library(ggplot2)
# library(dplyr)
# library(rnaturalearth)
# library(rgeos)
# library(mapproj)
# #
# #
# # ices_url <- "http://gis.ices.dk/shapefiles/ICES_areas.zip"
# # eco_url <- "http://gis.ices.dk/shapefiles/ICES_ecoregions.zip"
# # # eu_url <- "http://biogeo.ucdavis.edu/data/gadm2.8/gadm28.shp.zip"
# # #
# # tmp_path <- tempdir()
# # # eu_file <- "~/git/ices-dk/fisheryO/inst/extdata/EU_Country.zip"
# # # unzip(eu_file, exdir = tmp_path)
# # #
# # get_map <- function(URL) {
# #   tmp_file <- tempfile(fileext = ".zip")
# #   download.file(url = URL,
# #                 destfile = tmp_file,
# #                 mode = "wb", quiet = TRUE)
# #   unzip(tmp_file, exdir = tmp_path)
# # }
# # #
# # lapply(list(ices_url,
# #             eco_url), get_map)
# # #
# # ices_areas <- sf::st_read(dsn = tmp_path, layer = "ICES_Areas_20160601_dense", quiet = FALSE)
# # eco_areas <- sf::st_read(dsn = tmp_path, layer = "ICES_ecoregions_20150113_no_land", quiet = FALSE)
# # eu_areas <- sf::st_read(dsn = tmp_path, layer = "EU_Country", quiet = FALSE)
#
# # crs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"
#
# # europe_shape <- rnaturalearth::ne_countries(scale = 10,
# #                          type = "countries",
# #                          continent = "europe", returnclass = "sf") %>%
# #   dplyr::select(iso_a3, iso_n3, admin, geometry)
#
# rm(list = ls())
#
# ecoregion = "Baltic Sea"
#
#
# library(sf)
# library(dplyr)
# raw_data <- c("europe_shape",
#               "ices_shape",
#               "eco_shape")
# data(list = raw_data, envir = environment())
#
# crs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#
# eco_areas <- eco_shape %>%
#   filter(grepl(ecoregion, Ecoregion)) %>%
#   st_transform(crs = crs)
#
# Area_27_baltic <- c("3.d.27", "3.d.25", "3.d.24",
#                     "3.b.23", "3.c.22", "3.d.31",
#                     "3.d.30", "3.d.32", "3.d.29",
#                     "3.d.28.1", "3.d.28.2", "3.d.26")
#
# Area_27_ns <- c("3.a.20", "3.a.21",
#                 "4.a", "4.b", "4.c",
#                 "7.d")
#
#
# ices_areas <- ices_shape %>%
#   mutate(ECOREGION = case_when(
#     .$Area_27 %in% Area_27_baltic ~ "Baltic Sea Ecoregion",
#     .$Area_27 %in%  Area_27_ns ~ "Greater North Sea Ecoregion",
#     # ... add remaining ecoregions
#     TRUE ~ "OTHER")) %>%
#   st_transform(crs = crs)
#
# # Centroids for labels
# ices_area_centroids <- sf::st_centroid(ices_areas)
# centroids <- data.frame(ices_area_centroids$Area_27,
#                         ices_area_centroids$ECOREGION,
#                         matrix(unlist(ices_area_centroids$geometry),
#                                ncol = 2,
#                                byrow = TRUE))
#
# colnames(centroids) <- c("Area_27", "ECOREGION", "X", "Y")
#
# centroids <- centroids %>%
#   mutate(Area_27 = case_when(
#     .$ECOREGION == "Baltic Sea Ecoregion" ~ sub("3.b.|3.c.|3.d.", "", .$Area_27),
#     .$ECOREGION == "Greater North Sea Ecoregion" ~ as.character(.$Area_27),
#     TRUE ~ "OTHER"
#   ))
#
# ices_areas <- ices_areas %>%
#   filter(grepl(ecoregion, ECOREGION))
#
# centroids <- centroids %>%
#   filter(grepl(ecoregion, ECOREGION))
#
# xlims <- st_bbox(ices_areas)[c(1,3)]
# ylims <- st_bbox(ices_areas)[c(2,4)]
#
# p1 <- ggplot() +
#   geom_sf(data = europe_shape, fill = "grey80", color = "grey90") +
#   geom_sf(data = eco_areas, color = "grey80", fill = "white") +
#   geom_sf(data = ices_areas, color = "grey60", fill = "gold", alpha = 0.25) +
#   geom_text(data = centroids, aes(x = X, y = Y, label = Area_27), size = 2.5) +
#   theme_bw(base_size = 8) +
#   theme(plot.caption = element_text(size = 6),
#         plot.subtitle = element_text(size = 7)) +
#   coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
#   labs(title = "Area definitions", x = "", y = "",
#        subtitle = "ICES areas (dark lines) and Baltic Sea ecoregion\n(yellow shading)",
#        caption = "Made with Natural Earth and ICES Marine Data")
#
# ggsave(filename = "~/baltic_test.png",
#        plot = p1,
#        width = 89,
#        height = 100.5,
#        units = "mm",
#        dpi = 300)
