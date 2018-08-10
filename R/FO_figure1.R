#Packages required: rnaturalearth, rgeos, devtools(optional)
library(dplyr)

#1. LOAD DATA
#All these raw data can be saved in the local environment as they don´t change
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#1.1. DATA SOURCE: ICES Area and ecoregion shapefiles ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

get_map <- function(URL) {
  tmp_file <- tempfile(fileext = ".zip")
  download.file(url = URL,
                destfile = tmp_file,
                mode = "wb", quiet = TRUE)
  unzip(tmp_file, exdir = tmp_path)
}

tmp_path <- tempdir()
get_map("http://gis.ices.dk/shapefiles/ICES_areas.zip")
layer_name <- grep("ICES_Areas", gsub("\\..*", "", list.files(tmp_path)), value = TRUE)[1]
ices_shape <- sf::st_read(dsn = tmp_path, layer = layer_name, quiet = FALSE)
#Only if you want to save the file ices_shape in the data folder:
devtools::use_data(ices_shape, compress='xz', overwrite = TRUE)

tmp_path <- tempdir()
get_map("http://gis.ices.dk/shapefiles/ICES_ecoregions.zip")
layer_name <- grep("ICES_ecoregions", gsub("\\..*", "", list.files(tmp_path)), value = TRUE)[1]
eco_shape <- sf::st_read(dsn = tmp_path, layer = layer_name, quiet = FALSE)
#Only if you want to save the file eco_shape in the data folder:
devtools::use_data(eco_shape, compress='xz', overwrite = TRUE)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##1.2. DATA SOURCE: rnaturalearth 10 ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

europe_shape <- rnaturalearth::ne_countries(scale = 10,
                                            type = "countries",
                                            continent = "europe",
                                            returnclass = "sf")[, c("iso_a3", "iso_n3", "admin", "geometry")]

#################################
#2. CLEAN DATA
################
#Also clean files crs, eco_areas, ices_areas, europe_shape and centroids can 
#be saved in the local environment and directly go to PLOT everytime.

ecoregion = c("Greater North Sea Ecoregion","Baltic Sea Ecoregion",
                                          "Bay of Biscay and the Iberian Coast Ecoregion",
                                          "Celtic Seas Ecoregion","Icelandic Waters Ecoregion",
                                          "Norwegian Sea Ecoregion", "Barents Sea Ecoregion",
                                          "Arctic Ocean Ecoregion")
  
  crs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  
  eco_areas <- eco_shape %>%
    sf::st_transform(crs = crs) %>%
    mutate(Ecoregion = case_when(
      grepl("Baltic Sea", .$Ecoregion) ~ "Baltic Sea Ecoregion",
      grepl("Greater North Sea", .$Ecoregion) ~ "Greater North Sea Ecoregion",
      grepl("Bay of Biscay and the Iberian Coast", .$Ecoregion) ~ "Bay of Biscay and the Iberian Coast Ecoregion",
      grepl("Celtic Seas", .$Ecoregion) ~ "Celtic Seas Ecoregion",
      grepl("Iceland Sea", .$Ecoregion) ~ "Icelandic Waters Ecoregion",
      grepl("Norwegian Sea", .$Ecoregion) ~ "Norwegian Sea Ecoregion",
      grepl("Barents Sea", .$Ecoregion) ~ "Barents Sea Ecoregion",
      #grepl("Arctic Ocean", .$Ecoregion) ~ "Arctic Ocean Ecoregion",
      # ... add remaining ecoregions
      TRUE ~ "OTHER")) %>%
    filter(Ecoregion == ecoregion) %>%
    sf::st_sf()
  
  Area_27_baltic <- c("3.d.27", "3.d.25", "3.d.24",
                      "3.b.23", "3.c.22", "3.d.31",
                      "3.d.30", "3.d.32", "3.d.29",
                      "3.d.28.1", "3.d.28.2", "3.d.26")
  
  Area_27_ns <- c("3.a.20", "3.a.21",
                  "4.a", "4.b", "4.c",
                  "7.d", "7.e")
  
  #all these next area definitions need decissions on conflicts:
  
  Area_27_bob <- c("8.a", "8.b","8.c",
                   "8.d.2", "8.e.2", "9.a",
                   "9.b.2")
  Area_27_cs <- c("6.a", "6.b.2","7.a", "7.b", "7.c.2",
                  "7.f", "7.g", "7.h","7.j.2", "7.k.2")
  
  Area_27_is <- c("5.a.1", "5.a.2","12.a.4")
  
  Area_27_nw <- c("2.a.1", "2.a.2", "2.b.1", "2.b.2", "14.a")
  
  Area_27_br <- c("1.a", "1.b","2.a.2", "2.b.2")
  
  ices_areas <- ices_shape %>%
    sf::st_transform(crs = crs) %>%
    mutate(ECOREGION = case_when(
      .$Area_27 %in% Area_27_baltic ~ "Baltic Sea Ecoregion",
      .$Area_27 %in%  Area_27_ns ~ "Greater North Sea Ecoregion",
      .$Area_27 %in%  Area_27_bob ~ "Bay of Biscay and the Iberian Coast Ecoregion",
      .$Area_27 %in%  Area_27_cs ~ "Celtic Seas Ecoregion",
      .$Area_27 %in%  Area_27_is ~ "Icelandic Waters Ecoregion",
      
      #this nest line has to be turned on and off, because either areas are 
      # in norwegian or in barents
      .$Area_27 %in%  Area_27_nw ~ "Norwegian Sea Ecoregion",
      .$Area_27 %in%  Area_27_br ~ "Barents Sea Ecoregion",
      # ... add remaining ecoregions
      TRUE ~ "OTHER")) %>%
    sf::st_sf()
  
  # Centroids for labels
  ices_area_centroids <- sf::st_centroid(ices_areas)
  centroids <- data.frame(as.character(ices_area_centroids$Area_27),
                          ices_area_centroids$ECOREGION,
                          matrix(unlist(ices_area_centroids$geometry),
                                 ncol = 2,
                                 byrow = TRUE),
                          stringsAsFactors = FALSE)
  
  colnames(centroids) <- c("Area_27", "ECOREGION", "X", "Y")
  
  if (ecoregion == "Baltic Sea Ecoregion"){
    baltic_3a <- ices_areas %>%
      filter(SubArea == "3",
             Division == "a") %>%
      summarize(Area_27 = "3.a",
                ECOREGION = "Baltic Sea Ecoregion",
                geometry = sf::st_union(geometry)) %>%
      sf::st_sf() %>%
      sf::st_centroid()
    
    baltic_3a <- data.frame(baltic_3a$Area_27,
                            baltic_3a$ECOREGION,
                            matrix(unlist(baltic_3a$geometry),
                                   ncol = 2,
                                   byrow = TRUE),
                            stringsAsFactors = FALSE)
    
    colnames(baltic_3a) <- c("Area_27", "ECOREGION", "X", "Y")
    
    centroids <- bind_rows(centroids, baltic_3a)
  }
  
  centroids <- centroids %>%
    mutate(Area_27 = case_when(
      .$ECOREGION == "Baltic Sea Ecoregion" ~ sub("3.b.|3.c.|3.d.", "", .$Area_27),
      .$ECOREGION == "Greater North Sea Ecoregion" ~ as.character(.$Area_27),
      .$ECOREGION == "Bay of Biscay and the Iberian Coast Ecoregion" ~ as.character(.$Area_27),
      .$ECOREGION == "Celtic Seas Ecoregion" ~ as.character(.$Area_27),
      .$ECOREGION == "Icelandic Waters Ecoregion" ~ as.character(.$Area_27),
      .$ECOREGION == "Norwegian Sea Ecoregion" ~ as.character(.$Area_27),
      .$ECOREGION == "Barents Sea Ecoregion" ~ as.character(.$Area_27),
      TRUE ~ "OTHER"
    ))
  
  ices_areas <- ices_areas %>%
    filter(grepl(ecoregion, ECOREGION)) %>%
    sf::st_sf()
  
  centroids <- centroids %>%
    filter(grepl(ecoregion, ECOREGION))
  
#####################################  
  #3.PLOT
###########################  
  library(ggplot2)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # ICES Area and Ecoregion Map #
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  
 #set this up with the ecoregion everytime needed, and back again
  ecoregion = ecoregion [1]
  # ecoregion = c("Greater North Sea Ecoregion","Baltic Sea Ecoregion",
  #               "Bay of Biscay and the Iberian Coast Ecoregion",
  #               "Celtic Seas Ecoregion","Icelandic Waters Ecoregion",
  #               "Norwegian Sea Ecoregion", "Barents Sea Ecoregion",
  #               "Arctic Ocean Ecoregion")
  
  file_name <- gsub("\\s", "_", ecoregion)
  output_path <- "~/" 
   
  cap_lab <- labs(caption = "Made with Natural Earth and ICES Marine Data",
                      x = "",
                      y = "")
  xmin <- min(sf::st_bbox(eco_areas)[1], sf::st_bbox(ices_areas)[1])
  xmax <- max(sf::st_bbox(eco_areas)[3], sf::st_bbox(ices_areas)[3])
  ymin <- min(sf::st_bbox(eco_areas)[2], sf::st_bbox(ices_areas)[2])
  ymax <- max(sf::st_bbox(eco_areas)[4], sf::st_bbox(ices_areas)[4])
    
  xlims <- c(xmin, xmax)
  ylims <- c(ymin, ymax)
    
  p1 <- ggplot() +
    geom_sf(data = eco_areas, color = "grey90", fill = "gold") +
    # geom_sf(data = visahke, color = "grey80", fill = "gold") +
    geom_sf(data = europe_shape, fill = "grey80", color = "grey90") +
    geom_sf(data = ices_areas, color = "grey60", fill = "transparent") +
    geom_text(data = centroids, aes(x = X, y = Y, label = Area_27), size = 2.5) +
    #geom_text(data = visahke, aes(x = X, y = Y, label = Area_27), size = 2.5) +
    theme_bw(base_size = 8) +
    theme(plot.caption = element_text(size = 6),
          plot.subtitle = element_text(size = 7)) +
    coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
    cap_lab
    
    
  p1  
  ggsave(filename = paste0(output_path, file_name, ".png"),
         plot = p1,
         width = 178,
         height = 152,
         units = "mm",
         dpi = 300)  
