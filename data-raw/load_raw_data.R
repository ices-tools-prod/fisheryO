
load_raw_data <- function(raw_data = c("all", 
                                       "stock_list_raw", 
                                       "sag_summary_raw", "sag_refpts_raw", "sag_keys_raw", "sag_stock_status_raw",
                                       "ices_catch_official_raw", "ices_catch_historical_raw",
                                       "species_list_raw",
                                       "stecf_effort_raw", "stecf_landings_raw",
                                       "europe_shape", "ices_shape", "eco_shape")) {
  
  raw_names <- c("stock_list_raw", 
                 "sag_summary_raw", "sag_refpts_raw", "sag_keys_raw", "sag_stock_status_raw",
                 "ices_catch_official_raw", "ices_catch_historical_raw",
                 "species_list_raw",
                 "stecf_effort_raw", "stecf_landings_raw",
                 "europe_shape", "ices_shape", "eco_shape")
  
  raw_data <- match.arg(tolower(raw_data), c("all", raw_names),
                        several.ok = TRUE)
  
  if(any(raw_data %in% "all")) {
    raw_data <- raw_names
  }
  
  data("date_data", package = "fisheryO")

  # date_data$data_file <- as.character(date_data$data_file)
  # date_data$date <- as.Date(as.character(date_data$date))
  date_data_names <- raw_data[!raw_data %in% grep("stecf", raw_data, value = TRUE)]
  date_data$date[date_data$data_file %in% date_data_names] <- as.Date(as.character(Sys.Date()))
  
  devtools::use_data(date_data, overwrite = TRUE)
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #### DATA SOURCE: ICES Stock Database ####
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  if("stock_list_raw" %in% raw_data) {
    stock_list_raw <- jsonlite::fromJSON("http://sd.ices.dk/services/odata3/StockListDWs3",
                                         simplifyDataFrame = TRUE)$value
    devtools::use_data(stock_list_raw, overwrite = TRUE)
  }
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #### DATA SOURCE: ICES Stock Assessment Graphs Database ####
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  if("sag_summary_raw" %in% raw_data){
    sag_summary_raw <- icesSAG::getSAG(stock = NULL,
                                       year = 2014:2017,
                                       data = "summary",
                                       combine = TRUE)
    devtools::use_data(sag_summary_raw, overwrite = TRUE)
  }
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #### DATA SOURCE: ICES Stock Assessment Graphs Database ####
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  if("sag_refpts_raw" %in% raw_data) {
    sag_refpts_raw <- icesSAG::getSAG(stock = NULL,
                                      year = 2014:2017,
                                      data = "refpts",
                                      combine = TRUE)
    devtools::use_data(sag_refpts_raw, overwrite = TRUE)
  }
  
  if("sag_keys_raw" %in% raw_data) {
    
    sag_keys_raw <- do.call("rbind", lapply(2014:2017,
                                            function(x) icesSAG::findAssessmentKey(stock = NULL,
                                                                                   year = x,
                                                                                   full = TRUE)[, c("AssessmentYear",
                                                                                                    "AssessmentKey",
                                                                                                    "StockKeyLabel")]))
    devtools::use_data(sag_keys_raw, overwrite = TRUE)
  }
  
  if("sag_stock_status_raw" %in% raw_data){
    get_stock_status <- function(assessmentKey) {
      dat <- icesSAG::getStockStatusValues(assessmentKey)[[1]]
      if(is.null(dat)) stop(paste0("NULL value returned for assessmentKey = ", assessmentKey))
      dat
    }
    
    sag_stock_status_raw <- sag_keys_raw %>%
      mutate(stock_status = purrr::map(.x = AssessmentKey, purrr::possibly(get_stock_status, otherwise = NA_real_))) %>%
      filter(!is.na(stock_status)) %>%
      tidyr::unnest(stock_status)
    
    devtools::use_data(sag_stock_status_raw, overwrite = TRUE)
  }
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #### DATA SOURCE: ICES official catch statistics (1950-2010) #### 
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  if("ices_catch_historical_raw" %in% raw_data){
    historicURL <- "http://ices.dk/marine-data/Documents/CatchStats/HistoricalLandings1950-2010.zip"
    tmpFileHistoric <- tempfile(fileext = ".zip")
    download.file(historicURL, destfile = tmpFileHistoric, mode = "wb", quiet = TRUE)
    ices_catch_historical_raw <- read.csv(unz(tmpFileHistoric, "ICES_1950-2010.csv"),
                                          stringsAsFactors = FALSE,
                                          header = TRUE,
                                          fill = TRUE,
                                          na.strings = c("...", "-", "ns", "."))
    devtools::use_data(ices_catch_historical_raw, overwrite = TRUE)
  }
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #### DATA SOURCE: ICES official catch statistics (2006-2015) ####
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  if("ices_catch_official_raw" %in% raw_data) {
    catchURL <- "http://ices.dk/marine-data/Documents/CatchStats/OfficialNominalCatches.zip"
    tmpFileCatch <- tempfile(fileext = ".zip")
    download.file(catchURL, destfile = tmpFileCatch, mode = "wb", quiet = TRUE)
    ices_catch_official_raw <- read.csv(unz(tmpFileCatch,
                                            grep("ICESCatchDataset.*.csv", unzip(tmpFileCatch,
                                                                                 list = TRUE)$Name,
                                                 value = TRUE)),
                                        stringsAsFactors = FALSE,
                                        header = TRUE,
                                        fill = TRUE)
    # remove columns with all NA
    ices_catch_official_raw <- Filter(function(x)!all(is.na(x)), ices_catch_official_raw)
    
    devtools::use_data(ices_catch_official_raw, overwrite = TRUE)
  }
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #### DATA SOURCE: FAO species names and labels ####
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  if("species_list_raw" %in% raw_data){
    spURL <- "http://www.fao.org/fishery/static/ASFIS/ASFIS_sp.zip"
    tmpFileSp <- tempfile(fileext = ".zip")
    download.file(spURL, destfile = tmpFileSp, mode = "wb", quiet = TRUE)
    FAO_file <- grep(".*txt", unzip(tmpFileSp,list = TRUE)$Name, value = TRUE)
    species_list_raw <- read.delim(unz(tmpFileSp, FAO_file),
                                   fill = TRUE,
                                   stringsAsFactors = FALSE,
                                   header = TRUE,
                                   na.strings = "")
    devtools::use_data(species_list_raw, overwrite = TRUE)
  }
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #### DATA SOURCE: STECF Effort and Catch tables ####
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  
  if("stecf_effort_raw" %in% raw_data) {
    stecf_effort_raw <- readRDS("data-raw/STECF_effort_data.rds")
    devtools::use_data(stecf_effort_raw, overwrite = TRUE)
  }
  if("stecf_landings_raw" %in% raw_data) {
    stecf_landings_raw <- readRDS("data-raw/STECF_landings_data.rds")
    devtools::use_data(stecf_landings_raw, overwrite = TRUE)
  }
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #### DATA SOURCE: ICES Area and ecoregion shapefiles ####
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  
  get_map <- function(URL) {
    tmp_file <- tempfile(fileext = ".zip")
    download.file(url = URL,
                  destfile = tmp_file,
                  mode = "wb", quiet = TRUE)
    unzip(tmp_file, exdir = tmp_path)
  }
  
  if("ices_shape" %in% raw_data) {
    tmp_path <- tempdir()
    get_map("http://gis.ices.dk/shapefiles/ICES_areas.zip")
    layer_name <- grep("ICES_Areas", gsub("\\..*", "", list.files(tmp_path)), value = TRUE)[1]
    ices_shape <- sf::st_read(dsn = tmp_path, layer = layer_name, quiet = FALSE)
    devtools::use_data(ices_shape, compress='xz', overwrite = TRUE)
  }
  
  if("ices_eco" %in% raw_data) {
    
    tmp_path <- tempdir()
    get_map("http://gis.ices.dk/shapefiles/ICES_ecoregions.zip")
    layer_name <- grep("ICES_ecoregions", gsub("\\..*", "", list.files(tmp_path)), value = TRUE)[1]
    eco_shape <- sf::st_read(dsn = tmp_path, layer = layer_name, quiet = FALSE)
    devtools::use_data(eco_shape, compress='xz', overwrite = TRUE)
  }
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #### DATA SOURCE: rnaturalearth 10 ####
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  if("europe_shape" %in% raw_data) {
    europe_shape <- rnaturalearth::ne_countries(scale = 10,
                                                type = "countries",
                                                continent = "europe",
                                                returnclass = "sf")[, c("iso_a3", "iso_n3", "admin", "geometry")]
    
    devtools::use_data(europe_shape, compress='xz', overwrite = TRUE)
  }
  
}

load_raw_data(raw_data = "all")
