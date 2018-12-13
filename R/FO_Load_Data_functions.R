
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #### DATA SOURCE: ICES Stock Database ####
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
 #change this function to use icesSD package. 
    load_sid <- function(){
    stock_list_raw <- jsonlite::fromJSON("http://sd.ices.dk/services/odata4/StockListDWs4",
                                              simplifyDataFrame = TRUE)$value
    
    library(dplyr)
    stock_list_raw <<- unique(stock_list_raw)
    }
    
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #### DATA SOURCE: ICES Stock Assessment Graphs Database ####
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  
    #modified so you can state the active year and get the previous 3.
    
    load_sag <-  function(active_year){
    year <- ((active_year-3):active_year)
    sag_summary_raw <- icesSAG::getSAG(stock = NULL,
                                       # year = 2014:2017,
                                       year,
                                       data = "summary",
                                       combine = TRUE)
  
    # sag_summary_raw <<- sag_summary_raw %>% filter(Purpose %in% c("Advice", "InitAdvice"))
    sag_summary_raw <<- sag_summary_raw %>% filter(Purpose %in% c("Advice"))

    
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #### DATA SOURCE: ICES Stock Assessment Graphs Database ####
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
 
    sag_refpts_raw <- icesSAG::getSAG(stock = NULL,
                                      # year = 2014:2017,
                                      year ,
                                      purpose = "Advice",
                                      data = "refpts",
                                      combine = TRUE)
  
    sag_refpts_raw <<- unique (sag_refpts_raw)
    
     #cambio aqui tambien 2014:2017 por 2015:2018
    sag_keys_raw <- do.call("rbind", lapply(year,
                                            function(x) icesSAG::findAssessmentKey(stock = NULL,
                                                                                   year = x,
                                                                                   full = TRUE)[, c("AssessmentYear",
                                                                                                    "AssessmentKey",
                                                                                                    "StockKeyLabel", "Purpose")]))
    #to get rid of double 
    # sag_keys_raw <- sag_keys_raw %>% filter(Purpose %in% c("Advice", "InitAdvice"))
    sag_keys_raw <- sag_keys_raw %>% filter(Purpose %in% c("Advice"))
    sag_keys_raw <<- sag_keys_raw[,-4]
  
  # if("sag_stock_status_raw" %in% raw_data){
    get_stock_status <- function(assessmentKey) {
      dat <- icesSAG::getStockStatusValues(assessmentKey)[[1]]
      if(is.null(dat)) stop(paste0("NULL value returned for assessmentKey = ", assessmentKey))
      dat
    }
    
    sag_stock_status_raw <- sag_keys_raw %>%
      mutate(stock_status = purrr::map(.x = AssessmentKey, purrr::possibly(get_stock_status, otherwise = NA_real_))) %>%
      filter(!is.na(stock_status)) %>%
      tidyr::unnest(stock_status)
    
  sag_stock_status_raw <<- unique(sag_stock_status_raw)
  }
    
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #### DATA SOURCE: ICES official catch statistics (1950-2010) #### 
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
    
  #historical catches should be in the package
    historicURL <- "http://ices.dk/marine-data/Documents/CatchStats/HistoricalLandings1950-2010.zip"
    tmpFileHistoric <- tempfile(fileext = ".zip")
    download.file(historicURL, destfile = tmpFileHistoric, mode = "wb", quiet = TRUE)
    ices_catch_historical_raw <- read.csv(unz(tmpFileHistoric, "ICES_1950-2010.csv"),
                                          stringsAsFactors = FALSE,
                                          header = TRUE,
                                          fill = TRUE,
                                          na.strings = c("...", "-", "ns", "."))
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #### DATA SOURCE: ICES official catch statistics (2006-201X) ####
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
    load_official_catches<- function(){
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
    ices_catch_official_raw <<- Filter(function(x)!all(is.na(x)), ices_catch_official_raw)
    }
    
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #### DATA SOURCE: ICES preliminary catch statistics (2017) ###
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  
    load_preliminary_catches <- function (){
    prelimcatchURL<- "http://data.ices.dk/rec12/download/2017preliminaryCatchStatistics.csv"
    tmpFilePrelimCatch <- tempfile(fileext = ".csv")
    download.file(prelimcatchURL, destfile = tmpFilePrelimCatch, mode = "wb", quiet = TRUE)
    ices_catch_preliminary_raw <<- read.csv(tmpFilePrelimCatch,
                                        stringsAsFactors = FALSE,
                                        header = TRUE,
                                        fill = TRUE)
    }

  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #### DATA SOURCE: FAO species names and labels ####
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
    #this should be in the package
    spURL <- "http://www.fao.org/fishery/static/ASFIS/ASFIS_sp.zip"
    tmpFileSp <- tempfile(fileext = ".zip")
    download.file(spURL, destfile = tmpFileSp, mode = "wb", quiet = TRUE)
    FAO_file <- grep(".*txt", unzip(tmpFileSp,list = TRUE)$Name, value = TRUE)
    species_list_raw <- read.delim(unz(tmpFileSp, FAO_file),
                                   fill = TRUE,
                                   stringsAsFactors = FALSE,
                                   header = TRUE,
                                   na.strings = "")
   
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #### DATA SOURCE: STECF Effort and Catch tables ####
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #not sure what to do about this, how could we download it each time?
    stecf_effort_raw <- readRDS("data-raw/STECF_effort_data.rds")
    devtools::use_data(stecf_effort_raw, overwrite = TRUE)
 
    StecfURL<- "https://visualise.jrc.ec.europa.eu/vizql/t/dcf/w/effort_public_report/v/effort/vud/sessions/1D0D3F77E59B4727A20DB11E2275510F-1:1/views/14425121001966086373_5796457947632830931?csv=true"
    stecf<- data.frame()
    download.file(StecfURL, destfile = stecf,mode = "wb", quiet = TRUE)
    
    stecf_landings_raw <- readRDS("data-raw/STECF_landings_data.rds")
    devtools::use_data(stecf_landings_raw, overwrite = TRUE)
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #### DATA SOURCE: ICES Area and ecoregion shapefiles ####
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  
  #this should be in the package, all the geo data
    
  #   get_map <- function(URL) {
  #   tmp_file <- tempfile(fileext = ".zip")
  #   download.file(url = URL,
  #                 destfile = tmp_file,
  #                 mode = "wb", quiet = TRUE)
  #   unzip(tmp_file, exdir = tmp_path)
  # }
  # 
  #   tmp_path <- tempdir()
  #   get_map("http://gis.ices.dk/shapefiles/ICES_areas.zip")
  #   layer_name <- grep("ICES_Areas", gsub("\\..*", "", list.files(tmp_path)), value = TRUE)[1]
  #   ices_shape <- sf::st_read(dsn = tmp_path, layer = layer_name, quiet = FALSE)
  #   devtools::use_data(ices_shape, compress='xz', overwrite = TRUE)
  # 
  #   tmp_path <- tempdir()
  #   get_map("http://gis.ices.dk/shapefiles/ICES_ecoregions.zip")
  #   layer_name <- grep("ICES_ecoregions", gsub("\\..*", "", list.files(tmp_path)), value = TRUE)[1]
  #   eco_shape <- sf::st_read(dsn = tmp_path, layer = layer_name, quiet = FALSE)
  #   devtools::use_data(eco_shape, compress='xz', overwrite = TRUE)
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #### DATA SOURCE: rnaturalearth 10 #### SAME, in the package
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

    # europe_shape <- rnaturalearth::ne_countries(scale = 10,
    #                                             type = "countries",
    #                                             continent = "europe",
    #                                             returnclass = "sf")[, c("iso_a3", "iso_n3", "admin", "geometry")]
    
    
