# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: ICES Stock Database #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
stock_list_raw <- jsonlite::fromJSON("http://sd.ices.dk/services/odata3/StockListDWs3",
                                     simplifyDataFrame = TRUE)$value
devtools::use_data(stock_list_raw)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: ICES Stock Assessment Graphs Database #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
sag_summary_raw <- icesSAG::getSAG(stock = NULL,
                                   year = 2014:2017,
                                   data = "summary",
                                   combine = TRUE)
devtools::use_data(sag_summary_raw)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: ICES Stock Assessment Graphs Database #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

sag_refpts_raw <- icesSAG::getSAG(stock = NULL,
                                  year = 2014:2017,
                                  data = "refpts",
                                  combine = TRUE)
devtools::use_data(sag_refpts_raw)


sag_keys_raw <- do.call("rbind", lapply(2014:2017,
                                    function(x) icesSAG::findAssessmentKey(stock = NULL,
                                                                           year = x,
                                                                           full = TRUE)[, c("AssessmentYear",
                                                                                            "AssessmentKey",
                                                                                            "StockKeyLabel")]))
devtools::use_data(sag_keys_raw)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: ICES official catch statistics (1950-2010) #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
historicURL <- "http://ices.dk/marine-data/Documents/CatchStats/HistoricalLandings1950-2010.zip"
tmpFileHistoric <- tempfile(fileext = ".zip")
download.file(historicURL, destfile = tmpFileHistoric, mode = "wb", quiet = TRUE)
ices_catch_historical_raw <- read.csv(unz(tmpFileHistoric, "ICES_1950-2010.csv"),
                                  stringsAsFactors = FALSE,
                                  header = TRUE,
                                  fill = TRUE,
                                  na.strings = c("...", "-", "ns", "."))
devtools::use_data(ices_catch_historical_raw)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: ICES official catch statistics (2006-2014) #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

catchURL <- "http://ices.dk/marine-data/Documents/CatchStats/OfficialNominalCatches.zip"
tmpFileCatch <- tempfile(fileext = ".zip")
download.file(catchURL, destfile = tmpFileCatch, mode = "wb", quiet = TRUE)
ices_catch_official_raw <- read.csv(unz(tmpFileCatch, "ICESCatchDataset2006-2014.csv"),
                                stringsAsFactors = FALSE,
                                header = TRUE,
                                fill = TRUE)
# remove columns with all NA
ices_catch_official_raw <- Filter(function(x)!all(is.na(x)), ices_catch_official_raw)

devtools::use_data(ices_catch_official_raw)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: FAO species names and labels #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

spURL <- "ftp://ftp.fao.org/FI/STAT/DATA/ASFIS_sp.zip"
tmpFileSp <- tempfile(fileext = ".zip")
download.file(spURL, destfile = tmpFileSp, mode = "wb", quiet = TRUE)
species_list_raw <- read.delim(unz(tmpFileSp, "ASFIS_sp_Feb_2016.txt"),
                           fill = TRUE,
                           stringsAsFactors = FALSE,
                           header = TRUE,
                           na.strings = "")
devtools::use_data(species_list_raw)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: STECF Effort and Catch tables #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

stecf_effort_raw <- readRDS("data-raw/STECF_effort_data.rds")
devtools::use_data(stecf_effort_raw)

stecf_landings_raw <- readRDS("data-raw/STECF_landings_data.rds")
devtools::use_data(stecf_landings_raw)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: ICES Area and ecoregion shapefiles #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

ices_url <- "http://gis.ices.dk/shapefiles/ICES_areas.zip"
eco_url <- "http://gis.ices.dk/shapefiles/ICES_ecoregions.zip"

tmp_path <- tempdir()

get_map <- function(URL) {
  tmp_file <- tempfile(fileext = ".zip")
  download.file(url = URL,
                destfile = tmp_file,
                mode = "wb", quiet = TRUE)
  unzip(tmp_file, exdir = tmp_path)
}

lapply(list(ices_url,
            eco_url), get_map)

ices_shape <- sf::st_read(dsn = tmp_path, layer = "ICES_Areas_20160601_dense", quiet = FALSE)
eco_shape <- sf::st_read(dsn = tmp_path, layer = "ICES_ecoregions_20150113_no_land", quiet = FALSE)

devtools::use_data(ices_shape, compress='xz', overwrite = TRUE)
devtools::use_data(eco_shape, compress='xz', overwrite = TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: rnaturalearth 10  #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

europe_shape <- rnaturalearth::ne_countries(scale = 10,
                                        type = "countries",
                                        continent = "europe",
                                        returnclass = "sf")[, c("iso_a3", "iso_n3", "admin", "geometry")]

devtools::use_data(europe_shape, compress='xz', overwrite = TRUE)
