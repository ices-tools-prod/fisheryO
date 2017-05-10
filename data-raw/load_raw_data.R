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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: ICES official catch statistics (1950-2010) #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
historicURL <- "http://ices.dk/marine-data/Documents/CatchStats/HistoricalLandings1950-2010.zip"
tmpFileHistoric <- tempfile(fileext = ".zip")
download.file(historicURL, destfile = tmpFileHistoric, mode = "wb", quiet = TRUE)
catch_data_historical <- read.csv(unz(tmpFileHistoric, "ICES_1950-2010.csv"),
                                  stringsAsFactors = FALSE,
                                  header = TRUE,
                                  fill = TRUE,
                                  na.strings = c("...", "-", "ns", "."))
devtools::use_data(catch_data_historical)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: ICES official catch statistics (2006-2014) #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

catchURL <- "http://ices.dk/marine-data/Documents/CatchStats/OfficialNominalCatches.zip"
tmpFileCatch <- tempfile(fileext = ".zip")
download.file(catchURL, destfile = tmpFileCatch, mode = "wb", quiet = TRUE)
catch_data_official <- read.csv(unz(tmpFileCatch, "ICESCatchDataset2006-2014.csv"),
                                stringsAsFactors = FALSE,
                                header = TRUE,
                                fill = TRUE)
# remove columns with all NA
catch_data_official <- Filter(function(x)!all(is.na(x)), catch_data_official)

devtools::use_data(catch_data_official)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: FAO species names and labels #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

spURL <- "ftp://ftp.fao.org/FI/STAT/DATA/ASFIS_sp.zip"
tmpFileSp <- tempfile(fileext = ".zip")
download.file(spURL, destfile = tmpFileSp, mode = "wb", quiet = TRUE)
species_list <- read.delim("~/git/ices-dk/fisheryO/inst/extdata/ASFIS_sp_Feb_2016.txt",
                           fill = TRUE,
                           stringsAsFactors = FALSE,
                           header = TRUE,
                           na.strings = "")
devtools::use_data(species_list, overwrite = TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: STECF Effort and Catch tables #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

STECF_effort_data <- readRDS("data-raw/STECF_effort_data.rds")
devtools::use_data(STECF_effort_data, overwrite = TRUE)

STECF_landings_data <- readRDS("data-raw/STECF_landings_data.rds")
devtools::use_data(STECF_landings_data, overwrite = TRUE)

