rm(list = ls())
#
library(rgdal)
library(rgeos)
library(dplyr)
library(tidyr)
library(readxl)
library(countrycode)
#
################################
# Create Country look-up table #
################################
#
## NOW ZIPPED ##
homeFolder <- "D:/Profiles/Scott/My Documents/rCode/data/"
localDir <- paste0(homeFolder, 'mapData')
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: EU_Country shapefile #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
countryName <- "EU_Country"
counDataRaw <- readOGR(dsn = localDir, layer = countryName)
shapeNames <- data.frame(shapeNames = as.character(unique(counDataRaw@data$COUNTRY)),
                         stringsAsFactors = FALSE)
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: ICES official catch statistics (2006-2014) #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
catchURL <- "http://ices.dk/marine-data/Documents/CatchStats/OfficialNominalCatches.zip"
tmpFileCatch <- tempfile(fileext = ".zip")
download.file(catchURL, destfile = tmpFileCatch, mode = "wb", quiet = TRUE)
catchDat <- read.csv(unz(tmpFileCatch,
                         "ICESCatchDataset2006-2014.csv"),
                     stringsAsFactors = FALSE, header = TRUE, fill = TRUE)
catchNames <- data.frame(catchNames = unique(catchDat$Country),
                            stringsAsFactors = FALSE)
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: ICES official catch statistics #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
historicURL <- "http://ices.dk/marine-data/Documents/CatchStats/HistoricalLandings1950-2010.zip"
tmpFileHistoric <- tempfile(fileext = ".zip")
download.file(historicURL, destfile = tmpFileHistoric, mode = "wb", quiet = TRUE)
historicList <- read.csv(unz(tmpFileHistoric,
                             "ICES_1950-2010.csv"),
                         stringsAsFactors = FALSE, header = TRUE, fill = TRUE,
                         na.strings = c("...", "-", "ns", "."))
historicNames <- data.frame(historicNames = unique(historicList$Country),
                            stringsAsFactors = FALSE)
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: STECF Fishing Effort and Regimes #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#
effortURL <- "http://stecf.jrc.ec.europa.eu/documents/43805/870977/2014_STECF+14-20+-+Fishing+Effort+Regimes+data+tables.zip"
tmpFileEffort <- tempfile(fileext = ".zip")
download.file(effortURL, destfile = tmpFileEffort, mode = "wb", quiet = TRUE)
effortDat <- read_excel(unzip(tmpFileEffort,
                              files = "Electronic_appendices/Effort_trends_by_country.xlsx"),
                        sheet = "kWdays at sea")
stecfCatchDat <- read_excel(unzip(tmpFileEffort,
                                  files = "Electronic_appendices/Landings_Discards_Discard-rates.xlsx"),
                            sheet = "Land_Disc_Disc-rate_by_Country")
stecfNames <- data.frame(stNames = unique(c(stecfCatchDat$country,
                                               effortDat$country)),
                         stringsAsFactors = FALSE)
#
# ~~~ Clean up country codes from STECF data ~~~ #
stecfNames$stecfNames[stecfNames$stNames == "DEN"] <- "Denmark"
stecfNames$stecfNames[stecfNames$stNames %in% c("SCO", "ENG","GBG", "GBJ","IOM", "NIR")] <- "United Kingdom"
stecfNames$stecfNames[stecfNames$stNames == "GER"] <- "Germany"
stecfNames$stecfNames[stecfNames$stNames == "LAT"] <- "Latvia"
stecfNames$stecfNames[stecfNames$stNames == "LIT"] <- "Lithuania"
stecfNames$stecfNames[stecfNames$stNames == "NED"] <- "Netherlands"
stecfNames$stecfNames[is.na(stecfNames$stecfNames)]<- countrycode(stecfNames$stNames[is.na(stecfNames$stecfNames)],
                                                             "iso3c", "country.name")
stecfNames$ISO3 <- countrycode(stecfNames$stecfNames, "country.name", "iso3c")
#
# ~~~ Clean up historic ICES data  ~~~ #
historicNames$hNames[historicNames$historicNames %in% c("UK - Eng+Wales+N.Irl.",
                                                        "UK - England & Wales",
                                                        "UK - N. Ireland",
                                                        "UK - Scotland",
                                                        "Channel Is.- Guernsey",
                                                        "Channel Is.- Jersey",
                                                        "Channel Islands (ns)",
                                                        "Isle of Man")] <- "United Kingdom"
historicNames$hNames[historicNames$historicNames %in% unique(historicNames$historicNames)[grepl('^Germany',
                                                                               unique(historicNames$historicNames))]] <- "Germany"
historicNames$hNames[historicNames$historicNames %in% c("Faeroe Islands")] <- "Faroe Islands"
historicNames$hNames[historicNames$historicNames %in% c("Un. Sov. Soc. Rep.")] <- "Russia"
historicNames$hNames[is.na(historicNames$hNames)] <- historicNames$historicNames[is.na(historicNames$hNames)]
historicNames$ISO3 <- countrycode(historicNames$hNames, "country.name", "iso3c")
#
# ~~~ Clean up country codes from EU Shapefile  ~~~ #
shapeNames$sNames[shapeNames$shapeNames == "Azores"] <- "Portugal"
shapeNames$sNames[is.na(shapeNames$sNames)] <- shapeNames$shapeNames[is.na(shapeNames$sNames)]
shapeNames$ISO3 <- countrycode(shapeNames$sNames, "country.name", "iso3c")
#
catchNames$cNames <- countrycode(catchNames$catchNames, "iso2c", "country.name")
catchNames$cNames[catchNames$cNames %in% c("Guernsey",
                                           "Isle of Man",
                                           "Jersey")] <- "United Kingdom"
catchNames$ISO3 <- countrycode(catchNames$cNames, "country.name", "iso3c")
#
# Merge the data together and clean up
histShape <- merge(historicNames, shapeNames, by = "ISO3", all = TRUE)
histShapeStecf <- merge(histShape, stecfNames, by = "ISO3", all = TRUE)
histShapeStecfCatch <- merge(histShapeStecf, catchNames, by = "ISO3", all = TRUE)
histShapeStecfCatch$COUNTRY <- countrycode(histShapeStecfCatch$ISO3, "iso3c", "country.name")
#
countryNames <- histShapeStecfCatch %>%
  dplyr::select(-sNames, -hNames, -stecfNames, -cNames) %>%
  tidyr::gather(VARIABLE, VALUE, -ISO3, -COUNTRY) %>%
  dplyr::filter(!is.na(VALUE) &
         !is.na(ISO3)) %>%
  distinct()
write.csv(countryNames, "~/git/ices-dk/fisheryO/inst/extdata/countryNames.csv", row.names = FALSE)
