rm(list = ls())
################
library(tidyverse)
library(jsonlite)
library(ggrepel)
library(stringr)
# library(reshape2)
library(icesSAG)
library(RColorBrewer)
library(extrafont)
library(icesVocab)
# library(DT)
# library(shiny)
library(ReporteRs)
# library(XML)
# library(RCurl)
options(scipen = 5)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: FAO codes and ICES stock codes #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# (NOTE: current ICES stock codes do not necessarily match with FAO 3-character
# codes, in the future this look-up table should not be necessary - SL)
# speciesID <- read.csv("~/git/ices-dk/fisheryO/inst/extdata/ICESspeciesID_v1.csv",
#                       stringsAsFactors = FALSE)
# #

# findAphia("cod", full = TRUE)
# tt <- getCodeList("ICES_StockCode")
# tt <- getCodeList("IC_Species")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: Fishery guilds by ICES stock code #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# (NOTE: These guilds should become a part of the RECO database - SL)
# fisheryGuild <- read.csv("~/git/ices-dk/fisheryO/inst/extdata/fisheryGuild.csv",
#                          stringsAsFactors = FALSE)
#
# speciesGuild <- fisheryGuild %>%
#   mutate(speciesID = toupper(gsub( "-.*$", "", Stock.code)),
#          Stock.code = tolower(Stock.code)) %>%
#   full_join(speciesID, c("speciesID" = "oldCode")) %>%
#   select(STOCK.CODE = Stock.code,
#          FISHERIES.GUILD = Fisheries.Guild,
#          SPECIES.ID = speciesID,
#          SPECIES.NAME = speciesName,
#          -newCode)


# Get stock list
# url <- "http://sd.ices.dk/services/odata3/StockListDWs3?$filter=ActiveYear%20eq%202016"
# url <- "http://admin.ices.dk/StockListServices/odata/StockListDWsOData?$filter=ActiveYear%20eq%202016"
# url <- "~/git/ices-dk/fisheryO/inst/extdata/StockListDWsOData.json"

rawsl <- jsonlite::fromJSON("http://sd.ices.dk/services/odata3/StockListDWs3?$filter=ActiveYear%20eq%202016",
                            simplifyDataFrame = TRUE)$value

#
# ecoregions <- unique(unlist(strsplit(rawsl$EcoRegion, ", ")))
# ecoregions <- ecoregions[!is.na(ecoregions)]
#
# len <- max(str_count(rawsl$EcoRegion, pattern = ", "), na.rm = TRUE)


# sl <- rawsl %>%
#   select(StockCode, Description, SpeciesScientificName,
#          EcoRegion, DataCategory, YearOfLastAssessment,
#          AdviceCategory, FisheriesGuild)

  # separate(EcoRegion, paste0("X", 1:len), sep = ", ", extra = "merge")
#
# # Combine Norwegian and Barents Sea Ecoregions
# rawsl$NorwegianSeaandBarentsSeaEcoregion[!is.na(rawsl$NorwegianSeaEcoregion) |
#                                            !is.na(rawsl$BarentsSeaEcoregion)] <- "x"
# # Reorganize from a wide data frame to a long data frame with species guild information
# ecoregions <- colnames(rawsl)[grepl("^.+(Ecoregion)$", colnames(rawsl))]
#
# dots <- lapply(c("StockCode", "Description", "DataCategory", "AdviceType", ecoregions),
#                as.symbol)

sl <- rawsl %>%
  # select_(.dots = dots) %>%
  select(StockCode, Description, SpeciesScientificName,
         EcoRegion, DataCategory, YearOfLastAssessment,
         AdviceCategory, FisheriesGuild) %>%
  filter(!StockCode %in% c("cod-ingr", "cod-wgr", "sal-nea", "san-scow")) %>%
  # melt(id.vars = c("StockCode", "Description", "DataCategory", "AdviceType")) %>%
  # filter(!is.na(value),
  #        !variable %in% c("NorwegianSeaEcoregion", "BarentsSeaEcoregion")) %>%
  # select(STOCK.CODE = StockCode,
  #        STOCK.NAME = Description,
  #        CAT = DataCategory,
  #        ADVICE.TYPE = AdviceType,
  #        ECOREGION = variable,
  #        -value) %>%
  mutate(DataCategory = floor(as.numeric(DataCategory)),
         StockCode = tolower(StockCode),
         FisheriesGuild = tolower(FisheriesGuild),
         FisheriesGuild = ifelse(StockCode %in% c("arg-5b6a", "tsu-nea", "smr-5614"),
                                 "demersal",
                                 FisheriesGuild),
         Description = gsub(pattern = "\u2013", "-", Description), # remove en dashes in favor of hyphens
         AdviceCategory = ifelse(AdviceCategory == "MSY/PA",
                              "MSY", AdviceCategory),
         SpeciesID = toupper(gsub( "-.*$", "", StockCode)) ,
         SpeciesScientificName = recode(SpeciesScientificName,
                                        "Mustelus asterias" = "Mustelus"),
         SpeciesScientificName = recode(SpeciesScientificName,
                                        "Centrophorus squamosus, Centroscymnus coelolepis" = "Centroscymnus coelolepis")

  #        ECOREGION = as.character(ECOREGION),
  #        ECOREGION = recode(ECOREGION, "AzoresEcoregion" = "Azores"),
  #        ECOREGION = recode(ECOREGION, "BayofBiscayandtheIberianCoastEcoregion" = "Bay of Biscay and the Iberian Coast"),
  #        ECOREGION = recode(ECOREGION, "BalticSeaEcoregion" = "Baltic Sea"),
  #        ECOREGION = recode(ECOREGION, "CelticSeasEcoregion" = "Celtic Seas"),
  #        ECOREGION = recode(ECOREGION, "FaroesEcoregion" = "Faroes"),
  #        ECOREGION = recode(ECOREGION, "GreenlandSeaEcoregion" = "Greenland Sea"),
  #        ECOREGION = recode(ECOREGION, "IcelandSeaEcoregion" = "Iceland Sea"),
  #        ECOREGION = recode(ECOREGION, "GreaterNorthSeaEcoregion" = "Greater North Sea"),
  #        ECOREGION = recode(ECOREGION, "OceanicNortheastAtlanticEcoregion" = "Oceanic north-east Atlantic"),
  #        ECOREGION = recode(ECOREGION, "NorwegianSeaandBarentsSeaEcoregion" = "Norwegian Sea and Barents Sea")) %>%
  # left_join(speciesGuild, c("STOCK.CODE" = "STOCK.CODE"))
         )

# Format the species names appropriately
slFull <- bind_rows(
  # Normal binomial names
  sl %>%
    filter(!grepl(" spp", Description),
           SpeciesID != "ANG",
           grepl("[[:space:]]", SpeciesScientificName)) %>%
    mutate(Description = str_replace_all(string = Description,
                                         pattern = SpeciesScientificName,
                                         replacement = paste0("<em>", SpeciesScientificName, "</em>"))),
  # Anglerfish (w/ two species)
  sl %>%
    filter(SpeciesID == "ANG") %>%
    mutate(Description = str_replace_all(string = Description,
                                         pattern = "Lophius piscatorius and L. budegassa",
                                         replacement = "<em>Lophius piscatorius</em> and <em>L. budegassa</em>")),
  # Groups of species (.spp)
  sl %>%
    filter(grepl(" spp.*$", Description)) %>%
    mutate(Description = str_replace_all(string = Description,
                                         pattern = SpeciesScientificName,
                                         replacement = paste0("<em>", SpeciesScientificName, "</em>"))),
  # A bit different notation (embedded in 2 sets of parentheses)
  sl %>%
    filter(StockCode == "raj-mar") %>%
    mutate(Description = str_replace_all(string = Description,
                                         pattern = "Raja clavata",
                                         replacement = "<em>Raja clavata</em>")),
  # The "others" with no species name
  sl %>%
    filter(SpeciesID != "ANG") %>%
    filter(!grepl(" spp", Description)) %>%
    filter(StockCode != "raj-mar") %>%
    filter(!grepl("[[:space:]]", SpeciesScientificName))
)

# slBinomial <- sl %>%
#   filter(!grepl(" spp", Description),
#          SpeciesID != "ANG",
#          grepl("[[:space:]]", SpeciesScientificName)) %>%
#   mutate(Description = str_replace_all(string = Description,
#                                       pattern = SpeciesScientificName,
#                                       replacement = paste0("<em>", SpeciesScientificName, "</em>")))
# slANG <- sl %>%
#   filter(SpeciesID == "ANG") %>%
#   mutate(Description = str_replace_all(string = Description,
#                                       pattern = "Lophius piscatorius and L. budegassa",
#                                       replacement = "<em>Lophius piscatorius</em> and <em>L. budegassa</em>"))
# slSpp <- sl %>%
#   filter(grepl(" spp.*$", Description)) %>%
#   mutate(Description = str_replace_all(string = Description,
#                                       pattern = SpeciesScientificName,
#                                       replacement = paste0("<em>", SpeciesScientificName, "</em>")))

# slRAJ <- sl %>%
#   filter(StockCode == "raj-mar") %>%
#   mutate(Description = str_replace_all(string = Description,
#                                        pattern = "Raja clavata",
#                                        replacement = "<em>Raja clavata</em>"))

# slCYO <-  sl %>%
#   filter(SpeciesID == "CYO") %>%
#   mutate(STOCK.NAME = str_replace_all(STOCK.NAME,
#                                       pattern = "Centrophorus squamosus",
#                                       replacement = paste0("<em>Centrophorus squamosus</em>")),
#          STOCK.NAME = str_replace_all(STOCK.NAME,
#                                       pattern = SPECIES.NAME,
#                                       replacement = paste0("<em>", SPECIES.NAME, "</em>")))

# slRNG <- sl %>%
#   filter(SPECIES.ID == "RNG") %>%
#   filter(!grepl("[[:space:]]", SPECIES.NAME))
#
# slCheck <- sl %>%
#   select(-ECOREGION) %>%
#   distinct %>%
#   mutate(TT = str_match_all(STOCK.NAME,
#                              pattern = SPECIES.NAME))
#
# td <- slCheck[!lapply(slCheck$TT, length) >0,]
# td$TT <- NULL
# write.csv(x = td, file = "~/git/ices-dk/fisheryO/inst/extdata/stockListCheck.csv", row.names = FALSE)
# #
# slOthers <- sl %>%
#   filter(SpeciesID != "ANG") %>%
#   filter(!grepl(" spp", Description)) %>%
#   filter(StockCode != "raj-mar") %>%
#   # grepl("[[:space:]]", SpeciesScientificName)) %>%
#   filter(!grepl("[[:space:]]", SpeciesScientificName))
# #rng
#
# slFull <- slBinomial %>%
#   bind_rows(slANG, slSpp, slRAJ, slOthers)
  # select(STOCK.CODE,
  #        STOCK.NAME,
  #        FISHERIES.GUILD,
  #        ECOREGION,
  #        ADVICE.TYPE,
#          CAT)

msyStocks <- slFull$StockCode[slFull$AdviceCategory == "MSY"]
paStocks <- slFull$StockCode[slFull$AdviceCategory == "PA"]
mpStocks <- slFull$StockCode[slFull$AdviceCategory == "MP"]

sl_2014 <- slFull$StockCode[slFull$YearOfLastAssessment == 2014]
sl_2015 <- slFull$StockCode[slFull$YearOfLastAssessment == 2015]
sl_2016 <- slFull$StockCode[slFull$YearOfLastAssessment == 2016]

sumCols <- c("Year", "fishstock", "F", "SSB", "fishingPressureDescription",
             "stockSizeDescription", "landings", "catches")

summaryTbl_2014 <- getSAG(stock = sl_2014, combine = TRUE, year = 2014, data = "summary")
summaryTbl_2015 <- getSAG(stock = sl_2015, combine = TRUE, year = 2015, data = "summary")
summaryTbl_2016 <- getSAG(stock = sl_2016, combine = TRUE, year = 2016, data = "summary")

summaryTbl_2014 <- summaryTbl_2014[colnames(summaryTbl_2014) %in% sumCols]
summaryTbl_2015 <- summaryTbl_2015[colnames(summaryTbl_2015) %in% sumCols]
summaryTbl_2016 <- summaryTbl_2016[colnames(summaryTbl_2016) %in% sumCols]

summaryTbl <- rbind(summaryTbl_2014, summaryTbl_2015, summaryTbl_2016)


keeperF <- c("F", "F/FMSY", "F in winter rings", "Harvest rate", "Harvest rate/FMSY", "Fishing Pressure", "weighted F")
relativeF <- c("F/FMSY", "Harvest rate/FMSY")

keeperSSB <- c("SSB", "B/BMSY", "SSB & Biomass Age 4+", "UW Tv Index", "Stock Size", "Total biomass/BMSY")
relativeSSB <- c("B/BMSY", "Total biomass/BMSY")

summaryTblClean <- summaryTbl %>%
  select(Year,
         StockCode = fishstock,
         F,
         SSB,
         fishingPressureDescription,
         stockSizeDescription,
         landings,
         catches) %>%
  mutate(StockCode = tolower(StockCode),
         # SSB is not in SAG for meg-4a6a 2015. Added from:
         # http://ices.dk/sites/pub/Publication%20Reports/Expert%20Group%20Report/acom/2015/WGCSE/05.03_Megrim%20IV_VI_2015.pdf#page=22
         SSB = ifelse(Year == 2015 & StockCode == "meg-4a6a",
                      1.91,
                      SSB),
         fishingPressureDescription = gsub("Fishing Pressure: " , "", fishingPressureDescription),
         fishingPressureDescription = gsub("Fishing pressure: " , "", fishingPressureDescription),
         fishingPressureDescription = gsub("msy" , "MSY", fishingPressureDescription),
         fishingPressureDescription = gsub("Harvest Rate", "Harvest rate", fishingPressureDescription),
         fishingPressureDescription = gsub("F &amp; HR", "Harvest rate", fishingPressureDescription),

         stockSizeDescription = gsub("Stock Size: ", "", stockSizeDescription),
         stockSizeDescription = gsub("Stock size: ", "", stockSizeDescription),
         stockSizeDescription = gsub("msy", "MSY", stockSizeDescription),

         stockSizeDescription = ifelse(is.na(stockSizeDescription), "Relative",
                                       stockSizeDescription),
         FmsyDescription = "FMSY",
         FmsyDescription = ifelse(fishingPressureDescription %in% relativeF,
                                  "F/FMSY",
                                  fishingPressureDescription),
         BmsyDescription = "MSYBtrigger",
         BmsyDescription = ifelse(stockSizeDescription %in% relativeSSB,
                                  "SSB/BMSY",
                                  stockSizeDescription)) %>%
  filter(stockSizeDescription %in% keeperSSB |
           fishingPressureDescription %in% keeperF) %>%
  distinct(.keep_all = TRUE)

#
# tt <- summaryTblClean[!is.na(summaryTblClean$F) &
#                         summaryTblClean$StockCode %in% msyStocks,]
# unique(summaryTblClean$stockSizeDescription[!is.na(summaryTblClean$SSB) &
#                                               summaryTblClean$StockCode %in% msyStocks])
#

# refPts <- getSAG(stock = NULL, combine = TRUE, year = 2016, data = "refpts")
refCols <- c("FishStockName", "FLim", "Fpa", "Bpa", "Blim", "FMSY", "MSYBtrigger")

refPts_2014 <- getSAG(stock = sl_2014, combine = TRUE, year = 2014, data = "refpts")
refPts_2015 <- getSAG(stock = sl_2015, combine = TRUE, year = 2015, data = "refpts")
refPts_2016 <- getSAG(stock = sl_2016, combine = TRUE, year = 2016, data = "refpts")

colnames(refPts_2014) <- gsub(" /", "", colnames(refPts_2014))
refPts_2015 <- refPts_2015[,!names(refPts_2015) %in% grep(" /", colnames(refPts_2015), value = TRUE)]
refPts_2016 <- refPts_2016[,!names(refPts_2016) %in% grep(" /", colnames(refPts_2016), value = TRUE)]

refPts_2014 <- refPts_2014[colnames(refPts_2014) %in% refCols]
refPts_2015 <- refPts_2015[colnames(refPts_2015) %in% refCols]
refPts_2016 <- refPts_2016[colnames(refPts_2016) %in% refCols]

refPts <- rbind(refPts_2014, refPts_2015, refPts_2016)

refPts[refPts == ""] <- NA

refPtsClean <- refPts %>%
  rename(Flim = FLim,
         StockCode = FishStockName) %>%
  mutate(StockCode = tolower(StockCode)) %>%
  distinct(.keep_all = TRUE)


# unique(refPtsClean$StockCode[!refPtsClean$StockCode %in% summaryTblClean$StockCode])
#
# length(unique(summaryTblClean))
#
# unique(summaryTblClean$StockCode[summaryTblClean$StockCode %in% refPtsClean$StockCode])

fullSummary <- summaryTblClean %>%
  left_join(refPtsClean, by = c("StockCode" = "StockCode")) %>%
  left_join(slFull, by = c("StockCode" = "StockCode")) %>%
  mutate(MSYBtrigger = ifelse(stockSizeDescription %in% relativeSSB,
                              0.5,
                              MSYBtrigger),
         MSYBtrigger = ifelse(!stockSizeDescription %in% keeperSSB,
                             NA,
                             MSYBtrigger),
         MSYBtrigger = ifelse(MSYBtrigger == 0,
                              NA,
                              MSYBtrigger),
         FMSY = ifelse(fishingPressureDescription %in% relativeF,
                       1,
                       FMSY),
         FMSY = ifelse(!fishingPressureDescription %in% keeperF,
                       NA,
                       FMSY),
         FMSY = ifelse(FMSY == 0,
                       NA,
                       FMSY)
         )

numCols <- c("Year", "F", "SSB", "landings", "catches", "Flim",
             "Blim", "FMSY", "MSYBtrigger", "Fpa", "Bpa", "YearOfLastAssessment")

fullSummary[colnames(fullSummary) %in% numCols] <- lapply(fullSummary[colnames(fullSummary) %in% numCols],
                                                          as.numeric)

fmsySummary <- fullSummary %>%
  group_by(StockCode) %>%
  filter(Year >= YearOfLastAssessment - 3,
         Year <= YearOfLastAssessment - 1) %>%
  mutate(FMSY = ifelse(F <= FMSY, "GREEN", "RED"),
         Year = paste0("FMSY", Year)) %>%
  select(Year, FMSY, StockCode) %>%
  spread(Year, FMSY)

bmsySummary <- fullSummary %>%
  group_by(StockCode) %>%
  filter(Year >= YearOfLastAssessment - 2,
         Year <= YearOfLastAssessment) %>%
  mutate(BMSY = ifelse(SSB >= MSYBtrigger, "GREEN", "RED"),
         Year = paste0("BMSY", Year)) %>%
  select(Year, BMSY, StockCode) %>%
  spread(Year, BMSY)

fpaSummary <- fullSummary %>%
  group_by(StockCode) %>%
  filter(Year >= YearOfLastAssessment - 3,
         Year <= YearOfLastAssessment - 1) %>%
  ungroup() %>%
  mutate(FPA = if_else(F <= FMSY,
                       "GREEN",
                       NA_character_),
         FPA = if_else(F >= Flim, # If F is greater than Flim, FPA is
                       missing = FPA,
                       "RED",
                       ifelse(F <= FMSY, # if F is less than Flim and less than FMSY, FPA is
                              "GREEN",
                              if_else(F <= Fpa , # If F is greater than FMSY but less than Fpa, FPA is
                                      "GREEN",
                                      "ORANGE", # if F is greater than Fpa, FPA is orange
                                      ifelse(F <= Flim, # if Fpa is NA but Flim is available, FPA is
                                             "ORANGE",
                                             NA_character_)))), # If none of this fits the bill, NA.
         Year = paste0("FPA", Year)) %>%
  select(Year, FPA, StockCode) %>%
  spread(Year, FPA)

bpaSummary <- fullSummary %>%
  group_by(StockCode) %>%
  filter(Year >= YearOfLastAssessment - 2,
         Year <= YearOfLastAssessment) %>%
  ungroup() %>%
  mutate(BPA = if_else(SSB >= MSYBtrigger,
                       "GREEN",
                       NA_character_),
         BPA = if_else(SSB <= Blim, # If SSB is less than Blim, BPA is
                       missing = BPA,
                       "RED",
                       ifelse(SSB >= MSYBtrigger, # if SSB is greater than Blim and greater than MSY, BPA is
                              "GREEN",
                              if_else(SSB >= Bpa , # If SSB is less than MSY Btrigger but greater than Bpa, BPA is
                                      "GREEN",
                                      "ORANGE", # if SSB is less than Bpa, BPA is orange
                                      if_else(SSB >= Blim, # if Bpa is NA but Blim is available, BPA is
                                              "ORANGE",
                                              NA_character_)))), # If none of this fits the bill, NA.
         Year = paste0("BPA", Year)) %>%
  select(Year, BPA, StockCode) %>%
  spread(Year, BPA)

sblSummary <- fullSummary %>%
  select(StockCode,
         YearOfLastAssessment) %>%
  distinct(.keep_all = TRUE) %>%
  left_join(bpaSummary, by = "StockCode") %>%
  left_join(fpaSummary, by = "StockCode")

sblSummary <- bind_rows(
  sblSummary %>%
    filter(YearOfLastAssessment == 2014) %>%
    mutate(SBL = ifelse(FPA2013 == "GREEN"  &  BPA2014 == "GREEN",
                        "GREEN",
                        ifelse(FPA2013 == "RED"  &  BPA2014 == "RED",
                               "RED",
                               NA))),
  sblSummary %>%
  filter(YearOfLastAssessment == 2015) %>%
    mutate(SBL = ifelse(FPA2014 == "GREEN"  &  BPA2015 == "GREEN",
                        "GREEN",
                        ifelse(FPA2014 == "RED"  &  BPA2015 == "RED",
                               "RED",
                               NA))),
  sblSummary %>%
    filter(YearOfLastAssessment == 2016) %>%
    mutate(SBL = ifelse(FPA2015 == "GREEN"  &  BPA2016 == "GREEN",
                        "GREEN",
                        ifelse(FPA2015 == "RED"  &  BPA2016 == "RED",
                               "RED",
                               NA)))
  )[, c("StockCode", "SBL")]



stockDF <- fmsySummary %>%
  left_join(bmsySummary, by = "StockCode") %>%
  left_join(fpaSummary, by = "StockCode") %>%
  left_join(bpaSummary, by = "StockCode") %>%
  left_join(sblSummary, by = "StockCode")

stockDat <- slFull %>%
  left_join(stockDF, by = "StockCode") %>%
  mutate(F_2013 = ifelse(AdviceCategory %in% c("MSY", "MP"),
                         FMSY2013,
                         FPA2013),
         F_2014 = ifelse(AdviceCategory %in% c("MSY", "MP"),
                         FMSY2014,
                         FPA2014),
         F_2015 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                         FMSY2015,
                         FPA2015),
         SSB_2014 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                           BMSY2014,
                           BPA2014),
         SSB_2015 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                           BMSY2015,
                           BPA2015),
         SSB_2016 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                           BMSY2016,
                           BPA2016),
         D3C1 = NA,
         D3C2 = NA,
         GES = NA)

stockDat <- bind_rows(
  stockDat %>%
    filter(YearOfLastAssessment == 2014) %>%
    mutate(D3C1 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                         FMSY2013,
                         FPA2013),
           D3C2 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                         BMSY2014,
                         BPA2014)),
  stockDat %>%
    filter(YearOfLastAssessment == 2015) %>%
    mutate(D3C1 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                         FMSY2014,
                         FPA2014),
           D3C2 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                         BMSY2015,
                         BPA2015)),
  stockDat %>%
    filter(YearOfLastAssessment == 2016) %>%
    mutate(D3C1 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                         FMSY2015,
                         FPA2015),
           D3C2 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                         BMSY2016,
                         BPA2016))
)

stockDat <- select(stockDat, StockCode,
                   Description,
                   FisheriesGuild,
                   EcoRegion,
                   AdviceCategory,
                   DataCategory,
                   SBL,
                   F_2013, F_2014, F_2015,
                   SSB_2014, SSB_2015, SSB_2016,
                   D3C1, D3C2, GES)

stockDat$GES[is.na(stockDat$D3C1) |
               is.na(stockDat$D3C2)] <- NA
stockDat$GES[stockDat$D3C1 == "RED" |
               stockDat$D3C2 == "RED"] <- "RED"
stockDat$GES[stockDat$D3C1 == "GREEN" &
               stockDat$D3C2 == "GREEN"] <- "GREEN"

stockDat[c("SBL", "F_2013", "F_2014", "F_2015",
           "SSB_2014", "SSB_2015", "SSB_2016",
           "D3C1", "D3C2", "GES")][is.na(stockDat[c("SBL", "F_2013", "F_2014", "F_2015",
                                                    "SSB_2014", "SSB_2015", "SSB_2016",
                                                    "D3C1", "D3C2", "GES")])] <- "GREY"

stockDat[stockDat == "GREEN"] <- "<i class=\"glyphicon glyphicon-ok-sign\" style=\"color:green; font-size:2.2em\"></i>"
stockDat[stockDat == "RED"] <- "<i class=\"glyphicon glyphicon-remove-sign\" style=\"color:red; font-size:2.2em\"></i>"
stockDat[stockDat == "GREY"] <- "<i class=\"glyphicon glyphicon-question-sign\" style=\"color:grey; font-size:2.2em\"></i>"
stockDat[stockDat == "ORANGE"] <- "<i class=\"glyphicon glyphicon-record\" style=\"color:#FAB700; font-size:2.2em\"></i>"

stockDat <- data.frame(lapply(stockDat, factor))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Prepare subsets for R Markdown rendering #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ecoregion <- unique(stockDat$ECOREGION)
# fileName <-  "referencePointOverview-static.html"
# Render dynamic and static stock summary tables


stockPlotEcoregion <- function(ecoregion,
                               fileName = NULL) {

  stockPlot <- stockDat %>%
    filter(grepl(pattern = ecoregion, EcoRegion)) %>%
    select(-EcoRegion) %>%
    distinct() %>%
    arrange(StockCode)

  suppressWarnings(
  rmarkdown::render("~/git/ices-dk/fisheryO/vignettes/stockSummaryTable-static.Rmd",
                    output_file = paste0("~/git/ices-dk/fisheryO/output/", fileName, "-static.html"),
                    # rmarkdown::html_document(template = NULL),
                    envir = new.env())
  )

  suppressWarnings(
    rmarkdown::render("~/git/ices-dk/fisheryO/vignettes/stockSummaryTable-dynamic.rmd",
                    output_file = paste0("~/git/ices-dk/fisheryO/output/", fileName, "-dynamic.html"),
                    # rmarkdown::html_document(template = NULL),
                    envir = new.env())
  )

}

# lapply(unique(stockDat$ECOREGION), stockPlotEcoregion)

##############
# Pie graphs #
##############

pieDat <- slFull %>%
  select(StockCode,
         EcoRegion,
         FisheriesGuild,
         YearOfLastAssessment) %>%
  left_join(stockDF, by = "StockCode") %>%
  mutate(EcoRegion = strsplit(as.character(EcoRegion), ", ")) %>%
  unnest(EcoRegion) %>%
  mutate(FMSY = ifelse(YearOfLastAssessment == 2014,
                       FMSY2013,
                       ifelse(YearOfLastAssessment == 2015,
                              FMSY2014,
                              ifelse(YearOfLastAssessment == 2016,
                                     FMSY2015,
                                     NA))),
         FPA = ifelse(YearOfLastAssessment == 2014,
                      FPA2013,
                      ifelse(YearOfLastAssessment == 2015,
                             FPA2014,
                             ifelse(YearOfLastAssessment == 2016,
                                    FPA2015,
                                    NA))),
         BMSY = ifelse(YearOfLastAssessment == 2014,
                       BMSY2014,
                       ifelse(YearOfLastAssessment == 2015,
                              BMSY2015,
                              ifelse(YearOfLastAssessment == 2016,
                                     BMSY2016,
                                     NA))),
         BPA = ifelse(YearOfLastAssessment == 2014,
                      BPA2014,
                      ifelse(YearOfLastAssessment == 2015,
                             BPA2015,
                             ifelse(YearOfLastAssessment == 2016,
                                    BPA2016,
                                    NA))),
         SBL = SBL)

pieCount <- pieDat %>%
  select(EcoRegion,
         FisheriesGuild,
         FMSY,
         BMSY,
         FPA,
         BPA,
         SBL) %>%
  gather(VARIABLE, VALUE, -EcoRegion, -FisheriesGuild) %>%
  mutate(VALUE = ifelse(is.na(VALUE),
                        "GREY",
                        VALUE)) %>%
  group_by(EcoRegion, FisheriesGuild, VARIABLE, VALUE) %>%
  summarize(COUNT = n())

# Make sure that all possible combinations are provided
pieCount <- pieCount %>%
  tidyr::expand(VALUE = c("GREY", "GREEN", "RED", "ORANGE")) %>%
  left_join(pieCount, by = c("EcoRegion", "FisheriesGuild", "VARIABLE", "VALUE")) %>%
  arrange(EcoRegion, FisheriesGuild, VARIABLE, VALUE)
pieCount$COUNT[is.na(pieCount$COUNT)] <- 0

pieCount <- pieCount %>%
  group_by(EcoRegion, VARIABLE, VALUE) %>%
  mutate(FisheriesGuild = "total",
         COUNT = sum(COUNT)) %>%
  distinct(.keep_all = TRUE) %>%
  bind_rows(pieCount)

ecoregion = "Greater North Sea"
stockPieEcoregion <- function(ecoregion) {

  colList <- c("GREEN" = "#00B26D",
               "GREY" = "#d3d3d3",
               "ORANGE" = "#ff7f00",
               "RED" = "#d93b1c")

  rowDat <- pieCount %>%
    filter(grepl(pattern = ecoregion, EcoRegion)) %>%
    ungroup() %>%
    select(-EcoRegion) %>%
    group_by(FisheriesGuild, VARIABLE) %>%
    mutate(fraction = COUNT/ sum(COUNT),
           ymax = cumsum(fraction),
           ymin = c(0, head(ymax, n = -1))) %>%
    filter(fraction != 0) %>%
    mutate(pos = cumsum(fraction) - fraction/2) %>%
    ungroup() %>%
    mutate(VARIABLE = recode_factor(VARIABLE,
                                    "FMSY" = "Fishing pressure\n MSY",
                                    "BMSY" = "Stock size\n MSY",
                                    "FPA" = "Fishing pressure\n PA",
                                    "BPA" = "Stock size \n PA",
                                    "SBL" = "Within safe\n biological limits"))

  rowDat$FisheriesGuild <- factor(rowDat$FisheriesGuild, levels = c("total", "benthic", "crustacean", "elasmobranch", "demersal", "pelagic"))

  p1 <- ggplot(data = rowDat) +
    geom_bar(aes(x = "", y = fraction, fill = VALUE), stat = "identity") +
    geom_text(aes(x = "", y = pos, label = COUNT), size = 3) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = colList) +
    theme_bw(base_size = 9) +
    theme(panel.grid = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position="none") +
    theme(axis.text=element_blank(),
          axis.ticks=element_blank(),
          strip.background = element_blank()) +
    labs(title = "", x = "", y = "") +
    facet_grid(FisheriesGuild ~ VARIABLE)

  figName = "table1_"
  ggsave(filename = paste0("~/git/ices-dk/fisheryO/output/", figName, ecoregion, ".png"),
         plot = p1,
         width = 178,
         height = 152,
         units = "mm",
         dpi = 300)
}

lapply(unique(pieDat$ECOREGION)[6], stockPieEcoregion)

######################
### GES Pie Charts ###
######################

gesCatchDat <- fullSummary %>%
    group_by(StockCode) %>%
    filter(Year == YearOfLastAssessment - 1) %>%
    mutate(CATCH = ifelse(is.na(catches) & !is.na(landings),
                          landings,
                          catches),
           EcoRegion = strsplit(as.character(EcoRegion), ", ")) %>%
  unnest(EcoRegion) %>%
  select(EcoRegion,
         StockCode,
         CATCH) %>%
  left_join(pieDat, by = c("StockCode", "EcoRegion")) %>%
  ungroup() %>%
  select(EcoRegion,
         CATCH,
         D3C2 = FMSY,
         D3C1 = BMSY) %>%
  gather(VARIABLE, COLOR, -EcoRegion, -CATCH) %>%
  mutate(COLOR = ifelse(is.na(COLOR),
                        "GREY",
                        COLOR)) %>%
  group_by(EcoRegion, VARIABLE, COLOR) %>%
  summarize(COUNT = n(),
            TOTAL = sum(CATCH, na.rm = TRUE)) %>%
  mutate(TOTAL = ifelse(is.na(TOTAL),
                        0,
                        TOTAL)) %>%
  gather(METRIC, VALUE, -EcoRegion, -VARIABLE, -COLOR)


ecoregion <- "Greater North Sea"
gesPieEcoregion <- function(ecoregion) {

  colList <- c("GREEN" = "#00B26D",
               "GREY" = "#d3d3d3",
               # "ORANGE" = "#ff7f00",
               "RED" = "#d93b1c")

  rowDat <- gesCatchDat %>%
    filter(grepl(pattern = ecoregion, EcoRegion)) %>%
    ungroup() %>%
    select(-EcoRegion) %>%
    group_by(VARIABLE, METRIC) %>%
    mutate(fraction = VALUE/ sum(VALUE),
           ymax = cumsum(fraction),
           ymin = c(0, head(ymax, n = -1))) %>%
    filter(fraction != 0) %>%
    mutate(pos = cumsum(fraction) - fraction/2) %>%
    ungroup() %>%
    mutate(METRIC = recode_factor(METRIC,
                                  "COUNT" = "Number of stocks",
                                  "TOTAL" = "Proportion of catch\n (tonnes)"))
  sumDat <- rowDat %>%
    group_by(VARIABLE, METRIC) %>%
    summarize(sum = sum(VALUE))

  p1 <- ggplot(data = rowDat) +
    geom_bar(aes(x = "", y = fraction, fill = COLOR), stat = "identity") +
    geom_text(aes(x = "", y = pos, label = scales::comma(VALUE)), size = 3) + #########
    # geom_text_repel(aes(x = "", y = pos, label = scales::comma(VALUE)), size = 3,  point.padding = unit(0.5, "lines"),
    #                 segment.color = "black") +
    geom_text(data = sumDat, aes(x = 0, y = 0, label = paste0("total = ", scales::comma(sum))), size = 2.5) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = colList) +
    theme_bw(base_size = 9) +
    theme(panel.grid = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position="none") +
    theme(axis.text=element_blank(),
          axis.ticks=element_blank(),
          strip.background = element_blank()) +
    # annotate("text", x = 0, y = 0, label = "") +
    labs(title="", x = "", y = "") +
    facet_grid(METRIC ~ VARIABLE)

  figName = "table2_"
  ggsave(filename = paste0("~/git/ices-dk/fisheryO/output/", figName, ecoregion, ".png"),
         plot = p1,
         width = 89,
         height = 100.5,
         units = "mm",
         dpi = 300)
}
suppressWarnings(lapply(unique(gesPieDat$ECOREGION), gesPieEcoregion))


##############################
### Stock Status over time ###
##############################
colList <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
             "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")
ltyList <- c(1,3:6)


stockTrends <- fullSummary %>%
  left_join(slFull, by = "STOCK.CODE") %>%
  distinct(.keep_all = TRUE) %>%
  mutate(F_FMSY = ifelse(!is.na(FMSY),
                         F / FMSY,
                         NA),
         SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
                                  SSB / MSYBtrigger,
                                  NA)) %>%
  select(Year,
         STOCK.CODE,
         FISHERIES.GUILD,
         ECOREGION,
         F_FMSY,
         SSB_MSYBtrigger) %>%
  melt(id.vars = c("Year",  "STOCK.CODE", "FISHERIES.GUILD","ECOREGION"),
       measure.vars = c("F_FMSY", "SSB_MSYBtrigger"),
       variable.name = "METRIC",
       value.name = "stockValue") %>%
  group_by(ECOREGION, FISHERIES.GUILD, METRIC, Year) %>%
  mutate(ecoGuildMean = mean(stockValue, na.rm = TRUE))


allDat <- stockTrends %>%
  mutate(ECOGUILD = paste0(ECOREGION, ", ", FISHERIES.GUILD)) %>%
  ungroup() %>%
  select(pageGroup = ECOGUILD,
         lineGroup = STOCK.CODE,
         Year,
         plotGroup = METRIC,
         plotValue = stockValue) %>%
  filter(!is.na(plotValue))
#
oMean <- stockTrends %>%
  mutate(ECOGUILD = paste0(ECOREGION, ", ", FISHERIES.GUILD)) %>%
  ungroup() %>%
  distinct(ECOGUILD, METRIC, Year, .keep_all = TRUE) %>%
  select(pageGroup = ECOGUILD,
         Year,
         plotGroup = METRIC,
         plotValue = ecoGuildMean) %>%
  mutate(lineGroup = "MEAN") %>%
  filter(!is.na(plotValue))

allDat <- bind_rows(allDat, oMean)

# Set up colors
plotList <- allDat %>%
  group_by(pageGroup) %>%
  select(pageGroup, lineGroup) %>%
  mutate(nLines = n_distinct(lineGroup) - 1,
         COLOR = NA) %>%
  distinct(lineGroup, .keep_all = TRUE) %>%
  arrange(pageGroup)
#
singleList <- plotList %>%
  filter(nLines == 1) %>%
  mutate(COLOR = colList[1:length(nLines)])
#
normList <- plotList %>%
  filter(nLines <= 9 &
           nLines > 1 &
           lineGroup != "MEAN") %>%
  mutate(COLOR = colList[1:length(nLines)])
#
longList <- plotList %>%
  filter(nLines > 9 &
           lineGroup != "MEAN") %>%
  mutate(COLOR = "grey80")
#
meanList <- plotList %>%
  filter(nLines > 1 &
           lineGroup == "MEAN") %>%
  mutate(COLOR = "grey40")

colorList <- bind_rows(singleList, normList, longList, meanList)
allDat <- left_join(colorList, allDat, by = c("pageGroup", "lineGroup"))

allDat <- allDat %>%
  group_by(pageGroup) %>%
  mutate(nLines = n_distinct(lineGroup)) %>%
  filter(nLines > 2 | lineGroup != "MEAN") %>%
  filter(lineGroup != "MEAN" | Year != 2016 | plotGroup != "F_FMSY")


source("~/git/ices-dk/fisheryO/R/stockSummaryTrends.R")
# create stock summary trend figures
stockSummaryTrends(df = allDat[allDat$plotGroup == "F_FMSY",],
                   overallMean = TRUE,
                   plotDir = "~/git/ices-dk/fisheryO/output/")


#################
### KOBE Plot ###
#################

# stockStatusDat <- fullSummary # %>%
  # left_join(slFull, by = "StockCode")

# yearF <- stockStatusDat %>%
#   select(Year, StockCode, F) %>%
#   filter(!is.na(F) &
#            Year <= 2016) %>%
#   group_by(StockCode) %>%
#   summarize(yearF = max(Year, na.rm = TRUE))
#
# yearB <- stockStatusDat %>%
#   select(Year, StockCode, SSB) %>%
#   filter(!is.na(SSB) &
#            Year <= 2016) %>%
#   group_by(StockCode) %>%
#   summarize(yearB = max(Year, na.rm = TRUE))
#
# td <- yearF %>%
#   left_join(yearB, by = c("StockCode"))

stockStatusDat <- fullSummary %>%

  mutate(EcoRegion = strsplit(as.character(EcoRegion), ", ")) %>%
  unnest(EcoRegion) %>%
  distinct(.keep_all = TRUE) %>%
  # filter(Year == 2015) %>%
  mutate(F_FMSY = ifelse(!is.na(FMSY),
                         F / FMSY,
                         NA),
         SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
                                  SSB / MSYBtrigger,
                                  NA),
         CATCH = ifelse(is.na(catches) & !is.na(landings),
                           landings,
                        catches)) %>%
  select(Year,
         YearOfLastAssessment,
         StockCode,
         EcoRegion,
         FisheriesGuild,
         F_FMSY,
         SSB_MSYBtrigger,
         CATCH)

stockStatusF <- stockStatusDat %>%
  # left_join(td, by = "STOCK.CODE") %>%
  group_by(StockCode) %>%
  filter(Year == YearOfLastAssessment - 1) %>%
  # mutate(SSB_MSYBtrigger = NA) %>%
  select(-Year,
         -SSB_MSYBtrigger,
         -YearOfLastAssessment)

stockStatusB <- stockStatusDat %>%
  # left_join(td, by = "STOCK.CODE") %>%
  group_by(StockCode) %>%
  filter(Year == YearOfLastAssessment) %>%
  # mutate(F_FMSY = NA) %>%
  select(-Year,
         -F_FMSY,
         -CATCH,
         -YearOfLastAssessment)

stockStatusFull <- stockStatusF %>%
  left_join(stockStatusB, c("StockCode", "EcoRegion", "FisheriesGuild")) %>%
  mutate(colList = if_else(F_FMSY < 1 & SSB_MSYBtrigger >= 1,
                  "GREEN" ,
                  "RED",
                  "GREY"))

stockStatusFull$colList[is.na(stockStatusFull$F_FMSY) |
                          is.na(stockStatusFull$SSB_MSYBtrigger)] <- "GREY"

source("~/git/ices-dk/fisheryO/R/kobePlot.R")
lapply(unique(stockStatusFull$ECOREGION), plot_kobe, guild = "all", fig.width = 174, fig.height = 118)
lapply(unique(stockStatusFull$ECOREGION), plot_kobe, guild = "benthic")
lapply(unique(stockStatusFull$ECOREGION), plot_kobe, guild = "demersal")
lapply(unique(stockStatusFull$ECOREGION), plot_kobe, guild = "pelagic")
lapply(unique(stockStatusFull$ECOREGION), plot_kobe, guild = "crustacean")
lapply(unique(stockStatusFull$ECOREGION), plot_kobe, guild = "elasmobranch")


######################
### Catch MSY Plot ###
######################

catchMSYPlot <- fullSummary %>%
  left_join(slFull, by = "STOCK.CODE") %>%
  filter(Year >= 1995) %>%
  mutate(F_FMSY = ifelse(!is.na(FMSY),
                         F / FMSY,
                         NA),
         CATCH = ifelse(is.na(CATCHES) & !is.na(LANDINGS),
                        LANDINGS,
                        CATCHES),
         colList = if_else(F_FMSY < 1,
                           "GREEN",
                           "RED",
                           "GREY")) %>%
  group_by(ECOREGION, FISHERIES.GUILD, colList, Year) %>%
  summarize(totCatch = sum(CATCH, na.rm = TRUE))

catchMSY <- function(ecoregion, plotDir = "~/git/ices-dk/fisheryO/output/") {

  plotName <- paste0(plotDir, "figure09_", ecoregion, ".png")
  #
  catchMSYPlotDat <- catchMSYPlot %>%
    filter(ECOREGION == ecoregion)

  cP <- ggplot(catchMSYPlotDat, aes(x = Year, y = totCatch)) +
    geom_bar(stat = "identity", aes(fill = colList)) +
    scale_fill_manual(values = c("GREEN" = "#4daf4a",
                                 "RED" = "#e41a1c",
                                 "GREY" = "#d3d3d3"),
                      name = '',
                      labels = c("GREEN" = expression(F<F[MSY]),
                                 "RED" = expression(F>F[MSY]),
                                 "GREY" = expression(F[MSY]~unk.))) +
    coord_fixed(ratio = 1) +
    labs(x = "",
         y = "Catch (tonnes)") +
    theme_bw(base_size = 9) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          strip.background = element_blank(),
          legend.key = element_blank()) +
    facet_wrap(~FISHERIES.GUILD, scales = "free", ncol = 2)

  png(plotName,
      width = 174,
      height = 136,
      units = "mm",
      res = 300)
  print(cP)
  dev.off()
}

lapply(unique(catchMSYPlot$ECOREGION), catchMSY)


dynamicPie <- data.frame(pieDat,
                         FMSY = 0,
                         BMSY = 0,
                         FPA = 0,
                         BPA = 0,
                         SBL = 0)

dynamicPie$FMSY[dynamicPie$VARIABLE == "FMSY2015"] <- dynamicPie$COUNT[dynamicPie$VARIABLE == "FMSY2015"]
dynamicPie$BMSY[dynamicPie$VARIABLE == "BMSY2016"] <- dynamicPie$COUNT[dynamicPie$VARIABLE == "BMSY2016"]
dynamicPie$FPA[dynamicPie$VARIABLE == "FPA2015"] <- dynamicPie$COUNT[dynamicPie$VARIABLE == "FPA2015"]
dynamicPie$BPA[dynamicPie$VARIABLE == "BPA2016"] <- dynamicPie$COUNT[dynamicPie$VARIABLE == "BPA2016"]
dynamicPie$SBL[dynamicPie$VARIABLE == "SBL"] <- dynamicPie$COUNT[dynamicPie$VARIABLE == "SBL"]

# ecoregion = "Greater North Sea"

# stockSummaryEcoregion <- function(ecoregion) {
dyPie <- dynamicPie %>%
  filter(ECOREGION == ecoregion) %>%
  select(-ECOREGION, -VARIABLE, -VALUE, -COUNT, FISHERIES.GUILD, FMSY, BMSY, FPA, BPA, SBL) %>%
  group_by(FISHERIES.GUILD) %>%
  # melt(id.vars = "FISHERIES.GUILD")
  mutate(FMSY = paste(FMSY, collapse = ","),
         BMSY = paste(BMSY, collapse = ","),
         FPA = paste(FPA, collapse = ","),
         BPA = paste(BPA, collapse = ","),
         SBL = paste(SBL, collapse = ",")) %>%
  distinct(.keep_all = TRUE) %>%
  as.data.frame()

# Render dynamic and static stock status summary tables

  suppressWarnings(
    rmarkdown::render("~/git/ices-dk/fisheryO/vignettes/stockStatusSummaryTable-dynamic.rmd",
                      output_file = paste0("~/git/ices-dk/fisheryO/output/table1_", ecoregion, "-dynamic.html")),
                      # rmarkdown::html_document(template = NULL),
                      envir = new.env())
  # )




rmarkdown::render("~/git/ices-dk/FisheryO/vignettes/stockStatusSummaryTable-dynamic.Rmd",
          output_file = "~/git/ices-dk/FisheryO/output/annexA_fullDynamic.html",
          rmarkdown::html_document(template = NULL))
#
#
# renderFisheryOverview <- function(ecoregionID) {
#   # in a single for loop
#   #  1. define subgroup
#   #  2. render output
#   #
#   ecoPath <- gsub(" ", "_", ecoregionID)
#   ifelse(!dir.exists(file.path(plotDir, ecoPath)), dir.create(file.path(plotDir, ecoPath)), FALSE)
#   #
#   icesID <- areaID$value[areaID$Ecoregion == ecoregionID &
#                            areaID$areaType == "ICESarea"]
#   stecfID <- areaID$value[areaID$Ecoregion == ecoregionID &
#                             areaID$areaType == "STECFarea"]
#   #
#   catchDatECO <- catchDat %>%
#     Filter(f = function(x)!all(is.na(x))) %>%
#     filter(Area %in% icesID) %>%
#     melt(id.vars = c("Species", "Area", "Units", "Country"),
#          variable.name = "YEAR",
#          value.name = "VALUE") %>%
#     inner_join(spList, c("Species" = "X3A_CODE")) %>%
#     full_join(fisheryGuild, c("Species" = "newCode")) %>%
#     mutate(YEAR = as.numeric(gsub("X", "", YEAR)))
#   #
#   effortDatECO <-
#     effortDat %>%
#     Filter(f = function(x)!all(is.na(x))) %>%
#     filter(reg_area_cod %in% stecfID) %>%
#     melt(id.vars = c("annex", "reg_area_cod", "reg_gear_cod", "Specon_calc", "country", "vessel_length"),
#          variable.name = "YEAR",
#          value.name = "VALUE") %>%
#     mutate(YEAR = as.numeric(levels(YEAR))[YEAR])
#   #
#   stecfCatchDatECO <-
#     stecfCatchDat %>%
#     filter(reg_area %in% stecfID) %>%
#     melt(id.vars = c("annex", "reg_area", "country", "reg_gear", "specon", "species"),
#          variable.name = "YEAR",
#          value.name = "VALUE") %>%
#     mutate(METRIC = as.character(gsub(".*\\s", "", YEAR)),
#            YEAR = as.numeric(gsub("\\s.+$", "", YEAR))) %>%
#     filter(METRIC == "L")
#   #
#   guildListECO <- guildList %>%
#     filter(ECOREGION == ecoregionID)
#
#   #
#   rmarkdown::render(paste0(dataDir, "fisheriesAdvice_template.rmd"),
#                     output_dir = file.path(plotDir, ecoPath),
#                     output_file = paste0('FisheriesAdvice_', ecoregionID, '.html'),
#                     params = list(set_title = as.character(ecoregionID)),
#                     envir = new.env())
# }


