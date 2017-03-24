rm(list = ls())
################
library(tidyverse)
library(jsonlite)
library(ggrepel)
library(stringr)
# library(reshape2)
# devtools::install_github("ices-tools-prod/icesSAG")
library(icesSAG)
library(RColorBrewer)
library(extrafont)
library(icesVocab)
# library(DT)
# library(shiny)
library(ReporteRs)
library(lubridate)
# library(dplyr)
library(ggiraph)

# Function to help with consistent colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1 : n]
}

# library(XML)
# library(RCurl)
options(scipen = 5)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: ICES Stock Database #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
stock_list_raw <- jsonlite::fromJSON("http://sd.ices.dk/services/odata3/StockListDWs3?$filter=ActiveYear%20eq%202016",
                                     simplifyDataFrame = TRUE)$value

# Clean up the stock database data and remove a few non-conformers
stock_list <- stock_list_raw %>%
  select(StockCode = StockKeyLabel,
         Description = StockKeyDescription,
         SpeciesScientificName,
         EcoRegion,
         DataCategory,
         YearOfLastAssessment,
         AdviceCategory,
         FisheriesGuild) %>%
  filter(!StockCode %in% c("cod-ingr", "cod-wgr", "sal-nea", "san-scow", "sal-na")) %>%
  mutate(DataCategory = floor(as.numeric(DataCategory)),
         StockCode = tolower(StockCode),
         FisheriesGuild = tolower(FisheriesGuild),
         # FisheriesGuild = ifelse(StockCode %in% c("arg-5b6a", "tsu-nea", "smr-5614"),
         #                         "demersal",
         #                         FisheriesGuild),
         Description = gsub(pattern = "\u2013", "-", Description), # remove en dashes in favor of hyphens
         AdviceCategory = ifelse(AdviceCategory == "MSY/PA",
                              "MSY", AdviceCategory),
         SpeciesID = toupper(gsub( "-.*$", "", StockCode)) ,
         SpeciesScientificName = recode(SpeciesScientificName,
                                        "Mustelus asterias" = "Mustelus"),
         SpeciesScientificName = recode(SpeciesScientificName,
                                        "Centrophorus squamosus, Centroscymnus coelolepis" = "Centroscymnus coelolepis"),
         EcoRegion = strsplit(EcoRegion, ", ")
         ) %>%
  unnest(EcoRegion)

# Format the species names appropriately
# slFull
stock_list_frmt <- bind_rows(
  # Normal binomial names
  stock_list %>%
    filter(!grepl(" spp", Description),
           SpeciesID != "ANG",
           grepl("[[:space:]]", SpeciesScientificName)) %>%
    mutate(Description = str_replace_all(string = Description,
                                         pattern = SpeciesScientificName,
                                         replacement = paste0("<em>", SpeciesScientificName, "</em>"))),
  # Anglerfish (w/ two species)
  stock_list %>%
    filter(SpeciesID == "ANG") %>%
    mutate(Description = str_replace_all(string = Description,
                                         pattern = "Lophius piscatorius and L. budegassa",
                                         replacement = "<em>Lophius piscatorius</em> and <em>L. budegassa</em>")),
  # Groups of species (.spp)
  stock_list %>%
    filter(grepl(" spp.*$", Description)) %>%
    mutate(Description = str_replace_all(string = Description,
                                         pattern = SpeciesScientificName,
                                         replacement = paste0("<em>", SpeciesScientificName, "</em>"))),
  # A bit different notation (embedded in 2 sets of parentheses)
  stock_list %>%
    filter(StockCode == "raj-mar") %>%
    mutate(Description = str_replace_all(string = Description,
                                         pattern = "Raja clavata",
                                         replacement = "<em>Raja clavata</em>")),
  # The "others" with no species name
  stock_list %>%
    filter(SpeciesID != "ANG") %>%
    filter(!grepl(" spp", Description)) %>%
    filter(StockCode != "raj-mar") %>%
    filter(!grepl("[[:space:]]", SpeciesScientificName))
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: ICES Stock Assessment Graphs Database #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# msyStocks <- slFull$StockCode[slFull$AdviceCategory == "MSY"]
# paStocks <- slFull$StockCode[slFull$AdviceCategory == "PA"]
# mpStocks <- slFull$StockCode[slFull$AdviceCategory == "MP"]

# sl_2014 <- unique(slFull$StockCode[slFull$YearOfLastAssessment == 2014])
# sl_2015 <- unique(slFull$StockCode[slFull$YearOfLastAssessment == 2015])
# sl_2016 <- unique(slFull$StockCode[slFull$YearOfLastAssessment == 2016])
#
# sumCols <- c("Year", "fishstock", "F", "SSB", "fishingPressureDescription",
#              "stockSizeDescription", "landings", "catches", "discards")
#
# summaryTbl_2014 <- getSAG(stock = sl_2014, combine = TRUE, year = 2014, data = "summary")
# summaryTbl_2015 <- getSAG(stock = sl_2015, combine = TRUE, year = 2015, data = "summary")
# summaryTbl_2016 <- getSAG(stock = sl_2016, combine = TRUE, year = 2016, data = "summary")
#
# summaryTbl_2014 <- summaryTbl_2014[colnames(summaryTbl_2014) %in% sumCols]
# summaryTbl_2015 <- summaryTbl_2015[colnames(summaryTbl_2015) %in% sumCols]
# summaryTbl_2016 <- summaryTbl_2016[colnames(summaryTbl_2016) %in% sumCols]
#
# summaryTbl <- rbind(summaryTbl_2014, summaryTbl_2015, summaryTbl_2016)

sag_years <- unique(stock_list$YearOfLastAssessment)
sag_keys <- do.call("rbind", lapply(sag_years, function(x) findKey(stock = NULL,
                                                                   year = x,
                                                                   full = TRUE)[, c("AssessmentYear", "AssessmentKey", "FishStockName")]))
sag_keys$FishStockName <- tolower(sag_keys$FishStockName)

found_stocks <- stock_list %>%
  select(-EcoRegion) %>%
  distinct(.keep_all = TRUE) %>%
  left_join(sag_keys, by = c("StockCode" = "FishStockName",
                             "YearOfLastAssessment" = "AssessmentYear"))

# summaryTbl
sag_summary <- bind_rows(
  found_stocks %>%
    filter(!is.na(AssessmentKey)) %>%
    mutate(SummaryTable = purrr::map(AssessmentKey, getSummaryTable)) %>%
    unnest(SummaryTable),
  found_stocks %>%
    filter(is.na(AssessmentKey))
) %>%
  select(Year,
         StockCode,
         F,
         SSB,
         fishingPressureDescription,
         stockSizeDescription,
         landings,
         catches,
         discards)

# List of the F types that we want to include in the analysis and those that should be considered "relative"
keeper_F <- c("F", "F/FMSY", "F in winter rings", "Harvest rate", "Harvest rate/FMSY", "Fishing Pressure", "weighted F")
relative_F <- c("F/FMSY", "Harvest rate/FMSY")

# List of the SSB types that we want to include in the analysis and those that should be considered "relative"
keeper_SSB <- c("SSB", "B/BMSY", "SSB & Biomass Age 4+", "UW Tv Index", "Stock Size", "Total biomass/BMSY")
relative_SSB <- c("B/BMSY", "Total biomass/BMSY")

# Clean up the stock summary data
# summaryTblClean
sag_summary_clean <- sag_summary %>%
  # select(Year,
  #        StockCode = fishstock,
  #        F,
  #        SSB,
  #        fishingPressureDescription,
  #        stockSizeDescription,
  #        landings,
  #        catches,
  #        discards) %>%
  mutate(StockCode = tolower(StockCode),
         # SSB is not in SAG for meg-4a6a 2015. Added from:
         # http://ices.dk/sites/pub/Publication%20Reports/Expert%20Group%20Report/acom/2015/WGCSE/05.03_Megrim%20IV_VI_2015.pdf#page=22
         SSB = ifelse(Year == 2015 & StockCode == "meg-4a6a",
                      1.91,
                      SSB),
         # Clean up fishing pressure descriptions
         fishingPressureDescription = gsub("Fishing Pressure: " , "", fishingPressureDescription),
         fishingPressureDescription = gsub("Fishing pressure: " , "", fishingPressureDescription),
         fishingPressureDescription = gsub("msy" , "MSY", fishingPressureDescription),
         fishingPressureDescription = gsub("Harvest Rate", "Harvest rate", fishingPressureDescription),
         fishingPressureDescription = gsub("F &amp; HR", "Harvest rate", fishingPressureDescription),
         # Clean up SSB descriptions
         stockSizeDescription = gsub("Stock Size: ", "", stockSizeDescription),
         stockSizeDescription = gsub("Stock size: ", "", stockSizeDescription),
         stockSizeDescription = gsub("msy", "MSY", stockSizeDescription),
         stockSizeDescription = ifelse(is.na(stockSizeDescription), "Relative",
                                       stockSizeDescription),
         # Clean up reference points descriptions
         FmsyDescription = "FMSY",
         FmsyDescription = ifelse(fishingPressureDescription %in% relative_F,
                                  "F/FMSY",
                                  fishingPressureDescription),
         BmsyDescription = "MSYBtrigger",
         BmsyDescription = ifelse(stockSizeDescription %in% relative_SSB,
                                  "SSB/BMSY",
                                  stockSizeDescription)) # %>%
  # Remove the stocks that don't have F and SSB types that we want for the analysis
  # filter(stockSizeDescription %in% keeperSSB |
  #          fishingPressureDescription %in% keeperF) %>%
  # distinct(.keep_all = TRUE)


# refPtsClean
sag_ref_pts <- bind_rows(
  found_stocks %>%
    filter(!is.na(AssessmentKey)) %>%
    mutate(RefTable = purrr::map(AssessmentKey, getFishStockReferencePoints)) %>%
    unnest(RefTable),
  found_stocks %>%
    filter(is.na(AssessmentKey))
) %>%
  select(StockCode,
         Flim = FLim,
         Fpa,
         Bpa,
         Blim,
         FMSY,
         MSYBtrigger)

#
# # Get reference points from Stock Assessment Graphs
# refCols <- c("FishStockName", "FLim", "Fpa", "Bpa", "Blim", "FMSY", "MSYBtrigger")
#
# refPts_2014 <- getSAG(stock = sl_2014, combine = TRUE, year = 2014, data = "refpts")
# refPts_2015 <- getSAG(stock = sl_2015, combine = TRUE, year = 2015, data = "refpts")
# refPts_2016 <- getSAG(stock = sl_2016, combine = TRUE, year = 2016, data = "refpts")
#
# colnames(refPts_2014) <- gsub(" /", "", colnames(refPts_2014))
# refPts_2015 <- refPts_2015[,!names(refPts_2015) %in% grep(" /", colnames(refPts_2015), value = TRUE)]
# refPts_2016 <- refPts_2016[,!names(refPts_2016) %in% grep(" /", colnames(refPts_2016), value = TRUE)]
#
# refPts_2014 <- refPts_2014[colnames(refPts_2014) %in% refCols]
# refPts_2015 <- refPts_2015[colnames(refPts_2015) %in% refCols]
# refPts_2016 <- refPts_2016[colnames(refPts_2016) %in% refCols]
#
# refPts <- rbind(refPts_2014, refPts_2015, refPts_2016)
# refPts[refPts == ""] <- NA
#
# refPtsClean <- refPts %>%
#   rename(Flim = FLim,
#          StockCode = FishStockName) %>%
#   mutate(StockCode = tolower(StockCode)) %>%
#   distinct(.keep_all = TRUE)

# Merge together the stock data, reference points, and summary table
# fullSummary
sag_complete_summary <- stock_list_frmt %>%   #slFull
  left_join(sag_ref_pts , by = "StockCode") %>% # refPtsClean
  left_join(sag_summary_clean, by = "StockCode") %>% # summaryTblClean
  nest(EcoRegion) %>%
  mutate(MSYBtrigger = ifelse(stockSizeDescription %in% relative_SSB,
                              0.5,
                              MSYBtrigger),
         MSYBtrigger = ifelse(!stockSizeDescription %in% keeper_SSB,
                             NA,
                             MSYBtrigger),
         MSYBtrigger = ifelse(MSYBtrigger == 0,
                              NA,
                              MSYBtrigger),
         FMSY = ifelse(fishingPressureDescription %in% relative_F,
                       1,
                       FMSY),
         FMSY = ifelse(!fishingPressureDescription %in% keeper_F,
                       NA,
                       FMSY),
         FMSY = ifelse(FMSY == 0,
                       NA,
                       FMSY)
         )

# numCols <- c("Year", "F", "SSB", "landings", "catches", "Flim",
#              "Blim", "FMSY", "MSYBtrigger", "Fpa", "Bpa", "YearOfLastAssessment")
#
# fullSummary[colnames(fullSummary) %in% numCols] <- lapply(fullSummary[colnames(fullSummary) %in% numCols],
#                                                           as.numeric)

# Recreate check-mark tables
# fmsySummary
summary_fmsy <- sag_complete_summary %>% #fullSummary
  # select(-EcoRegion) %>%
  # distinct(.keep_all = TRUE) %>%
  group_by(StockCode) %>%
  filter(Year >= YearOfLastAssessment - 3,
         Year <= YearOfLastAssessment - 1) %>%
  mutate(FMSY = ifelse(F <= FMSY, "GREEN", "RED"),
         Year = paste0("FMSY", Year)) %>%
  select(Year, FMSY, StockCode) %>%
  spread(Year, FMSY)

summary_bmsy <- sag_complete_summary %>%
  # select(-EcoRegion) %>%
  # distinct(.keep_all = TRUE) %>%
  group_by(StockCode) %>%
  filter(Year >= YearOfLastAssessment - 2,
         Year <= YearOfLastAssessment) %>%
  mutate(BMSY = ifelse(SSB >= MSYBtrigger, "GREEN", "RED"),
         Year = paste0("BMSY", Year)) %>%
  select(Year, BMSY, StockCode) %>%
  spread(Year, BMSY)

summary_fpa <- sag_complete_summary %>%
  # select(-EcoRegion) %>%
  # distinct(.keep_all = TRUE) %>%
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

summary_bpa <- sag_complete_summary %>%
  # select(-EcoRegion) %>%
  # distinct(.keep_all = TRUE) %>%
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

summary_sbl <- sag_complete_summary %>%
  # select(-EcoRegion) %>%
  # distinct(.keep_all = TRUE) %>%
  select(StockCode,
         YearOfLastAssessment) %>%
  distinct(.keep_all = TRUE) %>%
  left_join(summary_bpa, by = "StockCode") %>%
  left_join(summary_fpa, by = "StockCode")

summary_sbl <- bind_rows(
  summary_sbl %>%
    filter(YearOfLastAssessment == 2014) %>%
    mutate(SBL = ifelse(FPA2013 == "GREEN"  &  BPA2014 == "GREEN",
                        "GREEN",
                        ifelse(FPA2013 == "RED"  &  BPA2014 == "RED",
                               "RED",
                               NA))),
  summary_sbl %>%
  filter(YearOfLastAssessment == 2015) %>%
    mutate(SBL = ifelse(FPA2014 == "GREEN"  &  BPA2015 == "GREEN",
                        "GREEN",
                        ifelse(FPA2014 == "RED"  &  BPA2015 == "RED",
                               "RED",
                               NA))),
  summary_sbl %>%
    filter(YearOfLastAssessment == 2016) %>%
    mutate(SBL = ifelse(FPA2015 == "GREEN"  &  BPA2016 == "GREEN",
                        "GREEN",
                        ifelse(FPA2015 == "RED"  &  BPA2016 == "RED",
                               "RED",
                               NA)))
  )[, c("StockCode", "SBL")]

#stockDF
summary_table <- summary_fmsy %>%
  full_join(summary_bmsy, by = "StockCode") %>%
  full_join(summary_fpa, by = "StockCode") %>%
  full_join(summary_bpa, by = "StockCode") %>%
  full_join(summary_sbl, by = "StockCode")

# stockDat
summary_table_frmt <- stock_list_frmt %>%  #slFull
  left_join(summary_table, by = "StockCode") %>%
  # nest(EcoRegion) %>%
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

summary_table_frmt <- bind_rows(
  summary_table_frmt %>%
    filter(YearOfLastAssessment == 2014) %>%
    mutate(D3C1 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                         FMSY2013,
                         FPA2013),
           D3C2 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                         BMSY2014,
                         BPA2014)),
  summary_table_frmt %>%
    filter(YearOfLastAssessment == 2015) %>%
    mutate(D3C1 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                         FMSY2014,
                         FPA2014),
           D3C2 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                         BMSY2015,
                         BPA2015)),
  summary_table_frmt %>%
    filter(YearOfLastAssessment == 2016) %>%
    mutate(D3C1 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                         FMSY2015,
                         FPA2015),
           D3C2 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                         BMSY2016,
                         BPA2016))
) %>%
  # unnest(EcoRegion) %>%
  select(StockCode,
         Description,
         FisheriesGuild,
         EcoRegion,
         AdviceCategory,
         DataCategory,
         SBL,
         F_2013, F_2014, F_2015,
         SSB_2014, SSB_2015, SSB_2016,
         D3C1, D3C2, GES)

summary_table_frmt$GES[is.na(summary_table_frmt$D3C1) |
               is.na(summary_table_frmt$D3C2)] <- NA
summary_table_frmt$GES[summary_table_frmt$D3C1 == "RED" |
                         summary_table_frmt$D3C2 == "RED"] <- "RED"
summary_table_frmt$GES[summary_table_frmt$D3C1 == "GREEN" &
                         summary_table_frmt$D3C2 == "GREEN"] <- "GREEN"

summary_table_frmt[c("SBL", "F_2013", "F_2014", "F_2015",
           "SSB_2014", "SSB_2015", "SSB_2016",
           "D3C1", "D3C2", "GES")][is.na(summary_table_frmt[c("SBL", "F_2013", "F_2014", "F_2015",
                                                    "SSB_2014", "SSB_2015", "SSB_2016",
                                                    "D3C1", "D3C2", "GES")])] <- "GREY"

summary_table_frmt[summary_table_frmt == "GREEN"] <- "<i class=\"glyphicon glyphicon-ok-sign\" style=\"color:green; font-size:2.2em\"></i>"
summary_table_frmt[summary_table_frmt == "RED"] <- "<i class=\"glyphicon glyphicon-remove-sign\" style=\"color:red; font-size:2.2em\"></i>"
summary_table_frmt[summary_table_frmt == "GREY"] <- "<i class=\"glyphicon glyphicon-question-sign\" style=\"color:grey; font-size:2.2em\"></i>"
summary_table_frmt[summary_table_frmt == "ORANGE"] <- "<i class=\"glyphicon glyphicon-record\" style=\"color:#FAB700; font-size:2.2em\"></i>"

summary_table_frmt <- data.frame(lapply(summary_table_frmt, factor))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Prepare subsets for R Markdown rendering #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ecoregion <- unique(stockDat$ECOREGION)
# fileName <-  "referencePointOverview-static.html"
# Render dynamic and static stock summary tables
ecoregion = unique(summary_table_frmt$EcoRegion)[10]

stockPlotEcoregion <- function(ecoregion,
                               fileName = NULL) {
  stockPlot <- summary_table_frmt %>%
    filter(grepl(pattern = ecoregion, EcoRegion)) %>%
    select(-EcoRegion) %>%
    distinct() %>%
    arrange(StockCode)

  if(is.null(fileName)) {
    fileName <- gsub("\\s", "_", ecoregion)
  }

  suppressWarnings(
  rmarkdown::render("~/git/ices-dk/fisheryO/vignettes/stockSummaryTable-static.Rmd",
                    output_file = paste0("~/git/ices-dk/fisheryO/output/", fileName, "-static.html"),
                    # rmarkdown::html_document(template = NULL),
                    envir = new.env())
  )

  suppressWarnings(
    rmarkdown::render("~/git/ices-dk/fisheryO/vignettes/stockSummaryTable-dynamic.rmd",
                    output_file = paste0("~/git/ices-dk/fisheryO/output/", fileName, "-dynamic.html"),
                    rmarkdown::html_document(template = NULL),
                    envir = new.env())
  )

}

eco_list <- unique(unlist(strsplit(as.character(stockDat$EcoRegion), ", ")))
lapply(eco_list[c(10, 7)], function(x) stockPlotEcoregion(x, fileName =  paste0("annexA-", gsub("\\s", "_", x))))

##############
# Pie graphs #
##############

# pieDat
pie_table <- stock_list_frmt %>% # slFull %>%
  select(StockCode,
         EcoRegion,
         FisheriesGuild,
         YearOfLastAssessment) %>%
  left_join(summary_table, by = "StockCode") %>% #stockDF
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

# pieCount
pie_table_count <- pie_table %>% #pieDat %>%
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
pie_table_count <- bind_rows(
  pie_table_count %>%
    tidyr::expand(VALUE = c("GREY", "GREEN", "RED", "ORANGE")) %>%
    left_join(pie_table_count, by = c("EcoRegion", "FisheriesGuild", "VARIABLE", "VALUE")) %>%
    arrange(EcoRegion, FisheriesGuild, VARIABLE, VALUE) %>%
    mutate(COUNT = ifelse(is.na(COUNT),
                          0,
                          as.numeric(COUNT))),
  pie_table_count %>%
    group_by(EcoRegion, VARIABLE, VALUE) %>%
    mutate(FisheriesGuild = "total",
           COUNT = sum(COUNT)) %>%
    distinct(.keep_all = TRUE)
)
#
#
# pieCount <- pieCount %>%
#   group_by(EcoRegion, VARIABLE, VALUE) %>%
#   mutate(FisheriesGuild = "total",
#          COUNT = sum(COUNT)) %>%
#   distinct(.keep_all = TRUE) %>%
#   bind_rows(pieCount)

stockPieEcoregion <- function(ecoregion) {

  colList <- c("GREEN" = "#00B26D",
               "GREY" = "#d3d3d3",
               "ORANGE" = "#ff7f00",
               "RED" = "#d93b1c")

  rowDat <- pie_table_count %>%
    filter(grepl(pattern = ecoregion, EcoRegion)) %>%
    ungroup() %>%
    select(-EcoRegion) %>%
    group_by(FisheriesGuild, VARIABLE) %>%
    mutate(fraction = COUNT/ sum(COUNT)) %>%
    filter(fraction != 0) %>%
    ungroup() %>%
    mutate(VARIABLE = recode_factor(VARIABLE,
                                    "FMSY" = "Fishing pressure\n MSY",
                                    "BMSY" = "Stock size\n MSY",
                                    "FPA" = "Fishing pressure\n PA",
                                    "BPA" = "Stock size \n PA",
                                    "SBL" = "Within safe\n biological limits"),
           FisheriesGuild = factor(FisheriesGuild,
                                      levels = c("total", "benthic", "crustacean", "elasmobranch", "demersal", "pelagic"))
           )

  # rowDat$FisheriesGuild <- factor(rowDat$FisheriesGuild,
  #                                 levels = c("total", "benthic", "crustacean", "elasmobranch", "demersal", "pelagic"))

  p1 <- ggplot(data = rowDat, aes(x = "", y = fraction, fill = VALUE)) +
    geom_bar(stat = "identity", width = 1) +
    geom_text(aes(label = COUNT),
              position = position_stack(vjust = 0.5),
              size = 3) +
    scale_fill_manual(values = colList) +
    theme_bw(base_size = 9) +
    theme(panel.grid = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position="none") +
    theme(axis.text=element_blank(),
          axis.ticks=element_blank(),
          strip.background = element_blank()) +
    labs(title = "", x = "", y = "",
         caption = "Data from ") +
    coord_polar(theta = "y", direction = 1) +
    facet_grid(FisheriesGuild ~ VARIABLE)

  figName = "table1_"
  ggsave(filename = paste0("~/git/ices-dk/fisheryO/output/", figName, ecoregion, ".png"),
         plot = p1,
         width = 178,
         height = 152,
         units = "mm",
         dpi = 300)
}

# lapply(unique(pieDat$ECOREGION)[6], stockPieEcoregion)

######################
### GES Pie Charts ###
######################

# Split and count by variable and color
# gesPieCount
ges_table_count <- pie_table %>% #pieDat %>%
  select(EcoRegion,
         D3C2 = FMSY,
         D3C1 = BMSY) %>%
  gather(VARIABLE, COLOR, -EcoRegion) %>%
  mutate(COLOR = ifelse(is.na(COLOR),
                        "GREY",
                        COLOR)) %>%
  group_by(EcoRegion, VARIABLE, COLOR) %>%
  summarize(VALUE = n(),
            METRIC = "count")

# Take last year of catch data. If catch is not available, use landings. Remove stocks without quantified catch or landings
# gesCatchStock
ges_catch_stock <-  sag_complete_summary %>% # fullSummary
  group_by(StockCode) %>%
  filter(Year == YearOfLastAssessment - 1) %>%
  ungroup() %>%
  mutate(CATCH = ifelse(is.na(catches) & !is.na(landings),
                        landings,
                        catches)) %>%
  filter(!is.na(CATCH)) %>%
  select(StockCode,
         CATCH) %>%
  distinct(.keep_all = TRUE)

# Split and sum catch by variable and color
# gesPieCatch
ges_table_catch <- ges_catch_stock %>%
 # <- gesCatchStock %>%
  left_join(pie_table, by = "StockCode") %>%
  select(EcoRegion,
         CATCH,
         D3C2 = FMSY,
         D3C1 = BMSY) %>%
  gather(VARIABLE, COLOR, -EcoRegion, -CATCH) %>%
  mutate(COLOR = ifelse(is.na(COLOR),
                        "GREY",
                        COLOR),
         CATCH = ifelse(is.na(CATCH),
                        0,
                        CATCH)) %>%
  group_by(EcoRegion, VARIABLE, COLOR) %>%
  summarize(VALUE = sum(CATCH),
            METRIC = "total_sum")

ges_table <- rbind(ges_table_count, ges_table_catch)


gesPieEcoregion <- function(ecoregion) {

  colList <- c("GREEN" = "#00B26D",
               "GREY" = "#d3d3d3",
               "RED" = "#d93b1c")

  rowDat <- ges_table %>%
    filter(grepl(pattern = ecoregion, EcoRegion)) %>%
    ungroup() %>%
    select(-EcoRegion) %>%
    group_by(VARIABLE, METRIC) %>%
    mutate(VALUE = ifelse(METRIC == "total_sum",
                          round(VALUE / 1000),
                          round(VALUE)),
           fraction = VALUE/ sum(VALUE)) %>%
    filter(fraction != 0) %>%
    ungroup() %>%
    mutate(METRIC = recode_factor(METRIC,
                                  "count" = "Number of stocks",
                                  "total_sum" = "Proportion of catch\n (thousand tonnes)"))
  sumDat <- rowDat %>%
    group_by(VARIABLE, METRIC) %>%
    summarize(sum = sum(VALUE),
              COLOR = NA)

  p1 <- ggplot(data = rowDat, aes(x = "", y = fraction, fill = COLOR)) +
    geom_bar(stat = "identity", width = 1) +
    geom_text(aes(label = scales::comma(VALUE)),
            position = position_stack(vjust = 0.5),
            size = 3) +
    geom_text(data = sumDat,
              aes(x = 0, y = 0,
                  label = paste0("total = ",
                                 scales::comma(sum))),
              size = 2.5) +
    scale_fill_manual(values = colList) +
    theme_bw(base_size = 9) +
    theme(panel.grid = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position="none") +
    theme(axis.text=element_blank(),
          axis.ticks=element_blank(),
          strip.background = element_blank()) +
    labs(title = "", x = "", y = "",
         caption = "Data from") +
    coord_polar(theta = "y") +
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
# colList <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
#              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")
# ltyList <- c(1,3:6)
# stockTrends
stock_trends <- sag_complete_summary %>% #fullSummary
  unnest(data) %>%
  mutate(F_FMSY = ifelse(!is.na(FMSY),
                         F / FMSY,
                         NA),
         SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
                                  SSB / MSYBtrigger,
                                  NA)) %>%
  select(Year,
         StockCode,
         FisheriesGuild,
         EcoRegion,
         F_FMSY,
         SSB_MSYBtrigger) %>%
  gather(METRIC, stockValue, -Year, -StockCode, -FisheriesGuild, -EcoRegion) %>%
  group_by(EcoRegion, FisheriesGuild, METRIC, Year) %>%
  mutate(ecoGuildMean = mean(stockValue, na.rm = TRUE))

stock_trends_frmt <- bind_rows(
  stock_trends %>%
    mutate(EcoGuild = paste0(EcoRegion, " - ", FisheriesGuild, " stocks")) %>%
    ungroup() %>%
    select(pageGroup = EcoGuild,
           lineGroup = StockCode,
           Year,
           plotGroup = METRIC,
           plotValue = stockValue) %>%
    filter(!is.na(plotValue)),

  stock_trends %>%
    mutate(EcoGuild = paste0(EcoRegion, " - ", FisheriesGuild, " stocks")) %>%
    ungroup() %>%
    distinct(EcoGuild, METRIC, Year, .keep_all = TRUE) %>%
    select(pageGroup = EcoGuild,
           Year,
           plotGroup = METRIC,
           plotValue = ecoGuildMean) %>%
    mutate(lineGroup = "MEAN") %>%
    filter(!is.na(plotValue))
)
# #
# # Set up colors
# plotList <- allDat %>%
#   group_by(pageGroup) %>%
#   select(pageGroup, lineGroup) %>%
#   mutate(nLines = n_distinct(lineGroup) - 1,
#          COLOR = NA) %>%
#   distinct(lineGroup, .keep_all = TRUE) %>%
#   arrange(pageGroup)
# #
# colorList <- bind_rows(
#   plotList %>%
#     filter(nLines == 1) %>%
#     mutate(COLOR = colList[1:length(nLines)]),
#   #
#   plotList %>%
#     filter(nLines <= 9 &
#              nLines > 1 &
#              lineGroup != "MEAN") %>%
#     mutate(COLOR = colList[1:length(nLines)]),
#   #
#   plotList %>%
#     filter(nLines > 9 &
#              lineGroup != "MEAN") %>%
#     mutate(COLOR = "grey80"),
#   #
#   plotList %>%
#     filter(nLines > 1 &
#              lineGroup == "MEAN") %>%
#     mutate(COLOR = "grey40")
# )


# allDat <- allDat %>%
#   left_join(colorList, by = c("pageGroup", "lineGroup")) %>%
#   group_by(pageGroup) %>%
#   mutate(nLines = n_distinct(lineGroup)) %>%
#   filter(nLines > 2 | lineGroup != "MEAN") %>%
#   filter(lineGroup != "MEAN" | Year != 2016 | plotGroup != "F_FMSY")



# allDat <- stockTrends
EcoGuild <- unique(stock_trends_frmt$pageGroup)[6]
stock_trends_function <- function(EcoGuild,
                                  fileName = NULL,
                                  dynamic = TRUE) {

  clicks <- sag_complete_summary %>%
    mutate(onclick = sprintf("window.open(\"%s%i/%i/%s.pdf\")",
                             "http://ices.dk/sites/pub/Publication%20Reports/Advice/",
                             YearOfLastAssessment,
                             YearOfLastAssessment,
                             StockCode)) %>%
    select(StockCode,
           Description,
           onclick) %>%
    distinct(.keep_all = TRUE)

    p1_dat <- stock_trends_frmt %>%
    left_join(clicks, by = c("lineGroup" = "StockCode")) %>%
    filter(grepl(EcoGuild, pageGroup)) %>%
    mutate(
      # tooltip_point = ifelse(plotGroup == "F_FMSY",
      #                             sprintf("<b>%s</b>
      #                                     <br>F/F<sub>MSY</sub>: %s</br>",
      #                                     Description,
      #                                     round(plotValue, 2)),
      #                             sprintf("<b>%s</b>
      #                                     <br>SSB/MSY B<sub>trigger</sub>: %s</br>",
      #                                     Description,
      #                                     round(plotValue, 2))),
           tooltip_line =   sprintf("<b>%s</b>",
                                    ifelse(lineGroup == "MEAN",
                                           "mean",
                                           Description)),
           plotGroup = factor(plotGroup,
                              labels = c("F/F[MSY]", "SSB/MSY~B[trigger]"))) %>%
    select(-Description)

  adj_names = sort(setdiff(unique(p1_dat$lineGroup), "MEAN"))
  values = gg_color_hue(length(adj_names))
  names(values) = adj_names
  values = c(values, c(MEAN = "black"))

  # p1_dat$plotGroup <- factor(p1_dat$plotGroup,
  #                            labels = c("F/F[MSY]", "SSB/MSY~B[trigger]"))

  if(is.null(fileName)) {
    fileName <- gsub("\\s", "_", EcoGuild)
    fileName <- gsub("_-_", "-", fileName)
  }

  plot_title <- gsub(".*\\s-\\s", "\\1", EcoGuild)

  p1_plot <- ggplot(p1_dat, aes(x = Year, y = plotValue,
                                color = lineGroup,
                                fill = lineGroup,
                                onclick = onclick,
                                data_id = lineGroup,
                                tooltip = tooltip_line)) +
    geom_hline(yintercept = 1, col = "grey60") +
    theme_bw(base_size = 9) +
    scale_color_manual(values = values) +
    scale_fill_manual(values = values) +
    guides(fill = FALSE) +
    theme(legend.position = "bottom",
          strip.text = element_text(size = 9, angle = 0, hjust = 0),
          strip.background = element_blank(),
          strip.placement = "outside",
          panel.grid.major = element_blank(),
          legend.key = element_rect(colour = NA)) +
    labs(title = plot_title, x = "Year", y = "", color = "Stock code",
         caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                           lubridate::year(Sys.time()),
                           lubridate::month(Sys.time(), label = TRUE, abbr = FALSE))) +
    facet_wrap(~ plotGroup, labeller = label_parsed, strip.position = "left")

  if(dynamic) {
  p1_plot <- p1_plot + geom_line_interactive(alpha = 0.6)
  suppressWarnings(
    rmarkdown::render("~/git/ices-dk/fisheryO/vignettes/stockStatusTrends-dynamic.rmd",
                      output_file = paste0("~/git/ices-dk/fisheryO/output/figure08_", fileName, "-dynamic.html"),
                      rmarkdown::html_document(template = NULL),
                      envir = new.env())
  )
  }
  if(!dynamic) {
    p1_plot <- p1_plot + geom_line(alpha = 0.6)

        ggsave(filename = paste0("~/git/ices-dk/fisheryO/output/figure08_", fileName, "-static.png"),
           plot = p1_plot,
           width = 170,
           height = 100.5,
           units = "mm",
           dpi = 300)
  }
}

EcoGuild <- unique(allDat$pageGroup)[8]
stock_trends_function(unique(allDat$pageGroup)[8],
                      fileName = "TESTER", dynamic = TRUE)
#
# eco_list <- unique(unlist(strsplit(as.character(p1$EcoRegion), ", ")))
# lapply(eco_list[c(10, 7)], function(x) stockPlotEcoregion(x, fileName =  paste0("annexA-", gsub("\\s", "_", x))))
#
# p1_plot <- ggplot(p1_dat, aes(x = Year, y = plotValue,
#                               color = lineGroup,
#                               fill = lineGroup,
#                               onclick = onclick,
#                               data_id = lineGroup,
#                               tooltip = tooltip_line)) +
#   geom_hline(yintercept = 1, col = "grey60") +
#   theme_bw(base_size = 9) +
#   scale_color_manual(values = values) +
#   scale_fill_manual(values = values) +
#   guides(fill = FALSE) +
#   theme(legend.position = "bottom",
#         strip.text = element_text(size = 9, angle = 0, hjust = 0),
#         caption.text = element_text(size = 7, angle = 0, hjust = 0),
#         strip.background = element_blank(),
#         legend.key = element_rect(colour = NA)) +
#   labs(title = "Demersal", x = "", y = "Year", color = "Stock code",
#        caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
#                          lubridate::year(Sys.time()),
#                          lubridate::month(Sys.time(), label = TRUE, abbr = FALSE))) +
#   facet_grid(~ plotGroup, labeller = label_parsed)
#
#
# p1_plot <- p1_plot + geom_line_interactive(alpha = 0.6)
#
# ggiraph(code = print(p1_plot),
#         hover_css = "cursor:pointer;stroke:black;stroke-width:3pt;")
#

#
# source("~/git/ices-dk/fisheryO/R/stockSummaryTrends.R")
# # create stock summary trend figures
# stockSummaryTrends(df = allDat[allDat$plotGroup == "F_FMSY",],
#                    overallMean = TRUE,
#                    plotDir = "~/git/ices-dk/fisheryO/output/")
#

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

stock_catch <- sag_complete_summary %>%
  unnest(data) %>%
  group_by(StockCode) %>%
  filter(Year == YearOfLastAssessment - 1) %>%
  mutate(F_FMSY =  ifelse(!is.na(FMSY),
                          F / FMSY,
                          NA)) %>%
  select(StockCode,
         FisheriesGuild,
         EcoRegion,
         F_FMSY,
         catches,
         landings,
         discards)

# Calculate any possible remaining values
stock_catch_full <-
  bind_rows(
    stock_catch %>% # all present and accounted for
      filter(!is.na(catches),
             !is.na(landings),
             !is.na(discards)), # 43
    stock_catch %>%  # Missing discards, but catches == landings
      filter(is.na(discards),
             catches == landings) %>%
    mutate(discards = 0), #36
    stock_catch %>% # Missing catches, but have landings and discards
      filter(is.na(catches),
             !is.na(landings),
             !is.na(discards)) %>%
      mutate(catches = landings + discards), #75
    stock_catch %>% # missing catches, but have landings
      filter(is.na(catches),
             !is.na(landings),
             is.na(discards)) %>%
      mutate(catches = NA,
             discards = NA),
    stock_catch %>% # missing everything
      filter(is.na(catches),
             is.na(landings),
             is.na(discards)) %>%
      mutate(catches = NA,
             discards = NA,
             landings = NA),
    stock_catch %>% # missing landings and discards
      filter(!is.na(catches),
             is.na(landings),
             is.na(discards)) %>%
      mutate(landings = NA,
             discards = NA),
    stock_catch %>% # landings and catches
      filter(is.na(catches),
             is.na(landings),
             !is.na(discards)) %>%
      mutate(catches = NA,
             landings = NA),
    stock_catch %>% # Missing discards, but have landings and catches
      filter(!is.na(catches),
             !is.na(landings),
             is.na(discards),
             landings != catches) %>%
      mutate(discards = catches - landings)
    )



stock_status_full <-
  full_join(
    # sag_complete_summary %>%
    #   unnest(data) %>%
    #   group_by(StockCode) %>%
    #   filter(Year == YearOfLastAssessment - 1) %>%
    #   mutate(F_FMSY =  ifelse(!is.na(FMSY),
    #                           F / FMSY,
    #                           NA)) %>%
    #   select(StockCode,
    #          FisheriesGuild,
    #          EcoRegion,
    #          F_FMSY,
    #          catches,
    #          landings),

    sag_complete_summary %>%
      unnest(data) %>%
      group_by(StockCode) %>%
      filter(Year == YearOfLastAssessment) %>%
      mutate(SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
                                      SSB / MSYBtrigger,
                                      NA)) %>%
      select(StockCode,
             FisheriesGuild,
             EcoRegion,
             SSB_MSYBtrigger),
    stock_catch_full,
    by = c("StockCode",
           "FisheriesGuild",
           "EcoRegion")
  ) %>%
  mutate(colList = if_else(F_FMSY < 1 & SSB_MSYBtrigger >= 1,
                           "GREEN" ,
                           "RED",
                           "GREY"))

# %>%
#
#   sag_complete_summary %>% # fullSummary
#   unnest(data) %>%
#   group_by(StockCode) %>%
#   filter(Year == YearOfLastAssessment - 1) %>%
#   ungroup() %>%
#   select(stockCode)
#   mutate(CATCH = ifelse(is.na(catches) & !is.na(landings),
#                         landings,
#                         catches)) %>%
#   filter(!is.na(CATCH)) %>%
#   select(StockCode,
#          CATCH) %>%
#
#
#   full_join(ges_catch_stock, by = "StockCode") %>%
#   mutate(colList = if_else(F_FMSY < 1 & SSB_MSYBtrigger >= 1,
#                            "GREEN" ,
#                            "RED",
#                            "GREY"))

# #
# #   mutate(EcoRegion = strsplit(as.character(EcoRegion), ", ")) %>%
# #   unnest(EcoRegion) %>%
# #   distinct(.keep_all = TRUE) %>%
#   # filter(Year == 2015) %>%
#   mutate(F_FMSY = ifelse(!is.na(FMSY),
#                          F / FMSY,
#                          NA),
#          SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
#                                   SSB / MSYBtrigger,
#                                   NA),
#          CATCH = ifelse(is.na(catches) & !is.na(landings),
#                            landings,
#                         catches)) %>%
#   select(Year,
#          YearOfLastAssessment,
#          StockCode,
#          EcoRegion,
#          FisheriesGuild,
#          F_FMSY,
#          SSB_MSYBtrigger,
#          CATCH)
#
# stockStatusF <- stockStatusDat %>%
#   # left_join(td, by = "STOCK.CODE") %>%
#   group_by(StockCode) %>%
#   filter(Year == YearOfLastAssessment - 1) %>%
#   # mutate(SSB_MSYBtrigger = NA) %>%
#   select(-Year,
#          -SSB_MSYBtrigger,
#          -YearOfLastAssessment)
#
# stockStatusB <- stockStatusDat %>%
#   # left_join(td, by = "STOCK.CODE") %>%
#   group_by(StockCode) %>%
#   filter(Year == YearOfLastAssessment) %>%
#   # mutate(F_FMSY = NA) %>%
#   select(-Year,
#          -F_FMSY,
#          -CATCH,
#          -YearOfLastAssessment)

# stockStatusFull <- stockStatusF %>%
#   left_join(stockStatusB, c("StockCode", "EcoRegion", "FisheriesGuild")) %>%
#   mutate(colList = if_else(F_FMSY < 1 & SSB_MSYBtrigger >= 1,
#                   "GREEN" ,
#                   "RED",
#                   "GREY"))

# stockStatusFull$colList[is.na(stockStatusFull$F_FMSY) |
#                           is.na(stockStatusFull$SSB_MSYBtrigger)] <- "GREY"

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


