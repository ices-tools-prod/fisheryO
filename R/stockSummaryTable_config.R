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
library(ReporteRs)
library(lubridate)
library(ggiraph)
library(gridExtra)
library(grid)

# Function to help with consistent colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1 : n]
}

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
  filter(!StockCode %in% c("cod-ingr", "cod-wgr", "sal-nea", "san-scow", "sal-na",
                           "sal-32", "trt-bal", "sal-wgc")) %>%
  mutate(DataCategory = floor(as.numeric(DataCategory)),
         StockCode = tolower(StockCode),
         FisheriesGuild = tolower(FisheriesGuild),
         Description = gsub(pattern = "\u2013", "-", Description), # remove en dashes in favor of hyphens
         Description = gsub(pattern = "Micromesistius poutasso",
                            "Micromesistius poutassou", Description), # misspelled blue whiting
         AdviceCategory = ifelse(AdviceCategory == "MSY/PA",
                              "MSY", AdviceCategory),
         SpeciesID = toupper(gsub( "-.*$", "", StockCode)) ,
         SpeciesScientificName = recode(SpeciesScientificName,
                                        "Mustelus asterias" = "Mustelus"),
         SpeciesScientificName = recode(SpeciesScientificName,
                                        "Centrophorus squamosus, Centroscymnus coelolepis" = "Centroscymnus coelolepis"),
         EcoRegion = strsplit(EcoRegion, ", ")
         ) %>%
  unnest(EcoRegion) %>%
  mutate(EcoRegion = ifelse(grepl("Norwegian|Barents", EcoRegion),
                       "Norwegian Sea and Barents Sea Ecoregions",
                       EcoRegion))

# Format so the species names will be italicized appropriately
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
  mutate(StockCode = tolower(StockCode),
         # SSB is not in SAG for meg-4a6a 2015. Added from:
         # http://ices.dk/sites/pub/Publication%20Reports/Expert%20Group%20Report/acom/2015/WGCSE/05.03_Megrim%20IV_VI_2015.pdf#page=22
         SSB = ifelse(Year == 2015 & StockCode == "meg-4a6a",
                      1.91,
                      SSB),
         discards = ifelse(Year == 2015 & StockCode == "nep-5",
                      NA,
                      discards),
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

# Merge together the stock data, reference points, and summary table
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

# ~~~~~~~~~~~~~~~~~~~~ #
# Stock Summary Table  #
# ~~~~~~~~~~~~~~~~~~~~ #
stockSummaryTable_fun <- function(ecoregion,
                                  fileName = NULL) {
  stockPlot <- summary_table_frmt %>%
    filter(grepl(pattern = ecoregion, EcoRegion)) %>%
    select(-EcoRegion) %>%
    distinct() %>%
    arrange(StockCode)

  if(is.null(fileName)) {
    fileName <- gsub("\\s", "_", ecoregion)
  }

  # suppressWarnings(
  #   rmarkdown::render("~/git/ices-dk/fisheryO/vignettes/stockSummaryTable-static.Rmd",
  #                     output_file = paste0("~/git/ices-dk/fisheryO/output/", fileName, "-static.html"),
  #                     # rmarkdown::html_document(template = NULL),
  #                     envir = new.env())
  # )

  suppressWarnings(
    rmarkdown::render("~/git/ices-dk/fisheryO/vignettes/stockSummaryTable-dynamic.rmd",
                      output_file = paste0("~/git/ices-dk/fisheryO/output/", fileName, "-dynamic.html"),
                      rmarkdown::html_document(template = NULL),
                      envir = new.env())
  )
}

# lapply(grep("Greater|Celtic|Baltic",
#             unique(summary_table_frmt$EcoRegion),
#             value = TRUE),
#        function(x) stockSummaryTable_fun(x,
#                                       fileName =  paste0("annexA-",
#                                                          gsub("\\s", "_", x))))

#~~~~~~~~~~~~#
# Pie graphs #
#~~~~~~~~~~~~#

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

stockPie_fun <- function(ecoregion, fig_name) {

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
          strip.background = element_blank(),
          plot.caption = element_text(size = 6)) +
    labs(title = "", x = "", y = "",
         caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                           lubridate::year(Sys.time()),
                           lubridate::month(Sys.time(), label = TRUE, abbr = FALSE))) +
    coord_polar(theta = "y", direction = 1) +
    facet_grid(FisheriesGuild ~ VARIABLE)

  # figName = "table1_"
  ggsave(filename = paste0("~/git/ices-dk/fisheryO/output/", fig_name, ecoregion, ".png"),
         plot = p1,
         width = 178,
         height = 152,
         units = "mm",
         dpi = 300)
}

lapply(grep("Greater|Celtic|Baltic",
            unique(pie_table_count$EcoRegion),
            value = TRUE), stockPie_fun, fig_name = "figure10_")

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


gesPie_fun <- function(ecoregion, fig_name) {

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
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          strip.background = element_blank(),
          plot.caption = element_text(size = 6)) +
    labs(title = "", x = "", y = "",
         caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                           lubridate::year(Sys.time()),
                           lubridate::month(Sys.time(), label = TRUE, abbr = FALSE))) +
    coord_polar(theta = "y") +
    facet_grid(METRIC ~ VARIABLE)

  # figName = "table2_"
  ggsave(filename = paste0("~/git/ices-dk/fisheryO/output/", fig_name, "_", ecoregion, ".png"),
         plot = p1,
         width = 89,
         height = 100.5,
         units = "mm",
         dpi = 300)
}
suppressWarnings(lapply(grep("Greater|Celtic|Baltic",
                             unique(pie_table_count$EcoRegion),
                             value = TRUE), gesPie_fun, fig_name = "figure11"))


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
stock_trends_fun <- function(EcoGuild,
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
    mutate(tooltip_line =   sprintf("<b>%s</b>",
                                    ifelse(lineGroup == "MEAN",
                                           "mean",
                                           Description)),
           plotGroup = factor(plotGroup,
                              labels = c("F/F[MSY]", "SSB/MSY~B[trigger]"))) %>%
    select(-Description)

    if(length(unique(p1_dat$lineGroup)) <= 2){
      p1_dat <- p1_dat %>%
        filter(lineGroup != "MEAN")
    }

    adj_names = sort(setdiff(unique(p1_dat$lineGroup), "MEAN"))
    values = gg_color_hue(length(adj_names))
    names(values) = adj_names
    values = c(values, c(MEAN = "black"))

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
          legend.key = element_rect(colour = NA),
          plot.caption = element_text(size = 6)) +
    labs(title = plot_title, x = "Year", y = "", color = "Stock code",
         caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                           lubridate::year(Sys.time()),
                           lubridate::month(Sys.time(), label = TRUE, abbr = FALSE))) +
    facet_wrap(~ plotGroup, labeller = label_parsed, strip.position = "left")

  if(dynamic) {
  p1_plot <- p1_plot + geom_line_interactive(alpha = 0.6)
  suppressWarnings(
    rmarkdown::render("~/git/ices-dk/fisheryO/vignettes/stockStatusTrends-dynamic.rmd",
                      output_file = paste0("~/git/ices-dk/fisheryO/output/", fileName, "_", EcoGuild, "-dynamic.html"),
                      rmarkdown::html_document(template = NULL),
                      envir = new.env())
  )
  }
  if(!dynamic) {
    p1_plot <- p1_plot + geom_line(alpha = 0.6)

        ggsave(filename = paste0("~/git/ices-dk/fisheryO/output/", fileName,"_", EcoGuild, "-static.png"),
           plot = p1_plot,
           width = 170,
           height = 100.5,
           units = "mm",
           dpi = 300)
  }
}


lapply(grep("Greater|Celtic|Baltic", unique(stock_trends_frmt$pageGroup), value = TRUE),
       function(x) stock_trends_fun(x,
                                    fileName = "figure12_",
                                    dynamic = FALSE))
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
  select(Year,
         YearOfLastAssessment,
         StockCode,
         Description,
         FisheriesGuild,
         EcoRegion,
         F,
         FMSY,
         SSB,
         MSYBtrigger,
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
    stock_catch_full %>%
    # sag_complete_summary %>%
      # unnest(data) %>%
      group_by(StockCode) %>%
      filter(Year == YearOfLastAssessment - 1) %>%
      mutate(F_FMSY =  ifelse(!is.na(FMSY),
                              F / FMSY,
                              NA)) %>%
      select(StockCode,
             Description,
             FisheriesGuild,
             EcoRegion,
             F_FMSY,
             catches,
             landings,
             discards,
             FMSY,
             F),
    stock_catch_full %>%
      # sag_complete_summary %>%
      # unnest(data) %>%
      group_by(StockCode) %>%
      filter(Year == YearOfLastAssessment) %>%
      mutate(SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
                                      SSB / MSYBtrigger,
                                      NA)) %>%
      select(StockCode,
             FisheriesGuild,
             EcoRegion,
             SSB_MSYBtrigger,
             SSB,
             MSYBtrigger),
    by = c("StockCode",
           "FisheriesGuild",
           "EcoRegion")
  ) %>%
  mutate(colList = if_else(F_FMSY < 1 & SSB_MSYBtrigger >= 1,
                           "GREEN" ,
                           "RED",
                           "GREY"),
         FisheriesGuild = ifelse(StockCode %in% c("whb-comb", "mac-nea"),
                                 "large-scale stocks",
                                 FisheriesGuild))


# ecoregion = "Greater North Sea Ecoregion"
# guild = "all"

plot_kobe <- function(ecoregion,
                      guild = c("all",
                                "benthic",
                                "demersal",
                                "pelagic",
                                "crustacean",
                                "elasmobranch",
                                "large-scale stocks")[1],
                      plotDir = "~/git/ices-dk/fisheryO/output/",
                      catch_limit = 0,
                      fileName = "figure13_",
                      plotTitle = NULL,
                      fig.width = 110,
                      fig.height = 75,
                      units = "mm",
                      res = 300,
                      dynamic = FALSE) {


  plotName <- paste0(plotDir, fileName, ecoregion, "-", guild, ".png")
  #
  labTitle <- guild

  # guild <- c("benthic", "demersal", "pelagic", "crustacean", "elasmobranch")
  if(any(guild %in% "all")) {
    guild <- c("benthic", "demersal", "pelagic", "crustacean", "elasmobranch")
    labTitle <- "All stocks"

  }

  kobeDat <- stock_status_full %>%
    filter(EcoRegion == ecoregion,
           FisheriesGuild %in% guild,
           !is.na(F_FMSY),
           !is.na(SSB_MSYBtrigger)) %>%
    group_by(StockCode) %>%
    mutate(max_bar = max(catches, landings, discards, na.rm = TRUE),
           catch_width = ifelse(is.na(catches),
                                0,
                                round((catches/(max_bar/1.25) * 100))),
           landings_width = ifelse(is.na(landings),
                                   0,
                                   round((landings/(max_bar/1.25) * 100))),
           discards_width = ifelse(is.na(discards),
                                   0,
                                   round((discards/(max_bar/1.25) * 100))),
           total = ifelse(all(is.na(catches) & is.na(landings)),
                          NA,
                          max(catches, landings, na.rm = TRUE))) %>%
    distinct(.keep_all = TRUE)

  kobeDat$tip <- sprintf('
                         <div class="tipchart">
                         <h6>%s</h6>
                         <table>
                         <tr class="tiprow">
                         <td class="tipbarticks">F / F<sub>MSY</sub></td>
                         <td class="tipbardiv"><div class="tipbar" style="width:0px;">%3.2f&nbsp/&nbsp%3.2f&nbsp=&nbsp%3.2f</div></td>
                         </tr>
                         <tr class="tiprow">
                         <td class="tipbarticks">SSB / MSY B<sub>trigger</sub></td>
                         <td class="tipbardiv"><div class="tipbar" style="width:0px;">%3.0f&nbsp/&nbsp%3.0f&nbsp=&nbsp%3.2f</div></td>
                         </tr>
                         <tr class="tiprow">
                         <td class="tipbarticks">Catch (tonnes)</td>
                         <td class="tipbardiv"><div class="tipbar" style="width:%dpx;">%3.0f</div></td>
                         </tr>
                         <tr class="tiprow">
                         <td class="tipbarticks">Landings (tonnes)</td>
                         <td class="tipbardiv"><div class="tipbar" style="width:%dpx;">%3.0f</div></td>
                         </tr>
                         <tr class="tiprow">
                         <td class="tipbarticks">Discards (tonnes)</td>
                         <td class="tipbardiv"><div class="tipbar" style="width:%dpx;">%3.0f</div></td>
                         </tr>
                         </table>
                         </div>',
                         kobeDat$Description,
                         kobeDat$F, kobeDat$FMSY, kobeDat$F/kobeDat$FMSY,
                         kobeDat$SSB, kobeDat$MSYBtrigger, kobeDat$SSB/kobeDat$MSYBtrigger,
                         kobeDat$catch_width, kobeDat$catches,
                         kobeDat$landings_width, kobeDat$landings,
                         kobeDat$discards_width, kobeDat$discards)

  # javascript is too dumb to deal with line breaks in strings well
  kobeDat$tip <- gsub("\\\n", "", kobeDat$tip)

  if(length(guild) >= 2) {
    kobeDat <- filter(kobeDat, total >= catch_limit)
  }
  # catch_limit = 10000
  if(nrow(kobeDat) != 0) {

    labs <- seq(0, max(kobeDat$F_FMSY, kobeDat$SSB_MSYBtrigger, na.rm = TRUE) + 1)

    kobe_plot <- ggplot(kobeDat, aes(x = F_FMSY, y = SSB_MSYBtrigger,
                                     data_id = StockCode,
                                     tooltip = tip)) +
      # geom_point(aes(color = colList,
      #                size = catches),
      #            alpha = 0.7) +
      geom_point(aes(color = colList), size = 2,
                 alpha = 0.7) +
      geom_hline(yintercept = 1, color = "grey60", linetype = "dashed") +
      geom_vline(xintercept = 1, color = "grey60", linetype = "dashed") +
      geom_text_repel(aes(label = StockCode),
                      # box.padding = unit(.5, 'lines'),
                      # label.padding = unit(.5, 'lines'),
                      segment.size = .25,
                      force = 5,
                      size = 2) +
      # scale_size("catches", range = c(1, 10)) +
      scale_color_manual(values = c("GREEN" = "#4daf4a",
                                    "RED" = "#e41a1c",
                                    "GREY" = "#d3d3d3")) +
      scale_y_continuous(breaks = labs) +
      scale_x_continuous(breaks = labs) +
      coord_equal(xlim = range(labs), ylim = range(labs)) +
      labs(x = expression(F/F[MSY]),
           y = expression(SSB/MSY~B[trigger]),
           caption ="") +
      theme_bw(base_size = 7) +
      theme(legend.position = 'none',
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.caption = element_text(size = 6))

    # Lollipop plot
    catchBar <- stock_status_full %>%
      # ungroup() %>%
      filter(EcoRegion == ecoregion,
             FisheriesGuild %in% guild) %>%
      distinct(.keep_all = TRUE) %>%
      group_by(StockCode) %>%
      mutate(total = ifelse(all(is.na(catches) & is.na(landings)),
                            NA,
                            max(catches, landings, na.rm = TRUE))) %>%
      ungroup() %>%
      arrange(!is.na(total), total) %>%
      mutate(StockCode = factor(StockCode, StockCode))


    if(length(guild) >= 2) {
      catchBar <- filter(catchBar, total >= catch_limit)
    }

    bar_plot <-
      ggplot(catchBar, aes(x = StockCode, y = catches)) +
      geom_segment(aes(x = StockCode, y = catches, xend = StockCode, yend = 0, color = colList), size = 2) +
      geom_segment(aes(x = StockCode, y = landings, xend = StockCode, yend = 0, color = colList), size = 2) +
      geom_point(stat = "identity", aes(y = catches, fill = colList), color = "grey50", shape = 24, size = 2, alpha = 0.8) +
      geom_point(stat = "identity", aes(y = landings, fill = colList), color = "grey50", shape = 21, size = 2, alpha = 0.8) +
      scale_fill_manual(values = c("GREEN" = "#4daf4a",
                                   "RED" = "#e41a1c",
                                   "GREY" = "#d3d3d3")) +
      scale_color_manual(values = c("GREEN" = "#4daf4a",
                                    "RED" = "#e41a1c",
                                    "GREY" = "#d3d3d3")) +
      labs(x = "Stock",
           y = "Catch and landings (tonnes)",
           caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                             lubridate::year(Sys.time()),
                             lubridate::month(Sys.time(), label = TRUE, abbr = FALSE))) +
      coord_equal() +
      coord_flip() +
      theme_bw(base_size = 7) +
      theme(legend.position = 'none',
            plot.caption = element_text(size = 6),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line( size = 0.1, color = "grey80"))

        if(!dynamic) {
      # kobe_plot <- kobe_plot +  geom_point(aes(color = colList,
      #                                          size = catches),
      #                                      alpha = 0.7)

      png(plotName, width = fig.width, height = fig.height, units = units, res = res)
      grid.arrange(kobe_plot,
                   bar_plot, ncol = 2, respect = TRUE, top = labTitle)
      dev.off()
      }

    if(dynamic) {
      kobe_plot <- kobe_plot +  geom_point_interactive(color = "white",
                                                       fill = "white",
                                                       shape = 21,
                                                       size = 2,
                                                       alpha = 0.01)

      if(length(guild) > 1) guild = "all"

      suppressWarnings(
        rmarkdown::render("~/git/ices-dk/fisheryO/vignettes/kobe-dynamic.rmd",
                          output_file = paste0("~/git/ices-dk/fisheryO/output/",
                                               fileName, "_",
                                               ecoregion, "-",
                                               guild, "-dynamic.html"),
                          envir = new.env())
              )
    }

  } else ("No stocks have MSY status")
}


lapply(grep("Greater|Celtic|Baltic",
            unique(stock_status_full$EcoRegion),
            value = TRUE)[3], function(x) plot_kobe(ecoregion = x,
                                                 guild = "all",
                                                 catch_limit = 0,
                                                 fileName = "figure13_",
                                                 fig.width = 174,
                                                 fig.height = 118,
                                                 dynamic = FALSE))
lapply(grep("Greater|Celtic|Baltic",
            unique(stock_status_full$EcoRegion),
            value = TRUE), function(x) plot_kobe(ecoregion = x,
                                                 guild = "large-scale stocks",
                                                 catch_limit = 0,
                                                 fileName = "figure13_",
                                                 fig.width = 174,
                                                 fig.height = 118,
                                                 dynamic = FALSE))
lapply(grep("Greater|Celtic|Baltic",
            unique(stock_status_full$EcoRegion),
            value = TRUE), function(x) plot_kobe(ecoregion = x,
                                                 guild = "benthic",
                                                 catch_limit = 0,
                                                 fileName = "figure13_",
                                                 fig.width = 174,
                                                 fig.height = 118,
                                                 dynamic = FALSE))
lapply(grep("Greater|Celtic|Baltic",
            unique(stock_status_full$EcoRegion),
            value = TRUE), function(x) plot_kobe(ecoregion = x,
                                                 guild = "demersal",
                                                 catch_limit = 0,
                                                 fileName = "figure13_",
                                                 fig.width = 174,
                                                 fig.height = 118,
                                                 dynamic = FALSE))

lapply(grep("Greater|Celtic|Baltic",
            unique(stock_status_full$EcoRegion),
            value = TRUE), function(x) plot_kobe(ecoregion = x,
                                                 guild = "elasmobranch",
                                                 catch_limit = 0,
                                                 fileName = "figure13_",
                                                 fig.width = 174,
                                                 fig.height = 118,
                                                 dynamic = FALSE))

lapply(grep("Greater|Celtic|Baltic",
            unique(stock_status_full$EcoRegion),
            value = TRUE), function(x) plot_kobe(ecoregion = x,
                                                 guild = "crustacean",
                                                 catch_limit = 0,
                                                 fileName = "figure13_",
                                                 fig.width = 174,
                                                 fig.height = 118,
                                                 dynamic = FALSE))
lapply(grep("Greater|Celtic|Baltic",
            unique(stock_status_full$EcoRegion),
            value = TRUE), function(x) plot_kobe(ecoregion = x,
                                                 guild = "pelagic",
                                                 catch_limit = 0,
                                                 fileName = "figure13_",
                                                 fig.width = 174,
                                                 fig.height = 118,
                                                 dynamic = FALSE))
######################
### Catch MSY Plot ###
######################
# fullSummary
count_by_msy <- sag_complete_summary %>%
  unnest(data) %>%
  filter(Year >= 1995) %>%
  group_by(EcoRegion, FisheriesGuild) %>%
  summarize(totCount = n_distinct(StockCode))

catch_by_msy <- sag_complete_summary %>%
  unnest(data) %>%
  filter(Year >= 1995) %>%
  mutate(F_FMSY = ifelse(!is.na(FMSY),
                         F / FMSY,
                         NA),
         CATCH = ifelse(is.na(catches) & !is.na(landings),
                        landings,
                        catches),
         colList = if_else(F_FMSY < 1,
                           "GREEN",
                           "RED",
                           "GREY")) %>%
  group_by(EcoRegion, FisheriesGuild, colList, Year) %>%
  summarize(totCatch = sum(CATCH, na.rm = TRUE)/1000) %>%
  left_join(count_by_msy, by = c("EcoRegion", "FisheriesGuild"))


catchMSY_fun <- function(ecoregion, plotDir = "~/git/ices-dk/fisheryO/output/") {

  plotName <- paste0(plotDir, "figure15_", ecoregion, ".png")
  #
  catch_by_msy_dat <- catch_by_msy %>%
    filter(EcoRegion == ecoregion) %>%
    ungroup() %>%
    mutate(FisheriesGuild = paste0(FisheriesGuild, " (n=",
                                   totCount, " stocks)"))


  cP <- ggplot(catch_by_msy_dat, aes(x = Year, y = totCatch)) +
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
         y = "Landings (thousand tonnes)",
         caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                           lubridate::year(Sys.time()),
                           lubridate::month(Sys.time(), label = TRUE, abbr = FALSE))) +
    theme_bw(base_size = 9) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          strip.background = element_blank(),
          legend.key = element_blank()) +
    facet_wrap(~FisheriesGuild, scales = "free", ncol = 2)

  png(plotName,
      width = 174,
      height = 136,
      units = "mm",
      res = 300)
  print(cP)
  dev.off()
}

lapply(grep("Greater|Celtic|Baltic", unique(catch_by_msy$EcoRegion),
            value = TRUE), function(x) catchMSY_fun(x))


##########################
# Discard rate over time #
##########################
ecoregion <- "Greater North Sea Ecoregion"
# discard_trends_fun(ecoregion)

# fileName = "//adminweb02/Overviews/"
# dir.exists(fileName)
discard_trends_fun <- function(ecoregion,
                               fileName = NULL,
                               dynamic = TRUE) {

  discard_clicks <- stock_catch_full %>%
    mutate(onclick = sprintf("window.open(\"%s%i/%i/%s.pdf\")",
                             "http://ices.dk/sites/pub/Publication%20Reports/Advice/",
                             YearOfLastAssessment,
                             YearOfLastAssessment,
                             StockCode)) %>%
    select(StockCode,
           Description,
           onclick) %>%
    distinct(.keep_all = TRUE)

  p2_dat <- stock_catch_full %>%
    left_join(discard_clicks, by = c("StockCode", "Description")) %>%
    filter(grepl(ecoregion, EcoRegion),
           Year >= 2000) %>%
    group_by(StockCode) %>%
    filter(!all(discards == 0) |
           !all(is.na(discards))) %>%
    mutate(tooltip =   sprintf("<b>%s</b>",
                                    Description),
           value = discards/catches) %>%
    select(-Description)
  # %>%
    # distinct(.keep_all = TRUE)

  # adj_names = sort(setdiff(unique(p2_dat$lineGroup), "MEAN"))
  # values = gg_color_hue(length(adj_names))
  # names(values) = adj_names
  # values = c(values, c(MEAN = "black"))

  # p1_dat$plotGroup <- factor(p1_dat$plotGroup,
  #                            labels = c("F/F[MSY]", "SSB/MSY~B[trigger]"))

  if(is.null(fileName)) {
    fileName <- gsub("\\s", "_", ecoregion)
    fileName <- gsub("_-_", "-", fileName)
  }

  # plot_title <- gsub(".*\\s-\\s", "\\1", EcoGuild)

  p2_plot <- ggplot(p2_dat, aes(x = Year, y = value,
                                color = StockCode,
                                fill = StockCode,
                                onclick = onclick,
                                data_id = StockCode,
                                tooltip = tooltip)) +
    # geom_line() +
    # facet_wrap(~FisheriesGuild)
    geom_hline(yintercept = 1, col = "grey60") +
    theme_bw(base_size = 9) +
    # scale_color_manual(values = values) +
    # scale_fill_manual(values = values) +
    guides(fill = FALSE) +
    theme(legend.position = "bottom",
          strip.text = element_text(size = 9, angle = 0, hjust = 0),
          strip.background = element_blank(),
          strip.placement = "outside",
          plot.caption = element_text(size = 6),
          panel.grid.major = element_blank(),
          legend.key = element_rect(colour = NA)) +
    labs(x = "Year", y = "", color = "Stock code",
         caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                           lubridate::year(Sys.time()),
                           lubridate::month(Sys.time(), label = TRUE, abbr = FALSE))) +
    facet_wrap(~ FisheriesGuild, strip.position = "left")

  if(dynamic) {
    p2_plot <- p2_plot + geom_line_interactive(alpha = 0.6)
    suppressWarnings(
      rmarkdown::render("~/git/ices-dk/fisheryO/vignettes/stockDiscardTrends-dynamic.rmd",
                        output_file = paste0("~/git/ices-dk/fisheryO/output/DiscardTester-dynamic.html"),
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

# Landings and discards disaggregated by guild
ecoregion = "Baltic Sea Ecoregion"
guild_discards_fun <- function(ecoregion,
                               fileName = NULL) {

  if(is.null(fileName)) {
    fileName <- gsub("\\s", "_", ecoregion)
    fileName <- gsub("_-_", "-", fileName)
  }

p3_dat <- stock_catch_full %>%
  filter(grepl(ecoregion, EcoRegion),
         Year >= 2012,
         Year <= 2015) %>%
  group_by(Year, FisheriesGuild) %>%
  summarize(guildLandings = sum(landings, na.rm = TRUE)/ 1000,
            guildDiscards = sum(discards, na.rm = TRUE)/ 1000)

p3_rate <- p3_dat %>%
  mutate(guildRate = guildDiscards/ (guildLandings + guildDiscards)) %>%
  gather(variable, value, -Year, -FisheriesGuild) %>%
  filter(!variable %in% c("guildDiscards", "guildLandings"))

p3_bar <- p3_dat %>%
  filter(Year == 2015) %>%
  # ungroup() %>%
  gather(variable, value, -Year, -FisheriesGuild) %>%
  ungroup() %>%
  select(-Year)

p3_bar_order <- p3_bar %>%
  group_by(FisheriesGuild) %>%
  summarize(total = sum(value, na.rm = TRUE)) %>%
  arrange(-total) %>%
  ungroup() %>%
  mutate(FisheriesGuild = factor(FisheriesGuild, FisheriesGuild))

p3_bar$FisheriesGuild <- factor(p3_bar$FisheriesGuild,
                                levels = p3_bar_order$FisheriesGuild[order(p3_bar_order$total)])

p3_rate_plot <- ggplot(p3_rate,
                  aes(x = Year, y = value, color = FisheriesGuild)) +
  geom_line() +
  geom_label_repel(data = p3_rate %>% filter(Year == 2015),
                   aes(label = FisheriesGuild,
                       color = FisheriesGuild,
                       fill = FisheriesGuild),
                   nudge_x = 1,
                   label.size = 0.2,
                   segment.size = 0.25,
                   size = 2,
                   color = 'white',
                   force = 2,
                   segment.color = 'grey60') +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(min(p3_rate$Year, na.rm = TRUE),
                                  max(p3_rate$Year, na.rm = TRUE), by = 1)) +
  geom_segment(aes(x = -Inf, xend = max(p3_rate$Year, na.rm = TRUE),
                   y = -Inf, yend = -Inf), color = "grey50") +
  geom_segment(aes(y = -Inf, yend = Inf,
                   x = -Inf, xend = -Inf), color = "grey50")+
  expand_limits(x = c(min(p3_rate$Year, na.rm = TRUE), 2017)) + # So that we have enough room along x-axis for labels.
  scale_color_brewer(type = "qual", palette = "Set2") +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  theme_bw(base_size = 9) +
  theme(legend.position = "none",
        # strip.text = element_text(size = 9, angle = 0, hjust = 0),
        # strip.background = element_blank(),
        # strip.placement = "outside",
        plot.caption = element_text(size = 6),
        panel.grid.major = element_blank(),
        legend.key = element_rect(colour = NA)) +
  labs(x = "Year", y = "Discard rate", caption = "", title = "a)")


p3_bar_plot <- ggplot(p3_bar,
                  aes(x = FisheriesGuild, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  # geom_label_repel(data = p3_rate %>% filter(Year == 2015),
  #                  aes(label = FisheriesGuild,
  #                      color = FisheriesGuild,
  #                      fill = FisheriesGuild),
  #                  nudge_x = 2.5,
  #                  label.size = 0.2,
  #                  segment.size = 0.25,
  #                  size = 2,
  #                  color = 'white',
  #                  force = 2,
  #                  segment.color = 'grey60') +
  # scale_y_continuous(labels = scales::comma) +
  # scale_x_continuous(breaks = seq(min(p3_rate$Year, na.rm = TRUE),
  #                                 max(p3_rate$Year, na.rm = TRUE), by = 1)) +
  # geom_segment(aes(x = -Inf, xend = max(p3_rate$Year, na.rm = TRUE),
  #                  y = -Inf, yend = -Inf), color = "grey50") +
  # geom_segment(aes(y = -Inf, yend = Inf,
  #                  x = -Inf, xend = -Inf), color = "grey50")+
  # expand_limits(x = c(min(p3_rate$Year, na.rm = TRUE), 2018)) + # So that we have enough room along x-axis for labels.
  scale_color_brewer(type = "qual", palette = "Set1") +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  coord_flip() +
  theme_bw(base_size = 9) +
  theme(legend.position = "none",
        # strip.text = element_text(size = 9, angle = 0, hjust = 0),
        # strip.background = element_blank(),
        # strip.placement = "outside",
        plot.caption = element_text(size = 6),
        panel.grid.major = element_blank(),
        legend.key = element_rect(colour = NA)) +
  labs(x = "", y = "Discards and landings (thousand tonnes)",
       title = "b)",
       caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                         lubridate::year(Sys.time()),
                         lubridate::month(Sys.time(), label = TRUE, abbr = FALSE)))


filename <- paste0("~/git/ices-dk/fisheryO/output/figure07_Discards", fileName, "-static.png")

png(filename,
    width = 170,
    height = 100.5,
    units = "mm",
    res = 300)

grid.arrange(p3_rate_plot,
             p3_bar_plot, ncol = 2, respect = TRUE)
dev.off()
}


lapply(grep("Greater|Celtic|Baltic", unique(stock_catch_full$EcoRegion),
            value = TRUE), function(x) guild_discards_fun(ecoregion = x,
                                                    fileName = NULL))
