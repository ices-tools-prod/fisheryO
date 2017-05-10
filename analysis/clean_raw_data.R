# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Clean up the Stock Database #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

data("stock_list_raw")
data("sag_summary_raw")
data("sag_keys")

stock_list <- stock_list_raw %>%
  dplyr::filter(ActiveYear == 2016) %>% ### This will need to be fixed for BS
  dplyr::select(StockCode = StockKeyLabel,
                Description = StockKeyDescription,
                SpeciesScientificName,
                EcoRegion,
                DataCategory,
                YearOfLastAssessment,
                AdviceCategory,
                FisheriesGuild) %>%
  dplyr::filter(!StockCode %in% c("cod-ingr", "cod-wgr",
                           "sal-nea", "san-scow",
                           "sal-na", "sal-32",
                           "trt-bal", "sal-wgc")) %>%
  dplyr::mutate(DataCategory = floor(as.numeric(DataCategory)),
         StockCode = tolower(StockCode),
         FisheriesGuild = tolower(FisheriesGuild),
         Description = gsub(pattern = "\u2013", "-", Description), # remove en dashes in favor of hyphens
         Description = gsub(pattern = "Micromesistius poutasso",
                            "Micromesistius poutassou", Description), # misspelled blue whiting
         AdviceCategory = ifelse(AdviceCategory == "MSY/PA",
                                 "MSY", AdviceCategory),
         SpeciesID = toupper(gsub( "-.*$", "", StockCode)) ,
         SpeciesScientificName = dplyr::recode(SpeciesScientificName,
                                        "Mustelus asterias" = "Mustelus"),
         SpeciesScientificName = dplyr::recode(SpeciesScientificName,
                                        "Centrophorus squamosus, Centroscymnus coelolepis" = "Centroscymnus coelolepis"),
         EcoRegion = strsplit(EcoRegion, ", ")
  ) %>%
  tidyr::unnest(EcoRegion) %>%
  dplyr::mutate(EcoRegion = ifelse(grepl("Norwegian|Barents", EcoRegion),
                            "Norwegian Sea and Barents Sea Ecoregions",
                            EcoRegion))

# Format so the species names will be italicized
stock_list_frmt <- dplyr::bind_rows(
  # Normal binomial names
  stock_list %>%
    dplyr::filter(!grepl(" spp", Description),
                  SpeciesID != "ANG",
                  grepl("[[:space:]]", SpeciesScientificName)) %>%
    dplyr::mutate(Description = stringr::str_replace_all(string = Description,
                                                         pattern = SpeciesScientificName,
                                                         replacement = paste0("<em>", SpeciesScientificName, "</em>"))),
  # Anglerfish (w/ two species)
  stock_list %>%
    dplyr::filter(SpeciesID == "ANG") %>%
    dplyr::mutate(Description = stringr::str_replace_all(string = Description,
                                                         pattern = "Lophius piscatorius and L. budegassa",
                                                         replacement = "<em>Lophius piscatorius</em> and <em>L. budegassa</em>")),
  # Groups of species (.spp)
  stock_list %>%
    dplyr::filter(grepl(" spp.*$", Description)) %>%
    dplyr::mutate(Description = stringr::str_replace_all(string = Description,
                                                         pattern = SpeciesScientificName,
                                                         replacement = paste0("<em>", SpeciesScientificName, "</em>"))),
  # A bit different notation (embedded in 2 sets of parentheses)
  stock_list %>%
    dplyr::filter(StockCode == "raj-mar") %>%
    dplyr::mutate(Description = stringr::str_replace_all(string = Description,
                                                         pattern = "Raja clavata",
                                                         replacement = "<em>Raja clavata</em>")),
  # The "others" with no species name
  stock_list %>%
    dplyr::filter(SpeciesID != "ANG") %>%
    dplyr::filter(!grepl(" spp", Description)) %>%
    dplyr::filter(StockCode != "raj-mar") %>%
    dplyr::filter(!grepl("[[:space:]]", SpeciesScientificName))
)


sag_keys$StockKeyLabel <- tolower(sag_keys$StockKeyLabel)

found_stocks <- stock_list %>%
  dplyr::select(-EcoRegion) %>%
  dplyr::distinct(.keep_all = TRUE) %>%
  dplyr::left_join(sag_keys, by = c("StockCode" = "StockKeyLabel",
                             "YearOfLastAssessment" = "AssessmentYear"))

sag_summary <- found_stocks %>%
  dplyr::left_join(sag_summary_raw, by = c("StockCode" = "fishstock",
                                    "YearOfLastAssessment" = "AssessmentYear")) %>%
  dplyr::select(Year,
         StockCode,
         F,
         SSB,
         fishingPressureDescription,
         stockSizeDescription,
         landings,
         catches,
         discards)

# List of the F types that we want to include in the analysis and those that should be considered "relative"
keeper_F <- c("F", "F/FMSY", "F in winter rings", "Harvest rate",
              "Harvest rate/FMSY", "Fishing Pressure", "weighted F")
relative_F <- c("F/FMSY", "Harvest rate/FMSY")

# List of the SSB types that we want to include in the analysis and those that should be considered "relative"
keeper_SSB <- c("SSB", "B/BMSY", "SSB & Biomass Age 4+", "UW Tv Index",
                "Stock Size", "Total biomass/BMSY", "Abundance", "Stock abundance")
relative_SSB <- c("B/BMSY", "Total biomass/BMSY")

# Clean up the stock summary data
sag_summary_clean <- sag_summary %>%
  dplyr::mutate(StockCode = tolower(StockCode),
         # ****************** #
         # SSB is not in SAG for meg-4a6a 2015. Added from:
         # http://ices.dk/sites/pub/Publication%20Reports/Expert%20Group%20Report/acom/2015/WGCSE/05.03_Megrim%20IV_VI_2015.pdf#page=22
         SSB = ifelse(Year == 2015 & StockCode == "meg-4a6a",
                      1.91,
                      SSB),
         # discards are erroneously uploaded to SAG for nep-5 2015.
         discards = ifelse(Year == 2015 & StockCode == "nep-5",
                           NA,
                           discards),
         # Real value is 0.201. before rounding rules and ADG erred towards green
         F = ifelse(Year == 2015 & StockCode == "sol-nsea",
                    0.20,
                    F),
         # ****************** #
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
                                  stockSizeDescription))
# ~~~~~~~~~~~~~~~~ #
# Reference Points #
# ~~~~~~~~~~~~~~~~ #

data("sag_refpts_raw")
# load("data/sag_refpts_raw.rda")
sag_ref_pts <- found_stocks %>%
  dplyr::left_join(sag_refpts_raw, by = c("StockCode"= "StockKeyLabel",
                                   "YearOfLastAssessment" = "AssessmentYear",
                                   "AssessmentKey" = "AssessmentKey")) %>%
  dplyr::select(StockCode,
         Flim = FLim,
         Fpa,
         Bpa,
         Blim,
         FMSY,
         MSYBtrigger) %>%
# ****************** #
# MSYBtrigger isn't appropriate for Sandeel
  dplyr::mutate(MSYBtrigger = ifelse(grepl("san", StockCode),
                              NA,
                              MSYBtrigger))
# ****************** #

# Merge the stock data, reference points, and summary table together
sag_ref_summary <- stock_list_frmt %>%
  dplyr::left_join(sag_ref_pts , by = "StockCode") %>%
  dplyr::left_join(sag_summary_clean, by = "StockCode") %>%
  tidyr::nest(EcoRegion)

sag_complete_summary <- sag_ref_summary %>%
  dplyr::mutate(MSYBtrigger = ifelse(stockSizeDescription %in% relative_SSB,
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

devtools::use_data(sag_complete_summary)

# Recreate check-mark tables
summary_fmsy <- sag_complete_summary %>%
  dplyr::group_by(StockCode) %>%
  dplyr::filter(Year >= YearOfLastAssessment - 3,
         Year <= YearOfLastAssessment - 1) %>%
  dplyr::mutate(FMSY = ifelse(F <= FMSY, "GREEN", "RED"),
         Year = paste0("FMSY", Year)) %>%
  dplyr::select(Year, FMSY, StockCode) %>%
  tidyr::spread(Year, FMSY)

summary_bmsy <- sag_complete_summary %>%
  dplyr::group_by(StockCode) %>%
  dplyr::filter(Year >= YearOfLastAssessment - 2,
         Year <= YearOfLastAssessment) %>%
  dplyr::mutate(BMSY = ifelse(SSB >= MSYBtrigger, "GREEN", "RED"),
         Year = paste0("BMSY", Year)) %>%
  dplyr::select(Year, BMSY, StockCode) %>%
  tidyr::spread(Year, BMSY) %>%
  # ****************** #
  # Nep-9 doesn't provide data for BMSY2016, so we'll make the tick marks consistent with advice
  dplyr::mutate(BMSY2013 = ifelse(StockCode == "nep-9",
                           "GREEN",
                           BMSY2013))
  # ****************** #

summary_fpa <- sag_complete_summary %>%
  dplyr::group_by(StockCode) %>%
  dplyr::filter(Year >= YearOfLastAssessment - 3,
         Year <= YearOfLastAssessment - 1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(FPA = dplyr::if_else(F <= FMSY,
                       "GREEN",
                       NA_character_),
         FPA = dplyr::if_else(F >= Flim, # If F is greater than Flim, FPA is
                       missing = FPA,
                       "RED",
                       ifelse(F <= FMSY, # if F is less than Flim and less than FMSY, FPA is
                              "GREEN",
                              dplyr::if_else(F <= Fpa , # If F is greater than FMSY but less than Fpa, FPA is
                                      "GREEN",
                                      "ORANGE", # if F is greater than Fpa, FPA is orange
                                      ifelse(F <= Flim, # if Fpa is NA but Flim is available, FPA is
                                             "ORANGE",
                                             NA_character_)))), # If none of this fits the bill, NA.
         Year = paste0("FPA", Year)) %>%
  dplyr::select(Year, FPA, StockCode) %>%
  tidyr::spread(Year, FPA)

summary_bpa <- sag_complete_summary %>%
  dplyr::group_by(StockCode) %>%
  dplyr::filter(Year >= YearOfLastAssessment - 2,
         Year <= YearOfLastAssessment) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(BPA = dplyr::if_else(SSB >= MSYBtrigger,
                       "GREEN",
                       NA_character_),
         BPA = dplyr::if_else(SSB <= Blim, # If SSB is less than Blim, BPA is
                       missing = BPA,
                       "RED",
                       ifelse(SSB >= MSYBtrigger, # if SSB is greater than Blim and greater than MSY, BPA is
                              "GREEN",
                              dplyr::if_else(SSB >= Bpa , # If SSB is less than MSY Btrigger but greater than Bpa, BPA is
                                      "GREEN",
                                      "ORANGE", # if SSB is less than Bpa, BPA is orange
                                      dplyr::if_else(SSB >= Blim, # if Bpa is NA but Blim is available, BPA is
                                              "ORANGE",
                                              NA_character_)))), # If none of this fits the bill, NA.
         Year = paste0("BPA", Year)) %>%
  dplyr::select(Year, BPA, StockCode) %>%
  tidyr::spread(Year, BPA)

summary_sbl <- sag_complete_summary %>%
  dplyr::select(StockCode,
         YearOfLastAssessment) %>%
  dplyr::distinct(.keep_all = TRUE) %>%
  dplyr::left_join(summary_bpa, by = "StockCode") %>%
  dplyr::left_join(summary_fpa, by = "StockCode")

summary_sbl <- bind_rows(
  summary_sbl %>%
    dplyr::filter(YearOfLastAssessment == 2014) %>%
    dplyr::mutate(SBL = ifelse(FPA2013 == "GREEN"  &  BPA2014 == "GREEN",
                        "GREEN",
                        ifelse(FPA2013 == "RED"  &  BPA2014 == "RED",
                               "RED",
                               NA))),
  summary_sbl %>%
    dplyr::filter(YearOfLastAssessment == 2015) %>%
    dplyr::mutate(SBL = ifelse(FPA2014 == "GREEN"  &  BPA2015 == "GREEN",
                        "GREEN",
                        ifelse(FPA2014 == "RED"  &  BPA2015 == "RED",
                               "RED",
                               NA))),
  summary_sbl %>%
    dplyr::filter(YearOfLastAssessment == 2016) %>%
    dplyr::mutate(SBL = ifelse(FPA2015 == "GREEN"  &  BPA2016 == "GREEN",
                        "GREEN",
                        ifelse(FPA2015 == "RED"  &  BPA2016 == "RED",
                               "RED",
                               NA)))
)[, c("StockCode", "SBL")]

summary_table <- summary_fmsy %>%
  dplyr::full_join(summary_bmsy, by = "StockCode") %>%
  dplyr::full_join(summary_fpa, by = "StockCode") %>%
  dplyr::full_join(summary_bpa, by = "StockCode") %>%
  dplyr::full_join(summary_sbl, by = "StockCode")

# stockDat
summary_table_frmt <- stock_list_frmt %>%  #slFull
  dplyr::left_join(summary_table, by = "StockCode") %>%
  dplyr::mutate(F_2013 = ifelse(AdviceCategory %in% c("MSY", "MP"),
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
    dplyr::filter(YearOfLastAssessment == 2014) %>%
    dplyr::mutate(D3C1 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                         FMSY2013,
                         FPA2013),
           D3C2 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                         BMSY2014,
                         BPA2014)),
  summary_table_frmt %>%
    dplyr::filter(YearOfLastAssessment == 2015) %>%
    dplyr::mutate(D3C1 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                         FMSY2014,
                         FPA2014),
           D3C2 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                         BMSY2015,
                         BPA2015)),
  summary_table_frmt %>%
    dplyr::filter(YearOfLastAssessment == 2016) %>%
    dplyr::mutate(D3C1 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                         FMSY2015,
                         FPA2015),
           D3C2 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                         BMSY2016,
                         BPA2016))
) %>%
  dplyr::select(StockCode,
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

devtools::use_data(summary_table_frmt)
#~~~~~~~~~~~~~~~~~~~~~#
# Data for pie graphs #
#~~~~~~~~~~~~~~~~~~~~~#
pie_table <- stock_list_frmt %>%
  dplyr::select(StockCode,
         EcoRegion,
         FisheriesGuild,
         YearOfLastAssessment) %>%
  dplyr::left_join(summary_table, by = "StockCode") %>%
  dplyr::mutate(FMSY = ifelse(YearOfLastAssessment == 2014,
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
         SBL = SBL) %>%
  #*****************#
  # nep-9 doesn't have the MSY2016, so we need to use MSY2015. Consider this a bandaid for now.
  dplyr::mutate(BMSY = ifelse(StockCode == "nep-9",
                       BMSY2015,
                       BMSY))
  #*****************#

pie_table_stock <- pie_table %>%
  dplyr::select(EcoRegion,
         StockCode,
         FisheriesGuild,
         FMSY,
         BMSY,
         FPA,
         BPA,
         SBL) %>%
  tidyr::gather(VARIABLE, VALUE, -EcoRegion, -StockCode, -FisheriesGuild) %>%
  dplyr::mutate(VALUE = ifelse(is.na(VALUE),
                        "GREY",
                        VALUE)) %>%
  dplyr::group_by(EcoRegion, FisheriesGuild, VARIABLE, VALUE)

pie_table_count <- pie_table %>%
  dplyr::select(EcoRegion,
         FisheriesGuild,
         FMSY,
         BMSY,
         FPA,
         BPA,
         SBL) %>%
  tidyr::gather(VARIABLE, VALUE, -EcoRegion, -FisheriesGuild) %>%
  dplyr::mutate(VALUE = ifelse(is.na(VALUE),
                        "GREY",
                        VALUE)) %>%
  dplyr::group_by(EcoRegion, FisheriesGuild, VARIABLE, VALUE) %>%
  dplyr::summarize(COUNT = n())

# Make sure that all possible combinations are provided
pie_table_count <- dplyr::bind_rows(
  pie_table_count %>%
    tidyr::expand(VALUE = c("GREY", "GREEN", "RED", "ORANGE")) %>%
    dplyr::left_join(pie_table_count, by = c("EcoRegion", "FisheriesGuild", "VARIABLE", "VALUE")) %>%
    dplyr::arrange(EcoRegion, FisheriesGuild, VARIABLE, VALUE) %>%
    dplyr::mutate(COUNT = ifelse(is.na(COUNT),
                          0,
                          as.numeric(COUNT))),
  pie_table_count %>%
    dplyr::group_by(EcoRegion, VARIABLE, VALUE) %>%
    dplyr::mutate(FisheriesGuild = "total",
           COUNT = sum(COUNT)) %>%
    dplyr::distinct(.keep_all = TRUE)
)

devtools::use_data(pie_table_count)
#~~~~~~~~~~~~~~~~~~~~~#
# GES Pie charts data #
#~~~~~~~~~~~~~~~~~~~~~#
# Split and count by variable and color
ges_table_count <- pie_table %>%
  dplyr::select(EcoRegion,
         D3C2 = FMSY,
         D3C1 = BMSY) %>%
  tidyr::gather(VARIABLE, COLOR, -EcoRegion) %>%
  dplyr::mutate(COLOR = ifelse(is.na(COLOR),
                        "GREY",
                        COLOR)) %>%
  dplyr::group_by(EcoRegion, VARIABLE, COLOR) %>%
  dplyr::summarize(VALUE = n(),
            METRIC = "count")

# Take last year of catch data. If catch is not available, use landings.
# Remove stocks without quantified catch or landings
ges_catch_stock <-  sag_complete_summary %>%
  dplyr::group_by(StockCode) %>%
  dplyr::filter(Year == YearOfLastAssessment - 1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(CATCH = ifelse(is.na(catches) & !is.na(landings),
                        landings,
                        catches)) %>%
  dplyr::filter(!is.na(CATCH)) %>%
  dplyr::select(StockCode,
         CATCH) %>%
  dplyr::distinct(.keep_all = TRUE)

# Split and sum catch by variable and color
ges_table_catch <- ges_catch_stock %>%
  dplyr::left_join(pie_table, by = "StockCode") %>%
  dplyr::select(EcoRegion,
         CATCH,
         D3C2 = FMSY,
         D3C1 = BMSY) %>%
  tidyr::gather(VARIABLE, COLOR, -EcoRegion, -CATCH) %>%
  dplyr::mutate(COLOR = ifelse(is.na(COLOR),
                        "GREY",
                        COLOR),
         CATCH = ifelse(is.na(CATCH),
                        0,
                        CATCH)) %>%
  dplyr::group_by(EcoRegion, VARIABLE, COLOR) %>%
  dplyr::summarize(VALUE = sum(CATCH),
            METRIC = "total_sum")

ges_table <- rbind(ges_table_count, ges_table_catch)

devtools::use_data(ges_table)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Stock Status over time data #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
stock_trends <- sag_complete_summary %>%
  tidyr::unnest(data) %>%
  dplyr::mutate(F_FMSY = ifelse(!is.na(FMSY),
                         F / FMSY,
                         NA),
         SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
                                  SSB / MSYBtrigger,
                                  NA)) %>%
  dplyr::select(Year,
         StockCode,
         FisheriesGuild,
         EcoRegion,
         F_FMSY,
         SSB_MSYBtrigger) %>%
  tidyr::gather(METRIC, stockValue, -Year, -StockCode, -FisheriesGuild, -EcoRegion) %>%
  dplyr::group_by(EcoRegion, FisheriesGuild, METRIC, Year) %>%
  dplyr::mutate(ecoGuildMean = mean(stockValue, na.rm = TRUE))

stock_trends_frmt <- dplyr::bind_rows(
  stock_trends %>%
    dplyr::mutate(EcoGuild = paste0(EcoRegion, " - ", FisheriesGuild, " stocks")) %>%
    dplyr::ungroup() %>%
    dplyr::select(pageGroup = EcoGuild,
           lineGroup = StockCode,
           Year,
           plotGroup = METRIC,
           plotValue = stockValue) %>%
    dplyr::filter(!is.na(plotValue)),

  stock_trends %>%
    dplyr::mutate(EcoGuild = paste0(EcoRegion, " - ", FisheriesGuild, " stocks")) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(EcoGuild, METRIC, Year, .keep_all = TRUE) %>%
    dplyr::select(pageGroup = EcoGuild,
           Year,
           plotGroup = METRIC,
           plotValue = ecoGuildMean) %>%
    dplyr::mutate(lineGroup = "MEAN") %>%
    dplyr::filter(!is.na(plotValue))
)

devtools::use_data(stock_trends_frmt)
#~~~~~~~~~~~~~~~~#
# KOBE Plot data #
#~~~~~~~~~~~~~~~~#
stock_catch <- sag_complete_summary %>%
  tidyr::unnest(data) %>%
  dplyr::select(Year,
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
  dplyr::bind_rows(
    stock_catch %>% # all present and accounted for
      dplyr::filter(!is.na(catches),
             !is.na(landings),
             !is.na(discards)), # 43
    stock_catch %>%  # Missing discards, but catches == landings
      dplyr::filter(is.na(discards),
             catches == landings) %>%
      dplyr::mutate(discards = 0), #36
    stock_catch %>% # Missing catches, but have landings and discards
      dplyr::filter(is.na(catches),
             !is.na(landings),
             !is.na(discards)) %>%
      dplyr::mutate(catches = landings + discards), #75
    stock_catch %>% # missing catches, but have landings
      dplyr::filter(is.na(catches),
             !is.na(landings),
             is.na(discards)) %>%
      dplyr::mutate(catches = NA,
             discards = NA),
    stock_catch %>% # missing everything
      dplyr::filter(is.na(catches),
             is.na(landings),
             is.na(discards)) %>%
      dplyr::mutate(catches = NA,
             discards = NA,
             landings = NA),
    stock_catch %>% # missing landings and discards
      dplyr::filter(!is.na(catches),
             is.na(landings),
             is.na(discards)) %>%
      dplyr::mutate(landings = NA,
             discards = NA),
    stock_catch %>% # landings and catches
      dplyr::filter(is.na(catches),
             is.na(landings),
             !is.na(discards)) %>%
      dplyr::mutate(catches = NA,
             landings = NA),
    stock_catch %>% # Missing discards, but have landings and catches
      dplyr::filter(!is.na(catches),
             !is.na(landings),
             is.na(discards),
             landings != catches) %>%
      dplyr::mutate(discards = catches - landings)
  )

devtools::use_data(stock_catch_full)

stock_status_full <-
  dplyr::full_join(
    stock_catch_full %>%
      dplyr::group_by(StockCode) %>%
      dplyr::filter(Year == YearOfLastAssessment - 1) %>%
      dplyr::mutate(F_FMSY =  ifelse(!is.na(FMSY),
                              F / FMSY,
                              NA)) %>%
      dplyr::select(StockCode,
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
      dplyr::group_by(StockCode) %>%
      dplyr::filter(Year == YearOfLastAssessment) %>%
      dplyr::mutate(SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
                                      SSB / MSYBtrigger,
                                      NA)) %>%
      dplyr::select(StockCode,
             FisheriesGuild,
             EcoRegion,
             SSB_MSYBtrigger,
             SSB,
             MSYBtrigger),
    by = c("StockCode",
           "FisheriesGuild",
           "EcoRegion")
  ) %>%
  dplyr::mutate(colList = ifelse(is.na(F_FMSY) | is.na(SSB_MSYBtrigger),
                          "GREY",
                          if_else(F_FMSY < 1 & SSB_MSYBtrigger >= 1,
                                  "GREEN",
                                  "RED",
                                  "GREY")),
         FisheriesGuild = ifelse(StockCode %in% c("whb-comb", "mac-nea"),
                                 "large-scale stocks",
                                 FisheriesGuild))

devtools::use_data(stock_status_full)

#############
### Catch ###
#############

data("catch_data_historical")
data("species_list")
data("stock_list_raw")

fish_category <- stock_list_raw %>%
  dplyr::filter(YearOfLastAssessment >= 2016,
         !is.na(FisheriesGuild)) %>%
  dplyr::mutate(X3A_CODE = gsub("-.*$", "", StockKeyLabel),
         X3A_CODE = gsub("\\..*", "", X3A_CODE),
         X3A_CODE = toupper(X3A_CODE),
         FisheriesGuild = tolower(FisheriesGuild)) %>%
  dplyr::select(X3A_CODE, FisheriesGuild) %>%
  dplyr::distinct(.keep_all = TRUE)

species_list <- species_list %>%
  dplyr::select(English_name, Scientific_name, X3A_CODE) %>%
  dplyr::left_join(fish_category, by = "X3A_CODE")

historic_bs <- paste0(c("III (not specified)", "III b  Baltic 23",
                        "III b+c (not specified)", "III b-d (not specified)",
                        "III c  Baltic 22", "III d  (not specified)",
                        "III d  Baltic 24", "III d  Baltic 25",
                        "III d  Baltic 26", "III d  Baltic 27",
                        "III d  Baltic 28 (not specified)", "III d  Baltic 28-1",
                        "III d  Baltic 28-2", "III d  Baltic 29",
                        "III d  Baltic 30", "III d  Baltic 31",
                        "III d  Baltic 32"),
                      collapse = "|")

historic_ns <- paste0(c("III a", "IIIa  and  IV  (not specified)",
                        "IIIa  and  IVa+b  (not specified)", "IV (not specified)",
                        "IV a", "IV a+b (not specified)",
                        "IV b", "IV b+c (not specified)",
                        "IV c", "VII d"),
                      collapse = "|")

historic_uk <- paste0(c("^UK", "^Channel", "^Isle of Man"),
                      collapse = "|")

catch_dat_1950 <- catch_data_historical %>%
  tidyr::gather(YEAR, VALUE, -Country, -Species, -Division) %>%
  dplyr::mutate(YEAR = as.numeric(gsub("X", "", YEAR)),
         VALUE = ifelse(VALUE == "<0.5",
                        as.numeric(0),
                        VALUE),
         VALUE = ifelse(!is.na(VALUE),
                        as.numeric(VALUE),
                        NA),
         COUNTRY = dplyr::case_when(
           grepl(historic_uk, .$Country) ~ "United Kingdom",
           grepl("^Germany", .$Country) ~ "Germany",
           grepl("Un. Sov. Soc. Rep.", .$Country) ~ "Russia",
           grepl("Faeroe Islands", .$Country) ~ "Faroe Islands",
           grepl("Other nei", .$Country) ~ "OTHER",
           TRUE ~ .$Country
         ),
         ISO3 = countrycode::countrycode(COUNTRY, "country.name", "iso3c", warn = FALSE),
         ECOREGION = dplyr::case_when(
           grepl(historic_bs, .$Division) ~ "Baltic Sea Ecoregion",
           grepl(historic_ns, .$Division) ~ "Greater North Sea Ecoregion",
           TRUE ~ "OTHER")) %>%
  dplyr::filter(YEAR <= 2005,
         ECOREGION != "OTHER",
         COUNTRY != "OTHER") %>%
  dplyr::left_join(y = species_list, c("Species" = "English_name")) %>% # Merge to add FAO species information
  # left_join(y = fish_category, by = "X3A_CODE") %>% # Merge to add FAO species information
  dplyr::select(YEAR,
         COUNTRY,
         ISO3,
         GUILD = FisheriesGuild,
         ECOREGION,
         SPECIES_NAME = Scientific_name,
         SPECIES_CODE = X3A_CODE,
         COMMON_NAME = Species,
         VALUE)

data("catch_data_official")
# load("data/catch_data_official.rda")

catch_dat_2010 <- catch_data_official %>%
  tidyr::gather(YEAR, VALUE, -Country, -Species, -Area, -Units) %>%
  dplyr::filter(Country != "") %>%
  dplyr::mutate(YEAR = as.numeric(gsub("X", "", YEAR)),
         VALUE = as.numeric(VALUE),
         Country = countrycode::countrycode(Country,"iso2c", "country.name"),
         Country = ifelse(grepl("Guernsey|Isle of Man|Jersey", Country),
                          "United Kingdom",
                          Country),
         ISO3 = countrycode::countrycode(Country, "country.name", "iso3c", warn = FALSE),
         Area = tolower(Area),
         ECOREGION = dplyr::case_when(
           grepl("27.3.bc|27.3.d|27.3_nk", .$Area) ~ "Baltic Sea Ecoregion",
           grepl("27.3.a|27.4|27.7.d", .$Area) ~ "Greater North Sea Ecoregion",
           TRUE ~ "OTHER")) %>%
  dplyr::filter(ECOREGION != "OTHER") %>%
  dplyr::left_join(species_list, c("Species" = "X3A_CODE")) %>%
  dplyr::select(YEAR,
         COUNTRY = Country,
         ISO3,
         GUILD = FisheriesGuild,
         ECOREGION,
         SPECIES_NAME = Scientific_name,
         SPECIES_CODE = Species,
         COMMON_NAME = English_name,
         VALUE)

allDat <- catch_dat_2010 %>%
  dplyr::bind_rows(catch_dat_1950) %>%
  dplyr::mutate(GUILD = ifelse(is.na(GUILD),
                        "undefined",
                        GUILD)) %>%
  dplyr::filter(!GUILD %in% c("elasmobranch", "crustacean") |
           ECOREGION != "Baltic Sea")
devtools::use_data(allDat)

# ~~~~~~~~~~~~~~~~~~~~~~ #
# STECF Catch and Effort #
# ~~~~~~~~~~~~~~~~~~~~~~ #

# For roxygen2 documentation
# cat(paste0("#'\t\\item{", colnames(effort_data), "{add text}\n"))
# cat(paste0("#'\t\\item{", colnames(landings_data), "{add text}\n"))

# load("data/effort_data.rda")
data("STECF_effort_data")
data("STECF_landings_data")
# load("data/species_list.rda")
# load("data/stock_list_raw.rda")


effortDat <- STECF_effort_data %>%
  dplyr::mutate(ISO3c = ifelse(grepl("SCO|ENG|GBG|GBJ|IOM|NIR", country),
                                "GBR",
                                country),
         COUNTRY = countrycode::countrycode(ISO3c, "iso3c", "country.name")) %>%
  dplyr::mutate(YEAR = as.numeric(year),
         EFFORT = as.numeric(nominal_effort)) %>%
  dplyr::select(YEAR,
         ANNEX = annex,
         AREA = regulated.area,
         COUNTRY,
         GEAR = regulated.gear,
         EFFORT)

stecfCatchDat <- STECF_landings_data %>%
  dplyr::mutate(ISO3c = ifelse(grepl("SCO|ENG|GBG|GBJ|IOM|NIR", country),
                        "GBR",
                        country),
         COUNTRY = countrycode::countrycode(ISO3c, "iso3c", "country.name")) %>%
  dplyr::left_join(species_list, c("species" = "X3A_CODE")) %>% # merge with FAO species names
  dplyr::mutate(LANDINGS = as.numeric(sum_landings),
         LANDINGS = ifelse(COUNTRY == "Germany" &
                             year == 2013 &
                             vessel.length == "U8M",
                           NA, LANDINGS)) %>%
  dplyr::select(YEAR = year,
         COUNTRY,
         ANNEX = annex,
         AREA = regulated.area,
         GEAR = regulated.gear,
         COMMON_NAME = English_name,
         LANDINGS)


gear_dat <- dplyr::full_join(
  effortDat %>%
    dplyr::select(ANNEX, AREA, GEAR),
  stecfCatchDat %>%
    dplyr::select(ANNEX, AREA, GEAR),
  by = c("ANNEX", "AREA", "GEAR")
) %>%
  dplyr::distinct(.keep_all = TRUE)

gear_dat_clean <- dplyr::bind_rows(
  gear_dat %>%
    dplyr::filter(ANNEX == "BAL") %>%
    dplyr::mutate(ECOREGION = "Baltic Sea Ecoregion",
           gear_class = dplyr::case_when(
             grepl("BEAM", .$GEAR) ~ "Beam trawl",
             grepl("DREDGE", .$GEAR) ~ "Dredge",
             grepl("GILL|TRAMMEL|LONGLINE", .$GEAR) ~ "Static/Gill net/LL",
             grepl("DEM_SEINE|OTTER|DEM_SEINE", .$GEAR) ~ "Otter trawl/seine",
             grepl("PEL_SEINE|PEL_TRAWL", .$GEAR) ~ "Pelagic trawl/seine",
             grepl("POTS", .$GEAR) ~ "Pots",
             grepl("NONE", .$GEAR) ~ "other",
             is.na(.$GEAR) ~ "other",
             TRUE ~ "other"
           )
    ),
  gear_dat %>%
    dplyr::filter(ANNEX == "IIA",
           AREA %in% c("3A", "3B1", "3B2", "3B3")) %>%
    dplyr::mutate(ECOREGION = "Greater North Sea Ecoregion",
           gear_class = dplyr::case_when(
             grepl("BEAM|BT1|BT2", .$GEAR) ~ "Beam trawl",
             grepl("DREDGE", .$GEAR) ~ "Dredge",
             grepl("GN1|GT1|LL1|TRAMMEL", .$GEAR) ~ "Static/Gill net/LL",
             grepl("TR1|TR2|TR3|OTTER|DEM_SEINE", .$GEAR) ~ "Otter trawl/seine",
             grepl("PEL_SEINE|PEL_TRAWL", .$GEAR) ~ "Pelagic trawl/seine",
             grepl("POTS", .$GEAR) ~ "Pots",
             grepl("NONE", .$GEAR) ~ "other",
             is.na(.$GEAR) ~ "other",
             TRUE ~ "other"
           )
    ),
  gear_dat %>%
    dplyr::filter(ANNEX == "CEL1") %>%
    dplyr::mutate(ECOREGION = "Celtic Seas Ecoregion",
           gear_class = dplyr::case_when(
             grepl("BEAM|BT1|BT2", .$GEAR) ~ "Beam trawl",
             grepl("DREDGE", .$GEAR) ~ "Dredge",
             grepl("GN1|GT1|LL1|TRAMMEL", .$GEAR) ~ "Static/Gill net/LL",
             grepl("TR1|TR2|TR3|OTTER|DEM_SEINE", .$GEAR) ~ "Otter trawl/seine",
             grepl("PEL_SEINE|PEL_TRAWL", .$GEAR) ~ "Pelagic trawl/seine",
             grepl("POTS", .$GEAR) ~ "Pots",
             grepl("NONE", .$GEAR) ~ "other",
             is.na(.$GEAR) ~ "other",
             TRUE ~ "other"
           )
    )
)

effortDatClean <- gear_dat_clean %>%
  dplyr::left_join(effortDat, by = c("ANNEX", "AREA", "GEAR")) %>%
  dplyr::select(YEAR,
         ANNEX,
         ECOREGION,
         AREA,
         GEAR = gear_class,
         COUNTRY,
         EFFORT)


stecfCatchDatClean <- gear_dat_clean %>%
  dplyr::left_join(stecfCatchDat, by = c("ANNEX", "AREA", "GEAR")) %>%
  dplyr::select(YEAR,
         ANNEX,
         ECOREGION,
         AREA,
         GEAR = gear_class,
         COUNTRY,
         LANDINGS) %>%
  dplyr::group_by(YEAR, ANNEX, ECOREGION, AREA, GEAR, COUNTRY) %>%
  dplyr::summarize(LANDINGS = sum(LANDINGS, na.rm = TRUE))

effortDatClean <- effortDatClean %>%
  dplyr::filter(!COUNTRY %in% c("Finland", "Estonia"))
