#' ICES Area and Ecoregion definitions
#'
#' \code{area_definition} returns list of sf dataframes with shape information of Europe, ICES Areas and ICES Ecoregions.
#'
#' @param ecoregion character of the ecoregion to plot. e.g., "Greater North Sea Ecoregion"
#'
#' @note European map is from Natural Earth (scale = 10) via the rnaturalearth package
#'
#' @return list of ices_shape (data frame), eco_shape (data frame), europe_shape (data frame) and centroids (data frame)
#'
#' @author Scott Large
#'
#' @seealso Used in \code{\link{area_definition_map}} to a map describing potential mismatches between ICES Ecoregions and ICES Areas.
#'
#' Input data: \code{\link{ices_shape}}, \code{\link{eco_shape}}, and \code{\link{europe_shape}}.
#'
#' @examples
#'
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ICES Area and Ecoregion definitions #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
area_definition <- function(ecoregion = "Greater North Sea Ecoregion"){

  raw_data <- c("europe_shape",
                "ices_shape",
                "eco_shape")
  data(list = raw_data, envir = environment())

  crs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

  eco_areas <- eco_shape %>%
    st_transform(crs = crs) %>%
    mutate(Ecoregion = case_when(
      grepl("Baltic Sea", .$Ecoregion) ~ "Baltic Sea Ecoregion",
      grepl("Greater North Sea", .$Ecoregion) ~ "Greater North Sea Ecoregion",
      # ... add remaining ecoregions
      TRUE ~ "OTHER")) %>%
    filter(Ecoregion == ecoregion)

  Area_27_baltic <- c("3.d.27", "3.d.25", "3.d.24",
                      "3.b.23", "3.c.22", "3.d.31",
                      "3.d.30", "3.d.32", "3.d.29",
                      "3.d.28.1", "3.d.28.2", "3.d.26")

  Area_27_ns <- c("3.a.20", "3.a.21",
                  "4.a", "4.b", "4.c",
                  "7.d", "7.e")

  ices_areas <- ices_shape %>%
    mutate(ECOREGION = case_when(
      .$Area_27 %in% Area_27_baltic ~ "Baltic Sea Ecoregion",
      .$Area_27 %in%  Area_27_ns ~ "Greater North Sea Ecoregion",
      # ... add remaining ecoregions
      TRUE ~ "OTHER")) %>%
    st_transform(crs = crs)

  # Centroids for labels
  ices_area_centroids <- sf::st_centroid(ices_areas)
  centroids <- data.frame(ices_area_centroids$Area_27,
                          ices_area_centroids$ECOREGION,
                          matrix(unlist(ices_area_centroids$geometry),
                                 ncol = 2,
                                 byrow = TRUE))

  colnames(centroids) <- c("Area_27", "ECOREGION", "X", "Y")

  centroids <- centroids %>%
    mutate(Area_27 = case_when(
      .$ECOREGION == "Baltic Sea Ecoregion" ~ sub("3.b.|3.c.|3.d.", "", .$Area_27),
      .$ECOREGION == "Greater North Sea Ecoregion" ~ as.character(.$Area_27),
      TRUE ~ "OTHER"
    ))

  ices_areas <- ices_areas %>%
    filter(grepl(ecoregion, ECOREGION))

  centroids <- centroids %>%
    filter(grepl(ecoregion, ECOREGION))

  return(list("ices_areas" = ices_areas,
              "eco_areas" = eco_areas,
              "europe_shape" = europe_shape,
              "centroids" = centroids,
              "crs" = crs))


}
#' Clean SAG reference points and summary table
#'
#' \code{clean_sag} returns a merged and tidied SAG reference points and summary table and a formatted (.html) stock list
#'
#' @param active_year numeric of the stock database version. e.g., 2016
#'
#' @note Periodically, ICES adds or removes stocks from the advisory process. The function returns
#' the SAG reference points and summary table for all published (in SAG) and active stocks for a given year.
#'
#' @return list of stock_list_frmt (data frame) and sag_complete_summary (tbl_df)
#'
#' @author Scott Large
#'
#' @seealso Used in \code{\link{stock_trends_fun}} to make clickable information on dynamic
#' line plots of F and SSB relative to F<sub>MSY</sub> and MSY B<sub>trigger</sub>
#' reference points for stocks of a fish category for an ecoregion.
#' Also, used in \code{\link{frmt_summary_table}}.
#'
#' Input data: \code{\link{stock_list_raw}}, \code{\link{sag_summary_raw}},\code{\link{sag_refpts_raw}}, and \code{\link{sag_keys_raw}}.
#'
#' @examples
#' head(clean_sag(active_year = 2016))
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Clean up the Stock Database #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
clean_sag <- function(active_year = 2016){

  raw_data <- c("stock_list_raw",
                "sag_summary_raw",
                "sag_keys_raw",
                "sag_refpts_raw")

  data(list = raw_data, envir = environment())

  stock_list <- stock_list_raw %>%
    filter(ActiveYear == active_year) %>%
    select(StockCode = StockKeyLabel,
           Description = StockKeyDescription,
           SpeciesScientificName,
           EcoRegion,
           DataCategory,
           YearOfLastAssessment,
           AdviceCategory,
           FisheriesGuild) %>%
    # check that these stocks are removed from the analysis
    filter(!StockCode %in% c("cod-ingr", "cod-wgr",
                             "sal-nea", "san-scow",
                             "sal-na", "sal-32",
                             "trt-bal", "sal-wgc")) %>%
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

  # Format so the species names will be italicized
  stock_list_frmt <- bind_rows(
    # Normal binomial names
    stock_list %>%
      filter(!grepl(" spp", Description),
             SpeciesID != "ANG",
             grepl("[[:space:]]", SpeciesScientificName)) %>%
      mutate(Description = stringr::str_replace_all(string = Description,
                                                    pattern = SpeciesScientificName,
                                                    replacement = paste0("<em>", SpeciesScientificName, "</em>"))),
    # Anglerfish (w/ two species)
    stock_list %>%
      filter(SpeciesID == "ANG") %>%
      mutate(Description = stringr::str_replace_all(string = Description,
                                                    pattern = "Lophius piscatorius and L. budegassa",
                                                    replacement = "<em>Lophius piscatorius</em> and <em>L. budegassa</em>")),
    # Groups of species (.spp)
    stock_list %>%
      filter(grepl(" spp.*$", Description)) %>%
      mutate(Description = stringr::str_replace_all(string = Description,
                                                    pattern = SpeciesScientificName,
                                                    replacement = paste0("<em>", SpeciesScientificName, "</em>"))),
    # A bit different notation (embedded in 2 sets of parentheses)
    stock_list %>%
      filter(StockCode == "raj-mar") %>%
      mutate(Description = stringr::str_replace_all(string = Description,
                                                    pattern = "Raja clavata",
                                                    replacement = "<em>Raja clavata</em>")),
    # The "others" with no species name
    stock_list %>%
      filter(SpeciesID != "ANG") %>%
      filter(!grepl(" spp", Description)) %>%
      filter(StockCode != "raj-mar") %>%
      filter(!grepl("[[:space:]]", SpeciesScientificName))
  )

  sag_keys_raw$StockKeyLabel <- tolower(sag_keys_raw$StockKeyLabel)

  found_stocks <- stock_list %>%
    select(-EcoRegion) %>%
    distinct(.keep_all = TRUE) %>%
    left_join(sag_keys_raw, by = c("StockCode" = "StockKeyLabel",
                               "YearOfLastAssessment" = "AssessmentYear"))

  sag_summary <- found_stocks %>%
    left_join(sag_summary_raw, by = c("StockCode" = "fishstock",
                                      "YearOfLastAssessment" = "AssessmentYear")) %>%
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
  keeper_F <- c("F", "F/FMSY", "F in winter rings", "Harvest rate",
                "Harvest rate/FMSY", "Fishing Pressure", "weighted F")
  relative_F <- c("F/FMSY", "Harvest rate/FMSY")

  # List of the SSB types that we want to include in the analysis and those that should be considered "relative"
  keeper_SSB <- c("SSB", "B/BMSY", "SSB & Biomass Age 4+", "UW Tv Index",
                  "Stock Size", "Total biomass/BMSY", "Abundance", "Stock abundance")
  relative_SSB <- c("B/BMSY", "Total biomass/BMSY")

  # Clean up the stock summary data
  sag_summary_clean <- sag_summary %>%
    mutate(StockCode = tolower(StockCode),
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
  sag_ref_pts <- found_stocks %>%
    left_join(sag_refpts_raw, by = c("StockCode"= "StockKeyLabel",
                                     "YearOfLastAssessment" = "AssessmentYear",
                                     "AssessmentKey" = "AssessmentKey")) %>%
    select(StockCode,
           Flim = FLim,
           Fpa,
           Bpa,
           Blim,
           FMSY,
           MSYBtrigger) %>%
    # ****************** #
    # MSYBtrigger isn't appropriate for Sandeel
    mutate(MSYBtrigger = ifelse(grepl("san", StockCode),
                                NA,
                                MSYBtrigger))
  # ****************** #

  # Merge the stock data, reference points, and summary table together
  sag_ref_summary <- stock_list_frmt %>%
    left_join(sag_ref_pts , by = "StockCode") %>%
    left_join(sag_summary_clean, by = "StockCode") %>%
    nest(EcoRegion)

  sag_complete_summary <- sag_ref_summary %>%
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
  return(list("stock_list_frmt" = stock_list_frmt,
              "sag_complete_summary" = sag_complete_summary))
}

#' Format stock summary table
#'
#' \code{frmt_summary_tbl} returns the stock summary table plain and formatted with html (e.g., glyphicons and italics)
#'
#' @param active_year numeric of the stock database version. e.g., 2016
#' @param return_clean_sag logical to return objects from clean_sag()
#'
#' @note Periodically, ICES adds or removes stocks from the advisory process. The function returns
#' the stock summary table for all published (in SAG) and active stocks for a given year.
#'
#' @return data frame
#'
#' @author Scott Large
#'
#' @seealso Used in \code{\link{stockSummaryTable_fun}} to create the "Status of stock summary relative to reference points"
#' table for all stocks for an ecoregion. Input data: SAG summary table and reference points come from \code{\link{clean_sag}}.
#' @examples
#' head(frmt_summary_tbl(active_year = 2016)$summary_table)
#' @export

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Recreate Status of stock summary relative to reference points table #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

frmt_summary_tbl <- function(active_year = 2016,
                             return_clean_sag = FALSE) {

  dat <- clean_sag(active_year)
  sag_sum <- dat$sag_complete_summary
  stck_frmt <- dat$stock_list_frmt
  rm(dat,  envir = environment())

  summary_fmsy <- sag_sum %>%
    group_by(StockCode) %>%
    filter(Year >= YearOfLastAssessment - 3,
           Year <= YearOfLastAssessment - 1) %>%
    mutate(FMSY = ifelse(F <= FMSY, "GREEN", "RED"),
           Year = paste0("FMSY", Year)) %>%
    select(Year, FMSY, StockCode) %>%
    spread(Year, FMSY)

  summary_bmsy <- sag_sum %>%
    group_by(StockCode) %>%
    filter(Year >= YearOfLastAssessment - 2,
           Year <= YearOfLastAssessment) %>%
    mutate(BMSY = ifelse(SSB >= MSYBtrigger, "GREEN", "RED"),
           Year = paste0("BMSY", Year)) %>%
    select(Year, BMSY, StockCode) %>%
    spread(Year, BMSY) %>%
    # ****************** #
    # Nep-9 doesn't provide data for BMSY2016, so we'll make the tick marks consistent with advice
    mutate(BMSY2013 = ifelse(StockCode == "nep-9",
                             "GREEN",
                             BMSY2013))
  # ****************** #

  summary_fpa <- sag_sum %>%
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

  summary_bpa <- sag_sum %>%
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

  summary_sbl <- sag_sum %>%
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

  summary_table <- summary_fmsy %>%
    full_join(summary_bmsy, by = "StockCode") %>%
    full_join(summary_fpa, by = "StockCode") %>%
    full_join(summary_bpa, by = "StockCode") %>%
    full_join(summary_sbl, by = "StockCode")

  # stockDat
  summary_table_frmt <- stck_frmt %>%  #slFull
    left_join(summary_table, by = "StockCode") %>%
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

  if(return_clean_sag){
    return(list("summary_table_frmt" = summary_table_frmt,
                "summary_table" = summary_table,
                "sag_complete_summary" = sag_sum,
                "stock_list_frmt" = stck_frmt))
  }
  if(!return_clean_sag){
    return(list("summary_table_frmt" = summary_table_frmt,
                "summary_table" = summary_table))
  }
}

#' Proportion of stocks relative to reference points
#'
#' \code{stock_props} returns a list of the proportion of stocks
#'  relative to reference points for fish categories in ecoregions.
#'
#' @param active_year numeric of the stock database version (year). e.g., 2016
#' @param return_clean_sag logical to return objects from clean_sag()
#'
#' @note Periodically, ICES adds or removes stocks from the advisory process. The function returns
#' the SAG reference points and summary table for all published (in SAG) and active stocks for a given year.
#'
#' @return returns a list with the data frame \code{stock_props} of the proportion of stocks relative
#' to reference points for all ecoregions. When \code{return_clean_sag = TRUE}, \code{sag_complete_summary} and \code{stock_list_frmt}
#' are also returned from \code{\link{clean_sag}}.
#'
#' @seealso Used in \code{\link{ices_stock_props}} and \code{\link{ges_stock_props}} to evaluate stocks relative
#' to ICES and GES reference points.
#' Input data: SAG summary table and reference points come from \code{\link{clean_sag}}. \code{\link{frmt_summary_table}} evaluates
#' status relative to reference points and formats the table for .html.
#'
#' @author Scott Large
#'
#' @examples
#' \dontrun{
#' stock_props(2016)
#' }
#' @export
#~~~~~~~~~~~~~~~~~~~~~#
# Data for pie graphs #
#~~~~~~~~~~~~~~~~~~~~~#
stock_props <- function(active_year,
                        return_clean_sag = FALSE) {

  dat <- frmt_summary_tbl(active_year,
                          return_clean_sag = TRUE)
  stck_frmt <- dat$stock_list_frmt
  summary_tbl <- dat$summary_table

  pie_table <- stck_frmt %>%
    select(StockCode,
           EcoRegion,
           FisheriesGuild,
           YearOfLastAssessment) %>%
    left_join(summary_tbl, by = "StockCode") %>%
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
           SBL = SBL) %>%
    #*****************#
    # nep-9 doesn't have the MSY2016, so we need to use MSY2015. Consider this a bandaid for now.
    mutate(BMSY = ifelse(StockCode == "nep-9",
                         BMSY2015,
                         BMSY))
  #*****************#
  if(return_clean_sag){
    return(list("stock_props" = pie_table,
                "sag_complete_summary" = dat$sag_complete_summary))
  }
  if(!return_clean_sag) {
    return(list("stock_props" = pie_table))
  }
}

#' Proportion of stocks relative to ICES reference points
#'
#' \code{ices_stock_props} returns a data frame of the proportion of stocks
#'  relative to ICES reference points for fish categories in ecoregions.
#'
#' @param active_year numeric of the stock database version (year). e.g., 2016
#'
#' @note Periodically, ICES adds or removes stocks from the advisory process. The function returns
#' the SAG reference points and summary table for all published (in SAG) and active stocks for a given year.
#'
#' @return returns a data frame of the proportion of stocks relative
#' to ICES reference points for fish categories in ecoregions.
#'
#' @seealso Used in \code{\link{stockPie_fun}} to plot proportion of stocks relative
#' to ICES reference points for fish categories in all ecoregions.
#' Input data: From \code{\link{stock_props}}.
#'
#' @author Scott Large
#'
#' @examples
#' \dontrun{
#' ices_stock_props(2016)
#' }
#' @export

# ~~~~~~~~~~~~~~~~~~~~~~~~ #
# Data for ICES pie graphs #
# ~~~~~~~~~~~~~~~~~~~~~~~~ #

ices_stock_props <- function(active_year) {

  pie_tbl <- stock_props(active_year,
                         return_clean_sag = FALSE)$stock_props

  pie_table_stock <- pie_tbl %>%
    select(EcoRegion,
           StockCode,
           FisheriesGuild,
           FMSY,
           BMSY,
           FPA,
           BPA,
           SBL) %>%
    gather(VARIABLE, VALUE, -EcoRegion, -StockCode, -FisheriesGuild) %>%
    mutate(VALUE = ifelse(is.na(VALUE),
                          "GREY",
                          VALUE)) %>%
    group_by(EcoRegion, FisheriesGuild, VARIABLE, VALUE)

  pie_table_count <- pie_tbl %>%
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
      expand(VALUE = c("GREY", "GREEN", "RED", "ORANGE")) %>%
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

  return(pie_table_count)
}

#' Proportion of stocks relative to GES reference points
#'
#' \code{ges_stock_props} returns a data frame of the proportion of stocks
#'  relative to GES reference points for all ecoregion.
#'
#' @param active_year numeric of the stock database version (year). e.g., 2016
#'
#' @note Periodically, ICES adds or removes stocks from the advisory process. The function returns
#' the SAG reference points and summary table for all published (in SAG) and active stocks for a given year.
#'
#' @return returns a data frame of the proportion of stocks relative
#' to ICES reference points for fish categories in ecoregions.
#'
#' @seealso Used in \code{\link{gesPie_fun}} to plot proportion of stocks relative
#' to GES reference points for all ecoregions.
#' Input data: From \code{\link{stock_props}}, which brings \code{sag_complete_summary} and \code{stock_list_frmt}
#' from \code{\link{clean_sag}}.
#'
#' @author Scott Large
#'
#' @examples
#' \dontrun{
#' ges_stock_props(2016)
#' }
#' @export

#~~~~~~~~~~~~~~~~~~~~~#
# GES Pie charts data #
#~~~~~~~~~~~~~~~~~~~~~#
ges_stock_props <- function(active_year){

  # Split and count by variable and color
  dat <- stock_props(active_year, return_clean_sag = TRUE)
  pie_tbl <- dat$stock_props
  sag_complete_smmry <- dat$sag_complete_summary

  ges_table_count <- pie_tbl %>% #pie_table
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

# Take last year of catch data. If catch is not available, use landings.
# Remove stocks without quantified catch or landings
ges_catch_stock <-  sag_complete_smmry %>%
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
ges_table_catch <- ges_catch_stock %>%
  left_join(pie_tbl, by = "StockCode") %>%
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

# devtools::use_data(ges_table)
return(ges_table)
}


#' Clean stock status trends
#'
#' \code{clean_stock_trends} returns list of 2: data frame of F and SSB relative to
#'  F<sub>MSY</sub> and MSY B<sub>trigger</sub> reference points for stocks of a fish category for an
#'  ecoregion and \code{sag_complete_summary} from \code{\link{clean_sag}}.
#'
#' @param active_year numeric of the stock database version (year). e.g., 2016
#'
#' @note Stocks are linked to ecoregions and fish categories via the ICES Stock database.
#' Reference points are as published in ICES Stock Assessment Graphs database. In some cases,
#' status relative to reference points may vary from published ICES advice
#' when reported F or SSB are very close to reference points (e.g., F = 0.201 > F<sub>MSY</sub> = 0.20).
#'
#' @return A list of 2: 1) data frame of F and SSB relative to F<sub>MSY</sub> and MSY B<sub>trigger</sub> reference points
#' for stocks of a fish category for an ecoregion and 2) \code{sag_complete_summary} from \code{\link{clean_sag}}.
#'
#' @seealso Used in \code{\link{stock_trends_fun}} for line plots of F and SSB relative to F<sub>MSY</sub> and MSY B<sub>trigger</sub>
#' reference points for stocks of a fish category for an ecoregion.
#' Input data: From \code{\link{clean_sag}}
#'
#' @author Scott Large
#'
#' @examples
#' \dontrun{
#' clean_stock_trends(2016)
#' }
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Stock Status over time data #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

clean_stock_trends <- function(active_year = 2016) {

  dat <-  clean_sag(active_year)
  sag_complete_smmry <- dat$sag_complete_summary

  stock_trends <- sag_complete_smmry %>%
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

  # devtools::use_data(stock_trends_frmt)
  return(list("stock_trends_frmt" = stock_trends_frmt,
              "sag_complete_summary" = dat$sag_complete_summary))
}



#' Catch, discards, and landings by stock
#'
#' \code{stock_catch} returns a data frame of reference points, catch, discards, and landings by stock over time.
#'
#' @param active_year numeric of the stock database version (year). e.g., 2016
#'
#' @note Stocks are linked to ecoregions and fish categories via the ICES Stock database.
#' Reference points are as published in ICES Stock Assessment Graphs database. In some cases,
#' status relative to reference points may vary from published ICES advice
#' when reported F or SSB are very close to reference points (e.g., F = 0.201 > F<sub>MSY</sub> = 0.20).
#'
#' @return a data frame of reference points, catch, discards, and landings by stock over time.
#'
#' @seealso Used in \code{\link{guild_discards_fun}} for plots of discard rate and landings by fish category for an ecoregion.
#' Input data: From \code{\link{clean_sag}}
#' @author Scott Large
#'
#' @examples
#' head(stock_catch(2016))
#' @export

# ~~~~~~~~~~~~~~~ #
# Catch by stocks #
# ~~~~~~~~~~~~~~~ #
stock_catch <- function(active_year = 2016) {

  stock_catch <- clean_sag(active_year)$sag_complete_summary %>%
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

  return(stock_catch_full)
}

#' Stock status and catch relative to reference points
#'
#' \code{stock_status} returns a data frame of stock status relative to reference points and
#' catch, discards, and landings by stock for the most recent assessment.
#'
#' @param active_year numeric of the stock database version (year). e.g., 2016
#'
#' @note Stocks are linked to ecoregions and fish categories via the ICES Stock database.
#' Reference points are as published in ICES Stock Assessment Graphs database. In some cases,
#' status relative to reference points may vary from published ICES advice
#' when reported F or SSB are very close to reference points (e.g., F = 0.201 > F<sub>MSY</sub> = 0.20).
#'
#' @return a data frame of stock status relative to reference points and catch, discards, and landings
#' by stock for the most recent assessment.
#'
#' @seealso Used in \code{\link{plot_kobe}} to plot a scatter plot of F/F<sub>MSY</sub> and SSB/MSY B<sub>trigger</sub>
#' by fish category and ecoregion and a "lollipop" plot of total catch (divided into discards and landings) by stock.
#' Input data: From \code{\link{stock_catch}}
#'
#' @author Scott Large
#'
#' @examples
#' head(stock_status(2016))
#' @export

stock_status <- function(active_year = 2016){

  stck_ctch_fll <- stock_catch(active_year)

  stock_status_full <-
    full_join(
      stck_ctch_fll %>%
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
      stck_ctch_fll %>%
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
    mutate(colList = ifelse(is.na(F_FMSY) | is.na(SSB_MSYBtrigger),
                            "GREY",
                            if_else(F_FMSY < 1 & SSB_MSYBtrigger >= 1,
                                    "GREEN",
                                    "RED",
                                    "GREY")),
           FisheriesGuild = ifelse(StockCode %in% c("whb-comb", "mac-nea"),
                                   "large-scale stocks",
                                   FisheriesGuild))

  # devtools::use_data(stock_status_full)
  return(stock_status_full)
}


#' ICES catch time series
#'
#' \code{ices_catch_data} returns a data frame of landings by country, species, and fish category
#'
#' @return a data frame of landings by country, species, and fish category.
#'
#' @seealso Used in \code{\link{ices_catch_plot}} to create an area or line plot of landings (historic and official catch)
#' for an ecoregion by country, fish category, or species.
#' Input data: \code{\link{ices_catch_official_raw}}, \code{\link{ices_catch_historical_raw}}, \code{\link{species_list_raw}},
#' and \code{\link{stock_list_raw}}.
#'
#' @author Scott Large
#'
#' @examples
#' head(ices_catch_data())
#' @export

# ~~~~~ #
# Catch #
# ~~~~~ #
ices_catch_data <- function() {

  raw_data <- c("ices_catch_historical_raw",
                "species_list_raw",
                "stock_list_raw",
                "ices_catch_official_raw")

  data(list = raw_data, envir = environment())

  fish_category <- stock_list_raw %>%
    filter(ActiveYear >= 2016,
           !is.na(FisheriesGuild)) %>%
    mutate(X3A_CODE = gsub("-.*$", "", StockKeyLabel),
           X3A_CODE = gsub("\\..*", "", X3A_CODE),
           X3A_CODE = toupper(X3A_CODE),
           FisheriesGuild = tolower(FisheriesGuild)) %>%
    mutate(
           FisheriesGuild = case_when(
             .$X3A_CODE %in% c("SMN", "REB") ~ "pelagic",
             .$X3A_CODE %in% c("ANF") ~ "benthic",
             .$X3A_CODE %in% c("SMR", "ARG", "GUG", "RNG") ~ "demersal",
             .$X3A_CODE %in% c("SHO") ~ "elasmobranch",
             TRUE ~ .$FisheriesGuild
           )
    ) %>%
    select(X3A_CODE, FisheriesGuild) %>%
    distinct(.keep_all = TRUE)

  species_list_raw <- species_list_raw %>%
    select(English_name, Scientific_name, X3A_CODE)

  historic_bs <- c("III (not specified)", "III b  Baltic 23",
                   "III b+c (not specified)", "III b-d (not specified)",
                   "III c  Baltic 22", "III d  (not specified)",
                   "III d  Baltic 24", "III d  Baltic 25",
                   "III d  Baltic 26", "III d  Baltic 27",
                   "III d  Baltic 28 (not specified)", "III d  Baltic 28-1",
                   "III d  Baltic 28-2", "III d  Baltic 29",
                   "III d  Baltic 30", "III d  Baltic 31",
                   "III d  Baltic 32")

  historic_ns <- c("III a", "IIIa  and  IV  (not specified)",
                   "IIIa  and  IVa+b  (not specified)", "IV (not specified)",
                   "IV a", "IV a+b (not specified)",
                   "IV b", "IV b+c (not specified)",
                   "IV c", "VII d")

  historic_uk <- paste0(c("^UK", "^Channel", "^Isle of Man"),
                        collapse = "|")

  catch_dat_1950 <- ices_catch_historical_raw %>%
    gather(YEAR, VALUE, -Country, -Species, -Division) %>%
    mutate(YEAR = as.numeric(gsub("X", "", YEAR)),
           VALUE = ifelse(VALUE == "<0.5",
                          as.numeric(0),
                          VALUE),
           VALUE = ifelse(!is.na(VALUE),
                          as.numeric(VALUE),
                          NA),
           COUNTRY = case_when(
             grepl(historic_uk, .$Country) ~ "United Kingdom",
             grepl("^Germany", .$Country) ~ "Germany",
             .$Country %in% c("Un. Sov. Soc. Rep.") ~ "Russian Federation",
             grepl("Faeroe Islands", .$Country) ~ "Faroe Islands",
             grepl("Other nei", .$Country) ~ "OTHER",
             TRUE ~ .$Country
           ),
           ISO3 = countrycode::countrycode(COUNTRY, "country.name", "iso3c", warn = FALSE),
           ECOREGION = case_when(
             .$Division %in% historic_bs ~ "Baltic Sea Ecoregion",
             .$Division %in% historic_ns ~ "Greater North Sea Ecoregion",
             TRUE ~ "OTHER")) %>%
    filter(YEAR <= 2005) %>%  #,
           # ECOREGION != "OTHER",
           # COUNTRY != "OTHER") %>%
    left_join(y = species_list_raw, c("Species" = "English_name")) %>% # Merge to add FAO species information
    left_join(y = species_list_raw, c("Species" = "Scientific_name", # Merge to add FAO species information
                                  "X3A_CODE")) %>%
    left_join(y = fish_category, by = "X3A_CODE") %>%
    select(YEAR,
           COUNTRY,
           ISO3,
           GUILD = FisheriesGuild,
           ECOREGION,
           SPECIES_NAME = Scientific_name,
           SPECIES_CODE = X3A_CODE,
           COMMON_NAME = Species,
           VALUE)

  catch_dat_2010 <- ices_catch_official_raw %>%
    gather(YEAR, VALUE, -Country, -Species, -Area, -Units) %>%
    filter(Country != "") %>%
    mutate(YEAR = as.numeric(gsub("X", "", YEAR)),
           VALUE = as.numeric(VALUE),
           Country = countrycode::countrycode(Country,"iso2c", "country.name"),
           Country = ifelse(grepl("Guernsey|Isle of Man|Jersey", Country),
                            "United Kingdom",
                            Country),
           ISO3 = countrycode::countrycode(Country, "country.name", "iso3c", warn = FALSE),
           Country = gsub("(United Kingdom) .*", "\\1", Country),
           Area = tolower(Area),
           ECOREGION = case_when(
             .$Area %in% c("27.3.bc", "27.3.d", "27.3_nk") ~ "Baltic Sea Ecoregion",
             .$Area %in% c("27.3.a", "27.4", "27.7.d") ~ "Greater North Sea Ecoregion",
             TRUE ~ "OTHER")) %>%
    filter(ECOREGION != "OTHER") %>%
    left_join(species_list_raw, c("Species" = "X3A_CODE")) %>%
    left_join(fish_category, by = c("Species" = "X3A_CODE")) %>%
    select(YEAR,
           COUNTRY = Country,
           ISO3,
           GUILD = FisheriesGuild,
           ECOREGION,
           SPECIES_NAME = Scientific_name,
           SPECIES_CODE = Species,
           COMMON_NAME = English_name,
           VALUE)

  ices_catch_dat <- catch_dat_2010 %>%
    bind_rows(catch_dat_1950) %>%
    mutate(GUILD = ifelse(is.na(GUILD),
                          "undefined",
                          GUILD))

  return(ices_catch_dat)
}

#' STECF catch and effort time series
#'
#' \code{stecf_data} returns a data frame of effort and landings by country, species, and fish category
#'
#' @return a data frame of stock status relative to reference points and catch, discards, and landings
#' by stock for the most recent assessment.
#'
#' @seealso Used in \code{\link{stecf_plot}} to create an area or line plot of landings and effort for an ecoregion
#' by country and guild.
#' Input data: \code{\link{stecf_effort_raw}} and \code{\link{stecf_landings_raw}}.
#'
#' @author Scott Large
#'
#' @examples
#' head(stecf_data())
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~ #
# STECF Catch and Effort #
# ~~~~~~~~~~~~~~~~~~~~~~ #
stecf_data <- function() {

  raw_data <- c("stecf_effort_raw",
                "stecf_landings_raw")
  data(list = raw_data, envir = environment())


stecf_effort_df <- stecf_effort_raw %>%
  mutate(ISO3c = ifelse(grepl("SCO|ENG|GBG|GBJ|IOM|NIR", country),
                                "GBR",
                                country),
         COUNTRY = countrycode::countrycode(ISO3c, "iso3c", "country.name")) %>%
  mutate(YEAR = as.numeric(year),
         EFFORT = as.numeric(nominal_effort)) %>%
  select(YEAR,
         ANNEX = annex,
         AREA = regulated.area,
         COUNTRY,
         GEAR = regulated.gear,
         EFFORT)

stecf_landings_df <- stecf_landings_raw %>%
  mutate(ISO3c = ifelse(grepl("SCO|ENG|GBG|GBJ|IOM|NIR", country),
                        "GBR",
                        country),
         COUNTRY = countrycode::countrycode(ISO3c, "iso3c", "country.name")) %>%
  mutate(YEAR = as.numeric(year),
         LANDINGS = as.numeric(sum_landings),
         LANDINGS = ifelse(COUNTRY == "Germany" &
                             year == 2013 &
                             vessel.length == "U8M",
                           NA, LANDINGS)) %>%
  select(YEAR = year,
         COUNTRY,
         ANNEX = annex,
         AREA = regulated.area,
         GEAR = regulated.gear,
         LANDINGS)

gear_dat <- full_join(
  stecf_effort_df %>%
    select(ANNEX, AREA, GEAR),
  stecf_landings_df %>%
    select(ANNEX, AREA, GEAR),
  by = c("ANNEX", "AREA", "GEAR")
) %>%
  distinct(.keep_all = TRUE)

gear_dat_clean <- bind_rows(
  gear_dat %>%
    filter(ANNEX == "BAL") %>%
    mutate(ECOREGION = "Baltic Sea Ecoregion",
           gear_class = case_when(
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
    filter(ANNEX == "IIA",
           AREA %in% c("3A", "3B1", "3B2", "3B3")) %>%
    mutate(ECOREGION = "Greater North Sea Ecoregion",
           gear_class = case_when(
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
    filter(ANNEX == "CEL1") %>%
    mutate(ECOREGION = "Celtic Seas Ecoregion",
           gear_class = case_when(
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

stecf_effort_clean <- gear_dat_clean %>%
  left_join(stecf_effort_df, by = c("ANNEX", "AREA", "GEAR")) %>%
  select(YEAR,
         ANNEX,
         ECOREGION,
         AREA,
         GEAR = gear_class,
         COUNTRY,
         EFFORT) %>%
  filter(!COUNTRY %in% c("Finland", "Estonia"))

stecf_landings_clean <- gear_dat_clean %>%
  left_join(stecf_landings_df, by = c("ANNEX", "AREA", "GEAR")) %>%
  select(YEAR,
         ANNEX,
         ECOREGION,
         AREA,
         GEAR = gear_class,
         COUNTRY,
         LANDINGS) %>%
  group_by(YEAR, ANNEX, ECOREGION, AREA, GEAR, COUNTRY) %>%
  summarize(LANDINGS = sum(LANDINGS, na.rm = TRUE))

return(list("stecf_effort_clean" = stecf_effort_clean,
            "stecf_landings_clean" = stecf_landings_clean))
}
