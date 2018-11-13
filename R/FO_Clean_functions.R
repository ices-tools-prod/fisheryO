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
#'
#' Input data: \code{\link{ices_shape}}, \code{\link{eco_shape}}, and \code{\link{europe_shape}}.
#'
#' @examples
#'
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ICES Area and Ecoregion definitions # 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#in order to remove it, I should prepare all data by ecoregion, manybe is not worthy?
area_definition <- function(ecoregion)
                              # c("Greater North Sea Ecoregion","Baltic Sea Ecoregion",
                              #             "Bay of Biscay and the Iberian Coast Ecoregion",
                              #             "Celtic Seas Ecoregion","Icelandic Waters Ecoregion",
                              #             "Norwegian Sea Ecoregion", "Barents Sea Ecoregion",
                              #             "Arctic Ocean Ecoregion"))
      {
  
  raw_data <- c("europe_shape",
                "ices_shape",
                "eco_shape")
  data(list = raw_data, envir = environment())
  
  crs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  
  eco_areas <- eco_shape %>%
    sf::st_transform(crs = crs) %>%
    mutate(Ecoregion = case_when(
      grepl("Baltic Sea", .$Ecoregion) ~ "Baltic Sea Ecoregion",
      grepl("Greater North Sea", .$Ecoregion) ~ "Greater North Sea Ecoregion",
      grepl("Bay of Biscay and the Iberian Coast", .$Ecoregion) ~ "Bay of Biscay and the Iberian Coast Ecoregion",
      grepl("Celtic Seas", .$Ecoregion) ~ "Celtic Seas Ecoregion",
      grepl("Iceland Sea", .$Ecoregion) ~ "Icelandic Waters Ecoregion",
      grepl("Norwegian Sea", .$Ecoregion) ~ "Norwegian Sea Ecoregion",
      grepl("Barents Sea", .$Ecoregion) ~ "Barents Sea Ecoregion",
      #grepl("Arctic Ocean", .$Ecoregion) ~ "Arctic Ocean Ecoregion",
      # ... add remaining ecoregions
      TRUE ~ "OTHER")) %>%
    filter(Ecoregion == ecoregion) %>%
    sf::st_sf()
  
  Area_27_baltic <- c("3.d.27", "3.d.25", "3.d.24",
                      "3.b.23", "3.c.22", "3.d.31",
                      "3.d.30", "3.d.32", "3.d.29",
                      "3.d.28.1", "3.d.28.2", "3.d.26")
  #have to do like this, because both have the 7.e in the map
  if(ecoregion == "Greater North Sea Ecoregion") {
  Area_27_ns <- c("3.a.20", "3.a.21",
                  "4.a", "4.b", "4.c",
                  "7.d", "7.e")
  }
  if(ecoregion == "Celtic Seas Ecoregion") {
  Area_27_cs <- c("6.a", "6.b.2","7.a", "7.b", "7.c.2", "7.e",
                  "7.f", "7.g", "7.h","7.j.2", "7.k.2")
  }
  
  #all these next area definitions need decissions:
  
  Area_27_bob <- c("8.a", "8.b","8.c",
                   "8.d.2", "8.e.2", "9.a",
                   "9.b.2")
  Area_27_is <- c("5.a.1", "5.a.2","12.a.4")
  
  Area_27_nw <- c("2.a.1", "2.a.2", "2.b.1", "2.b.2", "14.a")
  
  Area_27_br <- c("1.a", "1.b","2.a.2", "2.b.2")
  
  ices_areas <- ices_shape %>%
    sf::st_transform(crs = crs) %>%
    mutate(ECOREGION = case_when(
      .$Area_27 %in% Area_27_baltic ~ "Baltic Sea Ecoregion",
      # .$Area_27 %in%  Area_27_ns ~ "Greater North Sea Ecoregion",
      .$Area_27 %in%  Area_27_bob ~ "Bay of Biscay and the Iberian Coast Ecoregion",
      #.$Area_27 %in%  Area_27_cs ~ "Celtic Seas Ecoregion",
      .$Area_27 %in%  Area_27_is ~ "Icelandic Waters Ecoregion",
      
      #this bit has to be turned on and off, because either areas are 
      # in norwegian or in barents
      # .$Area_27 %in%  Area_27_nw ~ "Norwegian Sea Ecoregion",
      .$Area_27 %in%  Area_27_br ~ "Barents Sea Ecoregion",
      # ... add remaining ecoregions
      TRUE ~ "OTHER")) %>%
    sf::st_sf()
  
  if(ecoregion == "Greater North Sea Ecoregion") {
    ices_areas <- ices_shape %>%
      sf::st_transform(crs = crs) %>%
      mutate(ECOREGION = case_when(
        .$Area_27 %in%  Area_27_ns ~ "Greater North Sea Ecoregion"))%>%
          sf::st_sf()
  }
  
  if(ecoregion == "Celtic Seas Ecoregion") {
    ices_areas <- ices_shape %>%
      sf::st_transform(crs = crs) %>%
      mutate(ECOREGION = case_when(
       .$Area_27 %in% Area_27_cs ~ "Celtic Seas Ecoregion"))%>%
      sf::st_sf()
  }
  
  
  # Centroids for labels
  ices_area_centroids <- sf::st_centroid(ices_areas)
  centroids <- data.frame(as.character(ices_area_centroids$Area_27),
                          ices_area_centroids$ECOREGION,
                          matrix(unlist(ices_area_centroids$geometry),
                                 ncol = 2,
                                 byrow = TRUE),
                          stringsAsFactors = FALSE)
  
  colnames(centroids) <- c("Area_27", "ECOREGION", "X", "Y")
  
  if(ecoregion == "Celtic Seas Ecoregion") {
    extracentroids <-centroids %>% filter(Area_27 %in% c("4.a", "2.a.2", "5.b"))
    #mutate, change the position of labels so they are close to Celtic Seas
    extracentroids[,3] <- c(3710000, 3760000)
    extracentroids[,4] <- c(4250000, 4500000)
    extraareas <- ices_areas%>% filter(Area_27 %in% c("4.a", "2.a.2"))
    extracentroids<<- extracentroids
    extraareas<<- extraareas
    }
  
  if(ecoregion == "Baltic Sea Ecoregion") {
    baltic_3a <- ices_areas %>%
      filter(SubArea == "3",
             Division == "a") %>%
      summarize(Area_27 = "3.a",
                ECOREGION = "Baltic Sea Ecoregion",
                geometry = sf::st_union(geometry)) %>%
      sf::st_sf() %>%
      sf::st_centroid()
    
    baltic_3a <- data.frame(baltic_3a$Area_27,
                            baltic_3a$ECOREGION,
                            matrix(unlist(baltic_3a$geometry),
                                   ncol = 2,
                                   byrow = TRUE),
                            stringsAsFactors = FALSE)
    
    colnames(baltic_3a) <- c("Area_27", "ECOREGION", "X", "Y")
    
    centroids <- bind_rows(centroids, baltic_3a)
  }
  
  
  centroids <- centroids %>%
    mutate(Area_27 = case_when(
      .$ECOREGION == "Baltic Sea Ecoregion" ~ sub("3.b.|3.c.|3.d.", "", .$Area_27),
      .$ECOREGION == "Greater North Sea Ecoregion" ~ as.character(.$Area_27),
      .$ECOREGION == "Bay of Biscay and the Iberian Coast Ecoregion" ~ as.character(.$Area_27),
      .$ECOREGION == "Celtic Seas Ecoregion" ~ as.character(.$Area_27),
      .$ECOREGION == "Icelandic Waters Ecoregion" ~ as.character(.$Area_27),
      .$ECOREGION == "Norwegian Sea Ecoregion" ~ as.character(.$Area_27),
      .$ECOREGION == "Barents Sea Ecoregion" ~ as.character(.$Area_27),
      TRUE ~ "OTHER"
    ))
  
  ices_areas <- ices_areas %>%
    filter(grepl(ecoregion, ECOREGION)) %>%
    sf::st_sf()
  
  centroids <- centroids %>%
    filter(grepl(ecoregion, ECOREGION))
  
  ices_areas <<- ices_areas
  eco_areas <<- eco_areas
  europe_shape <<- europe_shape
  centroids <<- centroids
   extracentroids <<- extracentroids
   extraareas <<- extraareas
  crs <<- crs
  
  # return(list("ices_areas" = ices_areas,
  #             "eco_areas" = eco_areas,
  #             "europe_shape" = europe_shape,
  #             "centroids" = centroids,
  #             "crs" = crs))
  
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

clean_sag <- function(active_year = active_year){
  
  raw_data <- c("stock_list_raw",
                "sag_summary_raw",
                "sag_keys_raw",
                "sag_refpts_raw")
  
   data(list = raw_data, envir = environment())
  
  filter_stocks <- c("cod-ingr", "cod-wgr", "sal-nea", "san-scow",
                   "sal-na", "sal-32", "sal-2431",
                   "trt-bal", "sal-wgc", #"cod.21.1", "cod.21.1a-e",
                   "sal.27.nea", "san.27.6a", "sal.21.2-5", "sal.27.32",
                   "sal.27.22-31", "trs.27.22-32", "sal.2127.1a-f14", "ele.2737.nea")

  stock_list <- stock_list_raw %>%
  filter(ActiveYear == active_year,
         !is.na(YearOfLastAssessment)) %>%
  mutate(StockKeyLabel = case_when(is.na(YearOfLastAssessment) &
                                     is.na(YearOfNextAssessment) ~ StockKeyLabel,
                                   active_year >= 2017 &
                                     YearOfLastAssessment < 2017 &
                                     # YearOfNextAssessment > 2017 &
                                     AdviceCategory %in% c("MSY", "MP", "MSY/PA") ~ PreviousStockKeyLabel,
                                   TRUE ~ StockKeyLabel),
         ID2016 = toupper(gsub( "-.*$", "", StockKeyLabel)),
         ID2017 = toupper(gsub("\\..*", "", StockKeyLabel)),
         SpeciesID = case_when(active_year < 2017 ~ ID2016,
                               YearOfLastAssessment < 2017 &
                                 # YearOfNextAssessment > 2017 &
                                 AdviceCategory %in% c("MSY", "MP", "MSY/PA") ~ ID2016,
                               TRUE ~ ID2017)) %>%
  select(StockKeyLabel,
         Description = StockKeyDescription,
         SpeciesScientificName,
         EcoRegion,
         DataCategory,
         YearOfLastAssessment,
         AdviceCategory,
         FisheriesGuild,
         SpeciesID) %>%
  filter(!StockKeyLabel %in% filter_stocks) %>%
  mutate(DataCategory = floor(as.numeric(DataCategory)),
         StockKeyLabel = tolower(StockKeyLabel),
         FisheriesGuild = tolower(FisheriesGuild),
         Description = gsub(pattern = "\u2013", "-", Description), # remove en dashes in favor of hyphens
         Description = gsub(pattern = "Micromesistius poutasso",
                            "Micromesistius poutassou", Description), # misspelled blue whiting
         Description = gsub(pattern = "Solea spp.",
                            "Solea solea", Description), # wrong description for sole
         Description = gsub(pattern = "Platichtys flesus",
                            "Platichthys flesus", Description),
         Description = gsub(pattern = "Lophius piscatorius and L. budegassa",
                            "Lophius piscatorius and Lophius budegassa", Description),
         AdviceCategory = ifelse(AdviceCategory == "MSY/PA",
                                 "MSY", AdviceCategory),
         SpeciesScientificName = recode(SpeciesScientificName,
                                        "Mustelus asterias" = "Mustelus"))

# Format so the species names will be italicized
  stock_list_frmt <- bind_rows(
  # Normal binomial names
    stock_list %>%
     filter(grepl("[[:space:]]", SpeciesScientificName)) %>%
     mutate(StockKeyDescription = stringr::str_replace_all(string = Description,
                                                          pattern = SpeciesScientificName,
                                                          replacement = paste0("<em>", SpeciesScientificName, "</em>"))),
  # Groups of species (.spp)
  stock_list %>%
    filter(grepl(" spp.*$", Description)) %>%
    mutate(StockKeyDescription = stringr::str_replace_all(string = Description,
                                                          pattern = SpeciesScientificName,
                                                          replacement = paste0("<em>", SpeciesScientificName, "</em>"))),
  # A bit different notation (embedded in 2 sets of parentheses)
  stock_list %>%
    filter(StockKeyLabel %in% c("raj-mar", "raj.27.1012")) %>%
    mutate(Description = stringr::str_replace_all(string = Description,
                                                  pattern = "Raja clavata",
                                                  replacement = "<em>Raja clavata</em>")),
  
  # The "others" with no species name
  stock_list %>%
    filter(!grepl(" spp.*$", Description)) %>%
    filter(!StockKeyLabel %in% c("raj-mar", "raj.27.1012")) %>%
    filter(!grepl("[[:space:]]", SpeciesScientificName))
)

sag_keys_raw$StockKeyLabel <- tolower(sag_keys_raw$StockKeyLabel)

found_stocks <- stock_list %>%
  select(-EcoRegion) %>%
  distinct(.keep_all = TRUE) %>%
  left_join(sag_keys_raw, by = c("StockKeyLabel",
                                 "YearOfLastAssessment" = "AssessmentYear"))

sag_summary <- found_stocks %>%
  left_join(sag_summary_raw, by = c("StockKeyLabel" = "fishstock",
                                    "YearOfLastAssessment" = "AssessmentYear")) %>%
  select(Year,
         StockKeyLabel,
         F,
         SSB,
         fishingPressureDescription,
         stockSizeDescription,
         landings,
         catches,
         #recruitment,
         discards)

# List of the F types that we want to include in the analysis and those that should be considered "relative"
keeper_F <- c("F", "F/FMSY", "F in winter rings", "Harvest rate", "F/Fmsy", "Harvest Rate","F(ages 3-6)",
              "Harvest rate/FMSY", "Fishing Pressure", "weighted F", "Total biomass/Bmsy")
relative_F <- c("F/FMSY", "F/Fmsy", "Harvest rate/FMSY")

# List of the SSB types that we want to include in the analysis and those that should be considered "relative"
keeper_SSB <- c("SSB", "B/BMSY", "SSB & Biomass Age 4+", "UW Tv Index", "Total biomass/Bmsy", "B/Bmsy",
                "Stock Size", "Total biomass/BMSY", "Abundance", "Stock abundance", "Abundance Index", "UW Tv index")
relative_SSB <- c("B/BMSY", "B/Bmsy", "Total biomass/BMSY", "Total biomass/Bmsy")

# Clean up the stock summary data
sag_summary_clean <- sag_summary %>%
  mutate(StockKeyLabel = tolower(StockKeyLabel),
         # discards are erroneously uploaded to SAG for nep-5 2015.
         discards = ifelse(Year == 2015 & StockKeyLabel %in% c("nep-5", "nep.fu.5"),
                           NA,
                           discards),
         # landings are erroneously uploaded for nop.27.3a4 and pra.27.4a20 2017
         landings = ifelse(StockKeyLabel %in% c("nop.27.3a4", "pra.27.4a20"),
                           NA,
                           landings),
         # Wrong units for san.sa.1r
         landings = ifelse(StockKeyLabel == "san.sa.1r",
                           landings/1000,
                           landings),
         catches = ifelse(StockKeyLabel == "san.sa.1r",
                          catches/1000,
                          catches),
         # Real value is 0.201. before rounding rules and ADG erred towards green
         F = ifelse(Year == 2015 & StockKeyLabel %in% c("sol-nsea", "sol.27.4"),
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
  left_join(sag_refpts_raw, by = c("StockKeyLabel",
                                   "YearOfLastAssessment" = "AssessmentYear",
                                   "AssessmentKey" = "AssessmentKey")) %>%
  select(StockKeyLabel,
         Flim = FLim,
         Fpa,
         Bpa,
         Blim,
         FMSY,
         MSYBtrigger) %>%
  # ****************** #
  # MSYBtrigger isn't appropriate for Sandeel
  mutate(MSYBtrigger = ifelse(grepl("san", StockKeyLabel),
                              NA,
                              MSYBtrigger))
# ****************** #

# Merge the stock data, reference points, and summary table together
sag_ref_pts<- unique(sag_ref_pts)
sag_summary_clean <- unique(sag_summary_clean)
sag_ref_summary <- stock_list_frmt %>%
  left_join(sag_ref_pts , by = "StockKeyLabel") %>%
  left_join(sag_summary_clean, by = "StockKeyLabel") #%>%


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
 stock_list <<- stock_list
 stock_list_frmt <<- stock_list_frmt
 sag_complete_summary <<- sag_complete_summary

}

##need to add a new function to give back stock_list_frmt, sag_complete_summary and
#stock_list by ecoregion

# sag_ecoregion <- function(ecoregion){
#   sag_ecoregion <- sag_complete_summary %>% filter(grepl(pattern = ecoregion, EcoRegion))
#   if(ecoregion == "Celtic Seas Ecoregion"){
#     CSout_stocks <- c("aru.27.123a4", "bli.27.nea", "bll.27.3a47de",
#                       "cap.27.2a514", "her.27.1-24a514a", "lin.27.5b", "reb.2127.sp",
#                        "reg.27.561214", "rjb.27.3a4", "rng.27.1245a8914ab",
#                         "san.sa.7r", "smn-dp")
#     sag_ecoregion <<- sag_ecoregion [!(sag_ecoregion$StockKeyLabel %in% CSout_stocks), ]
#   }
# }



#' Format stock summary table
#'
#' \code{frmt_summary_tbl} returns the stock summary table plain and formatted with html (e.g., glyphicons and italics)
#'
#' @param active_year numeric of the stock database version. e.g., 2016
#' @param return_clean_sag logical to return objects from clean_sag()
#' @param calculate_status logical on whether to use raw SAG output to calculate stock status or to use the hard-coded values from stock summary table
#'
#' @note Periodically, ICES adds or removes stocks from the advisory process. The function returns the stock summary table for all published (in SAG) and active stocks for a given year.
#' \code{calculate_status = TRUE} calculates stock status
#' relative to published reference points. This will represent PA and SBL for ecoregions with proxy reference points. \code{calculate_status = TRUE} takes the
#' raw icons from published advice. Note, before 2017 not all stocks status tables have been added to the SAG database and only few stocks had MSY proxy reference points.
#'
#' @return data frame
#'
#' @author Scott Large
#'
#' @seealso Used in \code{\link{stockSummaryTable_fun}} to create the "Status of stock summary relative to reference points"
#' table for all stocks for an ecoregion. Input data: SAG summary table and reference points come from \code{\link{clean_sag}}.
#' @examples
#' head(frmt_summary_tbl(active_year = 2017)$summary_table)
#' @export

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Recreate Status of stock summary relative to reference points table #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

#I removed the option to calculate status, as it can be modified by decissions,
#but it is in last year´s code.

format_summary_table <- function(active_year = active_year) {

  sag_sum <- sag_complete_summary 
  stck_frmt <- stock_list_frmt 


# Clean up the SAG output
  
  sag_stock_status <- sag_stock_status_raw %>%
    mutate(status = case_when(status == 0 ~ "UNDEFINED",
                            status == 1 ~ "GREEN",
                            status == 2 ~ "qual_GREEN", #qualitative green
                            status == 3 ~ "ORANGE",
                            status == 4 ~ "RED",
                            status == 5 ~ "qual_RED", #qualitative red
                            status == 6 ~ "GREY",
                            status == 7 ~ "qual_UP",
                            status == 8 ~ "qual_STEADY",
                            status == 9 ~ "qual_DOWN",
                            TRUE ~ "OTHER"),
         year = gsub("2023", "2016", year),
         fishingPressure = case_when(fishingPressure == "-" &
                                       type == "Fishing pressure" ~ "FQual",
                                     TRUE ~ fishingPressure),
         stockSize = case_when(stockSize == "-" &
                                 type == "Stock Size" ~ "SSBQual",
                               TRUE ~ stockSize),
         stockSize = gsub("MSY BT*|MSY Bt*|MSYBT|MSYBt", "MSYBt", stockSize),
         variable = case_when(type == "Fishing pressure" ~ fishingPressure,
                              type == "Stock Size" ~ stockSize,
                              TRUE ~ type),
         variable = case_when(lineDescription == "Management plan" &
                                type == "Fishing pressure" ~ "FMGT",
                              lineDescription == "Management plan" &
                                type == "Stock Size" ~ "SSBMGT",
                              TRUE ~ variable),
         variable = case_when(
           grepl("Fpa", variable) ~ "FPA",
           grepl("Bpa", variable) ~ "BPA",
           grepl("^Qual*", variable) ~ "SSBQual",
           grepl("-", variable) ~ "FQual",
           grepl("^BMGT", variable) ~ "SSBMGT",
           grepl("MSYBtrigger", variable) ~ "BMSY",
           grepl("FMSY", variable) ~ "FMSY",
           TRUE ~ variable
         )) %>%
  filter(variable != "-") %>%
  arrange(variable, year) %>%
  mutate(year = paste0(variable, year),
         year = factor(year, levels = unique(year)))

# ID the list of stocks
stockID <- stock_list_raw %>% select(StockKeyLabel, PreviousStockKeyLabel)
stockID <- unique(stockID)%>% drop_na()
stockID$correct <- stockID$StockKeyLabel
stockID <- stockID [,-1]
stockID$StockKeyLabel <- stockID$PreviousStockKeyLabel 
stockID <- stockID [,-1]

sag_stock_status <- unique(sag_stock_status)
sag_wrong <- sag_stock_status %>% filter(StockKeyLabel %in% stockID$StockKeyLabel)
sag_stock_status <-sag_stock_status[!(sag_stock_status$StockKeyLabel %in% sag_wrong$StockKeyLabel),]

sag_wrong <- sag_wrong %>% left_join(stockID)
sag_wrong$StockKeyLabel <- sag_wrong$correct
sag_wrong <- sag_wrong[,-17]
sag_stock_status <- sag_stock_status %>% rbind(sag_wrong)

stock_list <-unique(stock_list)

stock_summary_table <-  stck_frmt %>%
  select(-EcoRegion) %>%
  distinct(.keep_all = TRUE) %>%
  left_join(sag_stock_status, by = c("StockKeyLabel" = "StockKeyLabel",
                                     "YearOfLastAssessment" = "AssessmentYear")) %>%
  filter(!is.na(variable)) %>%
  select(StockKeyLabel,
         YearOfLastAssessment,
         year, status)
stock_summary_table <- unique(stock_summary_table)%>%
  tidyr::spread(year, status)


#' Convert FPA to SBL
#'
#' \code{pa_maker} helper function that takes FPA and BPA and converts to SB.
#'
#' @param df numeric of the stock database version. e.g., 2016
#' @param assessment_year logical to return objects from clean_sag()
#'
#' @note internal function
#'
#' @return data frame
#'
#' @author Scott Large
#'
#' @seealso Used in \code{\link{frmt_summary_tbl}} to create the "Status of stock summary relative to reference points"
#' table for all stocks for an ecoregion.
#' @keywords internal


pa_maker <- function(df,
                     assessment_year){
  
  FPA_val = rlang::sym(paste0("FPA", assessment_year - 1))
  BPA_val = rlang::sym(paste0("BPA", assessment_year))
  BPA_val2 = rlang::sym(paste0("BPA", assessment_year - 1))
  
  
  if(!any(df$YearOfLastAssessment %in% assessment_year)) stop("No cases meet this condition.")
  
  df %>%
    select(StockKeyLabel,
           YearOfLastAssessment,
           contains("PA")) %>%
    filter(YearOfLastAssessment == assessment_year) %>%
    mutate(SBL = case_when(rlang::UQ(FPA_val) == "GREEN" & rlang::UQ(BPA_val) == "GREEN" ~ "GREEN",
                           rlang::UQ(FPA_val) == "GREEN" & rlang::UQ(BPA_val2) == "GREEN" & is.na(rlang::UQ(BPA_val)) ~ "GREEN",
                           rlang::UQ(FPA_val) == "RED" | rlang::UQ(BPA_val) == "RED" ~ "RED",
                           rlang::UQ(FPA_val) == "ORANGE"  |  rlang::UQ(BPA_val) == "ORANGE" ~ "RED",
                           TRUE ~ "GREY"))
}

# summary_table <- stock_summary_table %>%
#   left_join(do.call("rbind",
#                     lapply(unique(stock_summary_table$YearOfLastAssessment),
#                            function(x) pa_maker(df = stock_summary_table,
#                                                 assessment_year = x)))[, c("StockKeyLabel", "SBL")],
#             by = "StockKeyLabel")%>%
#   select(StockKeyLabel,
#          contains("MSY"),
#          contains("PA"),
#          contains("SBL"),
#          -contains("proxy"),
#          -contains("escapement"))
# 
# names <- c(paste0("FMSY_",active_year-3),
#                 paste0("FMSY_",active_year-2),
#                 paste0("FMSY_",active_year-1),
#                 paste0("SSBMSY_",active_year-2),
#                 paste0("SSBMSY_",active_year-1),
#                 paste0("SSBMSY_",active_year))
# new_var <- data.frame(matrix(ncol = 6, nrow =length(0)))
# 
# colnames(new_var) <- names
# 
# 
# # stockDat
# summary_table_frmt <- stck_frmt %>%  #slFull
#   left_join(summary_table, by = "StockKeyLabel")%>%
#   mutate(assign(paste0("FMSY_",active_year-3)),ifelse(AdviceCategory  %in% c("MSY", "MP", "PA"),
#                                                       "FMSY2015",
#                                                       "FPA2015"))
# 
# new_var <- data.frame(matrix(ncol = 6, nrow =nrow(summary_table_frmt)))
# colnames(new_var) <- names
# 
# 
# summary_table_frmt <-cbind(summary_table_frmt,new_var)
# 
# summary_table_frmt <- stck_frmt %>%  #slFull
#   left_join(summary_table, by = "StockKeyLabel") %>%
#   mutate(names = ifelse(AdviceCategory %in% c("MSY", "MP"),
#                          FMSY2015,
#                          FPA2015),
#          F_2016 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
#                          FMSY2016,
#                          FPA2016),
#          F_2017 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
#                          FMSY2017,
#                          FPA2017),
#          SSB_2016 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
#                            BMSY2016,
#                            BPA2016),
#          SSB_2017 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
#                            BMSY2017,
#                            BPA2017),
#          SSB_2018 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
#                            BMSY2018,
#                            BPA2018),
#          D3C1 = NA,
#          D3C2 = NA,
#          GES = NA)
#   
# 
#   new_var[,1]<- summary_table_frmt %>% ifelse(AdviceCategory  %in% c("MSY", "MP", "PA"),
#                             "FMSY2015",
#                             "FPA2015"),
#               eval(parse(text=new_var[2])) == ifelse(AdviceCategory  %in% c("MSY", "MP", "PA"),
#                              old_var[2],old_var[5]),
#                    eval(parse(text=new_var[3])) == ifelse(AdviceCategory  %in% c("MSY", "MP", "PA"),
#                              old_var[3],old_var[6]),
#                         eval(parse(text=new_var[4])) == ifelse(AdviceCategory  %in% c("MSY", "MP", "PA"),
#                              old_var[7],old_var[10]),
#                              eval(parse(text=new_var[5]) == ifelse(AdviceCategory  %in% c("MSY", "MP", "PA"),
#                              old_var[8],old_var[11]),
#                              eval(parse(text=new_var[6])) == ifelse(AdviceCategory  %in% c("MSY", "MP", "PA"),
#                              old_var[9],old_var[12]),
#          D3C1 = NA,
#          D3C2 = NA,
#          GES = NA))

summary_table <- stock_summary_table %>%
  left_join(do.call("rbind",
                    lapply(unique(stock_summary_table$YearOfLastAssessment),
                           function(x) pa_maker(df = stock_summary_table,
                                                assessment_year = x)))[, c("StockKeyLabel", "SBL")],
            by = "StockKeyLabel")%>%
  select(StockKeyLabel,
         contains("MSY"),
         contains("PA"),
         contains("SBL"),
         -contains("proxy"),
         -contains("escapement"))



# stockDat
summary_table_frmt <- stck_frmt %>%  #slFull
  left_join(summary_table, by = "StockKeyLabel") %>%
  mutate(FMSY_2014 = ifelse(AdviceCategory %in% c("MSY", "MP", "PA"),
                            FMSY2014,FPA2014),
         FMSY_2015 = ifelse(AdviceCategory  %in% c("MSY", "MP", "PA"),
                            FMSY2015,FPA2015),
         FMSY_2016 = ifelse(AdviceCategory  %in% c("MSY", "MP", "PA"),
                            FMSY2016,FPA2016),
         FMSY_2017 = ifelse(AdviceCategory  %in% c("MSY", "MP", "PA"),
                            FMSY2017,FPA2017),
         SSBMSY_2015 = ifelse(AdviceCategory  %in% c("MSY", "MP", "PA"),
                              BMSY2015,BPA2015),
         SSBMSY_2016 = ifelse(AdviceCategory  %in% c("MSY", "MP", "PA"),
                              BMSY2016,BPA2016),
         SSBMSY_2017 = ifelse(AdviceCategory  %in% c("MSY", "MP", "PA"),
                              BMSY2017,BPA2017),
         SSBMSY_2018 = ifelse(AdviceCategory  %in% c("MSY", "MP", "PA"),
                              BMSY2018,BPA2018),
         D3C1 = NA,
         D3C2 = NA,
         GES = NA)
#this table, was the one discussed by Eskild and Colm, flag it
#problem is here, I loose stocks with year of last assessment 2015.

summary_table_frmt <- bind_rows(
  summary_table_frmt %>%
    filter(YearOfLastAssessment == active_year-2) %>%
    mutate(D3C1 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                         FMSY2015,
                         FPA2015),
           D3C2 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                         BMSY2016,
                         BPA2016)),
  summary_table_frmt %>%
    filter(YearOfLastAssessment == active_year-1) %>%
    mutate(D3C1 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                         FMSY2016,
                         FPA2016),
           D3C2 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                         BMSY2017,
                         BPA2017)),
  summary_table_frmt %>%
    filter(YearOfLastAssessment == active_year) %>%
    mutate(D3C1 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                         FMSY2017,
                         FPA2017),
           D3C2 = ifelse(AdviceCategory  %in% c("MSY", "MP"),
                         BMSY2018,
                         BPA2018)
    )
) 
summary_table_frmt <-summary_table_frmt%>%
  select(StockKeyLabel,
         Description,
         SpeciesScientificName,
         FisheriesGuild,
         EcoRegion,
         AdviceCategory,
         DataCategory,
         SBL,
         FMSY_2014, FMSY_2015, FMSY_2016,FMSY_2017,
         SSBMSY_2015, SSBMSY_2016, SSBMSY_2017,SSBMSY_2018,
         D3C1, D3C2, GES)


summary_table_frmt$GES[is.na(summary_table_frmt$D3C1) |
                         is.na(summary_table_frmt$D3C2)] <- NA
summary_table_frmt$GES[summary_table_frmt$D3C1 == "RED" |
                         summary_table_frmt$D3C2 == "RED"] <- "RED"
summary_table_frmt$GES[summary_table_frmt$D3C1 == "GREEN" &
                         summary_table_frmt$D3C2 == "GREEN"] <- "GREEN"

summary_table_frmt[c("SBL", "FMSY_2014", "FMSY_2015", "FMSY_2016","FMSY_2017",
                     "SSBMSY_2015", "SSBMSY_2016", "SSBMSY_2017","SSBMSY_2018",
                     "D3C1", "D3C2", "GES")][is.na(summary_table_frmt[c("SBL", "FMSY_2014", "FMSY_2015", "FMSY_2016","FMSY_2017",
                                                                        "SSBMSY_2015", "SSBMSY_2016","SSBMSY_2017","SSBMSY_2018",
                                                                        "D3C1", "D3C2", "GES")])] <- "GREY"

     # return(list("summary_table_frmt" = summary_table_frmt,
     #             "summary_table" = summary_table))

summary_table_frmt <<- summary_table_frmt
summary_table <<- summary_table
 }

#' Proportion of stocks relative to reference points
#'
#' \code{stock_props} returns a list of the proportion of stocks
#'  relative to reference points for fish categories in ecoregions.
#'
#' @param active_year numeric of the stock database version (year). e.g., 2016
#' @param ecoregion vector of ecoregions to include
#' @param fisheries_guild vector of fisheries guilds to include
#' @param return_clean_sag logical to return objects from clean_sag()
#' @param calculate_status logical on whether to use raw SAG output to calculate stock status or to use the hard-coded values from stock summary table
#'
#' @note Periodically, ICES adds or removes stocks from the advisory process. The function returns
#' the SAG reference points and summary table for all published (in SAG) and active stocks for a given year.\code{calculate_status = TRUE} calculates stock status
#' relative to published reference points. This will represent PA and SBL for ecoregions with proxy reference points. \code{calculate_status = TRUE} takes the
#' raw icons from published advice. Note, before 2017 not all stocks status tables have been added to the SAG database and only few stocks had MSY proxy reference points.
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
#could be called from inside ices_stock_props and ges_stock_props?
stock_props <- function(active_year = active_year,
                        ecoregion = ecoregion,
                        fisheries_guild = fisheries_guild) {

stck_frmt <- stock_list_frmt
stck_frmt <- stck_frmt %>%filter(grepl(pattern = ecoregion, EcoRegion))

if(ecoregion == "Celtic Seas Ecoregion"){
  CSout_stocks <- c("aru.27.123a4", "bli.27.nea", "bll.27.3a47de",
                    "cap.27.2a514", "her.27.1-24a514a", "lin.27.5b", "reb.2127.sp",
                    "reg.27.561214", "rjb.27.3a4", "rng.27.1245a8914ab",
                    "san.sa.7r", "smn-dp")
  stck_frmt <<- stck_frmt [!(stck_frmt$StockKeyLabel %in% CSout_stocks), ]
}
summary_tbl <- summary_table

pie_table <- stck_frmt %>%
  select(StockKeyLabel,
         EcoRegion,
         FisheriesGuild,
         YearOfLastAssessment) %>%
  left_join(summary_tbl, by = "StockKeyLabel") %>%
  # filter(EcoRegion %in% ecoregion)%>% #,
         #FisheriesGuild %in% fisheries_guild) %>%
  #the problem is here
  mutate(FMSY = ifelse(YearOfLastAssessment == 2014,
                       FMSY2013,
                       ifelse(YearOfLastAssessment == 2015,
                              FMSY2014,
                              ifelse(YearOfLastAssessment == 2016,
                                     FMSY2015,
                                     ifelse(YearOfLastAssessment == 2017,
                                            FMSY2016,
                                            ifelse(YearOfLastAssessment == 2018,
                                                   FMSY2017,
                                                   NA))))),
         FPA = ifelse(YearOfLastAssessment == 2014,
                      FPA2013,
                      ifelse(YearOfLastAssessment == 2015,
                             FPA2014,
                             ifelse(YearOfLastAssessment == 2016,
                                    FPA2015,
                                    ifelse(YearOfLastAssessment == 2017,
                                           FPA2016,
                                           ifelse(YearOfLastAssessment == 2018,
                                                  FPA2017,
                                                  NA))))),
         BMSY = ifelse(YearOfLastAssessment == 2014,
                       BMSY2014,
                       ifelse(YearOfLastAssessment == 2015,
                              BMSY2015,
                              ifelse(YearOfLastAssessment == 2016,
                                     BMSY2016,
                                     ifelse(YearOfLastAssessment == 2017,
                                            BMSY2017,
                                            ifelse(YearOfLastAssessment == 2018,
                                                   BMSY2018,
                                                   NA))))),
         BPA = ifelse(YearOfLastAssessment == 2014,
                      BPA2014,
                      ifelse(YearOfLastAssessment == 2015,
                             BPA2015,
                             ifelse(YearOfLastAssessment == 2016,
                                    BPA2016,
                                    ifelse(YearOfLastAssessment == 2017,
                                           BPA2017,
                                           ifelse(YearOfLastAssessment == 2018,
                                                  BPA2018,
                                                  NA))))))
  
 #Hopefully it wont be needed when is in sid:  
 # pie_table$FisheriesGuild[which(pie_table$StockKeyLabel == "tsu.27.nea")] <- "demersal"
   pie_table <<-pie_table

   # return(list("stock_props" = pie_table))

 }

#' Proportion of stocks relative to ICES reference points
#'
#' \code{ices_stock_props} returns a data frame of the proportion of stocks
#'  relative to ICES reference points for fish categories in ecoregions.
#'
#' @param active_year numeric of the stock database version (year). e.g., 2016
#' @param ecoregion vector of ecoregions to include
#' @param fisheries_guild vector of fisheries guilds to include
#' @param calculate_status logical on whether to use raw SAG output to calculate stock status or to use the hard-coded values from stock summary table
#'
#' @note Periodically, ICES adds or removes stocks from the advisory process. The function returns
#' the SAG reference points and summary table for all published (in SAG) and active stocks for a given year. \code{calculate_status = TRUE} calculates stock status
#' relative to published reference points. This will represent PA and SBL for ecoregions with proxy reference points. \code{calculate_status = TRUE} takes the
#' raw icons from published advice. Note, before 2017 not all stocks status tables have been added to the SAG database and only few stocks had MSY proxy reference points.
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

ices_stock_props <- function(active_year = active_year,
                             ecoregion = ecoregion,
                             fisheries_guild = fisheries_guild) {

pie_tbl <- pie_table

pie_table_stock <- pie_tbl %>%
  select(EcoRegion,
         StockKeyLabel,
         FisheriesGuild,
         FMSY,
         BMSY,
         FPA,
         BPA,
         SBL) %>%
  tidyr::gather(VARIABLE, VALUE, -EcoRegion, -StockKeyLabel, -FisheriesGuild) %>%
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
  tidyr::gather(VARIABLE, VALUE, -EcoRegion, -FisheriesGuild) %>%
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

pie_table_count <<- pie_table_count
# return(pie_table_count)
}

#' Proportion of stocks relative to GES reference points
#'
#' \code{ges_stock_props} returns a data frame of the proportion of stocks
#'  relative to GES reference points for all ecoregion.
#'
#' @param active_year numeric of the stock database version (year). e.g., 2016
#' @param ecoregion vector of ecoregions to include
#' @param fisheries_guild vector of fisheries guilds to include
#' @param calculate_status logical on whether to use raw SAG output to calculate stock status or to use the hard-coded values from stock summary table
#'
#' @note Periodically, ICES adds or removes stocks from the advisory process. The function returns
#' the SAG reference points and summary table for all published (in SAG) and active stocks for a given year.\code{calculate_status = TRUE} calculates stock status
#' relative to published reference points. This will represent PA and SBL for ecoregions with proxy reference points. \code{calculate_status = TRUE} takes the
#' raw icons from published advice. Note, before 2017 not all stocks status tables have been added to the SAG database and only few stocks had MSY proxy reference points.
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
ges_stock_props <- function(active_year = active_year,
                            ecoregion = ecoregion,
                            fisheries_guild = fisheries_guild){


pie_tbl <- pie_table
sag_complete_smmry <- sag_complete_summary

ges_table_count <- pie_tbl %>% #pie_table
  select(EcoRegion,
         D3C2 = BMSY,
         D3C1 = FMSY) %>%
  tidyr::gather(VARIABLE, COLOR, -EcoRegion) %>%
  mutate(COLOR = ifelse(is.na(COLOR),
                        "GREY",
                        COLOR)) %>%
  group_by(EcoRegion, VARIABLE, COLOR) %>%
  summarize(VALUE = n(),
            METRIC = "count")

# Take last year of catch data. If catch is not available, use landings.
# Remove stocks without quantified catch or landings
ges_catch_stock <-  sag_complete_smmry %>%
  tidyr::unnest(EcoRegion) %>%
  group_by(StockKeyLabel) %>%
  filter(Year == YearOfLastAssessment - 1,
         EcoRegion %in% ecoregion,
         FisheriesGuild %in% fisheries_guild) %>%
  ungroup() %>%
  mutate(CATCH = ifelse(is.na(catches) & !is.na(landings),
                        landings,
                        catches)) %>%
  filter(!is.na(CATCH)) %>%
  select(StockKeyLabel,
         CATCH) %>%
  distinct(.keep_all = TRUE)

# Split and sum catch by variable and color
ges_table_catch <- ges_catch_stock %>%
  left_join(pie_tbl, by = "StockKeyLabel") %>%
  select(EcoRegion,
         CATCH,
         D3C2 = BMSY,
         D3C1 = FMSY) %>%
  tidyr::gather(VARIABLE, COLOR, -EcoRegion, -CATCH) %>%
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
#' @param grouping_var character string of the desired grouping. Options include: EcoRegion, EcoGuild, or FisheriesGuild
#' @param plotting_var character string of the variable to plot. Options include: StockCode or FisheriesGuild (mean)
#' @param metric character string of the desired metric. Options include: MSY or MEAN (according to grouping_var)
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

clean_stock_trends <- function(active_year = active_year,
                               ecoregion =  c("Greater North Sea Ecoregion","Baltic Sea Ecoregion",
                                              "Bay of Biscay and the Iberian Coast Ecoregion",
                                              "Celtic Seas Ecoregion","Icelandic Waters Ecoregion",
                                              "Norwegian Sea Ecoregion", "Barents Sea Ecoregion",
                                              "Arctic Ocean Ecoregion"),
                               grouping_var = c("EcoGuild", "EcoRegion", "FisheriesGuild")[1],
                               plotting_var = c("StockKeyLabel", "FisheriesGuild")[1],
                               metric = "MSY") {
  
  # if(!grouping_var %in% c("EcoRegion",
  #                         "EcoGuild",
  #                         "FisheriesGuild")) {
  #   stop(paste0("grouping_var: '", grouping_var, "' is not supported. Please try: 'EcoRegion', 'EcoGuild', or 'FisheriesGuild'"))
  # }
  # if(!plotting_var %in% c("StockKeyLabel",
  #                         "FisheriesGuild")) {
  #   stop(paste0("plotting_var: '", plotting_var, "' is not supported. Please try: 'StockKeyLabel' or 'FisheriesGuild'"))
  # }
  # if(plotting_var == "FisheriesGuild" &
  #    grouping_var %in% c("EcoGuild", "FisheriesGuild")) {
  #   stop("plotting_var = 'FisheriesGuild' should only be used with grouping_var = 'EcoRegion'.")
  # }
  # if(!metric %in% c("MSY", "MEAN")) {
  #   stop(paste0("metric: '", metric, "' is not supported. Please try: 'MSY' or 'MEAN'"))
  # }
  # 
  sag_complete_smmry <- sag_complete_summary
  grouping_variable <- rlang::sym(grouping_var)
  plotting_variable <- rlang::sym(plotting_var)
  
  sag_complete_smmry <- sag_complete_smmry %>% filter(grepl(pattern = ecoregion, EcoRegion))
  
  if(ecoregion == "Celtic Seas Ecoregion"){
    sagno<- sag_complete_smmry%>%filter(StockKeyLabel %in% c("aru.27.123a4",
                                                   "bli.27.nea",
                                                   "bll.27.3a47de",
                                                   "cap.27.2a514",
                                                   "her.27.1-24a514a",
                                                   "lin.27.5b",
                                                   "reb.2127.sp",
                                                   "reg.27.561214",
                                                   "rjb.27.3a4",
                                                   "rng.27.1245a8914ab",
                                                   "san.sa.7r",
                                                   "smn-dp"))
    sag_complete_smmry <- anti_join(sag_complete_smmry, sagno, by = "StockKeyLabel")
  }
  
  # sag_complete_smmry$FisheriesGuild[which(sag_complete_smmry$StockKeyLabel == "usk.27.6b")] <- "demersal"
  
  stock_means <- sag_complete_smmry %>%
    tidyr::unnest(EcoRegion) %>%
    mutate(EcoGuild =  paste0(EcoRegion, " - ", FisheriesGuild, " stocks")) %>% 
    group_by(rlang::UQ(grouping_variable),
    # group_by((grouping_variable),
             Year) %>% 
    mutate(FMEAN = mean(F, na.rm = TRUE),
           SSBMEAN = mean(SSB, na.rm = TRUE),
           FMEAN = ifelse(!grepl("F|F(ages 3-6)", fishingPressureDescription),
                          NA,
                          FMEAN),
           SSBMEAN = ifelse(!grepl("SSB", stockSizeDescription),
                            NA,
                            SSBMEAN))
  
  stock_trends <- stock_means %>% 
    mutate(F_FMSY = ifelse(!is.na(FMSY),
                           F / FMSY,
                           NA),
           SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
                                    SSB / MSYBtrigger,
                                    NA),
           F_FMEAN = ifelse(!is.na(FMEAN),
                            F / FMEAN, 
                            NA),
           SSB_SSBMEAN = ifelse(!is.na(SSBMEAN),
                                SSB / SSBMEAN,
                                NA)) %>%
    select(Year,
           StockKeyLabel,
           FisheriesGuild,
           EcoRegion,
           EcoGuild,
           F_FMSY,
           SSB_MSYBtrigger,
           F_FMEAN,
           SSB_SSBMEAN) %>%
    tidyr::gather(METRIC, stockValue, -Year, -StockKeyLabel, -FisheriesGuild, -EcoRegion, -EcoGuild) %>%
    filter(!is.na(Year),
           grepl(metric, METRIC))
  
  stock_trends$EcoRegion <- ecoregion
  
  stock_trends_grp <- stock_trends %>%
    group_by(rlang::UQ(grouping_variable),
             rlang::UQ(plotting_variable), METRIC, Year) %>%
    summarize(plotValue = mean(stockValue, na.rm = TRUE)) %>%
    select(pageGroup = rlang::UQ(grouping_variable),
           lineGroup = rlang::UQ(plotting_variable),
           Year,
           plotGroup = METRIC,
           plotValue) %>%
    filter(!is.na(plotValue))
  
  #the problem of 2 means per year is here:
  
  stock_trends_mean <- stock_trends %>%
    #group_by(FisheriesGuild, METRIC, Year)%>%
    group_by(rlang::UQ(grouping_variable), METRIC, Year) %>%
    #for neafc plots
    #group_by(METRIC, Year)%>%
    summarize(plotValue = mean(stockValue, na.rm = TRUE),
              lineGroup = "MEAN") %>%
    #select(pageGroup = rlang::UQ(grouping_variable),
    #select(pageGroup = FisheriesGuild,
    #for neafc plots
    select(pageGroup = FisheriesGuild,
           lineGroup,
           Year,
           plotGroup = METRIC,
           plotValue) %>%
    filter(!is.na(plotValue))
  
  stock_trends_frmt <- bind_rows(stock_trends_grp,
                                 stock_trends_mean) %>%
    distinct(.keep_all = TRUE)
  
  
  stock_trends_frmt<<- stock_trends_frmt
              # "sag_complete_summary" = dat$sag_complete_summary))
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
stock_catch <- function(active_year = active_year) {
  
  # stock_catch <- sag_ecoregion %>%
   stock_catch <- sag_complete_summary %>%
      # tidyr::unnest(data) %>%
    select(Year,
           YearOfLastAssessment,
           StockKeyLabel,
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
        mutate(discards = catches - landings),
      stock_catch %>%  # Missing landings, but have catches and discards
        filter(!is.na(catches),
               is.na(landings),
               !is.na(discards)) %>%
        mutate(landings = catches - discards)
    )
  
  stock_catch_full <<- stock_catch_full
  # return(stock_catch_full)
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

stock_status <- function(active_year = active_year){
  
  stck_ctch_fll <- stock_catch_full
  
  stock_status_full <-
    full_join(
      stck_ctch_fll %>%
        group_by(StockKeyLabel) %>%
        filter(Year == YearOfLastAssessment - 1) %>%
        mutate(F_FMSY =  ifelse(!is.na(FMSY),
                                F / FMSY,
                                NA)) %>%
        select(StockKeyLabel,
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
        group_by(StockKeyLabel) %>%
        filter(Year == YearOfLastAssessment) %>%
        mutate(SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
                                        SSB / MSYBtrigger,
                                        NA)) %>%
        select(StockKeyLabel,
               FisheriesGuild,
               EcoRegion,
               SSB_MSYBtrigger,
               SSB,
               MSYBtrigger),
      by = c("StockKeyLabel",
             "FisheriesGuild",
             "EcoRegion")
    ) %>%
    mutate(colList = ifelse(is.na(F_FMSY) | is.na(SSB_MSYBtrigger),
                            "GREY",
                            if_else(F_FMSY < 1 & SSB_MSYBtrigger >= 1,
                                    "GREEN",
                                    "RED",
                                    "GREY")))
           # ,
           # FisheriesGuild = ifelse(StockKeyLabel %in% c("whb-comb", "mac-nea",
           #                                              "whb.27.1-91214", "mac.27.nea"),
           #                         "large-scale stocks",
           #                         FisheriesGuild))

  # devtools::use_data(stock_status_full)
  stock_status_full <<- stock_status_full
  # return(stock_status_full)
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
                "ices_catch_official_raw",
                "ices_catch_preliminary_raw")
  
  # data(list = raw_data, envir = environment())
  
  fish_category <- stock_list_raw %>%
    filter(YearOfLastAssessment == 2016, #should I change to 2017?
           !is.na(FisheriesGuild)) %>%
    mutate(X3A_CODE = gsub("-.*$", "", StockKeyLabel),
           X3A_CODE = gsub("\\..*", "", X3A_CODE),
           X3A_CODE = toupper(X3A_CODE),
           FisheriesGuild = tolower(FisheriesGuild)) %>%
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
  #these historical catches definition need decission on conflicts
  historic_bob <- c("VIII a", "VIII b", "VIII c", "VIII d2", "VIII e2",
                    "IX a", "IX b2", "VIII d (not specified)", "IX (not specified)")
  historic_cs <- c("VI a", "VI b2", "VII a", "VII b", "VII c2", "VII f", "VII g", "VII h",
                   "VII j2", "VII k2", "VII (not specified)", "VII b+c (not specified)",
                   "VII c (not specified)", "VII d-k (not specified)", "VII f-k (not specified)",
                   "VII g-k (not specified)", "VII j (not specified)")
  historic_nw <- c( "II a1", "II b1", "I  and  IIa (not specified)",
                    "II (not specified)", "II a2", "II b (not specified)",
                    "II b2", "XIV", "XIVa" )
  #check this!
  historic_br <- c( "II a1", "II b1", "I  and  IIa (not specified)",
                    "II (not specified)", "II a2", "II b (not specified)",
                    "II b2", "XIV", "XIVa" )
  catch_dat_1950 <- ices_catch_historical_raw %>%
    tidyr::gather(YEAR, VALUE, -Country, -Species, -Division) %>%
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
             .$Division %in% historic_bob ~ "Bay of Biscay and the Iberian Coast Ecoregion",
             .$Division %in% historic_cs ~ "Celtic Seas Ecoregion",
             .$Division %in% historic_nw ~ "Norwegian Sea Ecoregion",
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
    tidyr::gather(YEAR, VALUE, -Country, -Species, -Area, -Units) %>%
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
             
             .$Area %in% c("27.8.a", "27.8.b","27.8.c",
                           "27.8.d.2", "27.8.e.2", "27.9.a",
                           "27.9.b.2") ~ "Bay of Biscay and the Iberian Coast Ecoregion",
             .$Area %in% c("27.6.a", "27.6.b.2","27.7.a", "27.7.b", "27.7.c.2",
                           "27.7.f", "27.7.g", "27.7.h","27.7.j.2", "27.7.k.2") ~ "Celtic Seas Ecoregion",
             
             .$Area %in% c("5.a.1", "5.a.2","12.a.4") ~ "Icelandic Waters Ecoregion",
             
             .$Area %in% c("2.a.1", "2.a.2", "2.b.1", "2.b.2", "14.a") ~ "Norwegian Sea Ecoregion",
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
  
  catch_dat_prelim <- ices_catch_preliminary_raw %>%
    # tidyr::gather(ï..Year, -Country, -AphiaID, -Area, -Catch) %>%
    filter(Country != "") %>%
    mutate(YEAR = Year,
           VALUE = AMS.Catch.TLW.,
           Country = countrycode::countrycode(Country,"iso2c", "country.name"),
           Country = ifelse(grepl("Guernsey|Isle of Man|Jersey", Country),
                            "United Kingdom",
                            Country),
           ISO3 = countrycode::countrycode(Country, "country.name", "iso3c", warn = FALSE),
           Country = gsub("(United Kingdom) .*", "\\1", Country),
           Area = tolower(Area))
  
  
  #check why areas names are different!!
  catch_dat_prelim <- catch_dat_prelim%>%
    mutate(ECOREGION = case_when(
      .$Area %in% c("27_3_bc", "27_3_c_22","27_3_d","27_3_d_24","27_3_d_25","27_3_d_26","27_3_d_30",
                    "27_3_d_27","27_3_d_31","27_3_nk", "27_3_b_23", "27_3_d_28_2","27_3_d_32","27_3_d_29") ~ "Baltic Sea Ecoregion",
      .$Area %in% c("27_3_a", "27_4_a","27_4_b", "27_4_c", "27_7_d") ~ "Greater North Sea Ecoregion",
      
      .$Area %in% c("27_8_a", "27_8_b","27_8_c",
                    "27_8_d_2", "27_8_e_2", "27_9_a",
                    "27_9_b_2")~ "Bay of Biscay and the Iberian Coast Ecoregion",
      .$Area %in% c("27_6_a", "27_6_b_2","27_7_a", "27_7_b", "27_7_c_2","27_7.e",
                    "27_7_f", "27_7_g", "27_7_h","27_7_j_2", "27_7_k_2")~"Celtic Seas Ecoregion",
      
      .$Area %in% c("5_a_1", "5_a_2","12_a_4")~"Icelandic Waters Ecoregion",
      
      .$Area %in% c("2_a_1", "2_a_2", "2_b_1", "2_b_2", "14.a")~"Norwegian Sea Ecoregion",
      TRUE ~ "OTHER"))%>%
    filter(ECOREGION != "OTHER") %>%
    left_join(species_list_raw, c("Species.Latin.Name" = "Scientific_name"))
  
  catch_dat_prelim <- catch_dat_prelim%>%
    left_join(fish_category) %>%
    select(YEAR,
           COUNTRY = Country,
           ISO3,
           GUILD = FisheriesGuild,
           ECOREGION,
           SPECIES_NAME = Species.Latin.Name,
           SPECIES_CODE = X3A_CODE,
           COMMON_NAME = English_name,
           VALUE)
  catch_dat_prelim$COMMON_NAME[which(catch_dat_prelim$SPECIES_NAME == "Ammodytes")] <- "Sandeels(=Sandlances) nei"
  catch_dat_prelim$SPECIES_CODE[which(catch_dat_prelim$SPECIES_NAME == "Ammodytes")] <- "SAN"
  
  ices_catch_dat <- catch_dat_2010 %>%
    bind_rows(catch_dat_1950) %>%
    bind_rows(catch_dat_prelim)%>%
    mutate(GUILD = ifelse(is.na(GUILD),
                          "undefined",
                          GUILD)) %>%
    filter(!GUILD %in% c("elasmobranch", "crustacean") |
             ECOREGION != "Baltic Sea")
  
  
  ices_catch_dat$COUNTRY<-gsub("Russian Federation", "\\Russia\\",ices_catch_dat$COUNTRY)
  ices_catch_dat$COUNTRY<-gsub("Russia", "Russian Federation", ices_catch_dat$COUNTRY)
  ices_catch_dat <- ices_catch_dat %>%
    select(YEAR,
           COUNTRY,
           ISO3,
           GUILD ,
           ECOREGION,
           SPECIES_NAME,
           SPECIES_CODE,
           COMMON_NAME,
           VALUE)
  
  
  
  ices_catch_dat <<- ices_catch_dat
  # return(ices_catch_dat)
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


stecf_effort_df <- stecf_effort_raw
stecf_effort_df$country <- gsub("SCO|ENG|GBG|GBJ|IOM|NIR", "GBR", stecf_effort_df$country)
stecf_effort_df$ISO3C <- stecf_effort_df$country
stecf_effort_df <- stecf_effort_df%>%
  mutate(COUNTRY = countrycode::countrycode(ISO3C, "iso3c", "country.name"),
         COUNTRY = ifelse(grepl("United Kingdom", COUNTRY),
                          "United Kingdom",
                          COUNTRY)) %>%
  mutate(YEAR = year,
         EFFORT = as.numeric(nominal_effort)) %>%
  select(YEAR,
         ANNEX = annex,
         AREA = regulated.area,
         COUNTRY,
         GEAR = regulated.gear,
         EFFORT)

stecf_landings_df <- stecf_landings_raw 
stecf_landings_df$country <- gsub("SCO|ENG|GBG|GBJ|IOM|NIR", "GBR", stecf_landings_df$country)
stecf_landings_df$ISO3c <- stecf_landings_df$country
stecf_landings_df <- stecf_landings_df %>%
  mutate(COUNTRY = countrycode::countrycode(ISO3c, "iso3c", "country.name"),
         COUNTRY = ifelse(grepl("United Kingdom", COUNTRY),
                          "United Kingdom",
                          COUNTRY)) %>%
  mutate(YEAR = year,
         LANDINGS = as.numeric(sum_landings),
         LANDINGS = ifelse(COUNTRY == "Germany" &
                             year == 2013 &
                             `vessel length` == "U8M",
                           NA, LANDINGS)) %>%
  select(YEAR,
         COUNTRY,
         ANNEX = annex,
         AREA = as.character("regulated area"),
         GEAR = as.character("regulated gear"),
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

#check if needed and add  the Bob, CS, NWB, etc etc, by annex  

stecf_effort_clean <- gear_dat_clean %>%
  left_join(stecf_effort_df, by = c("ANNEX", "AREA", "GEAR")) %>%
  mutate(YEAR = as.numeric(YEAR))%>%
  select(YEAR ,
         ANNEX,
         ECOREGION,
         AREA,
         GEAR = gear_class,
         COUNTRY,
         EFFORT) %>%
  filter(!COUNTRY %in% c("Finland", "Estonia"))


stecf_landings_clean <- gear_dat_clean %>%
  left_join(stecf_landings_df, by = c("ANNEX", "AREA", "GEAR")) %>%
  mutate(YEAR = as.numeric(YEAR))%>%
  select(YEAR,
         ANNEX,
         ECOREGION,
         AREA,
         GEAR = gear_class,
         COUNTRY,
         LANDINGS) %>%
  group_by(YEAR, ANNEX, ECOREGION, AREA, GEAR, COUNTRY) %>%
  summarize(LANDINGS = sum(LANDINGS, na.rm = TRUE))

stecf_effort_clean <<- stecf_effort_clean
stecf_landings_clean <<- stecf_landings_clean

# return(list("stecf_effort_clean" = stecf_effort_clean,
#              "stecf_landings_clean" = stecf_landings_clean))
 }
