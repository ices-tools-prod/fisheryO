rm(list = ls())
################
# library(DT)
# library(shiny)
# library(ReporteRs)
library(dplyr)
library(jsonlite)
# library(XML)
library(stringr)
# library(RCurl)
library(reshape2)
library(icesSAG)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: FAO codes and ICES stock codes #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# (NOTE: current ICES stock codes do not necessarily match with FAO 3-character
# codes, in the future this look-up table should not be necessary - SL)
speciesID <- read.csv("~/git/ices-dk/fisheryO/inst/extdata/ICESspeciesID_v1.csv",
                      stringsAsFactors = FALSE)
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: Fishery guilds by ICES stock code #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# (NOTE: These guilds should become a part of the RECO database - SL)
fisheryGuild <- read.csv("~/git/ices-dk/fisheryO/inst/extdata/fisheryGuild.csv",
                         stringsAsFactors = FALSE)

speciesGuild <- fisheryGuild %>%
  mutate(speciesID = toupper(gsub( "-.*$", "", Stock.code)),
         Stock.code = tolower(Stock.code)) %>%
  full_join(speciesID, c("speciesID" = "oldCode")) %>%
  select(STOCK.CODE = Stock.code,
         FISHERIES.GUILD = Fisheries.Guild,
         SPECIES.ID = speciesID,
         SPECIES.NAME = speciesName,
         -newCode)


# Get stock list
# url <- "http://admin.ices.dk/StockListServices/odata/StockListDWsOData?$filter=ActiveYear%20eq%202016"
url <- "~/git/ices-dk/fisheryO/inst/extdata/StockListDWsOData.json"
rawsl <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)$value

# Combine Norwegian and Barents Sea Ecoregions
rawsl$NorwegianSeaandBarentsSeaEcoregion[!is.na(rawsl$NorwegianSeaEcoregion) |
                                           !is.na(rawsl$BarentsSeaEcoregion)] <- "x"
# Reorganize from a wide data frame to a long data frame with species guild information
ecoregions <- colnames(rawsl)[grepl("^.+(Ecoregion)$", colnames(rawsl))]

dots <- lapply(c("StockCode", "Description", "DataCategory", "AdviceType", ecoregions),
               as.symbol)

sl <- rawsl %>%
  select_(.dots = dots) %>%
  melt(id.vars = c("StockCode", "Description", "DataCategory", "AdviceType")) %>%
  filter(!is.na(value),
         !variable %in% c("NorwegianSeaEcoregion", "BarentsSeaEcoregion")) %>%
  select(STOCK.CODE = StockCode,
         STOCK.NAME = Description,
         CAT = DataCategory,
         ADVICE.TYPE = AdviceType,
         ECOREGION = variable,
         -value) %>%
  mutate(CAT = floor(as.numeric(CAT)),
         STOCK.CODE = tolower(STOCK.CODE),
         ADVICE.TYPE = ifelse(ADVICE.TYPE == "MSY/PA",
                              "MSY", ADVICE.TYPE),
         ECOREGION = as.character(ECOREGION),
         ECOREGION = recode(ECOREGION, "AzoresEcoregion" = "Azores"),
         ECOREGION = recode(ECOREGION, "BayofBiscayandtheIberianCoastEcoregion" = "Bay of Biscay and the Iberian Coast"),
         ECOREGION = recode(ECOREGION, "BalticSeaEcoregion" = "Baltic Sea"),
         ECOREGION = recode(ECOREGION, "CelticSeasEcoregion" = "Celtic Seas"),
         ECOREGION = recode(ECOREGION, "FaroesEcoregion" = "Faroes"),
         ECOREGION = recode(ECOREGION, "GreenlandSeaEcoregion" = "Greenland Sea"),
         ECOREGION = recode(ECOREGION, "IcelandSeaEcoregion" = "Iceland Sea"),
         ECOREGION = recode(ECOREGION, "GreaterNorthSeaEcoregion" = "Greater North Sea"),
         ECOREGION = recode(ECOREGION, "OceanicNortheastAtlanticEcoregion" = "Oceanic north-east Atlantic"),
         ECOREGION = recode(ECOREGION, "NorwegianSeaandBarentsSeaEcoregion" = "Norwegian Sea and Barents Sea")) %>%
  left_join(speciesGuild, c("STOCK.CODE" = "STOCK.CODE"))

# Format the species names appropriately
slBinomial <- sl %>%
  filter(!grepl(" spp", SPECIES.NAME),
         grepl("[[:space:]]", SPECIES.NAME),
         SPECIES.ID != "CYO") %>%
  mutate(STOCK.NAME = str_replace_all(string = STOCK.NAME,
                                      pattern = SPECIES.NAME,
                                      replacement = paste0("<em>", SPECIES.NAME, "</em>")))
slANG <- sl %>%
  filter(SPECIES.ID == "ANG") %>%
  mutate(STOCK.NAME = str_replace_all(STOCK.NAME,
                                      pattern = "Lophius piscatorius and L. budegassa",
                                      replacement = "<em> Lophius piscatorius </em> and <em>L. budegassa</em>"))
slSpp <- sl %>%
  filter(grepl(" spp", SPECIES.NAME)) %>%
  mutate(STOCK.NAME = str_replace_all(string = STOCK.NAME,
                                      pattern = SPECIES.NAME,
                                      replacement = paste0("<em>",
                                                           as.character(lapply(strsplit(SPECIES.NAME, split = " "), head, n = 1)),
                                                           "</em> spp.")))
slCYO <-  sl %>%
  filter(SPECIES.ID == "CYO") %>%
  mutate(STOCK.NAME = str_replace_all(STOCK.NAME,
                                      pattern = "Centrophorus squamosus",
                                      replacement = paste0("<em>Centrophorus squamosus</em>")),
         STOCK.NAME = str_replace_all(STOCK.NAME,
                                      pattern = SPECIES.NAME,
                                      replacement = paste0("<em>", SPECIES.NAME, "</em>")))

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

slOthers <- sl %>%
  filter(SPECIES.ID != "ANG") %>%
  filter(!grepl("[[:space:]]", SPECIES.NAME))
#rng

slFull <- slBinomial %>%
  bind_rows(slANG, slSpp, slCYO, slOthers) %>%
  select(STOCK.CODE,
         STOCK.NAME,
         FISHERIES.GUILD,
         ECOREGION,
         ADVICE.TYPE,
         CAT)

summaryTbl <- getSummaryTable(2016)

keeperF <- c("F", "F/FMSY", "F in winter rings", "Harvest rate", "Harvest rate/FMSY", "Fishing Pressure")
relativeF <- c("F/FMSY", "Harvest rate/FMSY")

keeperSSB <- c("SSB", "B/BMSY", "SSB & Biomass Age 4+", "UW Tv Index", "Stock Size", "Total biomass/BMSY")
relativeSSB <- c("B/BMSY", "Total biomass/BMSY")

summaryTblClean <- summaryTbl %>%
  filter(Year >= 2013) %>%
  select(Year,
         STOCK.CODE = fishstock,
         F,
         SSB,
         fishingPressureDescription,
         stockSizeDescription) %>%
  mutate(STOCK.CODE = tolower(STOCK.CODE),
         fishingPressureDescription = gsub("Fishing Pressure: " , "", fishingPressureDescription),
         fishingPressureDescription = gsub("Fishing pressure: " , "", fishingPressureDescription),
         fishingPressureDescription = gsub("msy" , "MSY", fishingPressureDescription),

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
           fishingPressureDescription %in% keeperF)

refPts <- getFishStockReferencePoints(2016)
refPts[refPts == ""] <- NA

refPtsClean <- refPts %>%
  select(-key,
         -AssessmentYear,
         -RecruitmentAge,
         -RecruitmentLength,
         -MSYBescapement,
         STOCK.CODE = FishStockName) %>%
  mutate(STOCK.CODE = tolower(STOCK.CODE))

fullSummary <- summaryTblClean %>%
  left_join(refPtsClean, by = c("STOCK.CODE" = "STOCK.CODE")) %>%
  mutate(MSYBtrigger = ifelse(stockSizeDescription %in% relativeSSB,
                              0.5,
                              MSYBtrigger),
         MSYBtrigger= ifelse(!stockSizeDescription %in% keeperSSB,
                             NA,
                             MSYBtrigger),
         FMSY = ifelse(fishingPressureDescription %in% relativeF,
                       1,
                       FMSY),
         FMSY = ifelse(!fishingPressureDescription %in% keeperF,
                       NA,
                       FMSY))
charCols <- c("STOCK.CODE", "fishingPressureDescription",
              "stockSizeDescription",
              "FmsyDescription",
              "BmsyDescription")
fullSummary[!colnames(fullSummary) %in% charCols] <- lapply(fullSummary[!colnames(fullSummary) %in% charCols],
                                                            as.numeric)

fmsySummary <- fullSummary %>%
  filter(Year <= 2015) %>%
  mutate(FMSY = ifelse(F <= FMSY, "GREEN", "RED"),
         Year = paste0("FMSY", Year)) %>%
  dcast(STOCK.CODE ~ Year, value.var = "FMSY")

bmsySummary <- fullSummary %>%
  filter(Year %in% 2013:2016) %>%
  mutate(BMSY = ifelse(SSB >= MSYBtrigger, "GREEN", "RED"),
         Year = paste0("BMSY", Year)) %>%
  dcast(STOCK.CODE ~ Year, value.var = "BMSY")

fpaSummary <- fullSummary %>%
  filter(Year <= 2015) %>%
  mutate(FPA = if_else(F <= FMSY,
                       "GREEN",
                       NA_character_),
         FPA = if_else(F >= FLim, # If F is greater than Flim, FPA is
                       missing = FPA,
                       "RED",
                       ifelse(F <= FMSY, # if F is less than Flim and less than FMSY, FPA is
                              "GREEN",
                              if_else(F <= Fpa , # If F is greater than FMSY but less than Fpa, FPA is
                                      "GREEN",
                                      "ORANGE", # if F is greater than Fpa, FPA is orange
                                      ifelse(F <= FLim, # if Fpa is NA but Flim is available, FPA is
                                             "ORANGE",
                                             NA_character_)))), # If none of this fits the bill, NA.
         Year = paste0("FPA", Year)) %>%
  dcast(STOCK.CODE ~ Year, value.var = "FPA")

bpaSummary <- fullSummary %>%
  filter(Year %in% 2013:2016) %>%
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
  dcast(STOCK.CODE ~ Year, value.var = "BPA")

stockDF <- fmsySummary %>%
  left_join(bmsySummary, by = "STOCK.CODE") %>%
  left_join(fpaSummary, by = "STOCK.CODE") %>%
  left_join(bpaSummary, by = "STOCK.CODE") %>%
  mutate(SBL = ifelse(FPA2015== "GREEN"  &  BPA2016 == "GREEN",
                      "GREEN",
                      ifelse(FPA2015== "RED"  &  BPA2016 == "RED",
                             "RED",
                             NA)))

stockDat <- slFull %>%
  left_join(stockDF, by = "STOCK.CODE") %>%
  mutate(F_2013 = ifelse(ADVICE.TYPE == "MSY",
                         FMSY2013,
                         FPA2013),
         F_2014 = ifelse(ADVICE.TYPE == "MSY",
                         FMSY2014,
                         FPA2014),
         F_2015 = ifelse(ADVICE.TYPE == "MSY",
                         FMSY2015,
                         FPA2015),
         SSB_2014 = ifelse(ADVICE.TYPE == "MSY",
                           BMSY2014,
                           BPA2014),
         SSB_2015 = ifelse(ADVICE.TYPE == "MSY",
                           BMSY2015,
                           BPA2015),
         SSB_2016 = ifelse(ADVICE.TYPE == "MSY",
                           BMSY2016,
                           BPA2016)
  ) %>%
  select(STOCK.CODE,
         STOCK.NAME,
         FISHERIES.GUILD,
         ECOREGION,
         ADVICE.TYPE,
         CAT,
         SBL,
         F_2013, F_2014, F_2015,
         SSB_2014, SSB_2015, SSB_2016)

stockDat[c("SBL", "F_2013", "F_2014", "F_2015",
           "SSB_2014", "SSB_2015", "SSB_2016")][is.na(stockDat[c("SBL", "F_2013", "F_2014", "F_2015",
                                                                 "SSB_2014", "SSB_2015", "SSB_2016")])] <- "GREY"



stockDat[stockDat == "GREEN"] <- "<i class=\"glyphicon glyphicon-ok-sign\" style=\"color:green; font-size:2.2em\"></i>"
stockDat[stockDat == "RED"] <- "<i class=\"glyphicon glyphicon-remove-sign\" style=\"color:red; font-size:2.2em\"></i>"
stockDat[stockDat == "GREY"] <- "<i class=\"glyphicon glyphicon-question-sign\" style=\"color:grey; font-size:2.2em\"></i>"
stockDat[stockDat == "ORANGE"] <- "<i class=\"glyphicon glyphicon-record\" style=\"color:#FAB700; font-size:2.2em\"></i>"

stockDat <- data.frame(lapply(stockDat, factor))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Prepare subsets for R Markdown rendering #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Render dynamic and static stock summary tables
stockPlotEcoregion <- function(ecoregion) {
  stockPlot <- stockDat %>%
    filter(ECOREGION == ecoregion) %>%
    select(-ECOREGION) %>%
    distinct() %>%
    arrange(STOCK.CODE)

  suppressWarnings(
  rmarkdown::render("~/git/ices-dk/fisheryO/vignettes/stockSummaryTable-static.rmd",
                    output_file = paste0("~/git/ices-dk/fisheryO/output/annexA_", ecoregion, "-static.html"),
                    # rmarkdown::html_document(template = NULL),
                    envir = new.env())
  )

  suppressWarnings(
    rmarkdown::render("~/git/ices-dk/fisheryO/vignettes/stockSummaryTable-dynamic.rmd",
                    output_file = paste0("~/git/ices-dk/fisheryO/output/annexA_", ecoregion, "-dynamic.html"),
                    rmarkdown::html_document(template = NULL),
                    envir = new.env())
  )

}

lapply(unique(stockDat$ECOREGION), stockPlotEcoregion)




##############
# Pie graphs #
##############

pieDat <- slFull %>%
  left_join(stockDF, by = "STOCK.CODE") %>%
  select(ECOREGION,
         FISHERIES.GUILD,
         FMSY2015,
         BMSY2016,
         FPA2015,
         BPA2016,
         SBL) %>%
  melt(id.vars = c("ECOREGION", "FISHERIES.GUILD"),
       variable.name = "VARIABLE",
       value.name = "VALUE") %>%
  mutate(VALUE = ifelse(is.na(VALUE),
                        "GREY",
                        VALUE)) %>%
  group_by(ECOREGION, FISHERIES.GUILD, VARIABLE, VALUE) %>%
  summarize(COUNT = n())

pieDat <- pieDat %>%
  tidyr::expand(VALUE = c("GREY", "GREEN", "RED", "ORANGE")) %>%
  left_join(pieDat, by = c("ECOREGION", "FISHERIES.GUILD", "VARIABLE", "VALUE")) %>%
  arrange(ECOREGION, FISHERIES.GUILD, VARIABLE, VALUE)
pieDat$COUNT[is.na(pieDat$COUNT)] <- 0

pieDat <- pieDat %>%
  group_by(ECOREGION, VARIABLE, VALUE) %>%
  mutate(FISHERIES.GUILD = "total",
         COUNT = sum(COUNT)) %>%
  distinct(.keep_all = TRUE) %>%
  bind_rows(pieDat)


stockPieEcoregion <- function(ecoregion) {

  tempDat <- pieDat[pieDat$ECOREGION == ecoregion,]

  makePie <- function(guild, ecoregion) {
    for(i in unique(tempDat$VARIABLE)) {
      colList <- c("GREEN" = "#4daf4a",
                   "GREY" = "#d3d3d3",
                   "ORANGE" = "#ff7f00",
                   "RED" = "#e41a1c")
      rowDat <- tempDat %>%
        group_by(ECOREGION, FISHERIES.GUILD, VARIABLE) %>%
        mutate(fraction = COUNT/ sum(COUNT),
               ymax = cumsum(fraction),
               ymin = c(0, head(ymax, n = -1))) %>%
        arrange(ECOREGION, FISHERIES.GUILD, VARIABLE, fraction) %>%
        filter(ECOREGION == ecoregion,
               FISHERIES.GUILD == guild,
               COUNT != 0) %>%
        filter_(.dots =  ~ VARIABLE == i) %>%
        mutate(pos = cumsum(COUNT) - COUNT/2)

      p1 <- ggplot(data = rowDat) +
        geom_bar(aes(x = "", y = COUNT, fill = VALUE), stat = "identity") +
        geom_text(aes(x = "", y = pos, label = COUNT)) +
        coord_polar(theta = "y") +
        scale_fill_manual(values = colList) +
        theme_bw(base_size = 9) +
        theme(panel.grid=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              legend.position="none") +
        theme(axis.text=element_blank()) +
        theme(axis.ticks=element_blank()) +
        annotate("text", x = 0, y = 0, label = "") +
        labs(title="", x = "", y = "")

      tempName <- path.expand(paste0("~/git/ices-dk/fisheryO/vignettes/TEMP_", i, "-", guild, ".png" ))
      ggsave(filename = tempName,
             plot =  p1,
             width = 2,
             height = 2,
             units ="in",
             dpi = 600,
             scale = 2,
             bg = "transparent")
    }
  }

  lapply(unique(tempDat$FISHERIES.GUILD), makePie, ecoregion = ecoregion)

  suppressWarnings(
    rmarkdown::render("~/git/ices-dk/fisheryO/vignettes/stockStatusSummaryTable-static.rmd",
                      output_file = paste0("~/git/ices-dk/fisheryO/output/table1_", ecoregion, "_static.html"),
                      rmarkdown::html_document(template = NULL),
                      envir = new.env())
  )

  #Now remove the generated
  file.remove(dir(
    "~/git/ices-dk/fisheryO/vignettes/",
    pattern = "^TEMP_(.*\\.png)",
    full.names = TRUE
  ))
  # remove old files you just made
}

# stockPieEcoregion(ecoregion = unique(pieDat$ECOREGION)[3])
lapply(unique(pieDat$ECOREGION), stockPieEcoregion)



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

ecoregion = "Greater North Sea"

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
  )




rmarkdown::render("~/git/ices-dk/FisheryO/vignettes/stockStatusSummaryTable-dynamic.Rmd",
          output_file = "~/git/ices-dk/FisheryO/output/annexA_fullDynamic.html",
          rmarkdown::html_document(template = NULL))


renderFisheryOverview <- function(ecoregionID) {
  # in a single for loop
  #  1. define subgroup
  #  2. render output
  #
  ecoPath <- gsub(" ", "_", ecoregionID)
  ifelse(!dir.exists(file.path(plotDir, ecoPath)), dir.create(file.path(plotDir, ecoPath)), FALSE)
  #
  icesID <- areaID$value[areaID$Ecoregion == ecoregionID &
                           areaID$areaType == "ICESarea"]
  stecfID <- areaID$value[areaID$Ecoregion == ecoregionID &
                            areaID$areaType == "STECFarea"]
  #
  catchDatECO <- catchDat %>%
    Filter(f = function(x)!all(is.na(x))) %>%
    filter(Area %in% icesID) %>%
    melt(id.vars = c("Species", "Area", "Units", "Country"),
         variable.name = "YEAR",
         value.name = "VALUE") %>%
    inner_join(spList, c("Species" = "X3A_CODE")) %>%
    full_join(fisheryGuild, c("Species" = "newCode")) %>%
    mutate(YEAR = as.numeric(gsub("X", "", YEAR)))
  #
  effortDatECO <-
    effortDat %>%
    Filter(f = function(x)!all(is.na(x))) %>%
    filter(reg_area_cod %in% stecfID) %>%
    melt(id.vars = c("annex", "reg_area_cod", "reg_gear_cod", "Specon_calc", "country", "vessel_length"),
         variable.name = "YEAR",
         value.name = "VALUE") %>%
    mutate(YEAR = as.numeric(levels(YEAR))[YEAR])
  #
  stecfCatchDatECO <-
    stecfCatchDat %>%
    filter(reg_area %in% stecfID) %>%
    melt(id.vars = c("annex", "reg_area", "country", "reg_gear", "specon", "species"),
         variable.name = "YEAR",
         value.name = "VALUE") %>%
    mutate(METRIC = as.character(gsub(".*\\s", "", YEAR)),
           YEAR = as.numeric(gsub("\\s.+$", "", YEAR))) %>%
    filter(METRIC == "L")
  #
  guildListECO <- guildList %>%
    filter(ECOREGION == ecoregionID)

  #
  rmarkdown::render(paste0(dataDir, "fisheriesAdvice_template.rmd"),
                    output_dir = file.path(plotDir, ecoPath),
                    output_file = paste0('FisheriesAdvice_', ecoregionID, '.html'),
                    params = list(set_title = as.character(ecoregionID)),
                    envir = new.env())
}


