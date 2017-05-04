rm(list = ls())
################
library(tidyverse)
library(jsonlite)
library(ggrepel)
library(stringr)
library(ggrepel)
library(readxl)
library(stringr)
# devtools::install_github("slowkow/ggrepel")
library(ggrepel)
library(ggthemes)
library(reshape2)

options(scipen = 5)
#
baseLoc <- system.file(package = "fisheryO")
extPath <- file.path(baseLoc, "extdata")
#
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: ICES official catch statistics #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
historicURL <- "http://ices.dk/marine-data/Documents/CatchStats/HistoricalLandings1950-2010.zip"
tmpFileHistoric <- tempfile(fileext = ".zip")
download.file(historicURL, destfile = tmpFileHistoric, mode = "wb", quiet = TRUE)
catchDat1950 <- read.csv(unz(tmpFileHistoric,
                             "ICES_1950-2010.csv"),
                         stringsAsFactors = FALSE, header = TRUE, fill = TRUE,
                         na.strings = c("...", "-", "ns", "."))
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: ICES official catch statistics (2006-2014) #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
catchURL <- "http://ices.dk/marine-data/Documents/CatchStats/OfficialNominalCatches.zip"
tmpFileCatch <- tempfile(fileext = ".zip")
download.file(catchURL, destfile = tmpFileCatch, mode = "wb", quiet = TRUE)
catchDat2010 <- read.csv(unz(tmpFileCatch,
                             "ICESCatchDataset2006-2014.csv"),
                         stringsAsFactors = FALSE, header = TRUE, fill = TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: FAO species names and labels #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# spURL <- "ftp://ftp.fao.org/FI/STAT/DATA/ASFIS_sp.zip"
# tmpFileSp <- tempfile(fileext = ".zip")
# download.file(spURL, destfile = tmpFileSp, mode = "wb", quiet = TRUE)

spList <- read.delim("~/git/ices-dk/fisheryO/inst/extdata/ASFIS_sp_Feb_2016.txt",
                     fill = TRUE,
                     stringsAsFactors = FALSE, header = TRUE)
#
spList <- spList %>%
  select(X3A_CODE,
         Scientific_name,
         English_name) %>%
  bind_rows(data.frame(X3A_CODE = "OTH",
                       Scientific_name = "OTHER",
                       English_name = "OTHER"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE:STECF effort and ICES catch databases #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

#(NOTE: this file was created manually - with stock list database this should not be necessary - SL)
areaDat <- read.csv("~/git/ices-dk/fisheryO/inst/extdata/areaList.csv",
                    stringsAsFactors = FALSE)

areaID <- areaDat %>%
  filter(areaType %in% c("historicArea", "ICESarea"),
         FO2016 == TRUE) %>%
  select(-ICESarea)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# CODE TO CREATE FILE: fisheryO/R/FisheriesAdvice/historicAreas.R #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
historicID <- read.csv("~/git/ices-dk/fisheryO/inst/extdata/historicAreas_with_ICESAreas.csv",
                       stringsAsFactors = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# CODE TO CREATE FILE: fisheryO/R/FisheriesAdvice/countryNames.R #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
countryNames <- read.csv("~/git/ices-dk/fisheryO/inst/extdata/countryNames.csv",
                         stringsAsFactors = FALSE)

countryNames <- countryNames %>%
  filter(VARIABLE %in% c("historicNames", "catchNames"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: FAO codes and ICES stock codes #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# (NOTE: current ICES stock codes do not necessarily match with FAO 3-character
# codes, in the future this look-up table should not be necessary - SL)
speciesID <- read.csv("~/git/ices-dk/fisheryO/inst/extdata/ICESspeciesID_v1.csv",
                      stringsAsFactors = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: Fishery guilds by ICES stock code #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# (NOTE: These guilds should become a part of the RECO database - SL)
fisheryGuild <- read.csv("~/git/ices-dk/fisheryO/inst/extdata/fishGuild.csv",
                         stringsAsFactors = FALSE)

catchAreaID <- areaID %>%
  filter(areaType == "ICESarea") %>%
  mutate(value = tolower(value),
         value = gsub("_nk", "_NK", value))


# ~~~ Clean up Fishery Guild information ~~~ #
fisheryGuild <- fisheryGuild %>%
  # mutate(speciesID = toupper(gsub( "-.*$", "", Stock.code))) %>%
  # inner_join(speciesID, c("speciesID" = "oldCode")) %>%
  select(GUILD,
         SPECIES_CODE) %>%
  distinct()

catchDat2010Clean <- catchDat2010 %>%
  Filter(f = function(x)!all(is.na(x))) %>% # Get rid of extra columns of NA
  select(-Units,
         ICES_Area = Area) %>%
  melt(id.vars = c("Species", "ICES_Area", "Country"), # Turn into a long data frame
       variable.name = "YEAR",
       value.name = "VALUE") %>%
  mutate(YEAR = as.numeric(gsub("X", "", YEAR)),
         VALUE = as.numeric(VALUE)) %>%
  left_join(spList, c("Species" = "X3A_CODE")) %>% # merge with FAO species names
  left_join(countryNames, c("Country" = "VALUE")) %>% # merge with ISO country names
  full_join(catchAreaID, c("ICES_Area" = "value")) %>% # Merge to add ecoregions (only areas defined in catchAreaID!)
  left_join(fisheryGuild, c("Species" = "SPECIES_CODE")) %>% # merge with guild
  filter(ICES_Area %in% catchAreaID$value) %>%
  select(YEAR,
         ECOREGION = Ecoregion,
         COUNTRY,
         ISO3,
         GUILD = GUILD,
         SPECIES_NAME = Scientific_name,
         COMMON_NAME = English_name,
         SPECIES_CODE = Species,
         ICES_Area,
         VALUE)


catchDat1950Clean <- catchDat1950 %>%
  melt(id.vars = c("Country", "Species", "Division"),
       variable.name = "YEAR",
       value.name = "VALUE") %>%
  rename(historicName = Division) %>%
  mutate(YEAR = as.numeric(gsub("X", "", YEAR))) %>% # Clean up YEAR and make VALUE numeric
  filter(YEAR <= 2005) %>%
  left_join(y = spList, c("Species" = "English_name")) %>% # Merge to add FAO species information
  left_join(y = spList, c("Species" = "Scientific_name",   # Merge to add FAO species information
                          "X3A_CODE")) %>%
  left_join(y = historicID, c("historicName" = "historicName")) %>% # Merge to add information on ICES Areas
  left_join(y = areaID, c("historicName" = "value")) %>% # Merge to add ecoregions
  left_join(y = countryNames, c("Country" = "VALUE")) %>% # Merge to add consistent country codes
  left_join(fisheryGuild, c("X3A_CODE" = "SPECIES_CODE")) %>% # merge with guild
  select(YEAR,
         ECOREGION = Ecoregion,# Grab necessary columns and rename accordingly
         COUNTRY,
         ISO3,
         GUILD,
         SPECIES_NAME = Scientific_name,
         COMMON_NAME = Species,
         SPECIES_CODE = X3A_CODE,
         ICES_Area, Area,
         Subarea, Division,
         Subdivision, Unit,
         VALUE) %>%
  mutate(VALUE = ifelse(VALUE == "<0.5",
                        as.numeric(0),
                        VALUE),
         VALUE = ifelse(!is.na(VALUE),
                        as.numeric(VALUE),
                        NA),
         # Correct the historic species names with the FAO species names
         SPECIES_NAME = recode(SPECIES_NAME, "Northern bluefin tuna" = "Atlantic bluefin tuna" ),
         SPECIES_NAME = recode(SPECIES_NAME, "Cuttlefish,bobtail squids nei" ="Cuttlefish,bobtail squids nei"),
         SPECIES_NAME = recode(SPECIES_NAME, "Razor clams nei" = "Razor clams, knife clams nei"),
         SPECIES_NAME = recode(SPECIES_NAME, "Thickback soles" = "Thickback soles nei"),
         SPECIES_NAME = recode(SPECIES_NAME, "Atlantic blue marlin" = "Blue marlin"),
         SPECIES_NAME = recode(SPECIES_NAME, "Canary drum (=Baardman)" = "Canary drum(=Baardman)"),
         SPECIES_NAME = recode(SPECIES_NAME, "Chub mackerel" = "Atlantic chub mackerel"),
         SPECIES_NAME = recode(SPECIES_NAME, "Eagle rays" = "Eagle rays nei"),
         SPECIES_NAME = recode(SPECIES_NAME, "Squids nei" = "Common squids nei"),
         SPECIES_NAME = recode(SPECIES_NAME, "St. Pauls fingerfin" = "St. Paul's fingerfin"),
         SPECIES_NAME = recode(SPECIES_NAME, "Knife-nosed chimaeras" = "Knife-nosed chimaeras nei"),
         SPECIES_NAME = recode(SPECIES_NAME, "Hairtails, cutlassfishes nei" = "Hairtails, scabbardfishes nei"),
         SPECIES_NAME = recode(SPECIES_NAME, "Houndsharks,smoothhounds nei" = "Houndsharks, smoothhounds nei"),
         SPECIES_NAME = recode(SPECIES_NAME, "Horse mussels" = "Horse mussels nei"),
         SPECIES_NAME = recode(SPECIES_NAME, "Wels(=Som)catfish" = "Wels(=Som) catfish"),
         SPECIES_NAME = recode(SPECIES_NAME, "Goose barnacles" = "Goose barnacles nei"),
         SPECIES_NAME = recode(SPECIES_NAME, "Mediterranean rainbow wrasse" = "Rainbow wrasse"),
         SPECIES_NAME = recode(SPECIES_NAME, "Morays" = "Morays nei"),
         SPECIES_NAME = recode(SPECIES_NAME, "Sockeye(=Red)salmon" = "Sockeye(=Red) salmon"),
         SPECIES_NAME = recode(SPECIES_NAME, "Swimcrabs nei" = "Callinectes swimcrabs nei"),
         SPECIES_NAME = recode(SPECIES_NAME, "Pink(=Humpback)salmon" = "Pink(=Humpback) salmon"),
         SPECIES_NAME = recode(SPECIES_NAME, "Striped mullet" = "Red mullet")) %>%
  select(-Area, -Subarea, -Division, -Subdivision, -Unit) %>%
  filter(ECOREGION != "ALL")

allDat <- catchDat2010Clean %>%
  bind_rows(catchDat1950Clean) %>%
  mutate(GUILD = ifelse(is.na(GUILD), "undefined", GUILD)) %>%
  filter(!GUILD %in% c("elasmobranch", "crustacean") |
           ECOREGION != "Baltic Sea")


icesAreaPlot <- function(IA = unique(allDat$ECOREGION)[1],
                         type = c("COMMON_NAME", "COUNTRY", "GUILD")[1],
                         line_count = 4,
                         # year_start = 1990,
                         plot_type = c("line", "area")[1],
                         fig_name = "figure2",
                         fig.save = FALSE,
                         fig.plot = TRUE,
                         fig.width = 174,
                         fig.height = 68,
                         text.size = 9,
                         ...) {

  if(line_count >= 10) warning("Color scales are hard to see beyond this point... try plotting fewer categories.")
  if(line_count == 14) stop("Color palette only has 14 colors... sorry.")

  iaDat <- allDat[allDat$ECOREGION == IA,]

  iaDat <- rename_(iaDat, .dots = setNames(type, "type_var"))

  catchPlot <- iaDat %>%
    group_by(type_var) %>%
    dplyr::summarize(typeTotal = sum(VALUE, na.rm = TRUE)) %>% # Overall catch to order plot
    arrange(-typeTotal) %>%
    filter(typeTotal >= 1) %>%
    dplyr::mutate(RANK = min_rank(desc(typeTotal))) %>%
    inner_join(iaDat, by = "type_var") %>%
    dplyr::mutate(type_var = replace(type_var, RANK > line_count, "other")) %>%
    group_by(type_var, YEAR) %>%
    dplyr::summarize(typeTotal = sum(VALUE, na.rm = TRUE) / 1000) %>%
    filter(!is.na(YEAR))
  #

  if(type == "COMMON_NAME") {
    # Clean up some of the FAO names... to appease ADGFO
    catchPlot <- catchPlot %>%
      ungroup() %>%
      mutate(type_var = gsub("Atlantic ", "", type_var),
             type_var = gsub("European ", "", type_var),
             type_var = gsub("Sandeels.*", "sandeel", type_var),
             type_var = gsub("Finfishes nei", "undefined finfish", type_var),
             type_var = gsub("Blue whiting.*", "blue whiting", type_var),
             type_var = gsub("Saithe.*", "saithe", type_var),
             type_var = ifelse(grepl("Norway", type_var),
                               type_var,
                               tolower(type_var))

      )
  }

  # if()
  my_caption <- sprintf("Historical Nominal Catches 1950-2010, \nOfficial Nominal Catches 2006-2014. Accessed %s/%s. ICES, Copenhagen.",
          lubridate::year(Sys.time()),
          lubridate::month(Sys.time(), label = TRUE, abbr = FALSE))

  catchPlot <- rbind(catchPlot[!catchPlot$type_var == "other",],
                     catchPlot[catchPlot$type_var == "other",])



  colList <- tableau_color_pal('tableau20')(line_count + 1)

  catch_order <- catchPlot %>%
    group_by(type_var) %>%
    summarize(total = sum(typeTotal, na.rm = TRUE)) %>%
    arrange(-total) %>%
    ungroup() %>%
    mutate(type_var = factor(type_var, type_var))

  catchPlot$type_var <- factor(catchPlot$type_var,
                               levels = catch_order$type_var[order(catch_order$total)])

  myColors <- colList[1:length(unique(catchPlot$type_var))]
  names(myColors) <- levels(catchPlot$type_var)
  myColors["other"] <- "#7F7F7F"


  pl <- ggplot(catchPlot, aes(x = YEAR, y = typeTotal)) +
    scale_fill_manual(values = myColors) +
    scale_color_manual(values = myColors) +
    scale_x_continuous(breaks = seq(min(catchPlot$YEAR),
                                    max(catchPlot$YEAR), by = 10)) +
    geom_segment(aes(x = -Inf, xend = 2014, y = -Inf, yend = -Inf), color = "grey50")+
    geom_segment(aes(y = -Inf, yend = Inf, x = -Inf, xend = -Inf), color = "grey50")+
    expand_limits(x = c(min(catchPlot$YEAR), 2035)) + # So that we have enough room along x-axis for labels.
    labs(x = "",
         y = "Landings (thousand tonnes)",
         caption = my_caption) +
    theme_bw(base_size = text.size) +
    theme(legend.position = 'none',
          plot.caption = element_text(size = 6),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          axis.line = element_blank())

  if(plot_type == "area") {

    cumPlot <- catchPlot %>%
      filter(YEAR == 2014) %>%
      ungroup() %>%
      arrange(desc(type_var)) %>%
      mutate(cs = cumsum(as.numeric(typeTotal)), # cumulative sum
             mp = lag(cs, order_by = desc(type_var)), # midpoint
             mp = ifelse(is.na(mp), 1, mp)) %>% # midpoint
      ungroup() %>%
      arrange(desc(type_var)) %>%
      mutate(td = rowMeans(.[,c("cs", "mp")]))#

    pl <- pl + geom_area(aes(fill = type_var, color = type_var),
                         alpha = .8,
                         position = "stack")
    pl <- pl + geom_label_repel(
      data = cumPlot,
      aes(y = td,
          fill = type_var,
          label = type_var),
      # direction = "y",
      nudge_x = 10,
      label.size = 0.2,
      segment.size = 0.25,
      size = 2,
      color = 'white',
      force = 3,
      segment.color = 'grey60')
  }

  if(plot_type == "line") {
    pl <- pl + geom_line(aes(color = type_var),
                         alpha = .8, position = "identity")
    pl <- pl + geom_label_repel(data = catchPlot %>% filter(YEAR == 2014),
                                aes(label = type_var,
                                    fill = type_var),
                                nudge_x = 10,
                                label.size = 0.2,
                                segment.size = 0.25,
                                size = 2,
                                color = 'white',
                                force = 3,
                                segment.color = 'grey60')
  }
  # return(pl)

  #
  if(fig.save == TRUE) {

    ggsave(filename = paste0("~/git/ices-dk/fisheryO/output/", fig_name, "_", IA, ".png"),
           plot = pl,
           width = fig.width,
           height = fig.height,
           units = "mm",
           dpi = 300)

  }
  if(fig.plot == TRUE) {
    return(pl)
  }
}

lapply(grep("Greater|Celtic|Baltic",
            unique(allDat$ECOREGION), value = TRUE),
       icesAreaPlot, type = "COUNTRY", line_count = 9,
       plot_type = "area", fig_name = "figure2", fig.save = TRUE, fig.plot = FALSE)


lapply(grep("Greater|Celtic|Baltic",
            unique(allDat$ECOREGION), value = TRUE)[3],
       icesAreaPlot, type = "GUILD", line_count = 9,
       plot_type = "line", fig_name = "figure4", fig.save = TRUE, fig.plot = FALSE)

lapply(grep("Greater|Celtic|Baltic",
            unique(allDat$ECOREGION), value = TRUE)[2],
       icesAreaPlot, type = "COMMON_NAME", line_count = 10, plot_type = "line",
       fig_name = "figure5", fig.save = TRUE, fig.plot = FALSE)


# remove elasmobranch from figure 4

