rm(list = ls())
################
library(tidyverse)
library(jsonlite)
library(ggrepel)
library(stringr)
library(ggrepel)
library(readxl)
library(stringr)
library(ggrepel)
library(ggthemes)

options(scipen = 5)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: STECF effort and catch statistics #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# (NOTE: Will be replaced with RDB data- SL)
# Accessed 16 March 2017 from: https://visualise.jrc.ec.europa.eu/t/dcf/views/effort_public_report/effort?:embed=y&:loadOrderID=0&:display_count=no&:display_spinner=no&:showVizHome=no
effortDatRaw <- readRDS("~/git/ices-dk/fisheryO/inst/extdata/effort_data.RDS")
#
stecfCatchDatRaw <- readRDS("~/git/ices-dk/fisheryO/inst/extdata/landings_data.RDS")


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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# CODE TO CREATE FILE: fisheryO/R/FisheriesAdvice/countryNames.R #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
countryNames <- read.csv("~/git/ices-dk/fisheryO/inst/extdata/countryNames.csv",
                         stringsAsFactors = FALSE)


countryNames <- countryNames %>%
  filter(VARIABLE %in% c("stNames"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Data cleaning and aggregating #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#
effortDat <- effortDatRaw %>%
  full_join(countryNames, c("country" = "VALUE")) %>% # merge with ISO country names
  mutate(YEAR = as.numeric(year),
         EFFORT = as.numeric(nominal_effort)) %>%
  select(YEAR,
         ANNEX = annex,
         AREA = regulated.area,
         COUNTRY,
         GEAR = regulated.gear,
         EFFORT)

stecfCatchDat <- stecfCatchDatRaw %>%
  left_join(spList, c("species" = "X3A_CODE")) %>% # merge with FAO species names
  left_join(countryNames, c("country" = "VALUE")) %>% # merge with ISO country names
  mutate(LANDINGS = as.numeric(sum_landings)) %>%
  select(YEAR = year,
         COUNTRY,
         ANNEX = annex,
         AREA = regulated.area,
         GEAR = regulated.gear,
         COMMON_NAME = English_name,
         LANDINGS)

ELSE <- TRUE
gear_dat <- full_join(
  effortDat %>%
    select(ANNEX, AREA, GEAR),
  stecfCatchDat %>%
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
             ELSE ~ "other"
           )
    ),
  gear_dat %>%
    filter(ANNEX == "IIA",
           AREA %in% c("3A", "3B1", "3B2", "3B3")) %>%
    mutate(ECOREGION = "Greater North Sea Ecoregion",
           gear_class = case_when(
             grepl("BEAM|BT1|BT2", .$GEAR) ~ "Beam trawl",
             grepl("DREDGE", .$GEAR) ~ "Dredge",
             grepl("GN1|GT1|LL1", .$GEAR) ~ "Static/Gill net/LL",
             grepl("TR1|TR2|TR3|OTTER|DEM_SEINE", .$GEAR) ~ "Otter trawl/seine",
             grepl("PEL_SEINE|PEL_TRAWL", .$GEAR) ~ "Pelagic trawl/seine",
             grepl("POTS", .$GEAR) ~ "Pots",
             grepl("NONE", .$GEAR) ~ "other",
             is.na(.$GEAR) ~ "other",
             ELSE ~ "other"
           )
    ),
  gear_dat %>%
    filter(ANNEX == "CEL1") %>%
    mutate(ECOREGION = "Celtic Seas Ecoregion",
           gear_class = case_when(
             grepl("BEAM|BT1|BT2", .$GEAR) ~ "Beam trawl",
             grepl("DREDGE", .$GEAR) ~ "Dredge",
             grepl("GN1|GT1|LL1", .$GEAR) ~ "Static/Gill net/LL",
             grepl("TR1|TR2|TR3|OTTER|DEM_SEINE", .$GEAR) ~ "Otter trawl/seine",
             grepl("PEL_SEINE|PEL_TRAWL", .$GEAR) ~ "Pelagic trawl/seine",
             grepl("POTS", .$GEAR) ~ "Pots",
             grepl("NONE", .$GEAR) ~ "other",
             is.na(.$GEAR) ~ "other",
             ELSE ~ "other"
           )
    )
)

effortDatClean <- gear_dat_clean %>%
  left_join(effortDat, by = c("ANNEX", "AREA", "GEAR")) %>%
  select(YEAR,
         ANNEX,
         ECOREGION,
         AREA,
         GEAR = gear_class,
         COUNTRY,
         EFFORT)
colnames(stecfCatchDatClean)
stecfCatchDatClean <- gear_dat_clean %>%
  left_join(stecfCatchDat, by = c("ANNEX", "AREA", "GEAR")) %>%
  select(YEAR,
         ANNEX,
         ECOREGION,
         AREA,
         GEAR = gear_class,
         COUNTRY,
         LANDINGS) %>%
  group_by(YEAR, ANNEX, ECOREGION, AREA, GEAR, COUNTRY) %>%
  summarize(LANDINGS = sum(LANDINGS, na.rm = TRUE))

# td <- stecfCatchDatClean %>%
#   filter(ECOREGION == "Greater North Sea Ecoregion") %>%
#   group_by(YEAR, gear_class) %>%
#   summarize(total = sum(VALUE, na.rm = TRUE)) %>%
#   filter(!is.na(YEAR)) %>%
#   spread(YEAR, total)

# allDat <- effortDatClean
# allDat <- stecfCatchDatClean
# IA = unique(allDat$ECOREGION)[1]
# type = c("GEAR", "COUNTRY", "COMMON_NAME")[1]
# line_count = 9
# fig_name = "Figure2"
# text.size = 9
# plot_type = c("line", "area")[1]


stecfEffortAreaPlot <- function(IA = unique(allDat$ECOREGION)[1],
                                type = c("GEAR", "COUNTRY")[1],
                                line_count = 4,
                                plot_type = c("line", "area")[1],
                                fig_name = "figure3",
                                fig.save = FALSE,
                                fig.plot = TRUE,
                                fig.width = 174,
                                fig.height = 68,
                                text.size = 9,
                                ...) {

  if(line_count >= 10) warning("Color scales are hard to see beyond this point... try plotting fewer categories.")
  if(line_count == 14) stop("Color palette only has 14 colors... sorry.")

  iaDat <- allDat[allDat$ECOREGION == IA,]

  value_type <- grep("EFFORT|LANDINGS", colnames(iaDat), value = TRUE)
  iaDat <- rename_(iaDat, .dots = setNames(c(type, value_type),
                                           c("type_var", "VALUE")))

  catchPlot <- iaDat %>%
    group_by(ANNEX, type_var) %>%
    dplyr::summarize(typeTotal = sum(VALUE, na.rm = TRUE)) %>% # Overall catch to order plot
    arrange(ANNEX, -typeTotal) %>%
    filter(typeTotal >= 1) %>%
    group_by(ANNEX) %>%
    dplyr::mutate(RANK = min_rank(desc(typeTotal))) %>%
    inner_join(iaDat, c("ANNEX", "type_var")) %>%
    ungroup() %>%
    dplyr::mutate(type_var = replace(type_var, RANK > line_count, "other"),
                  ANNEX = str_wrap(ANNEX, width = 26)) %>%
    group_by(ANNEX, type_var, YEAR) %>%
    dplyr::summarize(typeTotal = sum(VALUE, na.rm = TRUE)) %>%
    filter(!is.na(YEAR))


  if(nchar(max(catchPlot$typeTotal, na.rm = TRUE)) >= 6) {
    catchPlot$typeTotal <- catchPlot$typeTotal / 1000

    if(value_type == "EFFORT"){
      catchLabel <- "Nominal effort (1000 kW days at sea)"
    }
    if(value_type == "LANDINGS"){
      catchLabel <- "Landings (thousand tonnes)"
    }
  } else {
    if(value_type == "EFFORT"){
      catchLabel <- "Nominal effort (kW days at sea)"
    }
    if(value_type == "LANDINGS"){
      catchLabel <- "Landings (tonnes)"
    }
  }
  #
  catchPlot <- rbind(catchPlot[!catchPlot$type_var == "other",],
                     catchPlot[catchPlot$type_var == "other",])

  my_caption <- sprintf("STECF 16-20, Accessed %s/%s. doi:10.2788/502445",
          lubridate::year(Sys.time()),
          lubridate::month(Sys.time(), label = TRUE, abbr = FALSE))

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

  catchPlot$ANNEX <- as.factor(catchPlot$ANNEX)

    pl <- ggplot(catchPlot, aes(x = YEAR, y = typeTotal)) +
      scale_fill_manual(values = myColors) +
      scale_color_manual(values = myColors) +
      scale_x_continuous(breaks = seq(min(catchPlot$YEAR, na.rm = TRUE),
                                      max(catchPlot$YEAR, na.rm = TRUE), by = 2)) +
      geom_segment(aes(x = -Inf, xend = 2015, y = -Inf, yend = -Inf), color = "grey50")+
      geom_segment(aes(y = -Inf, yend = Inf, x = -Inf, xend = -Inf), color = "grey50")+
      expand_limits(x = c(min(catchPlot$YEAR, na.rm = TRUE), 2022)) + # So that we have enough room along x-axis for labels.
      labs(title = "", x = "", y = catchLabel,
      caption = my_caption) +
      theme_bw(base_size = text.size) +
      theme(legend.position = 'none',
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            strip.background = element_blank(),
            plot.caption = element_text(size = 6),
            axis.line = element_blank())

  if(plot_type == "area"){
    cumPlot <- catchPlot %>%
      filter(YEAR == 2015) %>%
      ungroup() %>%
      arrange(desc(type_var)) %>%
      mutate(cs = cumsum(as.numeric(typeTotal)), # cumulative sum
             mp = lag(cs, order_by = desc(type_var)), # midpoint
             mp = ifelse(is.na(mp), 1, mp)) %>% # midpoint
      ungroup() %>%
      arrange(desc(type_var)) %>%
      mutate(td = rowMeans(.[,c("cs", "mp")]))#

    pl <- pl + geom_area(aes(fill = type_var, color = type_var),
                         alpha = .9,
                         position = "stack")

    pl <- pl + geom_label_repel(
      data = cumPlot,
      aes(y = td,
          fill = type_var,
          label = type_var),
      nudge_x = 3,
      label.size = 0.2,
      segment.size = 0.25,
      size = 2,
      color = 'white',
      force = 3,
      segment.color = 'grey60')
  }

  if(plot_type == "line"){
    pl <- pl + geom_line(aes(color = type_var),
                         alpha = .9, position = "identity")
    pl <- pl + geom_label_repel(data = catchPlot %>% filter(YEAR == 2015),
                                aes(label = type_var,
                                    fill = type_var),
                                nudge_x = 3,
                                label.size = 0.2,
                                segment.size = 0.25,
                                size = 2,
                                color = 'white',
                                force = 3,
                                segment.color = 'grey60')
  }


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


# allDat <- effortDatEco

lapply(grep("Greater|Celtic|Baltic",
            unique(allDat$ECOREGION), value = TRUE),
       stecfEffortAreaPlot, type = "GEAR", line_count = 9, plot_type = "line", fig_name = "figure8", fig.save = TRUE, fig.plot = FALSE)

lapply(grep("Greater|Celtic|Baltic",
            unique(allDat$ECOREGION), value = TRUE),
       stecfEffortAreaPlot, type = "COUNTRY", line_count = 9, plot_type = "line", fig_name = "figure3", fig.save = TRUE, fig.plot = FALSE)

allDat <- stecfCatchDatClean
lapply(grep("Greater|Celtic|Baltic",
            unique(allDat$ECOREGION), value = TRUE),
       stecfEffortAreaPlot, type = "GEAR", line_count = 9, plot_type = "line", fig_name = "figure6",
       fig.save = TRUE, fig.plot = FALSE)

