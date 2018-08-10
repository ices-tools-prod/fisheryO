#STECF effort and catch data

#1.LOAD DATA
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#### DATA SOURCE: STECF Effort and Catch tables ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

#These two files were locally saved, to be downloaded every year.
#the second line keeps it in the data folder

stecf_effort_raw <- readRDS("data-raw/STECF_effort_data.rds")
devtools::use_data(stecf_effort_raw, overwrite = TRUE)

stecf_landings_raw <- readRDS("data-raw/STECF_landings_data.rds")
devtools::use_data(stecf_landings_raw, overwrite = TRUE)

###############
#2. CLEAN DATA
#################
#stecf_effort_clean and stecf_landings_clean can also be saved and go directly 
#to the plotting section

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
###############
#3. PLOT
#################

#Set ecoregion everytime in the beggining of the work session

ecoregion = c("Greater North Sea Ecoregion","Baltic Sea Ecoregion",
              "Bay of Biscay and the Iberian Coast Ecoregion",
              "Celtic Seas Ecoregion","Icelandic Waters Ecoregion",
              "Norwegian Sea Ecoregion", "Barents Sea Ecoregion",
              "Arctic Ocean Ecoregion")

ecoregion = ecoregion [2]

# ecoregion = c("Greater North Sea Ecoregion","Baltic Sea Ecoregion",
#               "Bay of Biscay and the Iberian Coast Ecoregion",
#               "Celtic Seas Ecoregion","Icelandic Waters Ecoregion",
#               "Norwegian Sea Ecoregion", "Barents Sea Ecoregion",
#               "Arctic Ocean Ecoregion")

#Set month and year of accession to the data, also chech STECF Report numbers 
#in my_caption

cap_month <-"August"
cap_year <-"2018"

fig.width = 174
fig.height = 68
text.size = 9
output_path <- "~/"
my_caption <- sprintf("STECF 17-09, Accessed %s/%s.",
                      cap_year,
                      cap_month)
output_path <- "~/"

##############
#3.1 Figure 3
##############
type = "COUNTRY"
line_count = 6
colList <- ggthemes::tableau_color_pal('tableau20')(line_count + 1)
file_name <- paste(gsub("\\s", "_",ecoregion),"_figure3", sep = "")

allDat <- stecf_effort_clean %>%
  filter(ECOREGION == ecoregion) %>%
  rename_(.dots = setNames(c(type, "EFFORT"),
                           c("type_var", "VALUE")))

catchPlot <- allDat %>%
  group_by(ANNEX, type_var) %>%
  summarize(typeTotal = sum(VALUE, na.rm = TRUE)) %>% # Overall catch to order plot
  arrange(ANNEX, -typeTotal) %>%
  filter(typeTotal >= 1) %>%
  group_by(ANNEX) %>%
  mutate(RANK = min_rank(desc(typeTotal))) %>%
  inner_join(allDat, c("ANNEX", "type_var")) %>%
  ungroup() %>%
  mutate(type_var = replace(type_var, RANK > line_count, "other"),
         ANNEX = stringr::str_wrap(ANNEX, width = 26)) %>%
  group_by(ANNEX, type_var, YEAR) %>%
  summarize(typeTotal = sum(VALUE, na.rm = TRUE)) %>%
  filter(!is.na(YEAR))

if(nchar(max(catchPlot$typeTotal, na.rm = TRUE)) >= 6) {
  catchPlot$typeTotal <- catchPlot$typeTotal / 1000
  catchLabel <- "Nominal effort (1000 kW days at sea)"
  } else {
    catchLabel <- "Nominal effort (kW days at sea)"
  }


#from now on, all is the same for the 3 plots, but I repeated it 
#to make the code more linear

catchPlot <- rbind(catchPlot[!catchPlot$type_var == "other",],
                   catchPlot[catchPlot$type_var == "other",])
cap_lab <- labs(title = "", x = "", y = catchLabel,
                caption = my_caption)
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

pl <- ggplot(ungroup(catchPlot), aes(x = YEAR, y = typeTotal)) +
  scale_fill_manual(values = myColors) +
  scale_color_manual(values = myColors) +
  scale_x_continuous(breaks = seq(min(catchPlot$YEAR, na.rm = TRUE),
                                  max(catchPlot$YEAR, na.rm = TRUE), by = 2)) +
  geom_segment(aes(x = -Inf, xend = 2016, y = -Inf, yend = -Inf), color = "grey50")+
  geom_segment(aes(y = -Inf, yend = Inf, x = -Inf, xend = -Inf), color = "grey50")+
  expand_limits(x = c(min(catchPlot$YEAR, na.rm = TRUE), 2022)) + # So that we have enough room along x-axis for labels.
  cap_lab +
  theme_bw(base_size = text.size) +
  theme(legend.position = 'none',
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(),
        plot.caption = element_text(size = 6),
        axis.line = element_blank())
pl <- pl + geom_line(aes(color = type_var),
                     alpha = .9, position = "identity")
pl <- pl + ggrepel::geom_label_repel(data = catchPlot %>% filter(YEAR == max(YEAR, na.rm = TRUE)),
                                     aes(label = type_var,
                                         fill = type_var),
                                     nudge_x = 3,
                                     label.size = 0.2,
                                     segment.size = 0.25,
                                     size = 2,
                                     color = 'white',
                                     force = 3,
                                     segment.color = 'grey60')
pl
write.csv(x = catchPlot, file = paste0(output_path, file_name, ".csv"), row.names = FALSE)
ggsave(filename = paste0(output_path, file_name, ".png"),
       plot = pl,
       width = fig.width,
       height = fig.height,
       units = "mm",
       dpi = 300)

##############
#3.2 Figure 6
##############
type = "GEAR"
line_count = 5
colList <- ggthemes::tableau_color_pal('tableau20')(line_count + 1)
file_name <- paste(gsub("\\s", "_",ecoregion),"_figure6", sep = "")
allDat <- stecf_landings_clean %>%
  filter(ECOREGION == ecoregion) %>%
  rename_(.dots = setNames(c(type, "LANDINGS"),
                           c("type_var", "VALUE")))
catchPlot <- allDat %>%
  group_by(ANNEX, type_var) %>%
  summarize(typeTotal = sum(VALUE, na.rm = TRUE)) %>% # Overall catch to order plot
  arrange(ANNEX, -typeTotal) %>%
  filter(typeTotal >= 1) %>%
  group_by(ANNEX) %>%
  mutate(RANK = min_rank(desc(typeTotal))) %>%
  inner_join(allDat, c("ANNEX", "type_var")) %>%
  ungroup() %>%
  mutate(type_var = replace(type_var, RANK > line_count, "other"),
         ANNEX = stringr::str_wrap(ANNEX, width = 26)) %>%
  group_by(ANNEX, type_var, YEAR) %>%
  summarize(typeTotal = sum(VALUE, na.rm = TRUE)) %>%
  filter(!is.na(YEAR))


if(nchar(max(catchPlot$typeTotal, na.rm = TRUE)) >= 6) {
  catchPlot$typeTotal <- catchPlot$typeTotal / 1000
    catchLabel <- "Landings (thousand tonnes)"
} else {
    catchLabel <- "Landings (tonnes)"
  }

catchPlot <- rbind(catchPlot[!catchPlot$type_var == "other",],
                   catchPlot[catchPlot$type_var == "other",])
cap_lab <- labs(title = "", x = "", y = catchLabel,
                caption = my_caption)
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

pl <- ggplot(ungroup(catchPlot), aes(x = YEAR, y = typeTotal)) +
  scale_fill_manual(values = myColors) +
  scale_color_manual(values = myColors) +
  scale_x_continuous(breaks = seq(min(catchPlot$YEAR, na.rm = TRUE),
                                  max(catchPlot$YEAR, na.rm = TRUE), by = 2)) +
  geom_segment(aes(x = -Inf, xend = 2016, y = -Inf, yend = -Inf), color = "grey50")+
  geom_segment(aes(y = -Inf, yend = Inf, x = -Inf, xend = -Inf), color = "grey50")+
  expand_limits(x = c(min(catchPlot$YEAR, na.rm = TRUE), 2022)) + # So that we have enough room along x-axis for labels.
  cap_lab +
  theme_bw(base_size = text.size) +
  theme(legend.position = 'none',
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(),
        plot.caption = element_text(size = 6),
        axis.line = element_blank())
pl <- pl + geom_line(aes(color = type_var),
                     alpha = .9, position = "identity")
pl <- pl + ggrepel::geom_label_repel(data = catchPlot %>% filter(YEAR == max(YEAR, na.rm = TRUE)),
                                     aes(label = type_var,
                                         fill = type_var),
                                     nudge_x = 3,
                                     label.size = 0.2,
                                     segment.size = 0.25,
                                     size = 2,
                                     color = 'white',
                                     force = 3,
                                     segment.color = 'grey60')
pl
write.csv(x = catchPlot, file = paste0(output_path, file_name, ".csv"), row.names = FALSE)
ggsave(filename = paste0(output_path, file_name, ".png"),
       plot = pl,
       width = fig.width,
       height = fig.height,
       units = "mm",
       dpi = 300)
##############
#3.3 Figure 8
##############
type = "GEAR"
line_count = 6
colList <- ggthemes::tableau_color_pal('tableau20')(line_count + 1)
file_name <- paste(gsub("\\s", "_",ecoregion),"_figure8", sep = "")
allDat <- stecf_effort_clean %>%
  filter(ECOREGION == ecoregion) %>%
  rename_(.dots = setNames(c(type, "EFFORT"),
                           c("type_var", "VALUE")))
catchPlot <- allDat %>%
  group_by(ANNEX, type_var) %>%
  summarize(typeTotal = sum(VALUE, na.rm = TRUE)) %>% # Overall catch to order plot
  arrange(ANNEX, -typeTotal) %>%
  filter(typeTotal >= 1) %>%
  group_by(ANNEX) %>%
  mutate(RANK = min_rank(desc(typeTotal))) %>%
  inner_join(allDat, c("ANNEX", "type_var")) %>%
  ungroup() %>%
  mutate(type_var = replace(type_var, RANK > line_count, "other"),
         ANNEX = stringr::str_wrap(ANNEX, width = 26)) %>%
  group_by(ANNEX, type_var, YEAR) %>%
  summarize(typeTotal = sum(VALUE, na.rm = TRUE)) %>%
  filter(!is.na(YEAR))


if(nchar(max(catchPlot$typeTotal, na.rm = TRUE)) >= 6) {
  catchPlot$typeTotal <- catchPlot$typeTotal / 1000
  catchLabel <- "Nominal effort (1000 kW days at sea)"
  }else {
    catchLabel <- "Nominal effort (kW days at sea)"
  }
catchPlot <- rbind(catchPlot[!catchPlot$type_var == "other",],
                   catchPlot[catchPlot$type_var == "other",])
cap_lab <- labs(title = "", x = "", y = catchLabel,
                caption = my_caption)
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

pl <- ggplot(ungroup(catchPlot), aes(x = YEAR, y = typeTotal)) +
  scale_fill_manual(values = myColors) +
  scale_color_manual(values = myColors) +
  scale_x_continuous(breaks = seq(min(catchPlot$YEAR, na.rm = TRUE),
                                  max(catchPlot$YEAR, na.rm = TRUE), by = 2)) +
  geom_segment(aes(x = -Inf, xend = 2016, y = -Inf, yend = -Inf), color = "grey50")+
  geom_segment(aes(y = -Inf, yend = Inf, x = -Inf, xend = -Inf), color = "grey50")+
  expand_limits(x = c(min(catchPlot$YEAR, na.rm = TRUE), 2022)) + # So that we have enough room along x-axis for labels.
  cap_lab +
  theme_bw(base_size = text.size) +
  theme(legend.position = 'none',
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(),
        plot.caption = element_text(size = 6),
        axis.line = element_blank())
pl <- pl + geom_line(aes(color = type_var),
                     alpha = .9, position = "identity")
pl <- pl + ggrepel::geom_label_repel(data = catchPlot %>% filter(YEAR == max(YEAR, na.rm = TRUE)),
                                     aes(label = type_var,
                                         fill = type_var),
                                     nudge_x = 3,
                                     label.size = 0.2,
                                     segment.size = 0.25,
                                     size = 2,
                                     color = 'white',
                                     force = 3,
                                     segment.color = 'grey60')
pl
write.csv(x = catchPlot, file = paste0(output_path, file_name, ".csv"), row.names = FALSE)
ggsave(filename = paste0(output_path, file_name, ".png"),
       plot = pl,
       width = fig.width,
       height = fig.height,
       units = "mm",
       dpi = 300)
