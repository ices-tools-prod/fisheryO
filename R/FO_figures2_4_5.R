#ICES catches time series:
library(dplyr)
#1.LOAD DATA
#all these different raw data could be saved in the local environment. 
# Only preliminary catches might change often

  #1.1 SID: stock_list_raw ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


stock_list_raw <- jsonlite::fromJSON("http://sd.ices.dk/services/odata4/StockListDWs4",
                                     simplifyDataFrame = TRUE)$value

stock_list_raw <- unique(stock_list_raw)


##1.2  FAO: species_list_raw ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
spURL <- "http://www.fao.org/fishery/static/ASFIS/ASFIS_sp.zip"
tmpFileSp <- tempfile(fileext = ".zip")
download.file(spURL, destfile = tmpFileSp, mode = "wb", quiet = TRUE)
FAO_file <- grep(".*txt", unzip(tmpFileSp,list = TRUE)$Name, value = TRUE)
species_list_raw <- read.delim(unz(tmpFileSp, FAO_file),
                               fill = TRUE,
                               stringsAsFactors = FALSE,
                               header = TRUE,
                               na.strings = "")


####1.3 ICES ices_historical_raw (1950-2010) #### 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
historicURL <- "http://ices.dk/marine-data/Documents/CatchStats/HistoricalLandings1950-2010.zip"
tmpFileHistoric <- tempfile(fileext = ".zip")
download.file(historicURL, destfile = tmpFileHistoric, mode = "wb", quiet = TRUE)
ices_catch_historical_raw <- read.csv(unz(tmpFileHistoric, "ICES_1950-2010.csv"),
                                      stringsAsFactors = FALSE,
                                      header = TRUE,
                                      fill = TRUE,
                                      na.strings = c("...", "-", "ns", "."))


#### 1.4 ICES ices_catch_official_raw (2006-2016) ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
catchURL <- "http://ices.dk/marine-data/Documents/CatchStats/OfficialNominalCatches.zip"
tmpFileCatch <- tempfile(fileext = ".zip")
download.file(catchURL, destfile = tmpFileCatch, mode = "wb", quiet = TRUE)
ices_catch_official_raw <- read.csv(unz(tmpFileCatch,
                                        grep("ICESCatchDataset.*.csv", unzip(tmpFileCatch,
                                                                             list = TRUE)$Name,
                                             value = TRUE)),
                                    stringsAsFactors = FALSE,
                                    header = TRUE,
                                    fill = TRUE)
# remove columns with all NA
ices_catch_official_raw <- Filter(function(x)!all(is.na(x)), ices_catch_official_raw)


#### 1.5 ICES ices_catch_preliminary201X (2017) ###
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

#August 2018: for the time being this URL has to be checked everytime, as it might change and
#new names are always different.

prelimcatchURL<- "http://data.ices.dk/rec12/download/zflrl4fx3q3fcthrzf4ikd3g314A0.csv "
tmpFilePrelim2017Catch <- tempfile(fileext = ".csv")
download.file(prelimcatchURL, destfile = tmpFilePrelim2017Catch, mode = "wb", quiet = TRUE)
ices_catch_preliminary2017_raw <- read.csv(tmpFilePrelim2017Catch,
                                           stringsAsFactors = FALSE,
                                           header = TRUE,
                                           fill = TRUE)

#To save this in the local environment:

devtools::use_data(ices_catch_official_raw, compress='xz', overwrite = TRUE)
devtools::use_data(ices_catch_historical_raw, compress='xz', overwrite = TRUE)
devtools::use_data(ices_catch_preliminary2017_raw, compress='xz', overwrite = TRUE)


###################################

#2.CLEAN DATA
#pretty sure this code can be simplified, but not important, it works as is
fish_category <- stock_list_raw %>%
  filter(YearOfLastAssessment == 2016, 
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
#these historical catches definition need decission on conflicts for the new FOs
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
           
           .$Area %in% c("8.a", "8.b","8.c",
                         "8.d.2", "8.e.2", "9.a",
                         "9.b.2") ~ "Bay of Biscay and the Iberian Coast Ecoregion",
           .$Area %in% c("6.a", "6.b.2","7.a", "7.b", "7.c.2",
                         "7.f", "7.g", "7.h","7.j.2", "7.k.2") ~ "Celtic Seas Ecoregion",
           
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

catch_dat_2017 <- ices_catch_preliminary2017_raw %>%
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

catch_dat_2017 <- catch_dat_2017%>%
  mutate(ECOREGION = case_when(
    .$Area %in% c("27_3_bc", "27_3_c_22","27_3_d","27_3_d_24","27_3_d_25","27_3_d_26","27_3_d_30",
                  "27_3_d_27","27_3_d_31","27_3_nk", "27_3_b_23", "27_3_d_28_2","27_3_d_32","27_3_d_29") ~ "Baltic Sea Ecoregion",
    .$Area %in% c("27_3_a", "27_4_a","27_4_b", "27_7_d") ~ "Greater North Sea Ecoregion",
    
    .$Area %in% c("8_a", "8_b","8_c",
                  "8_d_2", "8_e_2", "9_a",
                  "9_b_2")~ "Bay of Biscay and the Iberian Coast Ecoregion",
    .$Area %in% c("6_a", "6_b_2","7_a", "7_b", "7_c_2",
                  "7_f", "7_g", "7_h","7_j_2", "7_k_2")~"Celtic Seas Ecoregion",
    
    .$Area %in% c("5_a_1", "5_a_2","12_a_4")~"Icelandic Waters Ecoregion",
    
    .$Area %in% c("2_a_1", "2_a_2", "2_b_1", "2_b_2", "14.a")~"Norwegian Sea Ecoregion",
    TRUE ~ "OTHER"))%>%
  filter(ECOREGION != "OTHER") %>%
  left_join(species_list_raw, c("Species.Latin.Name" = "Scientific_name"))

catch_dat_2017 <- catch_dat_2017%>%
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

ices_catch_dat <- catch_dat_2010 %>%
  bind_rows(catch_dat_1950) %>%
  # bind_rows(catch_dat_2016)%>%
  bind_rows(catch_dat_2017)%>%
  mutate(GUILD = ifelse(is.na(GUILD),
                        "undefined",
                        GUILD)) %>%
  filter(!GUILD %in% c("elasmobranch", "crustacean") |
           ECOREGION != "Baltic Sea")

#Little trick to change all Russia to Russian Federation
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

#####################
#3.PLOTS
#################
#Set ecoregion everytime in the beggining of the work session

ecoregion = c("Greater North Sea Ecoregion","Baltic Sea Ecoregion",
              "Bay of Biscay and the Iberian Coast Ecoregion",
              "Celtic Seas Ecoregion","Icelandic Waters Ecoregion",
              "Norwegian Sea Ecoregion", "Barents Sea Ecoregion",
              "Arctic Ocean Ecoregion")

ecoregion = ecoregion [1]

# ecoregion = c("Greater North Sea Ecoregion","Baltic Sea Ecoregion",
#               "Bay of Biscay and the Iberian Coast Ecoregion",
#               "Celtic Seas Ecoregion","Icelandic Waters Ecoregion",
#               "Norwegian Sea Ecoregion", "Barents Sea Ecoregion",
#               "Arctic Ocean Ecoregion")

#Set month and year of accession to the data

cap_month <-"August"
cap_year <-"2018"

fig.width = 174
fig.height = 68
text.size = 9
output_path <- "~/"
cap_lab <-  labs(x = "",
                 y = "Landings (thousand tonnes)",
                 caption = sprintf("Historical Nominal Catches 1950-2010, \nOfficial Nominal Catches 2006-2016, \nPreliminary Catches 2017. Accessed %s/%s. ICES, Copenhagen.",
                                   cap_year,
                                   cap_month))
#####################
#3.1 FIGURE 2:
#############
type = "COUNTRY"
line_count = 9
file_name <- paste(gsub("\\s", "_",ecoregion),"_figure2", sep = "")
iaDat <- ices_catch_dat %>%
  filter(ECOREGION == ecoregion) %>%
  rename_(.dots = setNames(type, "type_var"))
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
catchPlot <- rbind(catchPlot[!catchPlot$type_var == "other",],
                   catchPlot[catchPlot$type_var == "other",])

colList <- ggthemes::tableau_color_pal('tableau20')(line_count + 1)

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

pl <- ggplot(ungroup(catchPlot), aes(x = YEAR, y = typeTotal)) +
  scale_fill_manual(values = myColors) +
  scale_color_manual(values = myColors) +
  scale_x_continuous(breaks = seq(min(catchPlot$YEAR),
                                  max(catchPlot$YEAR), by = 10)) +
  geom_segment(aes(x = -Inf, xend = max(catchPlot$YEAR), y = -Inf, yend = -Inf), color = "grey50")+
  geom_segment(aes(y = -Inf, yend = Inf, x = -Inf, xend = -Inf), color = "grey50")+
  expand_limits(x = c(min(catchPlot$YEAR), max(catchPlot$YEAR) + 20)) + # So that we have enough room along x-axis for labels.
  cap_lab +
  theme_bw(base_size = text.size) +
  theme(legend.position = 'none',
        plot.caption = element_text(size = 6),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank())
cumPlot <- catchPlot %>%
  filter(YEAR == max(YEAR, na.rm = TRUE)) %>%
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
pl <- pl + ggrepel::geom_label_repel(data = cumPlot,
                                     aes(y = td,
                                         fill = type_var,
                                         label = type_var),
                                     nudge_x = 10,
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
#####################
#3.2 FIGURE 4:
################
type = "GUILD"
line_count = 5
file_name <- paste(gsub("\\s", "_",ecoregion),"_figure4", sep = "")
iaDat <- ices_catch_dat %>%
  filter(ECOREGION == ecoregion) %>%
  rename_(.dots = setNames(type, "type_var"))
if(type == "GUILD" &
   ecoregion == "Baltic Sea Ecoregion"){
  # Get rid of crustacean and elasmobranchs in the Baltic... to appease ADGFO
  iaDat <- iaDat %>%
    filter(!type_var %in% c("elasmobranch",
                            "crustacean"))
}

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
catchPlot <- rbind(catchPlot[!catchPlot$type_var == "other",],
                   catchPlot[catchPlot$type_var == "other",])

colList <- ggthemes::tableau_color_pal('tableau20')(line_count + 1)

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

pl <- ggplot(ungroup(catchPlot), aes(x = YEAR, y = typeTotal)) +
  scale_fill_manual(values = myColors) +
  scale_color_manual(values = myColors) +
  scale_x_continuous(breaks = seq(min(catchPlot$YEAR),
                                  max(catchPlot$YEAR), by = 10)) +
  geom_segment(aes(x = -Inf, xend = max(catchPlot$YEAR), y = -Inf, yend = -Inf), color = "grey50")+
  geom_segment(aes(y = -Inf, yend = Inf, x = -Inf, xend = -Inf), color = "grey50")+
  expand_limits(x = c(min(catchPlot$YEAR), max(catchPlot$YEAR) + 20)) + # So that we have enough room along x-axis for labels.
  cap_lab +
  theme_bw(base_size = text.size) +
  theme(legend.position = 'none',
        plot.caption = element_text(size = 6),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank())
pl <- pl + geom_line(aes(color = type_var),
                     alpha = .8, position = "identity")
pl <- pl + ggrepel::geom_label_repel(data = catchPlot%>% filter(YEAR == max(YEAR, na.rm = TRUE)),
                                     aes(label = type_var,
                                         fill = type_var),
                                     nudge_x = 10,
                                     label.size = 0.2,
                                     segment.size = 0.25,
                                     size = 2,
                                     color = 'white',
                                     force = 3,
                                     segment.color = 'grey60')
pl
write.csv(x = catchPlot, file = paste0(output_path, file_name, ".csv"), row.names = FALSE)
ggsave(filename = paste0(output_path, file_name, ".png"),
       # paste0("~/git/ices-dk/fisheryO/output/", fig_name, "_", IA, ".png"),
       plot = pl,
       width = fig.width,
       height = fig.height,
       units = "mm",
       dpi = 300)


#####################
#3.3 FIGURE 5:
############
type = "COMMON_NAME"
line_count = 6
file_name <- paste(gsub("\\s", "_",ecoregion),"_figure5", sep = "")
iaDat <- ices_catch_dat %>%
  filter(ECOREGION == ecoregion) %>%
  rename_(.dots = setNames(type, "type_var"))
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

catchPlot <- rbind(catchPlot[!catchPlot$type_var == "other",],
                   catchPlot[catchPlot$type_var == "other",])

colList <- ggthemes::tableau_color_pal('tableau20')(line_count + 1)

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

pl <- ggplot(ungroup(catchPlot), aes(x = YEAR, y = typeTotal)) +
  scale_fill_manual(values = myColors) +
  scale_color_manual(values = myColors) +
  scale_x_continuous(breaks = seq(min(catchPlot$YEAR),
                                  max(catchPlot$YEAR), by = 10)) +
  geom_segment(aes(x = -Inf, xend = max(catchPlot$YEAR), y = -Inf, yend = -Inf), color = "grey50")+
  geom_segment(aes(y = -Inf, yend = Inf, x = -Inf, xend = -Inf), color = "grey50")+
  expand_limits(x = c(min(catchPlot$YEAR), max(catchPlot$YEAR) + 20)) + # So that we have enough room along x-axis for labels.
  cap_lab +
  theme_bw(base_size = text.size) +
  theme(legend.position = 'none',
        plot.caption = element_text(size = 6),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank())
pl <- pl + geom_line(aes(color = type_var),
                     alpha = .8, position = "identity")
pl <- pl + ggrepel::geom_label_repel(data = catchPlot%>% filter(YEAR == max(YEAR, na.rm = TRUE)),
                                     aes(label = type_var,
                                         fill = type_var),
                                     nudge_x = 10,
                                     label.size = 0.2,
                                     segment.size = 0.25,
                                     size = 2,
                                     color = 'white',
                                     force = 3,
                                     segment.color = 'grey60')
write.csv(x = catchPlot, file = paste0(output_path, file_name, ".csv"), row.names = FALSE)
ggsave(filename = paste0(output_path, file_name, ".png"),
       # paste0("~/git/ices-dk/fisheryO/output/", fig_name, "_", IA, ".png"),
       plot = pl,
       width = fig.width,
       height = fig.height,
       units = "mm",
       dpi = 300)
