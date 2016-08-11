rm(list = ls())
#
######################################################################################
# Procedure to translate historical division names into modern ICES area definitions #
######################################################################################
#
library(dplyr)
library(tidyr)
library(reshape2)
#
dataDir = "~/git/ices-dk/FisheriesAdvice/"
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: ICES official catch statistics #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#
historicURL <- "http://ices.dk/marine-data/Documents/CatchStats/HistoricalLandings1950-2010.zip"
tmpFileHistoric <- tempfile(fileext = ".zip")
download.file(historicURL, destfile = tmpFileHistoric, mode = "wb", quiet = TRUE)
historicList <- read.csv(unz(tmpFileHistoric,
                             "ICES_1950-2010.csv"),
                         stringsAsFactors = FALSE, header = TRUE, fill = TRUE,
                         na.strings = c("...", "-", "ns", "."))
#
areaList <- data.frame(historicName = as.character(unique(historicList$Division)),
                       stringsAsFactors = FALSE)
#
areaRedo <- areaList %>%
  # First step is to clean up unnecessary text and to split out multiple divisions "and" and subdivisions "+"
  filter(!historicName %in% "ICES Area (not specified)") %>%
  mutate(value = as.character(historicName)) %>% # Create new variable so we can check back with the original
  mutate(value = gsub("*\\([^\\)]+\\)", "", value)) %>% # Remove the unnecessary parentheticals, "not specified" is implied
  mutate(value = gsub("Baltic\\s", "", value)) %>% # Remove the unnecessary "Baltic"
  mutate(value = gsub("B", "b", value)) %>% # Fix capitalization for V b1B
  mutate(value = gsub("A", "a", value)) %>% # Fix capitalization for V b1A
  mutate(value = gsub("28-", "28.", value)) %>% # Change the dash to a period
  mutate(newValue = strsplit(value, "and", perl = TRUE)) %>% # Split on "and"
  tidyr::unnest(newValue) %>% # and create new rows
  mutate(Subarea = gsub("([^A-Z])", "", newValue, perl = TRUE)) %>% # Create Subarea column
  mutate(newValue = strsplit(as.character(newValue), "\\+", perl = TRUE)) %>%  # Split on "+"
  tidyr::unnest(newValue) %>%
  #
  # Second step is to split out groups of Subdivisions "-" and expand to all included Subdivisions
  mutate(rangeValue = ifelse(grepl("\\-", newValue) == TRUE,
                             newValue,
                             NA)) %>%
  mutate(rangeValue = gsub("([A-Z]+\\s)", "", rangeValue, perl = TRUE)) %>%
  mutate(from = ifelse(!is.na(rangeValue) == TRUE,
                       gsub("(\\-).*", "", rangeValue, perl = TRUE),
                       NA),
         to = ifelse(!is.na(rangeValue) == TRUE,
                     gsub(".*(\\-)", "", rangeValue, perl = TRUE),
                     NA)) %>%
  mutate(from = gsub("[[:space:]]", "", from, perl = TRUE),
         to = gsub("[[:space:]]", "", to, perl = TRUE))
#
# Simple function that takes two characters and expands them to the sequence of characters
# Warning! Checks are not yet included and this function should be verified. -SL-
rangeFunction <- function(from, to) {
  if(any(c(is.na(from), is.na(to)) == TRUE)) {
    return(NA)
  } else {
    from <- as.character(tolower(from))
    to <- as.character(tolower(to))
    rawFrom <- as.numeric(charToRaw(from))
    rawTo <- as.numeric(charToRaw(to))
    rawSeq <- as.raw(seq(rawFrom, rawTo))
    charSeq <- rawToChar(rawSeq)
    return(charSeq)
  }
}
# Add the sequence of characters to "rangeDivision" as a string
for(i in 1:nrow(areaRedo)) {
  areaRedo[i, "rangeDivision"] <- rangeFunction(areaRedo[i, "from"], areaRedo[i, "to"])
}
# The final step is to expand the string into new rows, convert Subarea and re-aggregate into the modern ICES area definitions
areaRedo <- areaRedo %>%
  mutate(rangeDivision = strsplit(rangeDivision, "", fixed = TRUE)) %>%
  tidyr::unnest(rangeDivision) %>%
  mutate(Subarea =  as.numeric(as.roman(Subarea)), # Convert Subarea to "Arabic" numerals
         rangeDivision = ifelse(is.na(rangeDivision),
                                gsub("([A-Z])", "", newValue, perl = TRUE), # Remove the subarea roman numerals from the string
                                rangeDivision)) %>%
  mutate(rangeDivision = gsub("([[:alpha:]])([[:digit:]])", "\\1.\\2", rangeDivision), # add periods before
         rangeDivision = gsub("([[:digit:]])([[:alpha:]])", "\\1.\\2", rangeDivision), # and after digits to facilitate separation
         rangeDivision = gsub("^\\s+|\\s+$", "", rangeDivision), # remove leading and trailing spaces
         rangeDivision = gsub("\\s", "\\1.", rangeDivision, perl = TRUE), # replace spaces with periods
         ICES_Area = paste0("27.", Subarea, ".", rangeDivision), # add the NAFO area "27"
         ICES_Area = gsub("^\\.+|\\.+$", "", ICES_Area)) %>% # remove leading and trailing periods
  select(ICES_Area, # Get rid of the columns used to hold the cleaning steps
         historicName) %>%
  tidyr::separate_(col = "ICES_Area", # Separate the into columns with the periods
                   sep = "\\.",
                   into = c("Area", "Subarea", "Division", "Subdivision", "Unit"),
                   extra = "merge",
                   remove = FALSE,
                   fill = "right") %>%
  mutate(Subarea = paste(Area, Subarea, sep = "."), # Make the proper ICES area definitions
         Division = ifelse(is.na(Division), NA, paste(Subarea, Division, sep = ".")),
         Subdivision = ifelse(is.na(Subdivision), NA, paste(Division, Subdivision, sep = ".")),
         Unit = ifelse(is.na(Unit), NA, paste(Subdivision, Unit, sep = "."))) %>%
  filter(ICES_Area != "27.7.i") # There is no 27.7.i... so it is removed.
#
write.csv(areaRedo, paste0(dataDir, "/historicAreas_with_ICESAreas.csv"), row.names = FALSE)
#
