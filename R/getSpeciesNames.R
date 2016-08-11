library(icesSAG)
library(dplyr)

getSpeciesNames <- function(x) {
  #
  icesList <- getListStocks(year = 0)
  #
  spURL <- "ftp://ftp.fao.org/FI/STAT/DATA/ASFIS_sp.zip"
  tmpFileSp <- tempfile(fileext = ".zip")
  download.file(spURL, destfile = tmpFileSp, mode = "wb", quiet = TRUE)
  spList <- read.delim(unz(tmpFileSp,
                           "ASFIS_sp_Feb_2016.txt"),
                       fill = TRUE,
                       stringsAsFactors = FALSE, header = TRUE)
  spDat <- spList %>%
    select(X3A_CODE, Scientific_name, English_name) %>%
    mutate(Scientific_name =  recode(Scientific_name, "Psetta maxima" = "Scophthalmus maximus"),
           Scientific_name =  recode(Scientific_name, "Aspitrigla cuculus" = "Chelidonichthys cuculus"),
           Scientific_name =  recode(Scientific_name, "Scyliorhinus stellaris" = "Skyliorhinus stellaris"),
           Scientific_name =  recode(Scientific_name, "Raja naevus" = "Leucoraja naevus"),
           Scientific_name =  recode(Scientific_name, "Sebastes marinus" = "Sebastes norvegicus"),
           Scientific_name =  recode(Scientific_name, "Raja batis" = "Dipturus batis"),
           Scientific_name =  recode(Scientific_name, "Raja fullonica" = "Leucoraja fullonica"),
           Scientific_name =  recode(Scientific_name, "Raja circularis" = "Leucoraja circularis"),
           Scientific_name =  recode(Scientific_name, "Raja alba" = "Rostroraja alba"),
           Scientific_name =  recode(Scientific_name, "Raja radiata" = "Amblyraja radiata"))

  icesDat <- icesList %>%
    select(SpeciesName,
           speciesID = FishStockName) %>%
    mutate(SpeciesName = gsub("  ", " ", SpeciesName), # Remove extra spaces
           SpeciesName = gsub("spp.", "spp", SpeciesName), # Remove period to match FAO
           SpeciesName = gsub( "\\s*\\([^\\)]+\\)", "", SpeciesName), # Remove notes on Anglerfish
           SpeciesName = gsub("\\s*\\+.*$", "", SpeciesName), # Remove the + for porguguese dogfish
           SpeciesName = recode(SpeciesName, "Lepidorhombus" = "Lepidorhombus spp"), # Correct a few ICES names
           SpeciesName = recode(SpeciesName, "Beryx" = "Beryx spp"),
           SpeciesName = recode(SpeciesName, "Ammodytes" = "Ammodytes spp"),
           SpeciesName = recode(SpeciesName, "Psetta maxima" = "Scophthalmus maximus"),
           speciesID   = toupper(gsub( "-.*$", "", speciesID))) %>%
    distinct()


  speciesID <- read.csv("~/rCode/Data/ICESspeciesID_v1.csv",
                        stringsAsFactors = FALSE)
  speciesID$speciesName[!speciesID$speciesName %in% tt$SpeciesName]

    tt <- icesDat %>%
    left_join(spDat, by = c("SpeciesName" = "Scientific_name"))
