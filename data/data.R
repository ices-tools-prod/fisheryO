#' ICES Stock database
#'
#' Data describing ICES Stocks. Accessed 9 May 2017
#'
#' @format A data frame with 1241 rows and 31 variables:
#' \describe{
#'   \item{StockDatabaseID}{Add text}
#'   \item{StockKey}{Add text}
#'   \item{StockKeyLabel}{Add text}
#'   \item{StockKeyDescription}{Add text}
#'   \item{PreviousStockKey}{Add text}
#'   \item{PreviousStockKeyLabel}{Add text}
#'   \item{ActiveYear}{Add text}
#'   \item{SpeciesScientificName}{Add text}
#'   \item{SpeciesCommonName}{Add text}
#'   \item{EcoRegion}{Add text}
#'   \item{ExpertGroup}{Add text}
#'   \item{ExpertGroupDescription}{Add text}
#'   \item{AdviceDraftingGroup}{Add text}
#'   \item{AdviceDraftingGroupDescription}{Add text}
#'   \item{DataCategory}{Add text}
#'   \item{YearOfLastAssessment}{Add text}
#'   \item{AssessmentFrequency}{Add text}
#'   \item{YearOfNextAssessment}{Add text}
#'   \item{AssessmentType}{Add text}
#'   \item{AdviceReleaseDate}{Add text}
#'   \item{AdviceCategory}{Add text}
#'   \item{AdviceType}{Add text}
#'   \item{UseOfDiscardsInAdvice}{Add text}
#'   \item{PABufferApplied}{Add text}
#'   \item{TrophicGuild}{Add text}
#'   \item{FisheriesGuild}{Add text}
#'   \item{SizeGuild}{Add text}
#'   \item{Published}{Add text}
#'   \item{GeneratedOn}{Add text}
#'   \item{SectionNumber}{Add text}
#'   \item{AssessmentKey}{Add text}
#' }
#' @source \url{https://sd.ices.dk/}
"stock_list_raw"

#' ICES Stock Assessment Graphs database - summary information from assessment output
#'
#' Data from published ICES advice from 2014-2017. “ICES Stock Assessment Database, 2017/May. ICES, Copenhagen”
#'
#' @format A data frame with 15770 rows and 23 variables:
#' \describe{
#'	\item{Year}{Add text}
#'	\item{recruitment}{Add text}
#'	\item{high_recruitment}{Add text}
#'	\item{low_recruitment}{Add text}
#'	\item{low_SSB}{Add text}
#'	\item{SSB}{Add text}
#'	\item{high_SSB}{Add text}
#'	\item{catches}{Add text}
#'	\item{landings}{Add text}
#'	\item{discards}{Add text}
#'	\item{low_F}{Add text}
#'	\item{F}{Add text}
#'	\item{high_F}{Add text}
#'	\item{StockPublishNote}{Add text}
#'	\item{Fage}{Add text}
#'	\item{fishstock}{Add text}
#'	\item{recruitment_age}{Add text}
#'	\item{AssessmentYear}{Add text}
#'	\item{units}{Add text}
#'	\item{stockSizeDescription}{Add text}
#'	\item{stockSizeUnits}{Add text}
#'	\item{fishingPressureDescription}{Add text}
#'	\item{fishingPressureUnits}{Add text}
#' }
#' @source \url{https://standardgraphs.ices.dk/}
"sag_summary_raw"

#' ICES Stock Assessment Graphs database - reference points
#'
#' Data from published ICES advice from 2014-2017. “ICES Stock Assessment Database, 2017/May. ICES, Copenhagen”
#'
#' @format A data frame with 436 rows and 15 variables:
#' \describe{
#'	\item{AssessmentKey}{Add text}
#'	\item{StockKeyLabel}{Add text}
#'	\item{StockDatabaseID}{Add text}
#'	\item{StockKey}{Add text}
#'	\item{AssessmentYear}{Add text}
#'	\item{FLim}{Add text}
#'	\item{Fpa}{Add text}
#'	\item{Bpa}{Add text}
#'	\item{Blim}{Add text}
#'	\item{FMSY}{Add text}
#'	\item{MSYBtrigger}{Add text}
#'	\item{Fmanagement}{Add text}
#'	\item{Bmanagement}{Add text}
#'	\item{RecruitmentAge}{Add text}
#'	\item{RecruitmentLength}{Add text}
#' }
#' @source \url{https://standardgraphs.ices.dk/}
"sag_refpts_raw"

#' Historical Nominal Catches 1950-2010
#'
#' Catches in FAO area 27 by country, species, area and year as provided
#' by the national authorities.
#' Source: Eurostat/ICES data compilation of catch statistics - ICES 2016, Copenhagen.
#' Version: 12-05-2016
#'
#' @format A data frame with 28582 rows and 64 variables:
#' \describe{
#'	\item{Country}{Add text}
#'	\item{Species}{Add text}
#'	\item{Division}{Add text}
#'	\item{X1950}{Add text}
#'  ...
#'	\item{X2010}{Add text}
#' }
#' @source \url{http://www.ices.dk/marine-data/dataset-collections/Pages/Fish-catch-and-stock-assessment.aspx}
"catch_data_historical"


#' Official Nominal Catches 2006-2014
#'
#' Catches in FAO area 27 by country, species, area and year.
#' Source: Eurostat/ICES database on catch statistics - ICES 2011, Copenhagen.
#' Version 30-11-2011
#'
#' @format A data frame with 49182 rows and 13 variables:
#' \describe{
#'	\item{Species}{Add text}
#'	\item{Area}{Add text}
#'	\item{Units}{Add text}
#'	\item{Country}{Add text}
#'	\item{X2014}{Add text}
#'	\item{X2013}{Add text}
#'	\item{X2012}{Add text}
#'	\item{X2011}{Add text}
#'	\item{X2010}{Add text}
#'	\item{X2009}{Add text}
#'	\item{X2008}{Add text}
#'	\item{X2007}{Add text}
#'	\item{X2006}{Add text}
#' }
#' @source \url{http://www.ices.dk/marine-data/dataset-collections/Pages/Fish-catch-and-stock-assessment.aspx}
"catch_data_official"

#' ASFIS list of species
#'
#' ASFIS list of species includes 12 700 species items selected according
#' to their interest or relation to fisheries and aquaculture.
#' For each species item stored in a record, codes (ISSCAAP group, taxonomic and 3-alpha)
#' and taxonomic information (scientific name, author(s), family, and higher taxonomic classification) are provided.
#'
#' Version 2-2016
#'
#' @format A data frame with 12700 rows and 11 variables:
#' \describe{
#'	\item{ISSCAAP}{Add text}
#'	\item{TAXOCODE}{Add text}
#'	\item{X3A_CODE}{Add text}
#'	\item{Scientific_name}{Add text}
#'	\item{English_name}{Add text}
#'	\item{French_name}{Add text}
#'	\item{Spanish_name}{Add text}
#'	\item{Author}{Add text}
#'	\item{Family}{Add text}
#'	\item{Order}{Add text}
#'	\item{Stats_data}{Add text}
#' }
#' @source \url{http://www.fao.org/fishery/collection/asfis/en}
"species_list"

#' STECF nominal effort
#'
#' STECF nominal effort from the FDI data call 2016. Accessed 16 March 2017.
#'
#' @format A data frame with 23993 rows and 13 variables:
#' \describe{
#'	\item{measure.calculation{add text}
#'	\item{annex{add text}
#'	\item{country{add text}
#'	\item{fishing_activity{add text}
#'	\item{fishing_capacity{add text}
#'	\item{gt_days_at_sea{add text}
#'	\item{no_vessels{add text}
#'	\item{nominal_effort{add text}
#'	\item{regulated.area{add text}
#'	\item{regulated.gear{add text}
#'	\item{specon{add text}
#'	\item{vessel.length{add text}
#'	\item{year{add text}
#' }
#' @source \url{https://stecf.jrc.ec.europa.eu/dd/effort/graphs-annex}
"STECF_effort_data"

#' STECF landings and discards
#'
#' STECF landings and discards from the FDI data call 2016. Accessed 16 March 2017.
#'
#' @format A data frame with 530230 rows and 11 variables:
#' \describe{
#'	\item{annex{add text}
#'	\item{country{add text}
#'	\item{regulated.area{add text}
#'	\item{reg_area_cod{add text}
#'	\item{regulated.gear{add text}
#'	\item{species{add text}
#'	\item{specon{add text}
#'	\item{sum_discards{add text}
#'	\item{sum_landings{add text}
#'	\item{vessel.length{add text}
#'	\item{year{add text}
#' }
#' @source \url{https://stecf.jrc.ec.europa.eu/dd/effort/graphs-annex}
"STECF_landings_data"
