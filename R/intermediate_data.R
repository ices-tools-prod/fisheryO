#' summary_table_frmt
#'
#' Summary table
#'
#' \itemize{
#'	\item{StockCode}{Add text}
#'	\item{Description}{Add text}
#'	\item{FisheriesGuild}{Add text}
#'	\item{EcoRegion}{Add text}
#'	\item{AdviceCategory}{Add text}
#'	\item{DataCategory}{Add text}
#'	\item{SBL}{Add text}
#'	\item{F_2013}{Add text}
#'	\item{F_2014}{Add text}
#'	\item{F_2015}{Add text}
#'	\item{SSB_2014}{Add text}
#'	\item{SSB_2015}{Add text}
#'	\item{SSB_2016}{Add text}
#'	\item{D3C1}{Add text}
#'	\item{D3C2}{Add text}
#'	\item{GES}{Add text}
#' }
#'
#' @format A data frame with 799 rows and 16 variables.
#' @seealso Used in \code{\link{stockPie_fun}} to plot pie charts of the proportion
#' of st stock status by ecoregion. Input data: \code{\link{stock_list_raw}},
#'  \code{\link{sag_summary_raw}},\code{\link{sag_refpts_raw}}, and \code{\link{sag_keys}}.
"summary_table_frmt"


#' pie_table_count
#'
#' Proportion of stocks relative to reference points for fish categories in an ecoregion.
#'
#' \itemize{
#'	\item{EcoRegion}{Ecoregion}
#'	\item{FisheriesGuild}{Fisheries category. e.g., benthic, demersal, pelagic, crustacean, elasmobranch, or undetermined}
#'	\item{VARIABLE}{Reference point}
#'	\item{VALUE}{Categorical value of status relative to reference points. e.g., GREEN, RED, GREY, or ORANGE}
#'	\item{COUNT}{Number of stocks that are within each ecoregion, fisheries category, and variable combination}
#' }
#' @format A data frame with 1189 rows and 5 variables.
#' @seealso Used in \code{\link{stockPie_fun}} to plot proportion of stocks relative
#'  to reference points for fish categories in an ecoregion. Input data: Input data: \code{\link{stock_list_raw}},
#'  \code{\link{sag_summary_raw}},\code{\link{sag_refpts_raw}}, and \code{\link{sag_keys}}.
"pie_table_count"

#' ges_table
#'
#' Proportion of stocks and landings relative to GES reference points in an ecoregion.
#'
#' \itemize{
#'	\item{EcoRegion}{Add text}
#'	\item{VARIABLE}{Add text}
#'	\item{COLOR}{Add text}
#'	\item{VALUE}{Add text}
#'	\item{METRIC}{Add text}
#' }
#'
#' @format A data frame with 128 rows and 5 variables.
#' @seealso Used in \code{\link{gesPie_fun}} to plot dynamic and
#'  static tables of stock status by ecoregion. Input data: Input data: \code{\link{stock_list_raw}},
#'  \code{\link{sag_summary_raw}},\code{\link{sag_refpts_raw}}, and \code{\link{sag_keys}}.
"ges_table"


#' stock_trends_frmt
#'
#' F and SSB relative to F<sub>MSY</sub> and MSY B<sub>trigger</sub>
#' reference points for stocks of a fish category for an ecoregion.
#'
#' \itemize{
#'	\item{pageGroup}{Add text}
#'	\item{lineGroup}{Add text}
#'	\item{Year}{Add text}
#'	\item{plotGroup}{Add text}
#'	\item{plotValue}{Add text}
#' }
#'
#' @format A data frame with 19692 rows and 5 variables.
#' @seealso Used in \code{\link{stock_trends_fun}} to plot line plots of F and SSB relative to F<sub>MSY</sub> and MSY B<sub>trigger</sub>
#' reference points for stocks of a fish category for an ecoregion. Input data: Input data: \code{\link{stock_list_raw}},
#'  \code{\link{sag_summary_raw}},\code{\link{sag_refpts_raw}}, and \code{\link{sag_keys}}.
"stock_trends_frmt"


#' sag_complete_summary
#'
#' Merged and tidied SAG reference points and summary table. Used to make clickable information on dynamic plots
#'
#' \itemize{
#'	\item{StockCode}{Add text}
#'	\item{Description}{Add text}
#'	\item{SpeciesScientificName}{Add text}
#'	\item{DataCategory}{Add text}
#'	\item{YearOfLastAssessment}{Add text}
#'	\item{AdviceCategory}{Add text}
#'	\item{FisheriesGuild}{Add text}
#'	\item{SpeciesID}{Add text}
#'	\item{Flim}{Add text}
#'	\item{Fpa}{Add text}
#'	\item{Bpa}{Add text}
#'	\item{Blim}{Add text}
#'	\item{FMSY}{Add text}
#'	\item{MSYBtrigger}{Add text}
#'	\item{Year}{Add text}
#'	\item{F}{Add text}
#'	\item{SSB}{Add text}
#'	\item{fishingPressureDescription}{Add text}
#'	\item{stockSizeDescription}{Add text}
#'	\item{landings}{Add text}
#'	\item{catches}{Add text}
#'	\item{discards}{Add text}
#'	\item{FmsyDescription}{Add text}
#'	\item{BmsyDescription}{Add text}
#'	\item{data}{Add text}
#' }
#'
#' @format A tbl_df with 7977 rows and 25 variables.
#'
#' @seealso Used in \code{\link{stock_trends_fun}}  to make clickable information on dynamic
#' line plots of F and SSB relative to F<sub>MSY</sub> and MSY B<sub>trigger</sub>
#' reference points for stocks of a fish category for an ecoregion. Input data: Input data: \code{\link{stock_list_raw}},
#'  \code{\link{sag_summary_raw}},\code{\link{sag_refpts_raw}}, and \code{\link{sag_keys}}.
"sag_complete_summary"

#' stock_status_full
#'
#' F/F<sub>MSY</sub> and SSB/MSY B<sub>trigger</sub> by fish category and ecoregion and
#' total catch (divided into discards and landings) by stock.
#'
#' \itemize{
#'	\item{StockCode}{Add text}
#'	\item{Description}{Add text}
#'	\item{FisheriesGuild}{Add text}
#'	\item{EcoRegion}{Add text}
#'	\item{F_FMSY}{Add text}
#'	\item{catches}{Add text}
#'	\item{landings}{Add text}
#'	\item{discards}{Add text}
#'	\item{FMSY}{Add text}
#'	\item{F}{Add text}
#'	\item{SSB_MSYBtrigger}{Add text}
#'	\item{SSB}{Add text}
#'	\item{MSYBtrigger}{Add text}
#'	\item{colList}{Add text}
#' }
#'
#' @format A data frame with 759 rows and 14 variables.
#' @seealso Used in \code{\link{plot_kobe}} to 2 plots: a scatter plot of F/F<sub>MSY</sub> and SSB/MSY B<sub>trigger</sub>
#' by fish category and ecoregion and a "lollipop" plot of total catch (divided into discards and landings) by stock.
#' Input data: Input data: \code{\link{stock_list_raw}},
#'  \code{\link{sag_summary_raw}},\code{\link{sag_refpts_raw}}, and \code{\link{sag_keys}}.
"stock_status_full"


#' stock_catch_full
#'
#' Discard rate and landings by fish category for an ecoregion.
#'
#' \itemize{
#'	\item{Year}{Add text}
#'	\item{YearOfLastAssessment}{Add text}
#'	\item{StockCode}{Add text}
#'	\item{Description}{Add text}
#'	\item{FisheriesGuild}{Add text}
#'	\item{EcoRegion}{Add text}
#'	\item{F}{Add text}
#'	\item{FMSY}{Add text}
#'	\item{SSB}{Add text}
#'	\item{MSYBtrigger}{Add text}
#'	\item{catches}{Add text}
#'	\item{landings}{Add text}
#'	\item{discards}{Add text}
#' }
#'
#' @format A data frame with 26485 rows and 13 variables.
#' @seealso Used in \code{\link{guild_discards_fun}} to 2 plots: a scatter plot of F/F<sub>MSY</sub> and SSB/MSY B<sub>trigger</sub>
#' by fish category and ecoregion and a "lollipop" plot of total catch (divided into discards and landings) by stock.
#' Input data: Input data: \code{\link{stock_list_raw}},
#'  \code{\link{sag_summary_raw}},\code{\link{sag_refpts_raw}}, and \code{\link{sag_keys}}.
"stock_catch_full"

#' ices_catch_dat
#'
#' Historic and official catch for an ecoregion by country, guild, and species.
#'
#' \itemize{
#'	\item{YEAR}{Add text}
#'	\item{COUNTRY}{Add text}
#'	\item{ISO3}{Add text}
#'	\item{GUILD}{Add text}
#'	\item{ECOREGION}{Add text}
#'	\item{SPECIES_NAME}{Add text}
#'	\item{SPECIES_CODE}{Add text}
#'	\item{COMMON_NAME}{Add text}
#'	\item{VALUE}{Add text}
#' }
#'
#' @format A data frame with 443077 rows and 9 variables.
#' @seealso Used in \code{\link{ices_catch_plot}} to create an area or line plot of landings (historic and official catch) for an ecoregion by country,
#' guild, or species.
#' Input data: Input data: \code{\link{catch_data_official}}, \code{\link{catch_data_historical}}, \code{\link{species_list}}, and \code{\link{stock_list_raw}}.
"ices_catch_dat"

#' stecf_effort_clean
#'
#' Historic and official catch for an ecoregion by country, guild, and species.
#'
#' \itemize{
#'	\item{YEAR}{Add text}
#'	\item{COUNTRY}{Add text}
#'	\item{ISO3}{Add text}
#'	\item{GUILD}{Add text}
#'	\item{ECOREGION}{Add text}
#'	\item{SPECIES_NAME}{Add text}
#'	\item{SPECIES_CODE}{Add text}
#'	\item{COMMON_NAME}{Add text}
#'	\item{VALUE}{Add text}
#' }
#'
#' @format A data frame with 443077 rows and 9 variables.
#' @seealso Used in \code{\link{ices_catch_plot}} to create an area or line plot of landings (historic and official catch) for an ecoregion by country,
#' guild, or species.
#' Input data: Input data: \code{\link{catch_data_official}}, \code{\link{catch_data_historical}}, \code{\link{species_list}}, and \code{\link{stock_list_raw}}.
"stecf_effort_clean"

#' stecf_landings_clean
#'
#' Historic and official catch for an ecoregion by country, guild, and species.
#'
#' \itemize{
#'	\item{YEAR}{Add text}
#'	\item{COUNTRY}{Add text}
#'	\item{ISO3}{Add text}
#'	\item{GUILD}{Add text}
#'	\item{ECOREGION}{Add text}
#'	\item{SPECIES_NAME}{Add text}
#'	\item{SPECIES_CODE}{Add text}
#'	\item{COMMON_NAME}{Add text}
#'	\item{VALUE}{Add text}
#' }
#'
#' @format A data frame with 443077 rows and 9 variables.
#' @seealso Used in \code{\link{ices_catch_plot}} to create an area or line plot of landings (historic and official catch) for an ecoregion by country,
#' guild, or species.
#' Input data: Input data: \code{\link{catch_data_official}}, \code{\link{catch_data_historical}}, \code{\link{species_list}}, and \code{\link{stock_list_raw}}.
"stecf_landings_clean"













