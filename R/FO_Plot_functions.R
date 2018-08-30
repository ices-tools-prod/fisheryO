
#' ICES Area and Ecoregion map
#'
#' \code{area_definition_map} returns a map describing potential mismatches between ICES Ecoregions and ICES Areas
#'
#' @param ecoregion ecoregion name, e.g. Greater North Sea Ecoregion
#' @param data_caption print the data source as a caption, boolean.
#'
#' @note
#'
#' @return A png or ggplot plot
#'
#' @seealso SAG summary table and reference points come from \code{\link{clean_sag}}. \code{\link{frmt_summary_table}} evaluates
#' status relative to reference points and formats the table for .html.
#'
#' @author Scott Large
#'
#' @examples
#' \dontrun{
#' area_definition_map("Greater North Sea")
#' }
#' @export

# AV: I am going to take out the save_plot, ggsave is enough, 
# could be added to the manual as suggestion
# ggsave("CS_figure1.png", path = "~/", width = 178, height = 152, units = "mm", dpi = 300)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ICES Area and Ecoregion Map #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

area_definition_map <- function() {
  # 
  # if(is.null(file_name)) {
  #   file_name <- gsub("\\s", "_", ecoregion)
  # }
  # 
  # if(is.null(output_path)) {
  #   output_path <- "~/"
  # }
  
    cap_lab <- labs(caption = "Made with Natural Earth and ICES Marine Data",
                    x = "",
                    y = "")
  xmin <- min(sf::st_bbox(eco_areas)[1], sf::st_bbox(ices_areas)[1])
  xmax <- max(sf::st_bbox(eco_areas)[3], sf::st_bbox(ices_areas)[3])
  ymin <- min(sf::st_bbox(eco_areas)[2], sf::st_bbox(ices_areas)[2])
  ymax <- max(sf::st_bbox(eco_areas)[4], sf::st_bbox(ices_areas)[4])
  
  xlims <- c(xmin, xmax)
  ylims <- c(ymin, ymax)
  
  p1 <- ggplot() +
     geom_sf(data = eco_areas, color = "grey90", fill = "gold") +
    # geom_sf(data = visahke, color = "grey80", fill = "gold") +
    geom_sf(data = europe_shape, fill = "grey80", color = "grey90") +
    # geom_sf(data = ices_areas, color = "grey60", fill = "gold") +
    geom_sf(data = extraareas, color = "grey80", fill = "transparent")+
    geom_sf(data = ices_areas, color = "grey60", fill = "transparent") +
    geom_text(data = centroids, aes(x = X, y = Y, label = Area_27), size = 2.5) +
    geom_text(data = extracentroids, aes(x = X, y = Y, label = Area_27), size = 2.5) +
    #geom_text(data = visahke, aes(x = X, y = Y, label = Area_27), size = 2.5) +
    theme_bw(base_size = 8) +
    theme(plot.caption = element_text(size = 6),
          plot.subtitle = element_text(size = 7)) +
    coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
    cap_lab

     return(p1)
}

#' Render html stock summary table
#'
#' This function returns "Status of stock summary relative to reference points" for all stocks
#' in an ecoregion.
#'
#' @param ecoregion ecoregion name, e.g. Greater North Sea
#' @param table_type type of table, "dynamic_html" (using DT) and "static_docx" (using \code{flextable} and \code{officer}) .docx tables.
#' @param output_path path for output to live.
#' @param file_name name for the output.
#' @param active_year numeric of the stock database version (year). e.g., 2016
#' @param return_data logical on returning a .csv of plotted data
#'
#' @note Stocks are linked to ecoregions via the ICES Stock database. Reference points are as published in ICES Stock Assessment
#' Graphs database. In some cases, status relative to reference points may vary from
#' published ICES advice when reported F or SSB are very close to reference points (e.g., F = 0.201 > F<sub>MSY</sub> = 0.20).
#'
#' Periodically, ICES adds or removes stocks from the advisory process. The function returns
#' the SAG reference points and summary table for all published (in SAG) and active stocks for a given year.
#'
#' @return A html file. When \code{file_name} is \code{NULL}, the file name is the ecoregion.
#' When \code{output_path} is \code{NULL}, the file is saved to "~/". When \code{table_type} is \code{"static"} or \code{"both"},
#' it might take a bit of time...
#'
#' @seealso SAG summary table and reference points come from \code{\link{clean_sag}}. \code{\link{frmt_summary_table}} evaluates
#' status relative to reference points and formats the table for .html.
#'
#' @author Scott Large
#'
#' @examples
#' \dontrun{
#' stockSummaryTable_fun("Greater North Sea Ecoregion")
#' }
#' @export
# ~~~~~~~~~~~~~~~~~~~~ #
# Stock Summary Table  #

#
# ~~~~~~~~~~~~~~~~~~~~ #
stockSummaryTable_fun <- function(ecoregion,
                                  active_year = 2018,
                                  output_path = NULL,
                                  file_name = NULL,
                                  return_data = FALSE) {

  if(is.null(file_name)) {
    file_name <- gsub("\\s", "_", ecoregion)
  }

  if(is.null(output_path)) {
    output_path <- "~/"
  }

  proxy_stocks <-  sag_stock_status_raw %>%
    filter(grepl("proxy", fishingPressure) | grepl("proxy", stockSize)) %>%
    select(StockKeyLabel) %>%
    distinct %>%
    pull(StockKeyLabel)
  
  
  stockPlot <- summary_table_frmt %>%
    filter(grepl(pattern = ecoregion, EcoRegion)) %>%
    select(-EcoRegion) %>%
    distinct(.keep_all = TRUE) %>%
    arrange(StockKeyLabel) %>%
    mutate(Description = ifelse(!grepl("<em>|</em>", Description),
                                paste0(Description, "<em></em>"),
                                Description),
           AdviceCategory = gsub("MSY|MP", "MSY", AdviceCategory),
           AdviceCategory = ifelse(StockKeyLabel %in% proxy_stocks,
                                   "MSY",
                                   AdviceCategory),
           DataCategory = as.factor(DataCategory))
  
  if(return_data) {
    write.csv(x = stockPlot, file = paste0(output_path, file_name, ".csv"), row.names = FALSE)
  }
  
  grey.path <- system.file("symbols", "grey_q.png", package = "fisheryO")
  red.path <- system.file("symbols", "red_cross.png", package = "fisheryO")
  green.path <- system.file("symbols", "green_check.png", package = "fisheryO")
  orange.path <- system.file("symbols", "orange_oh.png", package = "fisheryO")
  
  
  colkeys <- colnames(stockPlot[,names(stockPlot) != c("SpeciesScientificName")])
  
  FT <- stockPlot %>%
    # head() %>% 
    mutate(cname = gsub("<em>", "", stringr::str_extract(Description, ".*?<em>")),
           sname = gsub("<em>|</em>", "", stringr::str_extract(Description, "<em>.*?</em>")),
           rest = gsub("</em>", "", stringr::str_extract(Description, "</em>.*"))) %>%
    flextable::flextable(col_keys = colkeys) %>%
    flextable::display(col_key = "Description", pattern = "{{scientific_name}}{{common_name}}{{other}}",
                       formatters = list(common_name ~ cname,
                                         scientific_name ~ sname,
                                         other ~ rest),
                       fprops = list(scientific_name = officer::fp_text(italic = TRUE))) %>%
    flextable::display(i = ~ SBL == "RED", col_key = "SBL", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(SBL, src = red.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ SBL == "GREY", col_key = "SBL", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(SBL, src = grey.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ SBL == "GREEN", col_key = "SBL", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(SBL, src = green.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ SBL == "ORANGE", col_key = "SBL", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(SBL, src = orange.path, width = .15, height = .15))) %>%
    # FMSY_2014
    flextable::display(i = ~ FMSY_2014 == "RED", col_key = "FMSY_2014", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(FMSY_2014, src = red.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ FMSY_2014 == "GREY", col_key = "FMSY_2014", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(FMSY_2014, src = grey.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ FMSY_2014 == "GREEN", col_key = "FMSY_2014", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(FMSY_2014, src = green.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ FMSY_2014 == "ORANGE", col_key = "FMSY_2014", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(FMSY_2014, src = orange.path, width = .15, height = .15)))%>%
    # FMSY_2015
    flextable::display(i = ~ FMSY_2015 == "RED", col_key = "FMSY_2015", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(FMSY_2015, src = red.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ FMSY_2015 == "GREY", col_key = "FMSY_2015", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(FMSY_2015, src = grey.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ FMSY_2015 == "GREEN", col_key = "FMSY_2015", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(FMSY_2015, src = green.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ FMSY_2015 == "ORANGE", col_key = "FMSY_2015", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(FMSY_2015, src = orange.path, width = .15, height = .15)))%>%
    # FMSY_2016
    flextable::display(i = ~ FMSY_2016 == "RED", col_key = "FMSY_2016", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(FMSY_2016, src = red.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ FMSY_2016 == "GREY", col_key = "FMSY_2016", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(FMSY_2016, src = grey.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ FMSY_2016 == "GREEN", col_key = "FMSY_2016", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(FMSY_2016, src = green.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ FMSY_2016 == "ORANGE", col_key = "FMSY_2016", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(FMSY_2016, src = orange.path, width = .15, height = .15)))%>%
    # FMSY_2017
    flextable::display(i = ~ FMSY_2017 == "RED", col_key = "FMSY_2017", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(FMSY_2017, src = red.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ FMSY_2017 == "GREY", col_key = "FMSY_2017", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(FMSY_2017, src = grey.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ FMSY_2017 == "GREEN", col_key = "FMSY_2017", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(FMSY_2017, src = green.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ FMSY_2017 == "ORANGE", col_key = "FMSY_2017", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(FMSY_2017, src = orange.path, width = .15, height = .15))) %>%
    # SSBMSY_2015
    flextable::display(i = ~ SSBMSY_2015 == "RED", col_key = "SSBMSY_2015", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(SSBMSY_2015, src = red.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ SSBMSY_2015 == "GREY", col_key = "SSBMSY_2015", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(SSBMSY_2015, src = grey.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ SSBMSY_2015 == "GREEN", col_key = "SSBMSY_2015", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(SSBMSY_2015, src = green.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ SSBMSY_2015 == "ORANGE", col_key = "SSBMSY_2015", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(SSBMSY_2015, src = orange.path, width = .15, height = .15)))%>%
    # SSBMSY_2016
    flextable::display(i = ~ SSBMSY_2016 == "RED", col_key = "SSBMSY_2016", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(SSBMSY_2016, src = red.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ SSBMSY_2016 == "GREY", col_key = "SSBMSY_2016", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(SSBMSY_2016, src = grey.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ SSBMSY_2016 == "GREEN", col_key = "SSBMSY_2016", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(SSBMSY_2016, src = green.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ SSBMSY_2016 == "ORANGE", col_key = "SSBMSY_2016", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(SSBMSY_2016, src = orange.path, width = .15, height = .15)))%>%
    # SSBMSY_2017
    flextable::display(i = ~ SSBMSY_2017 == "RED", col_key = "SSBMSY_2017", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(SSBMSY_2017, src = red.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ SSBMSY_2017 == "GREY", col_key = "SSBMSY_2017", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(SSBMSY_2017, src = grey.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ SSBMSY_2017 == "GREEN", col_key = "SSBMSY_2017", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(SSBMSY_2017, src = green.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ SSBMSY_2017 == "ORANGE", col_key = "SSBMSY_2017", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(SSBMSY_2017, src = orange.path, width = .15, height = .15)))%>%
    # SSBMSY_2018
    flextable::display(i = ~ SSBMSY_2018 == "RED", col_key = "SSBMSY_2018", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(SSBMSY_2018, src = red.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ SSBMSY_2018 == "GREY", col_key = "SSBMSY_2018", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(SSBMSY_2018, src = grey.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ SSBMSY_2018 == "GREEN", col_key = "SSBMSY_2018", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(SSBMSY_2018, src = green.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ SSBMSY_2018 == "ORANGE", col_key = "SSBMSY_2018", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(SSBMSY_2018, src = orange.path, width = .15, height = .15)))%>%
    
    # D3C1
    flextable::display(i = ~ D3C1 == "RED", col_key = "D3C1", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(D3C1, src = red.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ D3C1 == "GREY", col_key = "D3C1", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(D3C1, src = grey.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ D3C1 == "GREEN", col_key = "D3C1", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(D3C1, src = green.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ D3C1 == "ORANGE", col_key = "D3C1", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(D3C1, src = orange.path, width = .15, height = .15)))%>%
    # D3C2
    flextable::display(i = ~ D3C2 == "RED", col_key = "D3C2", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(D3C2, src = red.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ D3C2 == "GREY", col_key = "D3C2", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(D3C2, src = grey.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ D3C2 == "GREEN", col_key = "D3C2", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(D3C2, src = green.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ D3C2 == "ORANGE", col_key = "D3C2", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(D3C2, src = orange.path, width = .15, height = .15)))%>%
    # GES
    flextable::display(i = ~ GES == "RED", col_key = "GES", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(GES, src = red.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ GES == "GREY", col_key = "GES", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(GES, src = grey.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ GES == "GREEN", col_key = "GES", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(GES, src = green.path, width = .15, height = .15))) %>%
    flextable::display(i = ~ GES == "ORANGE", col_key = "GES", pattern = "{{add_icon}}",
                       formatters = list(add_icon ~ flextable::as_image(GES, src = orange.path, width = .15, height = .15)))%>%
    flextable::fontsize(size = 9, part = "all") %>%
    flextable::add_header(StockKeyLabel = "Stock code",
                          Description = "Stock name",
                          FisheriesGuild = "Fish category",
                          AdviceCategory = "Reference point",
                          DataCategory = "Data category",
                          SBL = "SBL",
                          FMSY_2014 = "Fishing pressure",
                          FMSY_2015 = "Fishing pressure",
                          FMSY_2016 = "Fishing pressure",
                          FMSY_2017 = "Fishing pressure",
                          SSBMSY_2015 = "Stock size",
                          SSBMSY_2016 = "Stock size",
                          SSBMSY_2017 = "Stock size",
                          SSBMSY_2018 = "Stock size",
                          D3C1 = "MSFD descriptor",
                          D3C2 = "MSFD descriptor",
                          GES = "MSFD descriptor", top = TRUE) %>%
    flextable::set_header_labels(StockKeyLabel = "Stock code",
                                 Description = "Stock name",
                                 FisheriesGuild = "Fish category",
                                 AdviceCategory = "Reference point",
                                 DataCategory = "Data category",
                                 SBL = "SBL",
                                 FMSY_2014 = "2014",
                                 FMSY_2015 = "2015",
                                 FMSY_2016 = "2016",
                                 FMSY_2017 = "2017",
                                 SSBMSY_2015 = "2015",
                                 SSBMSY_2016 = "2016",
                                 SSBMSY_2017 = "2017",
                                 SSBMSY_2018 = "2018",
                                 D3C1 = "D3C1",
                                 D3C2 = "D3C2",
                                 GES = "GES") %>%
    # flextable::merge_h(part = "header") %>%
    # flextable::merge_v(part = "header") %>%
    flextable::align(j = c("StockKeyLabel",
                           "Description",
                           "FisheriesGuild",
                           "AdviceCategory"), align = "left", part = "all") %>%
    flextable::align(j = c("DataCategory",
                           "SBL",
                           "FMSY_2014",
                           "FMSY_2015",
                           "FMSY_2016",
                           "FMSY_2017",
                           "SSBMSY_2015",
                           "SSBMSY_2016",
                           "SSBMSY_2017",
                           "SSBMSY_2018",
                           "D3C1",
                           "D3C2",
                           "GES"), align = "center", part = "all") %>%
    # flextable::align(j = c("DataCategory"), align = "right", part = "body") %>%
    flextable::autofit()
  
  
  doc <- officer::read_docx() %>%
    flextable::body_add_flextable(FT)
  
  print(doc, target = paste0("CelticSeas_ListStocks.docx")) %>%
    invisible()

}
  

#' Pie chart of proportion of stocks relative to reference points
#'
#' The \code{stockPie_fun} function returns pie charts of the proportion of stocks
#' relative to reference points for fish categories in an ecoregion.
#'
#' @param ecoregion ecoregion name, e.g. Greater North Sea
#' @param fisheries_guild fisheries guild to include in proportions
#' @param calculate_status logical on whether to use raw SAG output to calculate stock status or to use the hard-coded values from stock summary table
#' @param data_caption print the data source as a caption, boolean.
#' @param output_path path for output to live.
#' @param active_year numeric of the stock database version (year). e.g., 2016
#' @param file_name name for the output.
#' @param save_plot logical to save plot.
#' @param return_plot logical to return plot to current environment.
#' @param return_data logical on returning a .csv of plotted data
#'
#' @note Stocks are linked to ecoregions and fish categories via the ICES Stock database.
#' Reference points are as published in ICES Stock Assessment Graphs database. In some cases,
#' status relative to reference points may vary from published ICES advice
#'  when reported F or SSB are very close to reference points (e.g., F = 0.201 > F<sub>MSY</sub> = 0.20). \code{calculate_status = TRUE} calculates stock status
#' relative to published reference points. This will represent PA and SBL for ecoregions with proxy reference points. \code{calculate_status = TRUE} takes the
#' raw icons from published advice. Note, before 2017 not all stocks status tables have been added to the SAG database and only few stocks had MSY proxy reference points.
#'
#'
#' @return A ggplot2 object or .png saved as \code{file_name} to \code{output_path}.
#' When \code{file_name} is \code{NULL}, the file name is the ecoregion.
#' When \code{output_path} is \code{NULL}, the file is saved to "~/".
#'
#' @author Scott Large
#'
#' @examples
#' \dontrun{
#' stockPie_fun("Greater North Sea Ecoregion", return_plot = TRUE)
#' }
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~ #
# Stock Summary Pie chart  #
# ~~~~~~~~~~~~~~~~~~~~~~~~ #
 stockPie_fun <- function(ecoregion,
                          fisheries_guild = c("pelagic", "demersal", "crustacean", "elasmobranch", "benthic"),
                          cap_month = "May",
                          cap_year = "2018",
                          file_name,
                          active_year = 2018,
                          save_plot = FALSE,
                          return_plot = TRUE,
                          return_data = FALSE,
                          output_path = NULL) {
 
   if(save_plot) {
     if(is.null(file_name)) {
       file_name <- gsub("\\s", "_", ecoregion)
     }
 
     if(is.null(output_path)) {
       output_path <- "~/"
     }
   }

library(ggplot2)
cap_lab <- labs(title = "", x = "", y = "",
                caption = sprintf("ICES Stock Assessment Database, %s %s. ICES, Copenhagen",
                                  cap_month,
                                  cap_year))


colList <- c("GREEN" = "#00B26D",
             "GREY" = "#d3d3d3",
             "ORANGE" = "#ff7f00",
             "RED" = "#d93b1c")

rowDat <- pie_table_count %>%
  filter(grepl(pattern = ecoregion, EcoRegion)) %>%
  ungroup() %>%
  select(-EcoRegion) %>%
  group_by(FisheriesGuild, VARIABLE) %>%
  mutate(fraction = COUNT/ sum(COUNT)) %>%
  filter(fraction != 0) %>%
  ungroup() %>%
  mutate(VARIABLE = recode_factor(VARIABLE,
                                  "FMSY" = "Fishing pressure\nMSY",
                                  "BMSY" = "Stock size\nMSY",
                                  "FPA" = "Fishing pressure\nPA",
                                  "BPA" = "Stock size\nPA",
                                  "SBL" = "Safe\nbiological limits"),
         FisheriesGuild = factor(FisheriesGuild,
                                 levels = c("total", "benthic", "crustacean", "elasmobranch", "demersal", "pelagic"))
  )

library(data.table)

rowDat <- data.table(rowDat)

rowDat<-  rowDat[, lapply(.SD, sum), by=list(FisheriesGuild, VARIABLE, VALUE)]


#need to take out the qualRED thing
library(ggplot2)

p1 <- ggplot(data = rowDat, aes(x = "", y = fraction, fill = VALUE)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = COUNT),
            position = position_stack(vjust = 0.5),
            size = 3) +
  scale_fill_manual(values = colList) +
  theme_bw(base_size = 9) +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none") +
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        strip.background = element_blank(),
        plot.caption = element_text(size = 6)) +
  cap_lab +
  coord_polar(theta = "y", direction = 1) +
  facet_grid(FisheriesGuild ~ VARIABLE)

if(return_plot) {
   return(p1)
 }

if(return_data) {
  write.csv(x = rowDat, file = paste0(output_path, file_name, ".csv"), row.names = FALSE)
}


if(save_plot) {
  ggsave(filename = paste0(output_path, file_name, ".png"),
         plot = p1,
         width = 178,
         height = 152,
         units = "mm",
         dpi = 300)
}
}

#' Pie chart of proportion of stocks relative to GES reference points
#'
#' The \code{gesPie_fun} function returns pie charts of the proportion of stocks
#' relative to GES reference points in an ecoregion.
#'
#' @param ecoregion ecoregion name, e.g. Greater North Sea
#' @param fisheries_guild fisheries guild to include in proportions
#' @param calculate_status logical on whether to use raw SAG output to calculate stock status or to use the hard-coded values from stock summary table
#' @param data_caption print the data source as a caption, boolean.
#' @param output_path path for output to live.
#' @param active_year numeric of the stock database version (year). e.g., 2016
#' @param file_name name for the output.
#' @param save_plot logical to save plot.
#' @param return_plot logical to return plot to current environment.
#' @param return_data logical on returning a .csv of plotted data
#'
#' @note Stocks are linked to ecoregions via the ICES Stock database.
#' Reference points are as published in ICES Stock Assessment Graphs database. In some cases,
#' status relative to reference points may vary from published ICES advice
#'  when reported F or SSB are very close to reference points (e.g., F = 0.201 > F<sub>MSY</sub> = 0.20). \code{calculate_status = TRUE} calculates stock status
#' relative to published reference points. This will represent PA and SBL for ecoregions with proxy reference points. \code{calculate_status = TRUE} takes the
#' raw icons from published advice. Note, before 2017 not all stocks status tables have been added to the SAG database and only few stocks had MSY proxy reference points.
#'
#' @return A ggplot2 object or .png saved as \code{file_name} to \code{output_path}.
#' When \code{file_name} is \code{NULL}, the file name is the ecoregion.
#' When \code{output_path} is \code{NULL}, the file is saved to "~/".
#'
#' @author Scott Large
#'
#' @examples
#' \dontrun{
#' gesPie_fun("Greater North Sea Ecoregion", return_plot = TRUE)
#' }
#' @export
#~~~~~~~~~~~~~~~~#
# GES Pie Charts #
#~~~~~~~~~~~~~~~~#
gesPie_fun <- function(ecoregion,
                       fisheries_guild = c("pelagic", "demersal", "crustacean", "elasmobranch", "benthic"),
                       cap_month = "May",
                       cap_year = "2018",
                       file_name = NULL,
                       active_year = 2018,
                       save_plot = FALSE,
                       return_plot = TRUE,
                       return_data = FALSE,
                       output_path = NULL) {
  
  if(save_plot) {
    if(is.null(file_name)) {
      file_name <- gsub("\\s", "_", ecoregion)
    }
    
    if(is.null(output_path)) {
      output_path <- "~/"
    }
  }
  
    cap_lab <- labs(title = "", x = "", y = "",
                    caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                                      cap_year,
                                      cap_month))
  
  colList <- c("GREEN" = "#00B26D",
               "GREY" = "#d3d3d3",
               "RED" = "#d93b1c",
               "ORANGE" = "#ff7f00")
  
 
  rowDat <-ges_table %>%
    filter(grepl(pattern = ecoregion, EcoRegion)) %>%
    ungroup() %>%
    select(-EcoRegion) %>%
    group_by(VARIABLE, METRIC) %>%
    mutate(VALUE = ifelse(METRIC == "total_sum",
                          round(VALUE / 1000),
                          round(VALUE)),
           fraction = VALUE/ sum(VALUE)) %>%
    filter(fraction != 0) %>%
    ungroup() %>%
    mutate(METRIC = recode_factor(METRIC,
                                  "count" = "Number of stocks",
                                  "total_sum" = "Proportion of catch\n (thousand tonnes)"))
  rowDat <- data.table(rowDat)
  rowDat<-  rowDat[, lapply(.SD, sum), by=list(VARIABLE, COLOR, METRIC)]
  
  sumDat <- rowDat %>%
    group_by(VARIABLE, METRIC) %>%
    summarize(sum = sum(VALUE),
              COLOR = NA)
  
  p1 <- ggplot(data = rowDat, aes(x = "", y = fraction, fill = COLOR)) +
    geom_bar(stat = "identity", width = 1) +
    geom_text(aes(label = scales::comma(VALUE)),
              position = position_stack(vjust = 0.5),
              size = 3) +
    geom_text(data = sumDat,
              aes(x = 0, y = 0,
                  label = paste0("total = ",
                                 scales::comma(sum))),
              size = 1.5) +
    scale_fill_manual(values = colList) +
    theme_bw(base_size = 9) +
    theme(panel.grid = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position="none") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          strip.background = element_blank(),
          plot.caption = element_text(size = 6)) +
    cap_lab +
    coord_polar(theta = "y") +
    facet_grid(METRIC ~ VARIABLE)
  
  
  if(return_plot) {
    return(p1)
  }
  
  if(return_data) {
    write.csv(x = rowDat, file = paste0(output_path, file_name, ".csv"), row.names = FALSE)
  }
  
  if(save_plot) {
    ggsave(filename = paste0(output_path, file_name, ".png"),
           plot = p1,
           width = 89,
           height = 100.5,
           units = "mm",
           dpi = 300)
  }
}

#' Stock status over time
#'
#' The \code{stock_trends_fun} function returns a series of line plots of F and SSB relative to F<sub>MSY</sub> and MSY B<sub>trigger</sub>
#' reference points for stocks of a fish category for an ecoregion.
#'
#' @param object name of data to plot. Must agree with the grouping_var argument. E.g., EcoGuild must be the combined ecoregion
#' name and fish category, e.g. "Greater North Sea Ecoregion - demersal stocks"
#' @param grouping_var character string of the desired grouping. Options include: EcoRegion, EcoGuild, or FisheriesGuild
#' @param plotting_var character string of the variable to plot. Options include: StockCode or FisheriesGuild (mean)
#' @param metric character string of the desired metric. Options include: MSY or MEAN (according to grouping_var)
#' @param data_caption print the data source as a caption, boolean.
#' @param active_year numeric of the stock database version (year). e.g., 2016
#' @param dynamic logical to generate html output with dynamic features.
#' @param output_path path for output to live.
#' @param file_name name for the output.
#' @param save_plot logical to save plot.
#' @param return_plot logical to return plot to current environment.
#' @param return_data logical on returning a .csv of plotted data
#'
#' @note Stocks are linked to ecoregions and fish categories via the ICES Stock database.
#' Reference points are as published in ICES Stock Assessment Graphs database. In some cases,
#' status relative to reference points may vary from published ICES advice
#' when reported F or SSB are very close to reference points (e.g., F = 0.201 > F<sub>MSY</sub> = 0.20).
#'
#' @return A ggplot2 object when \code{return_plot} is \code{TRUE}, html when \code{dynamic} is \code{TRUE}
#' or .png when \code{dynamic} is \code{FALSE}. Output is saved as \code{file_name} in \code{output_path}.
#' When \code{file_name} is \code{NULL}, the file name is the ecoregion.
#' When \code{output_path} is \code{NULL}, the file is saved to "~/".
#'
#' @author Scott Large
#'
#' @examples
#' \dontrun{
#' stock_trends_fun(object = "Greater North Sea Ecoregion - demersal", grouping_var = "EcoGuild", return_plot = TRUE)
#' }
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~#
# Stock Status over time #
#~~~~~~~~~~~~~~~~~~~~~~~~#
stock_trends_fun <- function(object,
                             plotting_var = c("StockKeyLabel", "FisheriesGuild")[1],
                             grouping_var = c("EcoGuild", "EcoRegion", "FisheriesGuild")[1],
                             metric = c("MSY", "MEAN")[1],
                             active_year = 2018,
                             cap_month = "June",
                             cap_year = "2018",
                             file_name = NULL,
                             save_plot = FALSE,
                             return_plot = TRUE,
                             return_data = FALSE,
                             output_path = NULL) {
  
  if(!grouping_var %in% c("EcoRegion",
                          "EcoGuild",
                          "FisheriesGuild")) {
    stop(paste0("grouping_var: '", grouping_var, "' is not supported. Please try: EcoRegion, EcoGuild, or FisheriesGuild"))
  }
  if(!plotting_var %in% c("StockKeyLabel",
                          "FisheriesGuild")) {
    stop(paste0("plotting_var: '", plotting_var, "' is not supported. Please try: stock or guild"))
  }
  if(plotting_var == "FisheriesGuild" &
     grouping_var %in% c("EcoGuild", "FisheriesGuild")) {
    stop("plotting_var = 'FisheriesGuild' should only be used with grouping_var = 'EcoRegion'.")
  }
  if(!metric %in% c("MSY", "MEAN")) {
    stop(paste0("metric: '", metric, "' is not supported. Please try: 'MSY' or 'MEAN'"))
  }
  
  grouping_variable <- rlang::sym(grouping_var)
  plotting_variable <- rlang::sym(plotting_var)
  
  
  p1_dat <- stock_trends_frmt %>%
    ungroup() %>%
    filter(grepl(object, pageGroup)) %>%
    mutate(plotGroup = case_when(plotGroup == "SSB_SSBMEAN"~ "SSB/SSB[mean]",
                            plotGroup == "F_FMEAN"~ "F/F[mean]",
                            plotGroup == "F_FMSY"~ "F/F[MSY]",
                            plotGroup == "SSB_MSYBtrigger"~ "SSB/MSY~B[trigger]"),
      plotGroup = factor(plotGroup))
  
  if(length(unique(p1_dat$lineGroup)) <= 2){
    p1_dat <- p1_dat %>%
      filter(lineGroup != "MEAN")
  }
  
  if(metric == "MEAN"){
    p1_dat <- p1_dat %>%
      filter(lineGroup != "MEAN")
  }
  
  adj_names <- sort(setdiff(unique(p1_dat$lineGroup), "MEAN"))
  values <- ggthemes::tableau_color_pal('tableau10')(length(adj_names))
  legend_pos <- "bottom"
  
  names(values) <- adj_names
  values <- c(values, c(MEAN = "black"))
  
  if(save_plot) {
    if(is.null(file_name)) {
      file_name <- gsub("\\s", "_", object)
      file_name <- gsub("_-_", "-", file_name)
    }
    
    if(is.null(output_path)) {
      output_path <- "~/"
    }
  }
  
  plot_title <- gsub(".*\\s-\\s", "\\1", object)
  plot_title <- gsub(" stocks", "", plot_title)
  
  cap_lab <- labs(title = plot_title, x = "Year", y = "",
                    caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                                      cap_year,
                                      cap_month))
  
  p1_plot <- ggplot(p1_dat %>% filter(lineGroup != "MEAN"),
                    aes(x = Year, y = plotValue,
                        color = lineGroup,
                        fill = lineGroup#,
                        # onclick = onclick,
                        # data_id = lineGroup,
                        # tooltip = tooltip_line
                    )) +
    geom_hline(yintercept = 1, col = "grey60") +
    theme_bw(base_size = 9) +
    scale_color_manual(values = values) +
    scale_fill_manual(values = values) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    guides(fill = FALSE) +
    theme(legend.position = legend_pos,
          strip.text = element_text(size = 9, angle = 0, hjust = 0),
          strip.background = element_blank(),
          strip.placement = "outside",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = NA),
          plot.caption = element_text(size = 6)) +
    cap_lab +
    facet_grid(rows= vars (plotGroup),scales = "free_y", labeller = label_parsed)
  
  p1_plot <- p1_plot + geom_line(alpha = 0.8)
  p1_plot <- p1_plot + geom_line(data = p1_dat %>% filter(lineGroup == "MEAN"),
                                 alpha = 0.9, size = 1.15)
  
  if(return_plot) {
    return(p1_plot)
  }
  
  if(return_data) {
    write.csv(x = p1_dat, file = paste0(output_path, file_name, ".csv"), row.names = FALSE)
  }
  
  if(save_plot) {
    ggsave(filename = paste0(output_path, file_name, ".png"),
           plot = p1_plot,
           width = 170,
           height = 100.5,
           units = "mm",
           dpi = 300)
  }
}

#' Kobe plot of stock status
#'
#' The \code{plot_kobe} function returns a 2 plots: a scatter plot of F/F<sub>MSY</sub> and SSB/MSY B<sub>trigger</sub>
#' by fish category and ecoregion and a "lollipop" plot of total catch (divided into discards and landings) by stock.
#'
#' @param ecoregion ecoregion name, e.g. Greater North Sea Ecoregion
#' @param guild fish category (options: "all", "benthic", "demersal", "pelagic", "crustacean", "elasmobranch", "large-scale stocks"), e.g. demersal
#' @param active_year numeric of the stock database version (year). e.g., 2016
#' @param dynamic logical to generate html output with dynamic features.
#' @param data_caption print the data source as a caption, boolean.
#' @param output_path path for output to live.
#' @param file_name name for the output.
#' @param save_plot logical to save plot.
#' @param return_plot logical to return plot to current environment.
#' @param return_data logical on returning a .csv of plotted data
#' @param catch_limit lower limit of catch to be included in the plot. Useful to cull the herd if there are many stocks with minimal catch.
#' @param fig.width width pf combined set of plots
#' @param fig.height height of combined set of plots,
#' @param units defaults to "mm"
#' @param res defaults to "300"
#'
#' @note Stocks are linked to ecoregions and fish categories via the ICES Stock database.
#' Reference points are as published in ICES Stock Assessment Graphs database. In some cases,
#' status relative to reference points may vary from published ICES advice
#' when reported F or SSB are very close to reference points (e.g., F = 0.201 > F<sub>MSY</sub> = 0.20).
#'
#' @return A ggplot2 object when \code{return_plot} is \code{TRUE}, html when \code{dynamic} is \code{TRUE}
#' or .png when \code{dynamic} is \code{FALSE}. Output is saved as \code{file_name} in \code{output_path}.
#' When \code{file_name} is \code{NULL}, the file name is the ecoregion.
#' When \code{output_path} is \code{NULL}, the file is saved to "~/".
#'
#' @author Scott Large
#'
#' @examples
#' \dontrun{
#' plot_kobe("Greater North Sea Ecoregion", guild = "demersal", return_plot = TRUE)
#' }
#' @export
#~~~~~~~~~~~#
# KOBE Plot #
#~~~~~~~~~~~#
plot_kobe <- function(ecoregion,
                      fisheries_guild = c("all",
                                "benthic",
                                "demersal",
                                "pelagic",
                                "crustacean",
                                "elasmobranch",
                                "large-scale stocks")[1],
                      active_year = 2018,
                      cap_month = "May",
                      cap_year = "2018",
                      output_path = NULL,
                      return_plot = TRUE,
                      return_data = FALSE,
                      save_plot = FALSE,
                      catch_limit = 0,
                      file_name = NULL,
                      plotTitle = NULL,
                      fig.width = 131.32,
                      fig.height = 88.9,
                      units = "mm",
                      res = 300,
                      dynamic = FALSE) {
  #
  if(save_plot) {
    if(is.null(file_name)) {
      file_name <- gsub("\\s", "_", ecoregion)
    }
    
    if(is.null(output_path)) {
      output_path <- "~/"
    }
  }
  
  if(any(fisheries_guild %in% "all")) {
    fisheries_guild <- c("benthic", "demersal", "pelagic", "crustacean", "elasmobranch")
    labTitle <- "All stocks"
  } else {
    labTitle <- fisheries_guild
  }
  
    cap_lab <- labs(x = "Stock",
                    y = "Catch and landings (thousand tonnes)",
                    caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                                      cap_year,
                                      cap_month))
  
  kobeDat <- stock_status_full %>%
    filter(EcoRegion %in% c("Baltic Sea Ecoregion", "Baltic Sea Ecoregion, Greater North Sea Ecoregion"),
           FisheriesGuild %in% fisheries_guild,
           !is.na(F_FMSY),
           !is.na(SSB_MSYBtrigger)) %>%
    group_by(StockKeyLabel) %>%
    mutate(max_bar = max(catches, landings, discards, na.rm = TRUE),
           catch_width = ifelse(is.na(catches),
                                0,
                                round((catches/(max_bar/1.25) * 100))),
           landings_width = ifelse(is.na(landings),
                                   0,
                                   round((landings/(max_bar/1.25) * 100))),
           discards_width = ifelse(is.na(discards),
                                   0,
                                   round((discards/(max_bar/1.25) * 100))),
           total = ifelse(all(is.na(catches) & is.na(landings)),
                          NA,
                          max(catches, landings, na.rm = TRUE))) %>%
    distinct(.keep_all = TRUE)
  
  kobeDat$tip <- sprintf('
                         <div class="tipchart">
                         <h6>%s</h6>
                         <table>
                         <tr class="tiprow">
                         <td class="tipbarticks">F / F<sub>MSY</sub></td>
                         <td class="tipbardiv"><div class="tipbar" style="width:0px;">%3.2f&nbsp/&nbsp%3.2f&nbsp=&nbsp%3.2f</div></td>
                         </tr>
                         <tr class="tiprow">
                         <td class="tipbarticks">SSB / MSY B<sub>trigger</sub></td>
                         <td class="tipbardiv"><div class="tipbar" style="width:0px;">%3.0f&nbsp/&nbsp%3.0f&nbsp=&nbsp%3.2f</div></td>
                         </tr>
                         <tr class="tiprow">
                         <td class="tipbarticks">Catch (tonnes)</td>
                         <td class="tipbardiv"><div class="tipbar" style="width:%dpx;">%3.0f</div></td>
                         </tr>
                         <tr class="tiprow">
                         <td class="tipbarticks">Landings (tonnes)</td>
                         <td class="tipbardiv"><div class="tipbar" style="width:%dpx;">%3.0f</div></td>
                         </tr>
                         <tr class="tiprow">
                         <td class="tipbarticks">Discards (tonnes)</td>
                         <td class="tipbardiv"><div class="tipbar" style="width:%dpx;">%3.0f</div></td>
                         </tr>
                         </table>
                         </div>',
                         kobeDat$Description,
                         kobeDat$F, kobeDat$FMSY, kobeDat$F/kobeDat$FMSY,
                         kobeDat$SSB, kobeDat$MSYBtrigger, kobeDat$SSB/kobeDat$MSYBtrigger,
                         kobeDat$catch_width, kobeDat$catches,
                         kobeDat$landings_width, kobeDat$landings,
                         kobeDat$discards_width, kobeDat$discards)
  
  # javascript is too dumb to deal with line breaks in strings well
  kobeDat$tip <- gsub("\\\n", "", kobeDat$tip)
  
  if(length(fisheries_guild) >= 2) {
    kobeDat <- filter(kobeDat, total >= catch_limit)
  }
  kobeDat$colList[which(kobeDat$StockKeyLabel == "her.27.28")] <- "GREEN"
  
     labs <- seq(0, max(kobeDat$F_FMSY, kobeDat$SSB_MSYBtrigger, na.rm = TRUE) + 1)
     kobe_plot <- ggplot(kobeDat, aes(x = F_FMSY, y = SSB_MSYBtrigger,
                                     data_id = StockKeyLabel,
                                     tooltip = tip)) +
      geom_point(aes(color = colList), size = 2,
                 alpha = 0.7, na.rm = TRUE) +
      geom_hline(yintercept = 1, color = "grey60", linetype = "dashed") +
      geom_vline(xintercept = 1, color = "grey60", linetype = "dashed") +
      ggrepel::geom_text_repel(aes(label = StockKeyLabel),
                               segment.size = .25,
                               force = 5,
                               size = 2) +
      scale_color_manual(values = c("GREEN" = "#4daf4a",
                                    "RED" = "#e41a1c",
                                    "GREY" = "#d3d3d3")) +
      scale_y_continuous(breaks = labs) +
      scale_x_continuous(breaks = labs) +
      coord_equal(xlim = range(labs), ylim = range(labs)) +
      labs(x = expression(F/F[MSY]),
           y = expression(SSB/MSY~B[trigger]),
           caption = "") +
      theme_bw(base_size = 7) +
      theme(legend.position = 'none',
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.caption = element_text(size = 6))
    
    # Lollipop plot
    catchBar <- stock_status_full %>%
      filter(EcoRegion %in% c("Baltic Sea Ecoregion", "Baltic Sea Ecoregion, Greater North Sea Ecoregion"),
             FisheriesGuild %in% fisheries_guild) %>%
      # filter(StockKeyLabel %in% c("spr.27.22-32","her.27.25-2932", "her.27.3031",
      #                             "her.27.20-24","her.27.28"))%>%
      distinct(.keep_all = TRUE) %>%
      group_by(StockKeyLabel) %>%
      mutate(total = ifelse(all(is.na(catches) & is.na(landings)),
                            NA,
                            max(catches, landings, na.rm = TRUE))) %>%
      ungroup() %>%
      arrange(!is.na(total), total) %>%
      mutate(StockKeyLabel = factor(StockKeyLabel, StockKeyLabel))
    
    if(length(fisheries_guild) >= 2) {
      catchBar <- filter(catchBar, total >= catch_limit)
    }
    catchBar$colList[which(catchBar$StockKeyLabel == "her.27.28")] <- "GREEN"
    
    bar_plot <-
      ggplot(catchBar, aes(x =StockKeyLabel, y = catches/1000)) +
      geom_segment(aes(x = StockKeyLabel, y = catches/1000,
                       xend = StockKeyLabel, yend = 0, color = colList), size = 2, na.rm = TRUE) +
      geom_segment(aes(x = StockKeyLabel, y = landings/1000,
                       xend = StockKeyLabel, yend = 0, color = colList), size = 2, na.rm = TRUE) +
      geom_point(stat = "identity", aes(y = catches/1000,
                                        fill = colList), color = "grey50",
                 shape = 24, size = 2, alpha = 0.8, na.rm = TRUE) +
      geom_point(stat = "identity", aes(y = landings/1000,
                                        fill = colList), color = "grey50",
                 shape = 21, size = 2, alpha = 0.8, na.rm = TRUE) +
      scale_fill_manual(values = c("GREEN" = "#4daf4a",
                                   "RED" = "#e41a1c",
                                   "GREY" = "#d3d3d3")) +
      scale_color_manual(values = c("GREEN" = "#4daf4a",
                                    "RED" = "#e41a1c",
                                    "GREY" = "#d3d3d3")) +
      cap_lab +
      coord_equal() +
      coord_flip() +
      theme_bw(base_size = 7) +
      theme(legend.position = 'none',
            plot.caption = element_text(size = 6),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line( size = 0.1, color = "grey80"))
    
    
    if(return_data) {
      write.csv(x = catchBar, file = paste0(output_path, file_name, "_bar.csv"), row.names = FALSE)
      write.csv(x = kobeDat, file = paste0(output_path, file_name, "_kobe.csv"), row.names = FALSE)
    }
    
      if(return_plot) {
        return(gridExtra::grid.arrange(kobe_plot,
                                       bar_plot, ncol = 2,
                                       respect = TRUE, top = labTitle))
      }
      if(save_plot) {
        png(paste0(output_path, file_name, ".png"),
            width = fig.width,
            height = fig.height,
            units = units,
            res = res)
        p1_plot<-gridExtra::grid.arrange(kobe_plot,
                                         bar_plot, ncol = 2,
                                         respect = TRUE, top = labTitle)
        dev.off()
      }
 }
   
#' Discard rate over time
#'
#' The \code{guild_discards_fun} function returns a series of plots of discard rate and landings by fish category for an ecoregion.
#'
#' @param ecoregion ecoregion name, e.g. Greater North Sea Ecoregion
#' @param active_year numeric of the stock database version (year). e.g., 2016
#' @param data_caption print the data source as a caption, boolean.
#' @param output_path path for output to live.
#' @param file_name name for the output.
#' @param save_plot logical to save plot.
#' @param return_plot logical to return plot to current environment.
#' @param return_data logical on returning a .csv of plotted data
#'
#' @note Stocks are linked to ecoregions and fish categories via the ICES Stock database.
#' Reference points are as published in ICES Stock Assessment Graphs database. In some cases,
#' status relative to reference points may vary from published ICES advice
#' when reported F or SSB are very close to reference points (e.g., F = 0.201 > F<sub>MSY</sub> = 0.20).
#' There is an assumption that discard rates for biannual stocks and
#'  are consistent over the years that we don't provide new advice.
#'
#' @return A ggplot2 object when \code{return_plot} is \code{TRUE}, html when \code{dynamic} is \code{TRUE}
#' or .png when \code{dynamic} is \code{FALSE}. Output is saved as \code{file_name} in \code{output_path}.
#' When \code{file_name} is \code{NULL}, the file name is the ecoregion.
#' When \code{output_path} is \code{NULL}, the file is saved to "~/".
#'
#' @author Scott Large
#'
#' @examples
#' \dontrun{
#' guild_discards_fun("Greater North Sea Ecoregion", return_plot = TRUE)
#' }
#' @export
########################################
# Discard rate over time and magnitude #
########################################

# Landings and discards disaggregated by guild
guild_discards_fun <- function(ecoregion,
                               active_year = 2017,
                               cap_month = "bla",
                               cap_year = "2018",
                               output_path = NULL,
                               save_plot = FALSE,
                               return_plot = TRUE,
                               return_data = FALSE,
                               file_name = NULL) {
  
  if(save_plot) {
    if(is.null(file_name)) {
      file_name <- gsub("\\s", "_", ecoregion)
    }
  }
    
    if(is.null(output_path)) {
      output_path <- "~/"
    }
  
    cap_lab <- labs(x = "", y = "Discards and landings (thousand tonnes)",
                    title = "b)",
                    caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                                      cap_year,
                                      cap_month))
  
  # The whole bit here is to make the assumption that discard rates for biannual stocks and
  # and are consistent over the years that we don't provide new advice.
  p3_dat_ts <-  stock_catch_full %>%
    filter(grepl(ecoregion, EcoRegion))
  
  p3_dat_ts <-  stock_catch(active_year) %>%
    filter(grepl(ecoregion, EcoRegion))
  
  if(ecoregion == "Baltic Sea Ecoregion"){
    # Get rid of crustacean and elasmobranchs in the Baltic... to appease ADGFO
    p3_dat_ts <- p3_dat_ts %>%
      filter(!FisheriesGuild %in% c("elasmobranch",
                                    "crustacean"))
  }

  p3_dat_ts <- p3_dat_ts %>%
    filter(Year %in% seq(active_year-5, active_year -1))
  
  p3_dat_na <- p3_dat_ts %>%
    tidyr::expand(Year, tidyr::nesting(StockKeyLabel, YearOfLastAssessment,
                                       Description, FisheriesGuild, EcoRegion)) %>%
    left_join(p3_dat_ts,
              by = c("Year", "StockKeyLabel", "YearOfLastAssessment",
                     "Description", "FisheriesGuild", "EcoRegion"))
  
  p3_dat_dcds <- p3_dat_na %>%
    select(StockKeyLabel, Year, discards, YearOfLastAssessment) %>%
    group_by(StockKeyLabel) %>%
    tidyr::spread(Year, discards) %>%
    mutate(`2017` = ifelse(YearOfLastAssessment == 2017 &
                             is.na(`2017`) &
                             !is.na(`2016`),
                           `2016`,
                           `2017`)) %>%
    tidyr::gather(Year, discards, `2013`:`2017`) %>%
    mutate(Year = as.numeric(Year),
           discards = as.numeric(discards))
  
  p3_dat_lnding <- p3_dat_na %>%
    select(StockKeyLabel, Year, landings, YearOfLastAssessment) %>%
    group_by(StockKeyLabel) %>%
    tidyr::spread(Year, landings) %>%
    mutate(`2017` = ifelse(YearOfLastAssessment == 2017 &
                             is.na(`2017`) &
                             !is.na(`2016`),
                           `2016`,
                           `2017`)) %>%
    tidyr::gather(Year, landings, `2013`:`2017`) %>%
    mutate(Year = as.numeric(Year),
           landings = as.numeric(landings))


p3_dat <- p3_dat_na %>%
  select(-discards,
         -landings) %>%
  left_join(p3_dat_dcds, by = c("Year", "StockKeyLabel", "YearOfLastAssessment")) %>%
  left_join(p3_dat_lnding, by = c("Year", "StockKeyLabel", "YearOfLastAssessment")) %>%
  group_by(Year, FisheriesGuild) %>%
  summarize(guildLandings = sum(landings, na.rm = TRUE)/ 1000,
            guildDiscards = sum(discards, na.rm = TRUE)/ 1000)

p3_rate <- p3_dat %>%
  mutate(guildRate = guildDiscards/ (guildLandings + guildDiscards)) %>%
  tidyr::gather(variable, value, -Year, -FisheriesGuild) %>%
  filter(!variable %in% c("guildDiscards", "guildLandings"))

p3_bar <- p3_dat %>%
  filter(Year == active_year - 1) %>%
  tidyr::gather(variable, value, -Year, -FisheriesGuild) %>%
  ungroup() %>%
  select(-Year)

p3_bar_order <- p3_bar %>%
  group_by(FisheriesGuild) %>%
  summarize(total = sum(value, na.rm = TRUE)) %>%
  arrange(-total) %>%
  ungroup() %>%
  mutate(FisheriesGuild = factor(FisheriesGuild, FisheriesGuild))

p3_bar$FisheriesGuild <- factor(p3_bar$FisheriesGuild,
                                levels = p3_bar_order$FisheriesGuild[order(p3_bar_order$total)])

p3_rate_plot <- ggplot(ungroup(p3_rate),
                       aes(x = Year,
                           y = value,
                           color = FisheriesGuild)) +
  geom_line() +
  ggrepel::geom_label_repel(data = p3_rate %>% filter(Year == active_year - 1),
                            aes(label = FisheriesGuild,
                                color = FisheriesGuild,
                                fill = FisheriesGuild),
                            nudge_x = 1,
                            label.size = 0.2,
                            segment.size = 0.25,
                            size = 2,
                            color = 'white',
                            force = 2,
                            segment.color = 'grey60') +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(min(p3_rate$Year, na.rm = TRUE),
                                  max(p3_rate$Year, na.rm = TRUE), by = 1)) +
  geom_segment(aes(x = -Inf, xend = max(p3_rate$Year, na.rm = TRUE),
                   y = -Inf, yend = -Inf), color = "grey50") +
  geom_segment(aes(y = -Inf, yend = Inf,
                   x = -Inf, xend = -Inf), color = "grey50")+
  expand_limits(x = c(min(p3_rate$Year, na.rm = TRUE), active_year + 1)) + # So that we have enough room along x-axis for labels.
  scale_color_brewer(type = "qual", palette = "Set2") +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  theme_bw(base_size = 9) +
  theme(legend.position = "none",
        plot.caption = element_text(size = 6),
        panel.grid = element_blank(),
        legend.key = element_rect(colour = NA)) +
  labs(x = "Year", y = "Discard rate", caption = "", title = "a)")


p3_bar_plot <- ggplot(ungroup(p3_bar),
                      aes(x = FisheriesGuild, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_color_brewer(type = "qual", palette = "Dark2", direction = -1) +
  scale_fill_brewer(type = "qual", palette = "Dark2", direction = -1) +
  coord_flip() +
  theme_bw(base_size = 9) +
  theme(legend.position = "none",
        plot.caption = element_text(size = 6),
        panel.grid = element_blank(),
        legend.key = element_rect(colour = NA)) +
  cap_lab + labs(x = "", y = "Discards and landings(thousand tonnes)",title = "b)")

if(return_data) {
  write.csv(x = p3_rate, file = paste0(output_path, file_name, "_rate.csv"), row.names = FALSE)
  write.csv(x = p3_bar, file = paste0(output_path, file_name, "_bar.csv"), row.names = FALSE)
}

if(return_plot){
  return(gridExtra::grid.arrange(p3_rate_plot,
                                 p3_bar_plot,
                                 ncol = 2,
                                 respect = TRUE))
}
if(save_plot){
  png(paste0(output_path, file_name, ".png"),
      width = 170,
      height = 100.5,
      units = "mm",
      res = 300)
  
  gridExtra::grid.arrange(p3_rate_plot,
                          p3_bar_plot,
                          ncol = 2,
                          respect = TRUE)
  dev.off()
}
}

#' Landings over time by country, guild, or species
#'
#' The \code{ices_catch_plot} function returns an area or line plot of landings (historic and official catch) for an ecoregion by country,
#' guild, or species.
#'
#' @param ecoregion ecoregion name, e.g. Greater North Sea Ecoregion
#' @param type the variable that will be used to group and display data: COMMON_NAME, GUILD, or COUNTRY
#' @param line_count number of lines to display
#' @param plot_type area or line plot
#' @param data_caption print the data source as a caption, boolean.
#' @param output_path path for output to live.
#' @param file_name name for the output.
#' @param save_plot logical to save plot.
#' @param return_plot logical to return plot to current environment.
#' @param return_data logical on returning a .csv of plotted data
#' @param fig.width width pf combined set of plots
#' @param fig.height height of combined set of plots
#' @param text.size = size of text in plots
#'
#' @note Historic and official nominal catch are actually only the landings and do not account for discards, misreporting, or other
#' potential issues.
#'
#' @return A ggplot2 object when \code{return_plot} is \code{TRUE} or .png when \code{save_plot} is \code{TRUE}.
#' Output is saved as \code{file_name} in \code{output_path}.
#' When \code{file_name} is \code{NULL}, the file name is the ecoregion.
#' When \code{output_path} is \code{NULL}, the file is saved to "~/".
#'
#' @author Scott Large
#'
#' @examples
#' \dontrun{
#' ices_catch_plot("Greater North Sea Ecoregion", type = "COMMON_NAME", return_plot = TRUE, line_count = 4)
#' }
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Landings over time by country, guild, or species #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
ices_catch_plot <- function(ecoregion, 
                            type = c("COMMON_NAME", "COUNTRY", "GUILD")[1],
                            line_count = 4,
                            # start_year = 1990,
                            plot_type = c("line", "area")[1],
                            cap_month = "August",
                            cap_year = "2018",
                            return_data = FALSE,
                            file_name = NULL,
                            output_path= NULL,
                            text.size = 9) {
  
  if(return_data) {
    if(is.null(file_name)) {
      file_name <- gsub("\\s", "_", ecoregion)
    }

    if(is.null(output_path)) {
      output_path <- "~/"
    }
  }
  
    cap_lab <-labs(x = "",
                     y = "Landings (thousand tonnes)",
                     caption = sprintf("Historical Nominal Catches 1950-2010, \nOfficial Nominal Catches 2006-2016, \nPreliminary Catches 2017. Accessed %s/%s. ICES, Copenhagen.",
                                       cap_year,
                                       cap_month))
  
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
  
  if(plot_type == "area") {
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
  }
  
  if(plot_type == "line") {
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
    
  }
  
  if(return_data) {
    write.csv(x = catchPlot, file = paste0(output_path, file_name, ".csv"), row.names = FALSE)
  }
  
    return(pl)
}

#' STECF Landings over time by country, guild, or species
#'
#' The \code{stecf_plot} function returns an area or line plot of landings (historic and official catch) for an ecoregion by country
#' guild, or species.
#'
#' @param ecoregion ecoregion name, e.g. Greater North Sea Ecoregion
#' @param metric the value to plot e.g., EFFORT or LANDINGS
#' @param type the variable that will be used to group and display data: COMMON_NAME, GUILD, or COUNTRY
#' @param line_count number of lines to display
#' @param plot_type area or line plot
#' @param data_caption print the data source as a caption, boolean.
#' @param output_path path for output to live.
#' @param file_name name for the output.
#' @param save_plot logical to save plot.
#' @param return_plot logical to return plot to current environment.
#' @param return_data logical on returning a .csv of plotted data
#' @param fig.width width pf combined set of plots
#' @param fig.height height of combined set of plots
#' @param text.size = size of text in plots
#'
#' @note Some considerable errors have been identified in the STECF data. Finland and Estonia effort data are not reliable,
#' and Germany recorded an erroneous haul in 2013. These values have been removed.
#'
#' @return A ggplot2 object when \code{return_plot} is \code{TRUE} or .png when \code{save_plot} is \code{TRUE}.
#' Output is saved as \code{file_name} in \code{output_path}.
#' When \code{file_name} is \code{NULL}, the file name is the ecoregion.
#' When \code{output_path} is \code{NULL}, the file is saved to "~/".
#'
#' @author Scott Large
#'
#' @examples
#' \dontrun{
#' stecf_plot("Greater North Sea Ecoregion", metric = "EFFORT", type = "GEAR", return_plot = TRUE, line_count = 4)
#' }
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Landings over time by country, guild, or species #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
stecf_plot <- function(ecoregion,
                       metric = c("EFFORT", "LANDINGS")[1],
                       type = c("GEAR", "COUNTRY")[1],
                       line_count = 4,
                       plot_type = c("line", "area")[1],
                       cap_month =" May",
                       cap_year = "2018",
                       stecf_report = "17-09",
                       return_data = FALSE,
                       file_name = NULL,
                       output_path = NULL,
                       text.size = 9,
                       ...) {
  
  if(metric == "EFFORT"){
    allDat <- stecf_effort_clean %>%
      filter(ECOREGION == ecoregion) %>%
      rename_(.dots = setNames(c(type, "EFFORT"),
                               c("type_var", "VALUE")))
  }
  
  if(metric == "LANDINGS"){
    if(type == "COUNTRY") stop("You should use the ices_catch_plot() function instead.")
    
    allDat <- stecf_landings_clean %>%
      filter(ECOREGION == ecoregion) %>%
      rename_(.dots = setNames(c(type, "LANDINGS"),
                               c("type_var", "VALUE")))
  }
  if(return_data) {
    if(is.null(file_name)) {
      file_name <- gsub("\\s", "_", ecoregion)
    }
    
    if(is.null(output_path)) {
      output_path <- "~/"
    }
  }
  
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
    
    if(metric == "EFFORT"){
      catchLabel <- "Nominal effort (1000 kW days at sea)"
    }
    if(metric == "LANDINGS"){
      catchLabel <- "Landings (thousand tonnes)"
    }
  } else {
    if(metric == "EFFORT"){
      catchLabel <- "Nominal effort (kW days at sea)"
    }
    if(metric == "LANDINGS"){
      catchLabel <- "Landings (tonnes)"
    }
  }
  #
  catchPlot <- rbind(catchPlot[!catchPlot$type_var == "other",],
                     catchPlot[catchPlot$type_var == "other",])
  
  my_caption = sprintf("STECF %s. Accessed %s/%s.",
                    stecf_report,
                    cap_month,
                    cap_year)
  
    cap_lab <- labs(title = "", x = "", y = catchLabel,
                    caption = my_caption)
    
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
  
  if(plot_type == "area"){
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
                         alpha = .9,
                         position = "stack")
    
    pl <- pl + ggrepel::geom_label_repel(data = cumPlot,
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
  }
  
  
  # if(save_plot) {
  #   ggsave(filename = paste0(output_path, file_name, ".png"),
  #          plot = pl,
  #          width = fig.width,
  #          height = fig.height,
  #          units = "mm",
  #          dpi = 300)
  #   
  # }
  
  if(return_data) {
    write.csv(x = catchPlot, file = paste0(output_path, file_name, ".csv"), row.names = FALSE)
  }
    return(pl)
}

