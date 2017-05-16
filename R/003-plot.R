#' Render html stock summary table
#'
#' This function returns "Status of stock summary relative to reference points" for all stocks
#' in an ecoregion.
#'
#' @param ecoregion ecoregion name, e.g. Greater North Sea
#' @param table_type type of table, "both" returns both "dynamic" (using DT) and "static" (using ReporteRs) html tables.
#' @param output_path path for output to live.
#' @param file_name name for the output.
#'
#' @note Stocks are linked to ecoregions via the ICES Stock database. Reference points are as published in ICES Stock Assessment
#' Graphs database. In some cases, status relative to reference points may vary from
#' published ICES advice when reported F or SSB are very close to reference points (e.g., F = 0.201 > F<sub>MSY</sub> = 0.20).
#'
#' @return A html file. When \code{file_name} is \code{NULL}, the file name is the ecoregion.
#' When \code{output_path} is \code{NULL}, the file is saved to "~/". When \code{table_type} is \code{"static"} or \code{"both"},
#' it might take a bit of time...
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
# ~~~~~~~~~~~~~~~~~~~~ #
stockSummaryTable_fun <- function(ecoregion,
                                  table_type = c("static", "dynamic", "both")[2],
                                  output_path = NULL,
                                  file_name = NULL) {

  if(is.null(fileName)) {
    file_name <- gsub("\\s", "_", ecoregion)
  }

  if(is.null(output_path)) {
    output_path <- "~/"
  }

  summary_table_frmt[summary_table_frmt == "GREEN"] <- "<i class=\"glyphicon glyphicon-ok-sign\" style=\"color:green; font-size:2.2em\"></i>"
  summary_table_frmt[summary_table_frmt == "RED"] <- "<i class=\"glyphicon glyphicon-remove-sign\" style=\"color:red; font-size:2.2em\"></i>"
  summary_table_frmt[summary_table_frmt == "GREY"] <- "<i class=\"glyphicon glyphicon-question-sign\" style=\"color:grey; font-size:2.2em\"></i>"
  summary_table_frmt[summary_table_frmt == "ORANGE"] <- "<i class=\"glyphicon glyphicon-record\" style=\"color:#FAB700; font-size:2.2em\"></i>"

  summary_table_frmt <- data.frame(lapply(summary_table_frmt, factor))

  stockPlot <- summary_table_frmt %>%
    filter(grepl(pattern = ecoregion, EcoRegion)) %>%
    select(-EcoRegion) %>%
    distinct(.keep_all = TRUE) %>%
    arrange(StockCode)

  if(table_type %in% c("static", "both")) {
    suppressWarnings(
      rmarkdown::render(system.file("rmd/stockSummaryTable-static.Rmd", package = "fisheryO"),
                        # "~/git/ices-dk/fisheryO/vignettes/stockSummaryTable-static.Rmd",
                        output_file = paste0(output_path, file_name, "-static.html"),
                        # rmarkdown::html_document(template = NULL),
                        envir = new.env())
    )
  }

  if(table_type %in% c("dynamic", "both")) {
    suppressWarnings(
      rmarkdown::render(system.file("rmd/stockSummaryTable-dynamic.Rmd", package = "fisheryO"),
                        # "~/git/ices-dk/fisheryO/vignettes/stockSummaryTable-dynamic.rmd",
                        output_file = paste0(output_path, file_name, "-dynamic.html"),
                        # output_file = paste0("~/git/ices-dk/fisheryO/output/", fileName, "-dynamic.html"),
                        rmarkdown::html_document(template = NULL),
                        envir = new.env())
    )
  }
}

#' Pie chart of proportion of stocks relative to reference points
#'
#' This function returns pie charts of the proportion of stocks
#' relative to reference points for fish categories in an ecoregion.
#'
#' @param ecoregion ecoregion name, e.g. Greater North Sea
#' @param output_path path for output to live.
#' @param file_name name for the output.
#' @param save_plot logical to save plot.
#' @param return_plot logical to return plot to current environment.
#'
#' @note Stocks are linked to ecoregions and fish categories via the ICES Stock database.
#' Reference points are as published in ICES Stock Assessment Graphs database. In some cases,
#' status relative to reference points may vary from published ICES advice
#'  when reported F or SSB are very close to reference points (e.g., F = 0.201 > F<sub>MSY</sub> = 0.20).
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
                         file_name,
                         save_plot = FALSE,
                         return_plot = TRUE,
                         output_path = NULL) {

  if(save_plot) {
    if(is.null(file_name)) {
      file_name <- gsub("\\s", "_", ecoregion)
    }

    if(is.null(output_path)) {
      output_path <- "~/"
    }
  }

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
                                    "FMSY" = "Fishing pressure\n MSY",
                                    "BMSY" = "Stock size\n MSY",
                                    "FPA" = "Fishing pressure\n PA",
                                    "BPA" = "Stock size \n PA",
                                    "SBL" = "Within safe\n biological limits"),
           FisheriesGuild = factor(FisheriesGuild,
                                   levels = c("total", "benthic", "crustacean", "elasmobranch", "demersal", "pelagic"))
    )

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
    labs(title = "", x = "", y = "",
         caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                           lubridate::year(Sys.time()),
                           lubridate::month(Sys.time(), label = TRUE, abbr = FALSE))) +
    coord_polar(theta = "y", direction = 1) +
    facet_grid(FisheriesGuild ~ VARIABLE)

  if(return_plot) {
    return(p1)
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
#' This function returns pie charts of the proportion of stocks
#' relative to GES reference points in an ecoregion.
#'
#' @param ecoregion ecoregion name, e.g. Greater North Sea
#' @param output_path path for output to live.
#' @param file_name name for the output.
#' @param save_plot logical to save plot.
#' @param return_plot logical to return plot to current environment.
#'
#' @note Stocks are linked to ecoregions via the ICES Stock database.
#' Reference points are as published in ICES Stock Assessment Graphs database. In some cases,
#' status relative to reference points may vary from published ICES advice
#'  when reported F or SSB are very close to reference points (e.g., F = 0.201 > F<sub>MSY</sub> = 0.20).
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
                       file_name,
                       save_plot = FALSE,
                       return_plot = TRUE,
                       output_path = NULL) {

  if(save_plot) {
    if(is.null(file_name)) {
      file_name <- gsub("\\s", "_", ecoregion)
    }

    if(is.null(output_path)) {
      output_path <- "~/"
    }
  }

  colList <- c("GREEN" = "#00B26D",
               "GREY" = "#d3d3d3",
               "RED" = "#d93b1c")

  rowDat <- ges_table %>%
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
              size = 2.5) +
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
    labs(title = "", x = "", y = "",
         caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                           lubridate::year(Sys.time()),
                           lubridate::month(Sys.time(), label = TRUE, abbr = FALSE))) +
    coord_polar(theta = "y") +
    facet_grid(METRIC ~ VARIABLE)

  if(return_plot) {
    return(p1)
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
#' This function returns a series of line plots of F and SSB relative to F<sub>MSY</sub> and MSY B<sub>trigger</sub>
#' reference points for stocks of a fish category for an ecoregion.
#'
#' @param EcoGuild combined ecoregion name and fish category, e.g. Greater North Sea
#' @param dynamic logical to generate html output with dynamic features.
#' @param output_path path for output to live.
#' @param file_name name for the output.
#' @param save_plot logical to save plot.
#' @param return_plot logical to return plot to current environment.
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
#' stock_trends_fun("Greater North Sea Ecoregion", return_plot = TRUE)
#' }
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~#
# Stock Status over time #
#~~~~~~~~~~~~~~~~~~~~~~~~#
stock_trends_fun <- function(EcoGuild,
                             dynamic = TRUE,
                             file_name = NULL,
                             save_plot = FALSE,
                             return_plot = TRUE,
                             output_path = NULL) {

  clicks <- sag_complete_summary %>%
    mutate(onclick = sprintf("window.open(\"%s%i/%i/%s.pdf\")",
                             "http://ices.dk/sites/pub/Publication%20Reports/Advice/",
                             YearOfLastAssessment,
                             YearOfLastAssessment,
                             StockCode)) %>%
    select(StockCode,
           Description,
           onclick) %>%
    distinct(.keep_all = TRUE)

  p1_dat <- stock_trends_frmt %>%
    left_join(clicks, by = c("lineGroup" = "StockCode")) %>%
    filter(grepl(EcoGuild, pageGroup)) %>%
    mutate(tooltip_line =   sprintf("<b>%s</b>",
                                    ifelse(lineGroup == "MEAN",
                                           "mean",
                                           Description)),
           plotGroup = factor(plotGroup,
                              labels = c("F/F[MSY]", "SSB/MSY~B[trigger]"))) %>%
    select(-Description)

  if(length(unique(p1_dat$lineGroup)) <= 2){
    p1_dat <- p1_dat %>%
      filter(lineGroup != "MEAN")
  }

  adj_names <- sort(setdiff(unique(p1_dat$lineGroup), "MEAN"))
  if(length(adj_names) > 10) {
    values <- rep("#7F7F7F", length(adj_names))
    legend_pos <- "none"
  }
  if(length(adj_names) <= 10) {
    values <- ggthemes::tableau_color_pal('tableau10')(length(adj_names))
    # values = gg_color_hue(length(adj_names))
    legend_pos <- "bottom"
  }

  names(values) <- adj_names
  values <- c(values, c(MEAN = "black"))

  if(save_plot) {
    if(is.null(file_name)) {
      file_name <- gsub("\\s", "_", EcoGuild)
      file_name <- gsub("_-_", "-", file_name)
    }

    if(is.null(output_path)) {
      output_path <- "~/"
    }
  }

  plot_title <- gsub(".*\\s-\\s", "\\1", EcoGuild)
  plot_title <- gsub(" stocks", "", plot_title)

  p1_plot <- ggplot(p1_dat %>% filter(lineGroup != "MEAN"),
                    aes(x = Year, y = plotValue,
                        color = lineGroup,
                        fill = lineGroup,
                        onclick = onclick,
                        data_id = lineGroup,
                        tooltip = tooltip_line)) +
    geom_hline(yintercept = 1, col = "grey60") +
    theme_bw(base_size = 9) +
    scale_color_manual(values = values) +
    scale_fill_manual(values = values) +
    guides(fill = FALSE) +
    theme(legend.position = legend_pos,
          strip.text = element_text(size = 9, angle = 0, hjust = 0),
          strip.background = element_blank(),
          strip.placement = "outside",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = NA),
          plot.caption = element_text(size = 6)) +
    labs(title = plot_title, x = "Year", y = "", color = "Stock code",
         caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                           lubridate::year(Sys.time()),
                           lubridate::month(Sys.time(), label = TRUE, abbr = FALSE))) +
    facet_wrap(~ plotGroup, labeller = label_parsed, strip.position = "left")

  if(dynamic) {
    p1_plot <- p1_plot + geom_line_interactive(alpha = 0.8)
    p1_plot <- p1_plot + geom_line_interactive(data = p1_dat %>% filter(lineGroup == "MEAN"),
                                               alpha = 0.9, size = 1.15)

    if(return_plot){
      return(ggiraph::ggiraph(code = print(p1_plot),
                              hover_css = "cursor:pointer;stroke:black;stroke-width:3pt;"))
    }

    if(save_plot) {
      suppressWarnings(
        rmarkdown::render(system.file("rmd/stockStatusTrends-dynamic.Rmd", package = "fisheryO"),
          # "~/git/ices-dk/fisheryO/vignettes/stockStatusTrends-dynamic.rmd",
                          output_file = paste0(output_path, file_name, "_", EcoGuild, "-dynamic.html"),
                          rmarkdown::html_document(template = NULL),
                          envir = new.env())
      )
    }
  }

  if(!dynamic) {
    p1_plot <- p1_plot + geom_line(alpha = 0.8)
    p1_plot <- p1_plot + geom_line(data = p1_dat %>% filter(lineGroup == "MEAN"),
                                   alpha = 0.9, size = 1.15)

    if(return_plot) {
      return(p1_plot)
    }

    if(save_plot) {
    ggsave(filename = paste0(output_path, file_name, "_", EcoGuild, "-static.png"),
           plot = p1_plot,
           width = 170,
           height = 100.5,
           units = "mm",
           dpi = 300)
    }
  }
}


#' Kobe plot of stock status
#'
#' This function returns a 2 plots: a scatter plot of F/F<sub>MSY</sub> and SSB/MSY B<sub>trigger</sub>
#' by fish category and ecoregion and a "lollipop" plot of total catch (divided into discards and landings) by stock.
#'
#' @param ecoregion ecoregion name, e.g. Greater North Sea Ecoregion
#' @param guild fish category (options: "all", "benthic", "demersal", "pelagic", "crustacean", "elasmobranch", "large-scale stocks"), e.g. demersal
#' @param dynamic logical to generate html output with dynamic features.
#' @param output_path path for output to live.
#' @param file_name name for the output.
#' @param save_plot logical to save plot.
#' @param return_plot logical to return plot to current environment.
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
                      guild = c("all",
                                "benthic",
                                "demersal",
                                "pelagic",
                                "crustacean",
                                "elasmobranch",
                                "large-scale stocks")[1],
                      output_path = NULL,
                      return_plot = TRUE,
                      save_plot = FALSE,
                      catch_limit = 0,
                      file_name = NULL,
                      plotTitle = NULL,
                      fig.width = 110,
                      fig.height = 75,
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

  if(any(guild %in% "all")) {
    guild <- c("benthic", "demersal", "pelagic", "crustacean", "elasmobranch")
    labTitle <- "All stocks"
  }

  kobeDat <- stock_status_full %>%
    filter(EcoRegion == ecoregion,
           FisheriesGuild %in% guild,
           !is.na(F_FMSY),
           !is.na(SSB_MSYBtrigger)) %>%
    group_by(StockCode) %>%
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

  if(length(guild) >= 2) {
    kobeDat <- filter(kobeDat, total >= catch_limit)
  }
  if(nrow(kobeDat) != 0) {
    labs <- seq(0, max(kobeDat$F_FMSY, kobeDat$SSB_MSYBtrigger, na.rm = TRUE) + 1)
    kobe_plot <- ggplot(kobeDat, aes(x = F_FMSY, y = SSB_MSYBtrigger,
                                     data_id = StockCode,
                                     tooltip = tip)) +
      geom_point(aes(color = colList), size = 2,
                 alpha = 0.7) +
      geom_hline(yintercept = 1, color = "grey60", linetype = "dashed") +
      geom_vline(xintercept = 1, color = "grey60", linetype = "dashed") +
      geom_text_repel(aes(label = StockCode),
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
           caption ="") +
      theme_bw(base_size = 7) +
      theme(legend.position = 'none',
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.caption = element_text(size = 6))

    # Lollipop plot
    catchBar <- stock_status_full %>%
      filter(EcoRegion == ecoregion,
             FisheriesGuild %in% guild) %>%
      distinct(.keep_all = TRUE) %>%
      group_by(StockCode) %>%
      mutate(total = ifelse(all(is.na(catches) & is.na(landings)),
                            NA,
                            max(catches, landings, na.rm = TRUE))) %>%
      ungroup() %>%
      arrange(!is.na(total), total) %>%
      mutate(StockCode = factor(StockCode, StockCode))

    if(length(guild) >= 2) {
      catchBar <- filter(catchBar, total >= catch_limit)
    }

    bar_plot <-
      ggplot(catchBar, aes(x = StockCode, y = catches)) +
      geom_segment(aes(x = StockCode, y = catches,
                       xend = StockCode, yend = 0, color = colList), size = 2) +
      geom_segment(aes(x = StockCode, y = landings,
                       xend = StockCode, yend = 0, color = colList), size = 2) +
      geom_point(stat = "identity", aes(y = catches,
                                        fill = colList), color = "grey50",
                 shape = 24, size = 2, alpha = 0.8) +
      geom_point(stat = "identity", aes(y = landings,
                                        fill = colList), color = "grey50",
                 shape = 21, size = 2, alpha = 0.8) +
      scale_fill_manual(values = c("GREEN" = "#4daf4a",
                                   "RED" = "#e41a1c",
                                   "GREY" = "#d3d3d3")) +
      scale_color_manual(values = c("GREEN" = "#4daf4a",
                                    "RED" = "#e41a1c",
                                    "GREY" = "#d3d3d3")) +
      labs(x = "Stock",
           y = "Catch and landings (tonnes)",
           caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                             lubridate::year(Sys.time()),
                             lubridate::month(Sys.time(), label = TRUE, abbr = FALSE))) +
      coord_equal() +
      coord_flip() +
      theme_bw(base_size = 7) +
      theme(legend.position = 'none',
            plot.caption = element_text(size = 6),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line( size = 0.1, color = "grey80"))

    if(!dynamic) {
      if(return_plot) {
        return(grid.arrange(kobe_plot,
                     bar_plot, ncol = 2,
                     respect = TRUE, top = guild))
      }
      if(save_plot) {
        png(paste0(output_path, file_name, "-", guild, ".png"),
            width = fig.width,
            height = fig.height,
            units = units,
            res = res)
        grid.arrange(kobe_plot,
                     bar_plot, ncol = 2,
                     respect = TRUE, top = guild)
        dev.off()
      }
    }

    if(dynamic) {
      kobe_plot <- kobe_plot +  geom_point_interactive(color = "white",
                                                       fill = "white",
                                                       shape = 21,
                                                       size = 2,
                                                       alpha = 0.01)
      if(length(guild) > 1) guild = "all"

      if(return_plot){
        return(ggiraph::ggiraph(code = gridExtra::grid.arrange(kobe_plot, bar_plot,
                                                               ncol = 2,
                                                               respect = FALSE),
                                zoom_max = 5))
        }

      if(save_plot){
        suppressWarnings(
          rmarkdown::render(system.file("rmd/kobe-dynamic.Rmd", package = "fisheryO"),
                            # "~/git/ices-dk/fisheryO/vignettes/kobe-dynamic.rmd",
                            output_file = paste0(output_path, file_name, "-", guild, "html"),
                            envir = new.env())
        )
      }
    }
  } else ("No stocks have MSY status")
}

#' Discard rate over time
#'
#' This function returns a series of plots of discard rate and landings by fish category for an ecoregion.
#'
#' @param ecoregion ecoregion name, e.g. Greater North Sea Ecoregion
#' @param output_path path for output to live.
#' @param file_name name for the output.
#' @param save_plot logical to save plot.
#' @param return_plot logical to return plot to current environment.
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
                               output_path = NULL,
                               save_plot = FALSE,
                               return_plot = TRUE,
                               file_name = NULL) {

  if(save_plot) {
    if(is.null(file_name)) {
      file_name <- gsub("\\s", "_", ecoregion)
    }

    if(is.null(output_path)) {
      output_path <- "~/"
    }
  }

  # The whole bit here is to make the assumption that discard rates for biannual stocks and
  # and are consistent over the years that we don't provide new advice.
  p3_dat_ts <-  stock_catch_full %>%
    filter(grepl(ecoregion, EcoRegion),
           Year >= 2012,
           Year <= 2015)

  p3_dat_na <- p3_dat_ts %>%
    expand(Year, nesting(StockCode, YearOfLastAssessment,
                         Description, FisheriesGuild, EcoRegion)) %>%
    left_join(p3_dat_ts,
              by = c("Year", "StockCode", "YearOfLastAssessment",
                     "Description", "FisheriesGuild", "EcoRegion"))

  p3_dat_dcds <- p3_dat_na %>%
    select(StockCode, Year, discards, YearOfLastAssessment) %>%
    group_by(StockCode) %>%
    spread(Year, discards) %>%
    mutate(`2015` = ifelse(YearOfLastAssessment == 2015 &
                             is.na(`2015`) &
                             !is.na(`2014`),
                           `2014`,
                           `2015`)) %>%
    gather(Year, discards, `2012`:`2015`) %>%
    mutate(Year = as.numeric(Year),
           discards = as.numeric(discards))

  p3_dat_lnding <- p3_dat_na %>%
    select(StockCode, Year, landings, YearOfLastAssessment) %>%
    group_by(StockCode) %>%
    spread(Year, landings) %>%
    mutate(`2015` = ifelse(YearOfLastAssessment == 2015 &
                             is.na(`2015`) &
                             !is.na(`2014`),
                           `2014`,
                           `2015`)) %>%
    gather(Year, landings, `2012`:`2015`) %>%
    mutate(Year = as.numeric(Year),
           landings = as.numeric(landings))

  p3_dat <- p3_dat_na %>%
    select(-discards,
           -landings) %>%
    left_join(p3_dat_dcds, by = c("Year", "StockCode", "YearOfLastAssessment")) %>%
    left_join(p3_dat_lnding, by = c("Year", "StockCode", "YearOfLastAssessment")) %>%
    group_by(Year, FisheriesGuild) %>%
    summarize(guildLandings = sum(landings, na.rm = TRUE)/ 1000,
              guildDiscards = sum(discards, na.rm = TRUE)/ 1000)

  p3_rate <- p3_dat %>%
    mutate(guildRate = guildDiscards/ (guildLandings + guildDiscards)) %>%
    gather(variable, value, -Year, -FisheriesGuild) %>%
    filter(!variable %in% c("guildDiscards", "guildLandings"))

  p3_bar <- p3_dat %>%
    filter(Year == 2015) %>%
    gather(variable, value, -Year, -FisheriesGuild) %>%
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

  p3_rate_plot <- ggplot(p3_rate,
                         aes(x = Year,
                             y = value,
                             color = FisheriesGuild)) +
    geom_line() +
    geom_label_repel(data = p3_rate %>% filter(Year == 2015),
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
    expand_limits(x = c(min(p3_rate$Year, na.rm = TRUE), 2017)) + # So that we have enough room along x-axis for labels.
    scale_color_brewer(type = "qual", palette = "Set2") +
    scale_fill_brewer(type = "qual", palette = "Set2") +
    theme_bw(base_size = 9) +
    theme(legend.position = "none",
          plot.caption = element_text(size = 6),
          panel.grid.major = element_blank(),
          legend.key = element_rect(colour = NA)) +
    labs(x = "Year", y = "Discard rate", caption = "", title = "a)")


  p3_bar_plot <- ggplot(p3_bar,
                        aes(x = FisheriesGuild, y = value, fill = variable)) +
    geom_bar(stat = "identity") +
    scale_color_brewer(type = "qual", palette = "Set1") +
    scale_fill_brewer(type = "qual", palette = "Set1") +
    coord_flip() +
    theme_bw(base_size = 9) +
    theme(legend.position = "none",
          plot.caption = element_text(size = 6),
          panel.grid.major = element_blank(),
          legend.key = element_rect(colour = NA)) +
    labs(x = "", y = "Discards and landings (thousand tonnes)",
         title = "b)",
         caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                           lubridate::year(Sys.time()),
                           lubridate::month(Sys.time(), label = TRUE, abbr = FALSE)))

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
#' This function returns an area or line plot of landings (historic and official catch) for an ecoregion by country,
#' guild, or species.
#'
#' @param ecoregion ecoregion name, e.g. Greater North Sea Ecoregion
#' @param type the variable that will be used to group and display data: COMMON_NAME, GUILD, or COUNTRY
#' @param line_count number of lines to display
#' @param plot_type area or line plot
#' @param output_path path for output to live.
#' @param file_name name for the output.
#' @param save_plot logical to save plot.
#' @param return_plot logical to return plot to current environment.
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
ices_catch_plot <- function(ecoregion, #IA = unique(allDat$ECOREGION)[1],
                            type = c("COMMON_NAME", "COUNTRY", "GUILD")[1],
                            line_count = 4,
                            # start_year = 1990,
                            plot_type = c("line", "area")[1],
                            fig_name = "figure2",
                            save_plot = FALSE,
                            return_plot = TRUE,
                            fig.width = 174,
                            fig.height = 68,
                            text.size = 9) {

  if(line_count >= 10) warning("Color scales are hard to see beyond this point... try plotting fewer categories.")
  if(line_count == 14) stop("Color palette only has 14 colors... sorry.")

  if(save_plot) {
    if(is.null(file_name)) {
      file_name <- gsub("\\s", "_", ecoregion)
    }

    if(is.null(output_path)) {
      output_path <- "~/"
    }
  }

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

  my_caption <- sprintf("Historical Nominal Catches 1950-2010, \nOfficial Nominal Catches 2006-2014. Accessed %s/%s. ICES, Copenhagen.",
                        lubridate::year(Sys.time()),
                        lubridate::month(Sys.time(), label = TRUE, abbr = FALSE))

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
    pl <- pl + geom_label_repel(data = cumPlot,
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
  #
  if(return_plot) {
    return(pl)
  }

  if(save_plot) {
    ggsave(filename = paste0(output_path, file_name, ".png"),
             # paste0("~/git/ices-dk/fisheryO/output/", fig_name, "_", IA, ".png"),
           plot = pl,
           width = fig.width,
           height = fig.height,
           units = "mm",
           dpi = 300)
  }
}



#' STECF Landings over time by country, guild, or species
#'
#' This function returns an area or line plot of landings (historic and official catch) for an ecoregion by country
#' guild, or species.
#'
#' @param ecoregion ecoregion name, e.g. Greater North Sea Ecoregion
#' @param metric the value to plot e.g., EFFORT or LANDINGS
#' @param type the variable that will be used to group and display data: COMMON_NAME, GUILD, or COUNTRY
#' @param line_count number of lines to display
#' @param plot_type area or line plot
#' @param output_path path for output to live.
#' @param file_name name for the output.
#' @param save_plot logical to save plot.
#' @param return_plot logical to return plot to current environment.
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
#' stecf_plot("Greater North Sea Ecoregion", type = "COMMON_NAME", return_plot = TRUE, line_count = 4)
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
                       file_name = "figure3",
                       save_plot = FALSE,
                       return_plot = TRUE,
                       fig.width = 174,
                       fig.height = 68,
                       text.size = 9,
                       ...) {

  if(metric == "EFFORT"){
    data("stecf_effort_clean")

    allDat <- stecf_effort_clean %>%
      filter(ECOREGION == ecoregion) %>%
      rename_(.dots = setNames(c(type, "EFFORT"),
                               c("type_var", "VALUE")))
    }

  if(metric == "LANDINGS"){
    if(type == "COUNTRY") stop("You should use the ices_catch_plot() function instead.")

    data("stecf_landings_clean")

    allDat <- stecf_landings_clean %>%
      filter(ECOREGION == ecoregion) %>%
      rename_(.dots = setNames(c(type, "LANDINGS"),
                               c("type_var", "VALUE")))
  }

  if(line_count >= 10) warning("Color scales are hard to see beyond this point... try plotting fewer categories.")
  if(line_count == 14) stop("Color palette only has 14 colors... sorry.")

  catchPlot <- allDat %>%
    group_by(ANNEX, type_var) %>%
    dplyr::summarize(typeTotal = sum(VALUE, na.rm = TRUE)) %>% # Overall catch to order plot
    arrange(ANNEX, -typeTotal) %>%
    filter(typeTotal >= 1) %>%
    group_by(ANNEX) %>%
    dplyr::mutate(RANK = min_rank(desc(typeTotal))) %>%
    inner_join(allDat, c("ANNEX", "type_var")) %>%
    ungroup() %>%
    dplyr::mutate(type_var = replace(type_var, RANK > line_count, "other"),
                  ANNEX = str_wrap(ANNEX, width = 26)) %>%
    group_by(ANNEX, type_var, YEAR) %>%
    dplyr::summarize(typeTotal = sum(VALUE, na.rm = TRUE)) %>%
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


  if(save_plot) {
    ggsave(paste0(output_path, file_name, ".png"),
      # filename = paste0("~/git/ices-dk/fisheryO/output/", fig_name, "_", IA, ".png"),
           plot = pl,
           width = fig.width,
           height = fig.height,
           units = "mm",
           dpi = 300)

  }
  if(return_plot) {
    return(pl)
  }
}

# col_roxygen
#' Internal function to generate roxygen formatting for data files.
#'
#' @keywords internal
#'
#' @param base_num The number to multiply by three
#'
#' @return Returns a string ready to copy and paste into Roxygen
#'
col_oxygen <- function(object) {
  stopifnot(is.object(object))

  cat(paste0("#'\t\\item{", colnames(object), "}{Add text}\n"))

  cat(paste0("#' }\n#'\n#' @format A ", gsub("\\.", " ", class(object)),
             " with ", nrow(object),
             " rows and ", ncol(object), " variables."))
}
