# rm(list = ls())
# devtools::install_github("ices-tools-prod/fisheryO", ref = "v0.2")
# library(fisheryO)
ecoregion = "Baltic Sea Ecoregion"
active_year = 2017
output_path <- "~/BS_FO/"
options(scipen = 5)


## Figure 1.
area_definition_map(ecoregion,
                    data_caption = FALSE,
                    return_plot = FALSE,
                    save_plot = TRUE,
                    output_path = output_path,
                    file_name = "baltic_figure1")

## Figure 2.
ices_catch_plot(ecoregion,
                data_caption = TRUE,
                type = "COUNTRY",
                line_count = 9,
                plot_type = "area",
                file_name = "baltic_figure2",
                save_plot = TRUE,
                output_path = output_path,
                return_data = TRUE,
                return_plot = FALSE,
                fig.width = 174,
                fig.height = 68,
                text.size = 9)

## Figure 3.
stecf_plot(ecoregion,
           data_caption = TRUE,
           metric = "EFFORT",
           type = "COUNTRY",
           line_count = 6,
           plot_type = "line",
           file_name = "baltic_figure3",
           save_plot = TRUE,
           output_path = output_path,
           return_data = TRUE,
           return_plot = FALSE,
           fig.width = 174,
           fig.height = 68,
           text.size = 9)


## Figure 4.
ices_catch_plot(ecoregion,
                data_caption = TRUE,
                type = "GUILD",
                line_count = 5,
                plot_type = "line",
                file_name = "baltic_figure4",
                save_plot = TRUE,
                output_path = output_path,
                return_data = TRUE,
                return_plot = FALSE,
                fig.width = 174,
                fig.height = 68,
                text.size = 9)

## Figure 5.
ices_catch_plot(ecoregion,
                data_caption = TRUE,
                type = "COMMON_NAME",
                line_count = 5,
                plot_type = "line",
                file_name = "baltic_figure5",
                save_plot = TRUE,
                output_path = output_path,
                return_plot = FALSE,
                return_data = TRUE,
                fig.width = 174,
                fig.height = 68,
                text.size = 9)

## Figure 6.
stecf_plot(ecoregion,
           data_caption = TRUE,
           metric = "LANDINGS",
           type = "GEAR",
           line_count = 5,
           plot_type = "line",
           file_name = "baltic_figure6",
           save_plot = TRUE,
           output_path = output_path,
           return_plot = FALSE,
           return_data = TRUE,
           fig.width = 174,
           fig.height = 68,
           text.size = 9)

## Figure 7.
guild_discards_fun(ecoregion,
                   data_caption = TRUE,
                   file_name = "baltic_figure7_update",
                   active_year = 2017,
                   output_path = output_path,
                   save_plot = TRUE,
                   return_data = TRUE,
                   return_plot = FALSE)


## Figure 8.
stecf_plot(ecoregion,
           data_caption = TRUE,
           metric = "EFFORT",
           type = "GEAR",
           line_count = 6,
           plot_type = "line",
           file_name = "baltic_figure8",
           save_plot = TRUE,
           output_path = output_path,
           return_plot = FALSE,
           return_data = TRUE,
           fig.width = 174,
           fig.height = 68,
           text.size = 9)



## Figure 9. <From Colin>

## Figure 10.
stockPie_fun(ecoregion,
             fisheries_guild = c("benthic", "demersal", "pelagic"),
             data_caption = TRUE,
             calculate_status = FALSE,
             file_name = "baltic_figure10_update",
             active_year = 2017,
             save_plot = TRUE,
             return_plot = FALSE,
             return_data = TRUE,
             output_path = output_path)

## Figure 11.
gesPie_fun(ecoregion,
           fisheries_guild = c("benthic", "demersal", "pelagic"),
           data_caption = TRUE,
           file_name = "baltic_figure11_update",
           calculate_status = FALSE,
           active_year = 2017,
           save_plot = TRUE,
           return_plot = FALSE,
           return_data = TRUE,
           output_path = output_path)

## Figure 12.
stock_trends_fun(object = "Baltic Sea Ecoregion - benthic stocks",
                 plotting_var = "StockCode",
                 grouping_var = "EcoGuild",
                 metric = "MSY",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "baltic_figure12_benthic_update",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = TRUE,
                 output_path = output_path)

stock_trends_fun(object = "Baltic Sea Ecoregion - demersal stocks",
                 plotting_var = "StockCode",
                 grouping_var = "EcoGuild",
                 metric = "MSY",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "baltic_figure12_demersal",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = TRUE,
                 output_path = output_path)

stock_trends_fun(object = "Baltic Sea Ecoregion - pelagic stocks",
                 plotting_var = "StockCode",
                 grouping_var = "EcoGuild",
                 metric = "MSY",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "baltic_figure12_pelagic",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = TRUE,
                 output_path = output_path)

## Figure 13.
plot_kobe(ecoregion,
          guild = "all",
          active_year = 2017,
          data_caption = TRUE,
          file_name = "baltic_figure13_all_update",
          output_path = output_path,
          return_plot = FALSE,
          return_data = TRUE,
          save_plot = TRUE)

plot_kobe(ecoregion,
          guild = "benthic",
          active_year = 2017,
          data_caption = TRUE,
          file_name = "baltic_figure13_benthic_update",
          output_path = output_path,
          return_plot = FALSE,
          return_data = TRUE,
          save_plot = TRUE)

plot_kobe(ecoregion,
          guild = "pelagic",
          active_year = 2017,
          data_caption = TRUE,
          file_name = "baltic_figure13_pelagic_update",
          output_path = output_path,
          return_plot = FALSE,
          return_data = TRUE,
          save_plot = TRUE)


plot_kobe(ecoregion,
          guild = "demersal",
          active_year = 2017,
          data_caption = TRUE,
          file_name = "baltic_figure13_demersal_update",
          output_path = output_path,
          return_plot = FALSE,
          return_data = TRUE,
          save_plot = TRUE)


dat <- clean_stock_trends(active_year)
clicks <- dat$sag_complete_summary %>%
  tidyr::unnest() %>%
  filter(EcoRegion == "Baltic Sea Ecoregion") %>%
  select(StockCode,
         YearOfLastAssessment) %>%
  distinct(.keep_all = TRUE) %>%
  mutate(onclick = sprintf("%s%i/%i/%s.pdf",
                           "http://ices.dk/sites/pub/Publication%20Reports/Advice/",
                           YearOfLastAssessment,
                           YearOfLastAssessment,
                           StockCode)) %>%
  write.csv(file = paste0(output_path, "baltic_links_update.csv"), row.names = FALSE)



data(sag_stock_status_raw)
proxy_stocks <-  sag_stock_status_raw %>%
  filter(grepl("proxy", fishingPressure) | grepl("proxy", stockSize)) %>%
  select(StockKeyLabel) %>%
  distinct %>%
  pull(StockKeyLabel)

stockPlot <- frmt_summary_tbl(active_year,
                              calculate_status = FALSE)$summary_table_frmt %>%
  filter(grepl(pattern = ecoregion, EcoRegion)) %>%
  select(-EcoRegion) %>%
  distinct(.keep_all = TRUE) %>%
  arrange(StockCode) %>%
  mutate(Description = ifelse(!grepl("<em>|</em>", Description),
                              paste0(Description, "<em></em>"),
                              Description),
         AdviceCategory = gsub("MSY|MP", "MSY", AdviceCategory),
         AdviceCategory = ifelse(StockCode %in% proxy_stocks,
                                 "MSY",
                                 AdviceCategory),
         DataCategory = as.factor(DataCategory))

grey.path <- system.file("symbols", "grey_q.png", package = "fisheryO")
red.path <- system.file("symbols", "red_cross.png", package = "fisheryO")
green.path <- system.file("symbols", "green_check.png", package = "fisheryO")
orange.path <- system.file("symbols", "orange_oh.png", package = "fisheryO")

if(!all(unlist(lapply(c(grey.path, red.path, green.path, orange.path), file.exists)))) {
  stop("Check path for stock status icons")
}

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
  # F_2013
  flextable::display(i = ~ F_2013 == "RED", col_key = "F_2013", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(F_2013, src = red.path, width = .15, height = .15))) %>%
  flextable::display(i = ~ F_2013 == "GREY", col_key = "F_2013", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(F_2013, src = grey.path, width = .15, height = .15))) %>%
  flextable::display(i = ~ F_2013 == "GREEN", col_key = "F_2013", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(F_2013, src = green.path, width = .15, height = .15))) %>%
  flextable::display(i = ~ F_2013 == "ORANGE", col_key = "F_2013", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(F_2013, src = orange.path, width = .15, height = .15))) %>%
  # F_2015
  flextable::display(i = ~ F_2014 == "RED", col_key = "F_2014", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(F_2014, src = red.path, width = .15, height = .15))) %>%
  flextable::display(i = ~ F_2014 == "GREY", col_key = "F_2014", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(F_2014, src = grey.path, width = .15, height = .15))) %>%
  flextable::display(i = ~ F_2014 == "GREEN", col_key = "F_2014", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(F_2014, src = green.path, width = .15, height = .15))) %>%
  flextable::display(i = ~ F_2014 == "ORANGE", col_key = "F_2014", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(F_2014, src = orange.path, width = .15, height = .15)))%>%
  # F_2015
  flextable::display(i = ~ F_2015 == "RED", col_key = "F_2015", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(F_2015, src = red.path, width = .15, height = .15))) %>%
  flextable::display(i = ~ F_2015 == "GREY", col_key = "F_2015", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(F_2015, src = grey.path, width = .15, height = .15))) %>%
  flextable::display(i = ~ F_2015 == "GREEN", col_key = "F_2015", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(F_2015, src = green.path, width = .15, height = .15))) %>%
  flextable::display(i = ~ F_2015 == "ORANGE", col_key = "F_2015", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(F_2015, src = orange.path, width = .15, height = .15)))%>%
  # F_2016
  flextable::display(i = ~ F_2016 == "RED", col_key = "F_2016", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(F_2016, src = red.path, width = .15, height = .15))) %>%
  flextable::display(i = ~ F_2016 == "GREY", col_key = "F_2016", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(F_2016, src = grey.path, width = .15, height = .15))) %>%
  flextable::display(i = ~ F_2016 == "GREEN", col_key = "F_2016", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(F_2016, src = green.path, width = .15, height = .15))) %>%
  flextable::display(i = ~ F_2016 == "ORANGE", col_key = "F_2016", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(F_2016, src = orange.path, width = .15, height = .15)))%>%
  # SSB_2014
  flextable::display(i = ~ SSB_2014 == "RED", col_key = "SSB_2014", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(SSB_2014, src = red.path, width = .15, height = .15))) %>%
  flextable::display(i = ~ SSB_2014 == "GREY", col_key = "SSB_2014", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(SSB_2014, src = grey.path, width = .15, height = .15))) %>%
  flextable::display(i = ~ SSB_2014 == "GREEN", col_key = "SSB_2014", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(SSB_2014, src = green.path, width = .15, height = .15))) %>%
  flextable::display(i = ~ SSB_2014 == "ORANGE", col_key = "SSB_2014", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(SSB_2014, src = orange.path, width = .15, height = .15)))%>%
  # SSB_2015
  flextable::display(i = ~ SSB_2015 == "RED", col_key = "SSB_2015", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(SSB_2015, src = red.path, width = .15, height = .15))) %>%
  flextable::display(i = ~ SSB_2015 == "GREY", col_key = "SSB_2015", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(SSB_2015, src = grey.path, width = .15, height = .15))) %>%
  flextable::display(i = ~ SSB_2015 == "GREEN", col_key = "SSB_2015", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(SSB_2015, src = green.path, width = .15, height = .15))) %>%
  flextable::display(i = ~ SSB_2015 == "ORANGE", col_key = "SSB_2015", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(SSB_2015, src = orange.path, width = .15, height = .15)))%>%
  # SSB_2016
  flextable::display(i = ~ SSB_2016 == "RED", col_key = "SSB_2016", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(SSB_2016, src = red.path, width = .15, height = .15))) %>%
  flextable::display(i = ~ SSB_2016 == "GREY", col_key = "SSB_2016", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(SSB_2016, src = grey.path, width = .15, height = .15))) %>%
  flextable::display(i = ~ SSB_2016 == "GREEN", col_key = "SSB_2016", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(SSB_2016, src = green.path, width = .15, height = .15))) %>%
  flextable::display(i = ~ SSB_2016 == "ORANGE", col_key = "SSB_2016", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(SSB_2016, src = orange.path, width = .15, height = .15)))%>%
  # SSB_2017
  flextable::display(i = ~ SSB_2017 == "RED", col_key = "SSB_2017", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(SSB_2017, src = red.path, width = .15, height = .15))) %>%
  flextable::display(i = ~ SSB_2017 == "GREY", col_key = "SSB_2017", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(SSB_2017, src = grey.path, width = .15, height = .15))) %>%
  flextable::display(i = ~ SSB_2017 == "GREEN", col_key = "SSB_2017", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(SSB_2017, src = green.path, width = .15, height = .15))) %>%
  flextable::display(i = ~ SSB_2017 == "ORANGE", col_key = "SSB_2017", pattern = "{{add_icon}}",
                     formatters = list(add_icon ~ flextable::as_image(SSB_2017, src = orange.path, width = .15, height = .15)))%>%
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
  flextable::add_header(StockCode = "Stock code",
                        Description = "Stock name",
                        FisheriesGuild = "Fish category",
                        AdviceCategory = "Reference point",
                        DataCategory = "Data category",
                        SBL = "SBL",
                        F_2013 = "Fishing pressure",
                        F_2014 = "Fishing pressure",
                        F_2015 = "Fishing pressure",
                        F_2016 = "Fishing pressure",
                        SSB_2014 = "Stock size",
                        SSB_2015 = "Stock size",
                        SSB_2016 = "Stock size",
                        SSB_2017 = "Stock size",
                        D3C1 = "MSFD descriptor",
                        D3C2 = "MSFD descriptor",
                        GES = "MSFD descriptor", top = TRUE) %>%
  flextable::set_header_labels(StockCode = "Stock code",
                               Description = "Stock name",
                               FisheriesGuild = "Fish category",
                               AdviceCategory = "Reference point",
                               DataCategory = "Data category",
                               SBL = "SBL",
                               F_2013 = "2013",
                               F_2014 = "2014",
                               F_2015 = "2015",
                               F_2016 = "2016",
                               SSB_2014 = "2014",
                               SSB_2015 = "2015",
                               SSB_2016 = "2016",
                               SSB_2017 = "2017",
                               D3C1 = "D3C1",
                               D3C2 = "D3C2",
                               GES = "GES") %>%
  # flextable::merge_h(part = "header") %>%
  # flextable::merge_v(part = "header") %>%
  flextable::align(j = c("StockCode",
                         "Description",
                         "FisheriesGuild",
                         "AdviceCategory"), align = "left", part = "all") %>%
  flextable::align(j = c("DataCategory",
                         "SBL",
                         "F_2013",
                         "F_2014",
                         "F_2015",
                         "F_2016",
                         "SSB_2014",
                         "SSB_2015",
                         "SSB_2016",
                         "SSB_2017",
                         "D3C1",
                         "D3C2",
                         "GES"), align = "center", part = "all") %>%
  # flextable::align(j = c("DataCategory"), align = "right", part = "body") %>%
  flextable::autofit()


doc <- officer::read_docx() %>%
  flextable::body_add_flextable(FT)

print(doc, target = paste0(output_path, "bs_annex_update.docx")) %>%
  invisible()

