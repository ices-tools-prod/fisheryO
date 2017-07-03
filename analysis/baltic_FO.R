devtools::install_github("slarge/fisheryO")
library(fisheryO)
rm(list = ls())
ecoregion = "Baltic Sea Ecoregion"
active_year = 2017
output_dir <- "XYZ"

## Figure 1.
area_definition_map(ecoregion,
                    data_caption = FALSE,
                    return_plot = TRUE,
                    save_plot = FALSE,
                    output_path = output_path,
                    file_name = "baltic_figure1")

## Figure 2.
ices_catch_plot(ecoregion,
                data_caption = FALSE,
                type = "COUNTRY",
                line_count = 9,
                plot_type = "area",
                file_name = "baltic_figure2",
                save_plot = FALSE,
                output_path = output_path,
                return_data = TRUE,
                return_plot = TRUE,
                fig.width = 174,
                fig.height = 68,
                text.size = 9)

## Figure 3.
stecf_plot(ecoregion,
           data_caption = FALSE,
           metric = "EFFORT",
           type = "COUNTRY",
           line_count = 6,
           plot_type = "line",
           file_name = "baltic_figure3",
           save_plot = FALSE,
           output_path = output_path,
           return_data = TRUE,
           return_plot = TRUE,
           fig.width = 174,
           fig.height = 68,
           text.size = 9)


## Figure 4.
ices_catch_plot(ecoregion,
                data_caption = FALSE,
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
                data_caption = FALSE,
                type = "COMMON_NAME",
                line_count = 5,
                plot_type = "line",
                file_name = "baltic_figure5",
                save_plot = FALSE,
                output_path = "output/",
                return_plot = TRUE,
                fig.width = 174,
                fig.height = 68,
                text.size = 9)

## Figure 6.
stecf_plot(ecoregion,
           data_caption = FALSE,
           metric = "LANDINGS",
           type = "GEAR",
           line_count = 5,
           plot_type = "line",
           file_name = "baltic_figure6",
           save_plot = FALSE,
           output_path = "output/",
           return_plot = TRUE,
           fig.width = 174,
           fig.height = 68,
           text.size = 9)

## Figure 7.
guild_discards_fun(ecoregion,
                   data_caption = FALSE,
                   file_name = "baltic_figure7",
                   active_year = 2017,
                   output_path = "output/",
                   save_plot = FALSE,
                   return_plot = TRUE)


## Figure 8.
stecf_plot(ecoregion,
           data_caption = FALSE,
           metric = "EFFORT",
           type = "GEAR",
           line_count = 6,
           plot_type = "line",
           file_name = "baltic_figure8",
           save_plot = FALSE,
           output_path = "output/",
           return_plot = TRUE,
           fig.width = 174,
           fig.height = 68,
           text.size = 9)



## Figure 9. <From Colin>

## Figure 10.
stockPie_fun(ecoregion,
             fisheries_guild = c("benthic", "demersal", "pelagic"),
             data_caption = FALSE,
             calculate_status = FALSE,
             file_name = "baltic_figure10",
             active_year = 2017,
             save_plot = TRUE,
             return_plot = FALSE,
             output_path = output_path)

## Figure 11.
gesPie_fun(ecoregion,
           fisheries_guild = c("benthic", "demersal", "pelagic"),
           data_caption = FALSE,
           file_name = "baltic_figure11",
           calculate_status = FALSE,
           active_year = 2017,
           save_plot = TRUE,
           return_plot = FALSE,
           output_path = output_path)

## Figure 12.
stock_trends_fun(EcoGuild = "Baltic Sea Ecoregion - benthic stocks",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = FALSE,
                 file_name = "baltic_figure12_benthic",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 output_path = output_path)

stock_trends_fun(EcoGuild = "Baltic Sea Ecoregion - demersal stocks",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = FALSE,
                 file_name = "baltic_figure12_demersal",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 output_path = output_path)

stock_trends_fun(EcoGuild = "Baltic Sea Ecoregion - pelagic stocks",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = FALSE,
                 file_name = "baltic_figure12_pelagic",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 output_path = output_path)

## Figure 13.
plot_kobe(ecoregion,
          guild = "all",
          active_year = 2017,
          data_caption = FALSE,
          file_name = "baltic_figure13_all",
          output_path = output_path,
          return_plot = FALSE,
          save_plot = TRUE)

plot_kobe(ecoregion,
          guild = "benthic",
          active_year = 2017,
          data_caption = FALSE,
          file_name = "baltic_figure13_benthic",
          output_path = output_path,
          return_plot = FALSE,
          save_plot = TRUE)

plot_kobe(ecoregion,
          guild = "pelagic",
          active_year = 2017,
          data_caption = FALSE,
          file_name = "baltic_figure13_pelagic",
          output_path = output_path,
          return_plot = FALSE,
          save_plot = TRUE)


plot_kobe(ecoregion,
          guild = "demersal",
          active_year = 2017,
          data_caption = FALSE,
          file_name = "baltic_figure13_demersal",
          output_path = output_path,
          return_plot = FALSE,
          save_plot = TRUE)

#
# stockSummaryTable_fun(ecoregion,
#                       active_year = 2017,
#                       table_type = "static_docx",
#                       output_path = output_path,
#                       file_name = "baltic_annex")

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
  write.csv(file = paste0(output_path, "baltic_links.csv"), row.names = FALSE)
