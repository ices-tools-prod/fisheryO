library(fisheryO)
rm(list = ls())
ecoregion = "Greater North Sea Ecoregion"
options(scipen = 5)

## Figure 1.
area_definition_map(ecoregion,
                    data_caption = FALSE,
                    return_plot = FALSE,
                    save_plot = TRUE,
                    output_path = "output/",
                    file_name = "ns_figure1")

## Figure 2.
ices_catch_plot(ecoregion,
                data_caption = FALSE,
                type = "COUNTRY",
                line_count = 9,
                plot_type = "area",
                file_name = "ns_figure2",
                save_plot = TRUE,
                output_path = "output/",
                return_plot = FALSE,
                fig.width = 174,
                fig.height = 68,
                text.size = 9)

## Figure 3.
stecf_plot(ecoregion,
           data_caption = FALSE,
           metric = "EFFORT",
           type = "COUNTRY",
           line_count = 8,
           plot_type = "line",
           file_name = "ns_figure3",
           save_plot = TRUE,
           output_path = "output/",
           return_plot = FALSE,
           fig.width = 174,
           fig.height = 68,
           text.size = 9)


## Figure 4.
ices_catch_plot(ecoregion,
                data_caption = FALSE,
                type = "GUILD",
                line_count = 6,
                plot_type = "line",
                file_name = "ns_figure4",
                save_plot = TRUE,
                output_path = "output/",
                return_plot = FALSE,
                fig.width = 174,
                fig.height = 68,
                text.size = 9)

## Figure 5.
ices_catch_plot(ecoregion,
                data_caption = FALSE,
                type = "COMMON_NAME",
                line_count = 10,
                plot_type = "line",
                file_name = "ns_figure5",
                save_plot = TRUE,
                output_path = "output/",
                return_plot = FALSE,
                fig.width = 174,
                fig.height = 68,
                text.size = 9)

## Figure 6.
stecf_plot(ecoregion,
           data_caption = FALSE,
           metric = "LANDINGS",
           type = "GEAR",
           line_count = 6,
           plot_type = "line",
           file_name = "ns_figure6",
           save_plot = TRUE,
           output_path = "output/",
           return_plot = FALSE,
           fig.width = 174,
           fig.height = 68,
           text.size = 9)

## Figure 7.
guild_discards_fun(ecoregion,
                   data_caption = FALSE,
                   file_name = "ns_figure7",
                   active_year = 2016,
                   output_path = "output/",
                   save_plot = TRUE,
                   return_plot = FALSE)


## Figure 8.
stecf_plot(ecoregion,
           data_caption = FALSE,
           metric = "EFFORT",
           type = "GEAR",
           line_count = 6,
           plot_type = "line",
           file_name = "ns_figure8",
           save_plot = TRUE,
           output_path = "output/",
           return_plot = FALSE,
           fig.width = 174,
           fig.height = 68,
           text.size = 9)



## Figure 9. <From Colin>

## Figure 10.
stockPie_fun(ecoregion,
             fisheries_guild = c("pelagic", "demersal", "crustacean", "elasmobranch", "benthic"),
             data_caption = FALSE,
             calculate_status = FALSE,
             file_name = "ns_figure10",
             active_year = 2016,
             save_plot = TRUE,
             return_plot = FALSE,
             output_path = "output/")

## Figure 11.
gesPie_fun(ecoregion,
           fisheries_guild = c("pelagic", "demersal", "crustacean", "elasmobranch", "benthic"),
           data_caption = FALSE,
           file_name = "ns_figure11",
           calculate_status = FALSE,
           active_year = 2016,
           save_plot = TRUE,
           return_plot = FALSE,
           output_path = "output/")

## Figure 12.
stock_trends_fun(EcoGuild = "Greater North Sea Ecoregion - benthic stocks",
                 active_year = 2016,
                 dynamic = FALSE,
                 data_caption = FALSE,
                 file_name = "ns_figure12_benthic",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 output_path = "output/")

stock_trends_fun(EcoGuild = "Greater North Sea Ecoregion - demersal stocks",
                 active_year = 2016,
                 dynamic = FALSE,
                 data_caption = FALSE,
                 file_name = "ns_figure12_demersal",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 output_path = "output/")

stock_trends_fun(EcoGuild = "Greater North Sea Ecoregion - pelagic stocks",
                 active_year = 2016,
                 dynamic = FALSE,
                 data_caption = FALSE,
                 file_name = "ns_figure12_pelagic",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 output_path = "output/")

stock_trends_fun(EcoGuild = "Greater North Sea Ecoregion - crustacean stocks",
                 active_year = 2016,
                 dynamic = FALSE,
                 data_caption = FALSE,
                 file_name = "ns_figure12_crustacean",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 output_path = "output/")

## Figure 13.
plot_kobe(ecoregion,
          guild = "all",
          catch_limit = 10000,
          active_year = 2016,
          data_caption = FALSE,
          file_name = "ns_figure13_all",
          output_path = "output/",
          return_plot = FALSE,
          save_plot = TRUE)

plot_kobe(ecoregion,
          guild = "benthic",
          active_year = 2016,
          data_caption = FALSE,
          file_name = "ns_figure13_benthic",
          output_path = "output/",
          return_plot = FALSE,
          save_plot = TRUE)

plot_kobe(ecoregion,
          guild = "crustacean",
          active_year = 2016,
          data_caption = FALSE,
          file_name = "ns_figure13_crustacean",
          output_path = "output/",
          return_plot = FALSE,
          save_plot = TRUE)

plot_kobe(ecoregion,
          guild = "demersal",
          active_year = 2016,
          data_caption = FALSE,
          file_name = "ns_figure13_demersal",
          output_path = "output/",
          return_plot = FALSE,
          save_plot = TRUE)

plot_kobe(ecoregion,
          guild = "pelagic",
          active_year = 2016,
          data_caption = FALSE,
          file_name = "ns_figure13_pelagic",
          output_path = "output/",
          return_plot = FALSE,
          save_plot = TRUE)

