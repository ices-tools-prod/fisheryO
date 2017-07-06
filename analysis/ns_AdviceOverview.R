rm(list = ls())
# devtools::install_github("ices-tools-prod/fisheryO")
# library(fisheryO)
ecoregion = "Greater North Sea Ecoregion"
active_year = 2017
output_path <- "~/AdviceOverview/"
options(scipen = 5)


stock_trends_fun(object = "Greater North Sea Ecoregion - demersal stocks",
                 plotting_var = "StockCode",
                 grouping_var = "EcoGuild",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "ns_demersal",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = FALSE,
                 output_path = output_path)

stock_trends_fun(object = "Greater North Sea Ecoregion - pelagic stocks",
                 plotting_var = "StockCode",
                 grouping_var = "EcoGuild",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "ns_pelagic",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = FALSE,
                 output_path = output_path)

stock_trends_fun(object = "Greater North Sea Ecoregion - benthic stocks",
                 plotting_var = "StockCode",
                 grouping_var = "EcoGuild",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "ns_benthic",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = FALSE,
                 output_path = output_path)


stock_trends_fun(object = "Greater North Sea Ecoregion - crustacean stocks",
                 plotting_var = "StockCode",
                 grouping_var = "EcoGuild",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "ns_crustacean",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = FALSE,
                 output_path = output_path)

stock_trends_fun(object = "Greater North Sea Ecoregion",
                 plotting_var = "FisheriesGuild",
                 grouping_var = "EcoRegion",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "ns_guild",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = FALSE,
                 output_path = output_path)
