rm(list = ls())
# devtools::install_github("ices-tools-prod/fisheryO")
# library(fisheryO)
# ecoregion = "Baltic Sea Ecoregion"
# active_year = 2017
output_path <- "~/AdviceOverview/"
options(scipen = 5)

stock_trends_fun(object = "Celtic Seas Ecoregion - demersal stocks",
                 plotting_var = "StockCode",
                 grouping_var = "EcoGuild",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "cs_demersal",
                 save_plot = FALSE,
                 return_plot = TRUE,
                 return_data = FALSE,
                 output_path = output_path)

stock_trends_fun(object = "Celtic Seas Ecoregion - pelagic stocks",
                 plotting_var = "StockCode",
                 grouping_var = "EcoGuild",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "cs_pelagic",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = FALSE,
                 output_path = output_path)

stock_trends_fun(object = "Celtic Seas Ecoregion - benthic stocks",
                 plotting_var = "StockCode",
                 grouping_var = "EcoGuild",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "cs_benthic",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = FALSE,
                 output_path = output_path)

stock_trends_fun(object = "Celtic Seas Ecoregion - elasmobranch stocks",
                 plotting_var = "StockCode",
                 grouping_var = "EcoGuild",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "cs_elasmobranch",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = FALSE,
                 output_path = output_path)

stock_trends_fun(object = "Celtic Seas Ecoregion - crustacean stocks",
                 plotting_var = "StockCode",
                 grouping_var = "EcoGuild",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "cs_crustacean",
                 save_plot = FALSE,
                 return_plot = TRUE,
                 return_data = FALSE,
                 output_path = output_path)

stock_trends_fun(object = "Celtic Seas Ecoregion",
                 plotting_var = "FisheriesGuild",
                 grouping_var = "EcoRegion",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "cs_guild",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = FALSE,
                 output_path = output_path)
