rm(list = ls())
# devtools::install_github("ices-tools-prod/fisheryO")
# library(fisheryO)
# ecoregion = "Baltic Sea Ecoregion"
active_year = 2017
output_path <- "~/AdviceOverview/"
options(scipen = 5)

stock_trends_fun(object = "Bay of Biscay and the Iberian Coast Ecoregion - demersal stocks",
                 plotting_var = "StockCode",
                 grouping_var = "EcoGuild",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "bob_demersal",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = FALSE,
                 output_path = output_path)

stock_trends_fun(object = "Bay of Biscay and the Iberian Coast Ecoregion - pelagic stocks",
                 plotting_var = "StockCode",
                 grouping_var = "EcoGuild",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "bob_pelagic",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = FALSE,
                 output_path = output_path)

stock_trends_fun(object = "Bay of Biscay and the Iberian Coast Ecoregion - benthic stocks",
                 plotting_var = "StockCode",
                 grouping_var = "EcoGuild",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "bob_benthic",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = FALSE,
                 output_path = output_path)

stock_trends_fun(object = "Bay of Biscay and the Iberian Coast Ecoregion - elasmobranch stocks",
                 plotting_var = "StockCode",
                 grouping_var = "EcoGuild",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "bob_elasmobranch",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = FALSE,
                 output_path = output_path)

stock_trends_fun(object = "Bay of Biscay and the Iberian Coast Ecoregion - crustacean stocks",
                 plotting_var = "StockCode",
                 grouping_var = "EcoGuild",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "bob_crustacean",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = FALSE,
                 output_path = output_path)

stock_trends_fun(object = "Bay of Biscay and the Iberian Coast Ecoregion",
                 plotting_var = "FisheriesGuild",
                 grouping_var = "EcoRegion",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "bob_guild",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = FALSE,
                 output_path = output_path)
