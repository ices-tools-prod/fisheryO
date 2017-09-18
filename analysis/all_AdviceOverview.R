rm(list = ls())
# devtools::install_github("ices-tools-prod/fisheryO")
# library(fisheryO)
# ecoregion = "Greater North Sea Ecoregion"
active_year = 2017
output_path <- "~/AdviceOverview/"
options(scipen = 5)

stock_trends_fun(object = "Bay of Biscay and the Iberian Coast Ecoregion",
                 plotting_var = "StockCode",
                 grouping_var = "EcoRegion",
                 metric = "MSY",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "bob_MSY",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = TRUE,
                 output_path = output_path)

stock_trends_fun(object = "Bay of Biscay and the Iberian Coast Ecoregion",
                 plotting_var = "StockCode",
                 grouping_var = "EcoRegion",
                 metric = "MEAN",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "bob_MEAN",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = TRUE,
                 output_path = output_path)

stock_trends_fun(object = "Celtic Seas Ecoregion",
                 plotting_var = "StockCode",
                 grouping_var = "EcoRegion",
                 metric = "MSY",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "cs_MSY",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = TRUE,
                 output_path = output_path)


stock_trends_fun(object = "Celtic Seas Ecoregion",
                 plotting_var = "StockCode",
                 grouping_var = "EcoRegion",
                 metric = "MEAN",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "cs_MEAN",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = TRUE,
                 output_path = output_path)

stock_trends_fun(object = "Greater North Sea Ecoregion",
                 plotting_var = "StockCode",
                 grouping_var = "EcoRegion",
                 metric = "MSY",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "ns_MSY",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = TRUE,
                 output_path = output_path)

stock_trends_fun(object = "Greater North Sea Ecoregion",
                 plotting_var = "StockCode",
                 grouping_var = "EcoRegion",
                 metric = "MEAN",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "ns_MEAN",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = TRUE,
                 output_path = output_path)

stock_trends_fun(object = "Baltic Sea Ecoregion",
                 plotting_var = "StockCode",
                 grouping_var = "EcoRegion",
                 metric = "MSY",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "bs_MSY",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = TRUE,
                 output_path = output_path)

stock_trends_fun(object = "Baltic Sea Ecoregion",
                 plotting_var = "StockCode",
                 grouping_var = "EcoRegion",
                 metric = "MEAN",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "bs_MEAN",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = TRUE,
                 output_path = output_path)

stock_trends_fun(object = "Oceanic Northeast Atlantic Ecoregion",
                 plotting_var = "StockCode",
                 grouping_var = "EcoRegion",
                 metric = "MSY",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "ona_MSY",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = TRUE,
                 output_path = output_path)

stock_trends_fun(object = "Oceanic Northeast Atlantic Ecoregion",
                 plotting_var = "StockCode",
                 grouping_var = "EcoRegion",
                 metric = "MEAN",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "ona_MEAN",
                 save_plot = TRUE,
                 return_plot = FALSE,
                 return_data = TRUE,
                 output_path = output_path)

