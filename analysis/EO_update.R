rm(list = ls())
# devtools::install_github("ices-tools-prod/fisheryO", ref = "v0.2")
# library(fisheryO)
# ecoregion = "Baltic Sea Ecoregion"
# active_year = 2017
output_path <- "~/EO_update/"
options(scipen = 5)

stock_trends_fun(object = "Bay of Biscay and the Iberian Coast Ecoregion - demersal stocks",
                 plotting_var = "StockCode",
                 grouping_var = "EcoGuild",
                 metric = "MSY",
                 active_year = 2017,
                 dynamic = FALSE,
                 data_caption = TRUE,
                 file_name = "bob_MSY",
                 save_plot = FALSE,
                 return_plot = TRUE,
                 return_data = FALSE,
                 output_path = output_path)
