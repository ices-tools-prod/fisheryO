
<!-- README.md is generated from README.Rmd. Please edit that file -->
[<img align="right" alt="ICES Logo" width="17%" height="17%" src="http://ices.dk/_layouts/15/1033/images/icesimg/iceslogo.png">](http://ices.dk)

fisheryO
========

The fisheryO package is offered to provide documentation of the processes used to download, aggregate, and analyze data for ICES Fisheries Overviews. Further, the package contains R functions to facilitate the standard plotting of these data.

In 2017, ICES will provide Fisheries Overviews for the following ecoregions:

-   [Fisheries Overview of Baltic Sea Ecoregion](https://community.ices.dk/Advice/Advice2017/BalticSea/Draft_advice/BalticSeaEcoregion_FisheriesOverviews.docx?Web=1)
-   [Fisheries Overview including mixed fisheries considerations of Celtic Seas Ecoregion](https://community.ices.dk/Advice/Advice2017/CelticSea/Draft_advice/CelticSeasEcoregion_FisheriesOverviews.docx?Web=1)
-   [Fisheries Overview including mixed fisheries considerations of Greater North Sea Ecoregion](https://community.ices.dk/Advice/Advice2017/NorthSea/Draft_advice/GreaterNorthSeaEcoregion_FisheriesOverviews.docx?Web=1)
-   [Fisheries Overview of Norwegian Sea and Barents Sea Ecoregions](https://community.ices.dk/Advice/Advice2017/BarentsSea/Draft_advice/NorwegianSeaBarentsSeaEcoregions_FisheriesOverviews.docx?Web=1)

Ultimately, the following Fisheries Overviews will also be produced:

-   [Fisheries Overview of Azores Ecoregion](https://community.ices.dk/Advice/Advice2016/Widely/Draft_advice/AzoresEcoregion_FisheriesOverviews.docx?Web=1)
-   [Fisheries overviews of Oceanic north-east Atlantic Ecoregion](https://community.ices.dk/Advice/Advice2016/Widely/Draft_advice/OceanicNortheastAtlanticEcoregion_FisheriesOverviews.docx?Web=1)
-   [Fisheries Overview including mixed fisheries considerations of Bay of Biscay and the Iberian Coast Ecoregion](https://community.ices.dk/Advice/Advice2016/Biscay/Draft_advice/BayofBiscayandtheIberianCoastEcoregion_FisheriesOverviews.docx?Web=1)
-   [Fisheries Overview of Faroes Ecoregion](https://community.ices.dk/Advice/Advice2016/Faroes/Draft_advice/FaroesEcoregion_FisheriesOverviews.docx?Web=1)
-   [Fisheries Overview of Icelandic Ecoregion](https://community.ices.dk/Advice/Advice2016/Iceland/Draft_advice/IcelandicEcoregion_FisheriesOverviews.docx?Web=1)
-   [Fisheries Overview of Greenland Sea Ecoregion](https://community.ices.dk/Advice/Advice2016/Iceland/Draft_advice/GreenlandSeaEcoregion_FisheriesOverviews.docx?Web=1)

Installation
------------

You can install fisheryO from github with:

``` r
# install.packages("devtools")
# devtools::install_github("slarge/fisheryO")
# library(fisheryO)
```

Work flow
---------

1.  Before the package is built, fisheryO downloads source data from ICES web services and databases and saves the raw data as .rdata files in the /data folder. This serves to create a final version of the data used to create each Fisheries Overview document, thatis, a github commit hash could be used as the "Greater North Sea ecoregion" final data reference. The raw data are available as a "promise" and can be explored extracted using the `data()` function. The nuts and bolts of these download steps can be found in the load\_raw\_data.R file in the /data-raw folder and links to the raw data can be found in the description files.

2.  Raw data processing is dependent on how the data will ultimately be displayed (e.g., figure or table) and several functions modify the raw data. These functions can be viewed in the clean\_raw\_data.R file in the /R folder to see the assumptions and data wrangling steps to move from raw data to figures and tables.

3.  Data aggregating functions are called from within the standard plotting functions, but can be run independently to explore the intermediate data.

The list of data can be found using:

``` r
knitr::kable(as.data.frame(data(package = "fisheryO")$results[,c("Item", "Title")]))
```

| Item                         | Title                                                                              |
|:-----------------------------|:-----------------------------------------------------------------------------------|
| eco\_shape                   | ICES Ecoregions                                                                    |
| europe\_shape                | Europe map                                                                         |
| ices\_catch\_historical\_raw | Historical Nominal Catches 1950-2010                                               |
| ices\_catch\_official\_raw   | Official Nominal Catches 2006-2014                                                 |
| ices\_shape                  | ICES Statistical Areas                                                             |
| sag\_keys\_raw               | ICES Stock Assessment Graphs database - keys                                       |
| sag\_refpts\_raw             | ICES Stock Assessment Graphs database - reference points                           |
| sag\_summary\_raw            | ICES Stock Assessment Graphs database - summary information from assessment output |
| species\_list\_raw           | ASFIS list of species                                                              |
| stecf\_effort\_raw           | STECF nominal effort                                                               |
| stecf\_landings\_raw         | STECF landings and discards                                                        |
| stock\_list\_raw             | ICES Stock database                                                                |

If you want more information about the data source for each data file, use the "?<data_name>" notation, e.g., `?ices_catch_historical_raw` function to explore the description and to find a url for the source.

Plots
-----

``` r

fisheryO::plot_kobe("Greater North Sea Ecoregion", guild = "demersal", return_plot = TRUE, dynamic = FALSE)
```

![](README-kobe_example-1.png)

Some of the more complex plots have the option to be dynamic .html graphics with the "dynamic = TRUE" argument.

[To do](https://github.com/slarge/fisheryO/issues/12): If you want more information about the data source used for each plot, use the "?<plot_function>" notation, e.g., `?plot_kobe` function to explore the description.

ICES Ecoregions are not quite the same as the ICES areas that most assessments are based on. In fact, much of the historic catch data (`?ices_catch_historical_raw`) is aggregated across multiple ICES areas that are may extend into other ecoregions. The following function will show the discrepancies between the ICES Ecoregions and ICES areas.

``` r
fisheryO::area_definition_map(ecoregion = "Greater North Sea Ecoregion", return_plot = TRUE)
```

![](README-map_area-1.png)

Notes
-----

References
----------

ICES Stock Assessment Graphs database: <http://sg.ices.dk>

ICES Stock Assessment Graphs web services: <http://sg.ices.dk/webservices.aspx>

ICES Stock Database: <http://sd.ices.dk>

ICES Stock Database web services: <http://sd.ices.dk/services/>

Development
-----------

fisheryO is developed openly on [GitHub](https://github.com/slarge/fisheryO).

Feel free to open an [issue](https://github.com/slarge/fisheryO/issues) there if you encounter problems or have suggestions for future versions.
