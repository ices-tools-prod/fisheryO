% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_functions.R
\name{plot_kobe}
\alias{plot_kobe}
\title{Kobe plot of stock status}
\usage{
plot_kobe(ecoregion, guild = c("all", "benthic", "demersal", "pelagic",
  "crustacean", "elasmobranch", "large-scale stocks")[1], active_year = 2016,
  data_caption = TRUE, output_path = NULL, return_plot = TRUE,
  return_data = FALSE, save_plot = FALSE, catch_limit = 0,
  file_name = NULL, plotTitle = NULL, fig.width = 131.32,
  fig.height = 88.9, units = "mm", res = 300, dynamic = FALSE)
}
\arguments{
\item{ecoregion}{ecoregion name, e.g. Greater North Sea Ecoregion}

\item{guild}{fish category (options: "all", "benthic", "demersal", "pelagic", "crustacean", "elasmobranch", "large-scale stocks"), e.g. demersal}

\item{active_year}{numeric of the stock database version (year). e.g., 2016}

\item{data_caption}{print the data source as a caption, boolean.}

\item{output_path}{path for output to live.}

\item{return_plot}{logical to return plot to current environment.}

\item{return_data}{logical on returning a .csv of plotted data}

\item{save_plot}{logical to save plot.}

\item{catch_limit}{lower limit of catch to be included in the plot. Useful to cull the herd if there are many stocks with minimal catch.}

\item{file_name}{name for the output.}

\item{fig.width}{width pf combined set of plots}

\item{fig.height}{height of combined set of plots,}

\item{units}{defaults to "mm"}

\item{res}{defaults to "300"}

\item{dynamic}{logical to generate html output with dynamic features.}
}
\value{
A ggplot2 object when \code{return_plot} is \code{TRUE}, html when \code{dynamic} is \code{TRUE}
or .png when \code{dynamic} is \code{FALSE}. Output is saved as \code{file_name} in \code{output_path}.
When \code{file_name} is \code{NULL}, the file name is the ecoregion.
When \code{output_path} is \code{NULL}, the file is saved to "~/".
}
\description{
The \code{plot_kobe} function returns a 2 plots: a scatter plot of F/F<sub>MSY</sub> and SSB/MSY B<sub>trigger</sub>
by fish category and ecoregion and a "lollipop" plot of total catch (divided into discards and landings) by stock.
}
\note{
Stocks are linked to ecoregions and fish categories via the ICES Stock database.
Reference points are as published in ICES Stock Assessment Graphs database. In some cases,
status relative to reference points may vary from published ICES advice
when reported F or SSB are very close to reference points (e.g., F = 0.201 > F<sub>MSY</sub> = 0.20).
}
\examples{
\dontrun{
plot_kobe("Greater North Sea Ecoregion", guild = "demersal", return_plot = TRUE)
}
}
\author{
Scott Large
}
