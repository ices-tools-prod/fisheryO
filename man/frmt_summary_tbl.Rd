% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_raw_data.R
\name{frmt_summary_tbl}
\alias{frmt_summary_tbl}
\title{Format stock summary table}
\usage{
frmt_summary_tbl(active_year = active_year,
  calculate_status = calculate_status, return_clean_sag = FALSE)
}
\arguments{
\item{active_year}{numeric of the stock database version. e.g., 2016}

\item{calculate_status}{logical on whether to use raw SAG output to calculate stock status or to use the hard-coded values from stock summary table}

\item{return_clean_sag}{logical to return objects from clean_sag()}
}
\value{
data frame
}
\description{
\code{frmt_summary_tbl} returns the stock summary table plain and formatted with html (e.g., glyphicons and italics)
}
\note{
Periodically, ICES adds or removes stocks from the advisory process. The function returns the stock summary table for all published (in SAG) and active stocks for a given year.
\code{calculate_status = TRUE} calculates stock status
relative to published reference points. This will represent PA and SBL for ecoregions with proxy reference points. \code{calculate_status = TRUE} takes the
raw icons from published advice. Note, before 2017 not all stocks status tables have been added to the SAG database and only few stocks had MSY proxy reference points.
}
\examples{
head(frmt_summary_tbl(active_year = 2016)$summary_table)
}
\seealso{
Used in \code{\link{stockSummaryTable_fun}} to create the "Status of stock summary relative to reference points"
table for all stocks for an ecoregion. Input data: SAG summary table and reference points come from \code{\link{clean_sag}}.
}
\author{
Scott Large
}
