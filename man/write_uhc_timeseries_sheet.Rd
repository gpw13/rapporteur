% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_uhc_timeseries.R
\name{write_uhc_timeseries_sheet}
\alias{write_uhc_timeseries_sheet}
\title{Write times series sheet}
\usage{
write_uhc_timeseries_sheet(
  df,
  wb,
  sheet_name,
  start_row,
  start_col,
  value_col,
  ind_df,
  ind_ids,
  end_year
)
}
\arguments{
\item{df}{data frame to be written}

\item{wb}{a \code{openxlsx} workbook to be edited.}

\item{sheet_name}{character name of the sheet to update}

\item{start_row}{integer start row of styling. Passed to \code{\link[openxlsx:addStyle]{openxlsx::addStyle()}}}

\item{start_col}{integer start col of styling. Passed to \code{\link[openxlsx:addStyle]{openxlsx::addStyle()}}}

\item{value_col}{Column name of column with indicator values.}

\item{ind_df}{data frame containing information on indicators}

\item{ind_ids}{Named vector of indicator codes for input indicators to the Billion.
Although separate indicator codes can be used than the standard, they must
be supplied as a named vector where the names correspond to the output of
\code{billion_ind_codes()}.}

\item{end_year}{End year(s) for contribution calculation, defaults to 2019 to 2025.}
}
\value{
\code{openxslx} Workbook object. Output file is in \code{output_folder}.
}
\description{
\code{export_hpop_country_summary_xls} Export a country-specific for HPOP billion.
}
