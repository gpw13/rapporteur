% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_hpop_summary_sheet.R
\name{write_sheet_header_hpop_summary}
\alias{write_sheet_header_hpop_summary}
\title{Write and style HPOP summary sheet header}
\usage{
write_sheet_header_hpop_summary(
  wb,
  sheet_name,
  iso,
  start_col,
  start_row,
  end_year,
  value_col,
  boxes_bounds
)
}
\arguments{
\item{wb}{a \code{openxlsx} workbook to be edited.}

\item{sheet_name}{character name of the sheet to update}

\item{iso}{ISO3 code of country to summarize.}

\item{start_col}{integer start col of styling. Passed to \code{\link[openxlsx:addStyle]{openxlsx::addStyle()}}}

\item{start_row}{integer start row of styling. Passed to \code{\link[openxlsx:addStyle]{openxlsx::addStyle()}}}

\item{end_year}{End year(s) for contribution calculation, defaults to 2019 to 2025.}

\item{value_col}{Column name of column with indicator values.}

\item{boxes_bounds}{named list of bounds for data frame boxes to be written in sheet.}
}
\description{
Write and style HPOP summary sheet header
}
