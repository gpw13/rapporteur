% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_dataframes_hpop_summary.R
\name{write_billion_contrib_ind_hpop_summary}
\alias{write_billion_contrib_ind_hpop_summary}
\title{Write the contribution to billion per indicator data frame to the data sheet}
\usage{
write_billion_contrib_ind_hpop_summary(
  df,
  wb,
  sheet_name,
  start_year,
  end_year,
  contribution_pct,
  contribution,
  contribution_pct_total_pop,
  ind_df,
  boxes_bounds,
  ind_ids
)
}
\arguments{
\item{df}{data frame to be written}

\item{wb}{a \code{openxlsx} workbook to be edited.}

\item{sheet_name}{character name of the sheet to update}

\item{start_year}{Base year for contribution calculation, defaults to 2018.}

\item{end_year}{End year(s) for contribution calculation, defaults to 2019 to
2025.}

\item{contribution_pct}{Column name of column(s) to store contribution (percent)
values. Must be the same length as \code{transform_value_col}.}

\item{contribution}{Column name of column(s) to store contribution (population)
values. Must be the same length as \code{transform_value_col}.}

\item{contribution_pct_total_pop}{Column name of column(s) to store contribution
(percent of total population of the country) values. Must be the same length
as \code{transform_value_col}.}

\item{ind_df}{data frame containing the indicators in the correct order and format to be used.}

\item{boxes_bounds}{named list of bounds for data frame boxes to be written in sheet.}

\item{ind_ids}{Named vector of indicator codes for input indicators to the Billion.
Although separate indicator codes can be used than the standard, they must
be supplied as a named vector where the names correspond to the output of
\code{billion_ind_codes()}.}
}
\description{
Used within \code{write_hpop_summary_sheet()}
}
