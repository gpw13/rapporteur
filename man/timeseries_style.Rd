% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timeseries_style.R
\name{timeseries_style}
\alias{timeseries_style}
\title{Change font of time series values}
\usage{
timeseries_style(df, wb, sheet_name, start_row, start_col, ind_df)
}
\arguments{
\item{df}{data frame to be written}

\item{wb}{a \code{openxlsx} workbook to be edited.}

\item{sheet_name}{character name of the sheet to update}

\item{start_row}{integer start row of styling. Passed to \code{\link[openxlsx:addStyle]{openxlsx::addStyle()}}}

\item{start_col}{integer start col of styling. Passed to \code{\link[openxlsx:addStyle]{openxlsx::addStyle()}}}

\item{ind_df}{data frame containing the indicators in the correct order and format to be used.}
}
\value{
\code{openxslx} Workbook object. Output file is in \code{output_folder}.
}
\description{
\code{timeseries_style()} changes the font of time series values based on the
indicator type it is:
}
\details{
\itemize{
\item bold: reported
\item normal: estimated
\item faded: imputed or projected
}
}
