% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/style_uhc_summary_sheet.R
\name{style_data_headers_uhc_summary}
\alias{style_data_headers_uhc_summary}
\title{Style UHC summary worksheet data headers}
\usage{
style_data_headers_uhc_summary(wb, sheet_name, boxes_bounds)
}
\arguments{
\item{wb}{a \code{openxlsx} workbook to be edited.}

\item{sheet_name}{character name of the sheet to update}

\item{boxes_bounds}{named list of bounds for data frame boxes to be written in sheet.}
}
\value{
\code{openxslx} Workbook object. Output file is in \code{output_folder}.
}
\description{
\code{style_data_headers_uhc_summary()} styles headers of the data part of the UHC
summary worksheet: indicators, latetest reported, and baseline/projection headers
}
