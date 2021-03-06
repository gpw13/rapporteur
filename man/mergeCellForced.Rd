% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-export.R
\name{mergeCellForced}
\alias{mergeCellForced}
\title{Force merge cell}
\usage{
mergeCellForced(wb, sheet, cols, rows)
}
\arguments{
\item{wb}{A workbook object}

\item{sheet}{A name or index of a worksheet}

\item{cols}{Columns to merge}

\item{rows}{corresponding rows to merge}
}
\description{
\code{mergeCellForced} wraps around \code{\link[openxlsx:removeCellMerge]{openxlsx::removeCellMerge()}} and \code{\link[openxlsx:mergeCells]{openxlsx::mergeCells()}}
to merge cells if there are merged cell in the specified range.
}
