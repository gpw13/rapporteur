% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_excel_formulas.R
\name{as_excel_formula}
\alias{as_excel_formula}
\title{Coerce coerce column to Excel Formula}
\usage{
as_excel_formula(col, array = FALSE)
}
\arguments{
\item{col}{integer or character vector identifying the column to be coerced}

\item{array}{A bool if the function written is of type array}
}
\description{
\code{as_excel_formula_column} coerce a specified column into an Excel formula to be
used in \code{\link[openxlsx:writeFormula]{openxlsx::writeFormula()}} or \code{\link[openxlsx:writeData]{openxlsx::writeData()}}.
This transformation allows to avoid issue of Excel formula being interpreted
as normal text by Excel.
}
