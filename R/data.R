#' outputs to fade out pathongens not applicable to country.
#'
#' @format A data frame with `r nrow(affected_pathogens)` rows and `r ncol(affected_pathogens)` variables:
#' \describe{
#'   \item{iso3}{Country ISO3 codes.}
#'   \item{yellow_fever}{TRUE if country is affected by yellow fever.}
#'   \item{cholera}{TRUE if country is affected by cholera.}
#'   \item{meningitis}{TRUE if country is affected by meningitis.}
#' }
"affected_pathogens"

#' Example dataset of all three billions
#'
#' This dataset contains dummy data for all three billions. The data is provided
#' as an example and does not reflect the situation in any of the the
#' three countries. This dataset should be understood as having going through
#' the `billionaiRe` pipeline for all three billions. See the
#' [billionaiRe](https://github.com/gpw13/billionaiRe) package for more details.
#'
#'
#' @format A data frame with `r nrow(all_billions_example)` rows and `r ncol(all_billions_example)` variables:
#' \describe{
#'   \item{iso3}{Country ISO3 codes.}
#'   \item{year}{Year.}
#'   \item{ind}{Indicator.}
#'   \item{value}{Value.}
#'   \item{use_dash}{Bolean indicating.}
#'   \item{source}{Source of the data.}
#'   \item{type}{Type of data.}
#'   \item{transform_value}{Transformed value by billionaiRe package.}
#'   \item{level}{Levels calculated by billionaiRe package.}
#'   \item{contribution}{Contribution of indicator to billion.}
#'   \item{contribution_percent}{Percent contribution to billion.}
#'   \item{population}{Population numbers to be used.}
#'   \item{contribution_percent_total_pop}{Percent of total population contribution to billion.}
#' }
"all_billions_example"
