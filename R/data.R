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
