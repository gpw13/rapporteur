#' Export country summary to Excel
#'
#' `export_country_summary_xls` Export a country-specific for all three
#' billions or for a specific billion.
#' @param iso ISO3 code of country to summarize.
#' @inherit export_all_countries_summaries_xls
#'
#' @export
export_regional_summary_xls <- function(df,
                                       billion = c("hpop", "hep", "uhc", "all"),
                                       year = "year",
                                       iso3 = "iso3",
                                       ind = "ind",
                                       value = "value",
                                       transform_value = "transform_value",
                                       scenario = NULL,
                                       type_col = "type",
                                       source_col = "source",
                                       population = "population",
                                       contribution = "contribution",
                                       contribution_pct = paste0(contribution, "_percent"),
                                       contribution_pct_total_pop = paste0(contribution, "_percent_total_pop"),
                                       start_year = 2018,
                                       end_year = 2019:2025,
                                       output_folder = "outputs") {
  billion <- rlang::arg_match(billion)

  bounds <- list(
    intro_intrep2 = list(start_row = 26, end_row = 26,
                      start_col = 3, end_col = 3)
  )

  wb <- write_regional_permanent_sheets(bounds, start_year, end_year)


}
