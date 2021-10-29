#' Write permanent sheets for regional summary
#'
#' `write_regional_permanent_sheets()` writes and styles the Intro, Indicator
#' View, and Country View sheets of the regional summary workbook. Those sheets
#' are present in all cases taken into account by the different functions.
#'
#' @inheritParams write_empty_white_data
#' @inheritParams export_all_countries_summaries_xls
#'
#' @return A `openxlsx` workbook object
write_regional_permanent_sheets <- function(bounds,
                                            start_year,
                                            end_year) {
  wb_file <- system.file("extdata",
                         "regional_summary_template.xlsx",
                         package = "rapporteur"
  )

  wb <- openxlsx::loadWorkbook(wb_file)

  return(wb)
}
