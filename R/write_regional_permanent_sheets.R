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
write_regional_permanent_sheets <- function(start_year,
                                            end_year) {
  wb_file <- system.file("extdata",
                         "regional_summary_template.xlsx",
                         package = "rapporteur"
  )

  wb <- openxlsx::loadWorkbook(wb_file)

  wb <- write_regional_intro_sheet(wb, start_year, end_year)

  return(wb)
}

write_regional_intro_sheet <- function(wb, start_year, end_year){
  intro_bounds <- get_box_bounds_regional("Intro")

  openxlsx::writeData(wb,
                      sheet = "Intro",
                      x = vec2emptyDF(glue::glue("{start_year} Baseline value against regional median")),
                      startRow = intro_bounds$interp2$start_row,
                      startCol = intro_bounds$interp2$start_col)

  openxlsx::writeData(wb,
                      sheet = "Intro",
                      x = vec2emptyDF(glue::glue("Expected progress between {start_year} - {end_year}")),
                      startRow = intro_bounds$interp3$start_row,
                      startCol = intro_bounds$interp3$start_col)

  openxlsx::writeData(wb,
                      sheet = "Intro",
                      x = vec2emptyDF(glue::glue("For the aggregate components of the UHC Billion (Financial Hardship and Average Service Coverage), the direction of the trend is shown. This shows the expected direction of change from {start_year}-{end_year}, without the impact of COVID")),
                      startRow = intro_bounds$interp4$start_row + 1,
                      startCol = intro_bounds$interp4$start_col)


  return(wb)
}
