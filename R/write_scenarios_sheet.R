#' Write scenario sheet
#'
#' @inherit write_baseline_projection_hpop_summary
#' @param ind_df data frame containing information on indicators
#' @inherit export_hpop_country_summary_xls
#' @inheritParams style_header_hpop_summary_sheet
#' @inheritParams style_scenarios_sheet
write_scenario_sheet <- function(df,
                                 wb = wb,
                                 billion = c("hep", "uhc", "hpop"),
                                 sheet_name = "Scenarios",
                                 start_row = 4,
                                 start_col = 2,
                                 value_col = "value",
                                 scenario_col = "scenario",
                                 ind_df,
                                 ind_ids,
                                 start_year = 2018,
                                 end_year = 2025,
                                 default_scenario = "default"){

  billion <- rlang::arg_match(billion)
  openxlsx::ungroupRows(wb, sheet = sheet_name, rows = 1:20000)
  openxlsx::deleteData(wb, sheet = sheet_name, cols = start_col:20, rows = start_row+2:20000, gridExpand = TRUE)

  unique_scenarios <- sort(unique(df[[scenario_col]]))

  scenarios_order <- c(default_scenario, unique_scenarios[-match(default_scenario, unique_scenarios)])

  nice_inds <- ind_df %>%
    dplyr::filter(.data[["ind"]] %in% ind_ids) %>%
    dplyr::select(c("ind", "short_name", "order")) %>%
    tidyr::expand_grid(scenario = scenarios_order)

  years_range_chr <- c(as.character(c(start_year:max(end_year))))

  df_iso_scenarios <- df %>%
    dplyr::select(dplyr::any_of(c(scenario_col, "ind", "year", value_col, "type"))) %>%
    dplyr::filter(.data[["year"]] >= start_year,
                  .data[["year"]] <= max(end_year),
                  .data[["ind"]] %in% ind_ids) %>%
    dplyr::arrange(.data[["year"]])

  df_iso_scenarios_wide <- df_iso_scenarios %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(scenario_col, "ind")))) %>%
    dplyr::select(-dplyr::any_of(c("type"))) %>%
    tidyr::pivot_wider(names_from = "year", values_from = .data[[value_col]]) %>%
    dplyr::ungroup()

  df_iso_scenarios_nice_ind_wide <- nice_inds %>%
    dplyr::left_join(df_iso_scenarios_wide, by = c("ind" = "ind", "scenario" = scenario_col)) %>%
    dplyr::arrange(order, factor(.data[[scenario_col]], scenarios_order)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(c(scenario_col, "short_name", years_range_chr)))


  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = vec2emptyDF(years_range_chr),
                      startCol = start_col + 2, startRow = start_row + 1,
                      colNames = TRUE
  )

  openxlsx::writeData(wb, sheet = sheet_name,
                      x = df_iso_scenarios_nice_ind_wide,
                      startCol = start_col, startRow = start_row+ 2,
                      colNames = FALSE)

  openxlsx::writeData(wb, sheet = sheet_name,
                      x = vec2emptyDF(c("Scenario", "Indicator", "Raw values*")),
                      startCol = start_col, startRow = start_row,
                      colNames = FALSE)

  style_scenarios_sheet(
    df = df_iso_scenarios,
    wb = wb,
    billion = billion,
    sheet_name = sheet_name,
    start_row = start_row, start_col = start_col,
    scenario_col = scenario_col,
    scenarios_order = scenarios_order,
    df_wide = df_iso_scenarios_nice_ind_wide,
    ind_df = ind_df,
    ind_ids = ind_ids,
    this_iso3 = unique(df[["iso3"]])
  )


}
