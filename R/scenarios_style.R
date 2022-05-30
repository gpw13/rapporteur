#' Change font of scenario_col values
#'
#' `scenarios_style()` changes the font of time series values based on the
#' indicator type it is:
#'
#' * bold: reported
#' * normal: estimated
#' * faded: imputed or projected
#'
#' @param scenarios_order (character) vector in which `scenario_col` should be `dplyr::arrange`
#' @inheritParams write_baseline_projection_hpop_summary
#' @inherit style_header_hpop_summary_sheet
#' @inheritParams write_hpop_timeseries_sheet
#' @inheritParams export_all_countries_summaries_xls
scenarios_style <- function(df,
                            wb,
                            sheet_name,
                            start_row,
                            start_col,
                            scenario_col,
                            ind_df,
                            scenarios_order) {

  ind_order <- ind_df %>%
    dplyr::select(.data[["ind"]], .data[["order"]]) %>%
    dplyr::distinct()

  wide_df <- df %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(c("ind", "year", "type", scenario_col))) %>%
    dplyr::left_join(ind_order, by = c("ind" = "ind")) %>%
    dplyr::arrange(order, factor(.data[[scenario_col]], scenarios_order)) %>%
    dplyr::filter(!stringr::str_detect(.data[["ind"]], "^hpop_healthier")) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("ind", scenario_col)))) %>%
    tidyr::pivot_wider(names_from = .data[["year"]], values_from = .data[["type"]]) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("20"), tidyr::replace_na, ""))

  wide_df <- ind_df[, "ind"] %>%
    dplyr::left_join(wide_df, by = "ind") %>%
    dplyr::select(-"order")

  args <- list(
    "type" = list("reported", "projected", "imputed", ""),
    "fontColour" = list(NULL, "grey", "grey", NULL),
    "textDecoration" = list("bold", NULL, NULL, NULL)
  )

  purrr::pwalk(args,
               type_styler,
               wb = wb,
               df = wide_df,
               sheet_name = sheet_name,
               start_row = start_row,
               start_col = start_col
  )
}

