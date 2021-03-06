#' Count number of data points since specified date
#'
#' `count_since` takes a `billionaire::load_billion_data()` dataframe and
#' count the number of data points exists for each indicator in `df` since the
#' specified `year`.
#'
#' @inheritParams export_country_summary_xls
#' @param year_specified numeric
#'
#' @return data frame

count_since <- function(df, year_specified) {
  billionaiRe:::assert_columns(df, "year", "ind", "iso3", "type")
  billionaiRe:::assert_numeric(year_specified)

  df %>%
    dplyr::filter(.data[["type"]] %in% c("estimated", "reported")) %>%
    dplyr::group_by(.data[["iso3"]], .data[["ind"]]) %>%
    dplyr::filter(.data[["year"]] >= !!year_specified) %>%
    dplyr::summarise(!!sym(glue::glue("count_{year_specified}")) := dplyr::n(), .groups = "drop")
}

#' Get order of indicator
#'
#' @param ind character vector of indicators
#'
#' @return character vector

get_ind_order <- function(ind) {
  data.frame(ind = ind) %>%
    dplyr::left_join(billionaiRe::indicator_df, by = c(ind = "ind")) %>%
    dplyr::pull("order")
}

#' Get latest reported data frame
#'
#' `get_latest_reported()` gets the latest reported data available for estimated
#' or reported data. Used in write functions.
#' @param level integer indicating the level column. Specific to HEP.
#' @inherit export_hpop_country_summary_xls
#' @inheritParams write_uhc_timeseries_sheet
#' @inheritParams write_hpop_summary_sheet
get_latest_reported_df <- function(df, value_col, transform_value_col = NULL, level = NULL, ind_df) {
  df <- df %>%
    dplyr::filter(.data[["type"]] %in% c("estimated", "reported"))

  if (nrow(df) > 1) {
    df <- df %>%
      dplyr::group_by(.data[["iso3"]], .data[["ind"]]) %>%
      dplyr::filter(.data[["year"]] == max(.data[["year"]])) %>%
      dplyr::ungroup()
  }

  df <- ind_df[, "ind"] %>%
    dplyr::left_join(df, by = c("ind" = "ind")) %>%
    dplyr::select(dplyr::all_of(c(
      "ind", value_col, transform_value_col, level, "year",
      "type", "source"
    ))) %>%
    dplyr::mutate(!!sym("year") := as.integer(.data[["year"]]))

  return(df)
}

#' Get baseline and projections data frame for specified dates
#'
#' `get_latest_reported()` gets the latest reported data available for estimated
#' or reported data. Used in write functions.
#'
#' @inherit export_hpop_country_summary_xls
#' @inheritParams export_all_countries_summaries_xls
#' @inheritParams write_hpop_summary_sheet
get_baseline_projection_df <- function(df, value_col, transform_value_col, start_year, end_year, ind_df) {
  df <- df %>%
    dplyr::filter(.data[["year"]] %in% c(!!start_year, max(!!end_year))) %>%
    dplyr::select(dplyr::all_of(c(
      "ind", "year", value_col, transform_value_col, "type",
      "source", "iso3"
    ))) %>%
    dplyr::group_by(.data[["ind"]], .data[["iso3"]]) %>%
    dplyr::arrange(.data[["year"]]) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(
      names_from = .data[["year"]],
      values_from = c(dplyr::all_of(c(value_col, transform_value_col)), "type", "source")
    ) %>%
    dplyr::ungroup()

  df <- ind_df[, "ind"] %>%
    dplyr::left_join(df, by = c("ind" = "ind"))
  return(df)
}
