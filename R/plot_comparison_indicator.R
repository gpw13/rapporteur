#' Export plot comparison of two billionaiRe data frames for indicator
#'
#' Plot comparison between two billionaiRe data frames fro an indicator
#'
#' @param iso3 vector of ISO3 country codes to plot
#' @param iso3_col name of column with
#' @param year_col Column name of column with years.
#' @param iso3_col Column name of column with country ISO3 codes.
#' @param ind Column name of column with indicator names.
#' @param value Column name of column(s) with indicator's values, used to
#'     calculate contributions.
#' @param type_col Column name of column with type data.
#' @param scale type of scale to be exported, as in \code{\link[ggplot2]{facet_wrap}}
#' @inherit write_hep_summary_sheet
#' @inheritParams export_country_summary_xls
#' @inheritParams export_plot_timeseries_indicator_pdf
#'
#' @export
#'
#' @return a `ggplot2` object
#'
plot_comparison_indicator <- function(new_df,
                                      old_df,
                                      indicator,
                                      ind = "ind",
                                      iso3_col = "iso3",
                                      year_col = "year",
                                      value = "value",
                                      type_col = "type",
                                      ind_ids = billion_ind_codes("all")) {

  billionaiRe:::assert_unique_rows(new_df, ind, iso3_col, year_col, ind_ids = ind_ids)
  billionaiRe:::assert_unique_rows(old_df, ind, iso3_col, year_col, ind_ids = ind_ids)

  cols_to_keep <- c(
    ind, iso3_col, year_col, value, type_col
  )

  new_df_ind_grp <- new_df %>%
    dplyr::select(dplyr::any_of(cols_to_keep)) %>%
    dplyr::filter(
      .data[[ind]] == indicator,
      !is.na(.data[[value]])
    ) %>%
    dplyr::rename("new_value" := .data[[value]],
                  "new_type" := .data[[type_col]])

  old_df_ind_grp <- old_df %>%
    dplyr::select(dplyr::any_of(cols_to_keep)) %>%
    dplyr::filter(
      .data[[ind]] == indicator,
      !is.na(.data[[value]])
    ) %>%
    dplyr::rename("old_value" := .data[[value]],
                  "old_type" := .data[[type_col]])

  full_df <- tidyr::expand_grid(
    "{iso3_col}" := unique(c(old_df_ind_grp[[iso3_col]], new_df_ind_grp[[iso3_col]])),
    "{ind}" := indicator,
    "{year_col}" := min(min(old_df_ind_grp[[year_col]]), min(new_df_ind_grp[[year_col]])):max(max(old_df_ind_grp[[year_col]]), max(new_df_ind_grp[[year_col]]))
  )

  combined_df <- full_df %>%
    dplyr::left_join(old_df_ind_grp, by = c(ind, iso3_col, year_col)) %>%
    dplyr::left_join(new_df_ind_grp, by = c(ind, iso3_col, year_col)) %>%
    dplyr::mutate(
      "{year_col}" := lubridate::as_date(paste(.data[[year_col]], 1, 1, sep = "-")),
      line_type = dplyr::case_when(
        is.na(.data[[glue::glue("new_{value}")]]) & !is.na(.data[[glue::glue("old_{value}")]]) ~ "Old",
        is.na(.data[[glue::glue("old_{value}")]]) & !is.na(.data[[glue::glue("new_{value}")]]) ~ "New",
        (.data[[glue::glue("old_{value}")]] - .data[[glue::glue("old_{value}")]]) == 0 ~ "No changes",
        TRUE ~ NA_character_
      ),
      "{value}" := dplyr::case_when(
        .data[["line_type"]] == "Old" ~ .data[[glue::glue("old_{value}")]],
        TRUE ~ .data[[glue::glue("new_{value}")]]
      )) %>%
    dplyr::filter(!is.na(.data[["line_type"]]))

  combined_df_long <- combined_df %>%
    tidyr::pivot_longer(dplyr::ends_with(value), names_to = glue::glue("{value}_type"), values_to = as.character(value))


  combined_df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[year_col]],
                                 colour = .data[["line_type"]]))+
    ggplot2::facet_wrap(~.data[[iso3_col]], ncol = 20)+
    ggplot2::geom_line(ggplot2::aes(y = .data[[value]], group = 1), size=.2)+
    ggplot2::geom_ribbon(ggplot2::aes(
      ymin = .data[[glue::glue("new_{value}")]],
      ymax=.data[[glue::glue("old_{value}")]]),
      linetype=0,fill="RED", alpha=0.6) +
    ggplot2::scale_x_date(date_labels = "%y", date_breaks = "10 years") +
    theme_billionaiRe()

}
