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
    "{ind}" := ind,
    "{year_col}" := min(min(old_df_ind_grp[[year_col]]), min(new_df_ind_grp[[year_col]])):max(max(old_df_ind_grp[[year_col]]), max(new_df_ind_grp[[year_col]]))
  )

  combined_df <- full_df %>%
    dplyr::left_join(old_df_ind_grp, by = c(ind, iso3_col, year_col)) %>%
    dplyr::left_join(new_df_ind_grp, by = c(ind, iso3_col, year_col))


  combined_df <- dplyr::bind_rows(old_df_ind_grp, new_df_ind_grp) %>%
    dplyr::mutate(type_line = dplyr::case_when(
      .data[[type_col]] %in% c("reported","imputed", "estimated") ~ "Reported, estimated or imputed",
      .data[[type_col]] == "projected" ~ "Projected",
      TRUE ~ .data[[type_col]]),
      "{year_col}" := lubridate::as_date(paste(.data[[year_col]], 1, 1, sep = "-")),
      "data_frame_type" := factor(.data[["data_frame_type"]], levels = c("old", "new"))
    )

  combined_df_wide <- combined_df %>%
    dplyr::group_by(dplyr::across(c(ind, iso3_col, year_col, data_frame_type))) %>%
    tidyr::pivot_wider(names_from = data_frame_type, values_from = c(value, type_line)) %>%
    dplyr::mutate(new_diff_old = .data[[glue::glue("{value}_old")]] - .data[[glue::glue("{value}_new")]],
                  combined_type = dplyr::case_when(
                    is.na(.data[[glue::glue("{value}_new")]]) ~ "Old",
                    is.na(.data[[glue::glue("{value}_old")]]) ~ "New",
                    new_diff_old == 0 ~ "No change",
                    TRUE ~ "Change"
                  )) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::any_of(c(ind, iso3_col, year_col, "combined_type")))
  tidyr::pivot_longer(dplyr::any_of(c(ind, iso3_col, year_col, "combined_type")),
                      names_to = "data_frame_type", values_to = value)

  #
  # has_changes_df <- combined_df %>%
  #   dplyr::group_by(dplyr::across(c(ind, iso3_col))) %>%
  #   dplyr::summarise(has_changes = dplyr::if_else(sum(.data[["new_diff_old"]], na.rm = TRUE) > 0, TRUE, FALSE))

  # dplyr::left_join(has_changes_df, by = c(iso3_col, ind)) %>%
  # dplyr::filter(.data[["has_changes"]]) %>%
  combined_df_wide %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[year_col]]))+
    ggplot2::geom_line(ggplot2::aes(y=.data[[glue::glue("{value}_new")]]), size=.2  ) +
    ggplot2::geom_point(ggplot2::aes(y=.data[[glue::glue("{value}_new")]]), colour=1,size=.2) +
    ggplot2::geom_line(ggplot2::aes(colour=.data[["type_line_new"]], y=.data[[glue::glue("{value}_old")]]) , linetype =2, size=.2 ) +
    ggplot2::geom_point(ggplot2::aes(colour=.data[["type_line_new"]], y=.data[[glue::glue("{value}_old")]]), size=.2)+
    ggplot2::geom_ribbon(ggplot2::aes(ymin=.data[[glue::glue("{value}_new")]],ymax=.data[[glue::glue("{value}_old")]]),linetype=0,fill="RED", alpha=0.6) +
    ggplot2::facet_wrap(~.data[[iso3_col]], ncol = 20)+
    ggplot2::scale_x_date(date_labels = "%y", date_breaks = "10 years") +
    theme_billionaiRe()

}
