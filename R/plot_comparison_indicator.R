#' Export plot comparison of two billionaiRe data frames for indicator
#'
#' Plot comparison between two billionaiRe data frames fro an indicator
#'
#' @param year_col Column name of column with years.
#' @param iso3_col Column name of column with country ISO3 codes.
#' @param ind Column name of column with indicator names.
#' @param value Column name of column(s) with indicator's values, used to
#'     calculate contributions.
#' @param type_col Column name of column with type data.
#' @inherit write_hep_summary_sheet
#' @inheritParams export_country_summary_xls
#' @inheritParams export_plot_timeseries_indicator_pdf
#' @inheritParams export_plot_comparison_pdf
#'
#' @return a `ggplot2` object
#'
plot_comparison_indicator <- function(...,
                                      indicator,
                                      ind = "ind",
                                      iso3_col = "iso3",
                                      year_col = "year",
                                      value = "value",
                                      type_col = "type",
                                      ind_ids = billionaiRe::billion_ind_codes("all")) {

  dfs <- rlang::list2(...)

  if(length(dfs) != 2){
    stop("Only two data frames are supported by comparaison plots ", call. = FALSE)
  }

  assert_columns(dfs[[1]], ind, iso3_col, year_col, type_col, value)
  assert_columns(dfs[[2]], ind, iso3_col, year_col, type_col, value)
  assert_unique_rows(dfs[[1]], ind, iso3_col, year_col, ind_ids = ind_ids)
  assert_unique_rows(dfs[[2]], ind, iso3_col, year_col, ind_ids = ind_ids)

  if(is.null(names(dfs))){
    old <- "old"
    new <- "new"
  }else{
    old <- ifelse(names(dfs)[1] != "", names(dfs)[1], "old")
    new <- ifelse(names(dfs)[2] != "", names(dfs)[2], "new")
  }
  cols_to_keep <- c(
    ind, iso3_col, year_col, value, type_col
  )

  new_df_ind_grp <- dfs[[1]] %>%
    dplyr::select(dplyr::any_of(cols_to_keep)) %>%
    dplyr::filter(
      .data[[ind]] == indicator,
      !is.na(.data[[value]])
    ) %>%
    dplyr::mutate(data_frame_type = new)

  old_df_ind_grp <- dfs[[2]] %>%
    dplyr::select(dplyr::any_of(cols_to_keep)) %>%
    dplyr::filter(
      .data[[ind]] == indicator,
      !is.na(.data[[value]])
    ) %>%
    dplyr::mutate(data_frame_type = old)

  if(nrow(new_df_ind_grp) == 0 | nrow(old_df_ind_grp) == 0){
    if(nrow(new_df_ind_grp) == 0){
      message(sprintf("no data in %s for %s", new, indicator))
    }
    if(nrow(old_df_ind_grp) == 0){
      message(sprintf("no data in %s for %s", old, indicator))
    }
    return(NULL)
  }

  full_df <- tidyr::expand_grid(
    "{iso3_col}" := unique(c(old_df_ind_grp[[iso3_col]], new_df_ind_grp[[iso3_col]])),
    "{ind}" := indicator,
    "{year_col}" := min(min(old_df_ind_grp[[year_col]]), min(new_df_ind_grp[[year_col]])):max(max(old_df_ind_grp[[year_col]]), max(new_df_ind_grp[[year_col]])),
    data_frame_type = c(new, old)
  )

  combined_df <- dplyr::bind_rows(old_df_ind_grp, new_df_ind_grp) %>%
    dplyr::full_join(full_df, by = c(ind, iso3_col, year_col, "data_frame_type")) %>%
    dplyr::mutate(
      "{year_col}" := lubridate::as_date(paste(.data[[year_col]], 1, 1, sep = "-"))
    ) %>%
    dplyr::ungroup()


  combined_df_wide <- combined_df %>%
    dplyr::group_by(.data[[iso3_col]], .data[[ind]], .data[[year_col]]) %>%
    dplyr::arrange(.data[["data_frame_type"]]) %>%
    tidyr::pivot_wider(-.data[[type_col]], names_from = .data[["data_frame_type"]], values_from = value)%>%
    dplyr::mutate("{old}" := dplyr::case_when(
      is.na(.data[[old]]) & !is.na(.data[[new]]) ~ .data[[new]],
      TRUE ~ .data[[old]]
    ),
    new = dplyr::case_when(
      is.na(.data[[new]]) & !is.na(.data[[old]]) ~ .data[[old]],
      TRUE ~ .data[[new]]
    )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(dplyr::if_all(c(old, new), ~ !is.na(.x)))

  combined_df_wide %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[year_col]]))+
    ggplot2::geom_text(
      ggplot2::aes(
        label = .data[[iso3_col]],
        x = as.Date(paste(min(.data[[year_col]]), 1, 1, sep = "-")),
        y = -Inf,
        vjust = -2,
        hjust = -0.5
      ),
      color = "grey50"
    ) +
    ggplot2::facet_wrap(~.data[[iso3_col]], ncol = 20)+
    ggplot2::geom_line(ggplot2::aes(y = .data[[old]],colour = !!old))+
    ggplot2::geom_line(ggplot2::aes(y = .data[[new]],colour = !!new))+
    ggh4x::stat_difference(ggplot2::aes(
                               ymax = .data[[new]],
                               ymin = .data[[old]],
                               fill = "Difference"
                               ),
                           alpha = 0.7,
                           fill = "#B2182B"
    )+
    ggplot2::scale_color_manual(values = c("#003f5c","#bc5090"))+
    ggplot2::scale_x_date(date_labels = "%y", date_breaks = "10 years") +
    theme_billionaiRe()+
    ggplot2::ggtitle(
      glue::glue("Comparing {old} and {new}: {indicator}")
    )

}
