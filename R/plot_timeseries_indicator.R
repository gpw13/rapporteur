#' Plot timeseries for indicator
#'
#' Plot timeseries to provide line plots for each scenario present in `scenario`
#' with some manipulations. Produce two faceted plots where each facet is an
#' `iso3`.
#'
#' @param iso3 vector of ISO3 country codes to plot
#' @param iso3_col name of column with
#' @param year_col Column name of column with years.
#' @param iso3_col Column name of column with country ISO3 codes.
#' @param ind Column name of column with indicator names.
#' @param value Column name of column(s) with indicator
#'     values, used to calculate contributions.
#' @param scenario Column name of column with scenario identifiers. Useful for
#'     calculating contributions on data in long format rather than wide format.
#' @param type_col Column name of column with type data.
#' @param scale type of scale to be exported, as in \code{\link[ggplot2]{facet_wrap}}
#' @inherit write_hep_summary_sheet
#' @inheritParams export_country_summary_xls
#' @inheritParams export_plot_timeseries_indicator_pdf
#' @param base_scenarios (vector) named vector with the names of the base scenarios. For
#'    more details see \href{../doc/scenarios.html}{\code{vignette("scenarios", package = "billionaiRe")}}

#'
#' @export
#'
#' @return a `ggplot2` object
#'
plot_timeseries_indicator <- function(df,
                                      iso3,
                                      indicator,
                                      scale = c("free", "fixed", "free_x", "free_y"),
                                      ind = "ind",
                                      iso3_col = "iso3",
                                      year_col = "year",
                                      value = "value",
                                      type_col = "type",
                                      scenario = "scenario",
                                      default_scenario = "default",
                                      base_scenarios = c("routine" = "routine", "reference_infilling" ="reference_infilling"),
                                      start_year = 2018) {
  scale <- rlang::arg_match(scale)

  df_ind_grp <- df %>%
    dplyr::filter(
      .data[[iso3_col]] %in% !!iso3,
      .data[[ind]] == indicator,
      !is.na(.data[[value]])
    )

  if("recycled" %in% names(df_ind_grp)) {
    df_ind_grp <- billionaiRe::remove_recycled_data(df_ind_grp)
  }

  df_ind_grp <- df_ind_grp %>%
    dplyr::select(dplyr::any_of(c(iso3_col,ind, year_col, value, type_col, scenario, "source", "scenario_detail"))) %>%
    dplyr::mutate(
      plot_type = dplyr::case_when(
        .data[[scenario]] %in% c(default_scenario, base_scenarios) ~ stringr::str_to_sentence(.data[[type_col]]),
        .data[[scenario]] == "sdg" ~ "SDG",
        .data[[scenario]] == "acceleration" ~ "Acceleration",
        .data[[scenario]] == "pre_covid_trajectory" ~ "Pre-COVID-19 trajectories",
        .data[[scenario]] == "covid_shock" ~ "COVID-19 shock",
        TRUE ~ as.character(.data[[scenario]])
      ),
      plot_group = dplyr::case_when(
        .data[[scenario]] %in% base_scenarios ~ "base",
        TRUE ~ .data[[scenario]]
      ),
      plot_color = get_scenario_colour(.data[["plot_type"]])
    ) %>%
    dplyr::arrange(.data[[iso3_col]], .data[[year_col]])

  if (nrow(df_ind_grp) == 0) {
    warning(paste0("No values for ", indicator))

    return(NULL)
  }

  color_type_breaks_labels <- df_ind_grp %>%
    dplyr::select(dplyr::all_of(c("plot_type", "plot_color"))) %>%
    dplyr::distinct()

  plot_title <- ifelse(scale == "fixed",
                       paste0(indicator, ": ", iso3[1], " - ", iso3[length(iso3)], " - fixed scale"),
                       paste0(indicator, ": ", iso3[1], " - ", iso3[length(iso3)], " - free scale")
  )

  df_ind_grp_line <- df_ind_grp %>%
    dplyr::group_by(.data[[iso3_col]], .data[["plot_group"]]) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::across(dplyr::any_of(c(iso3_col, year_col, ind, "plot_group"))))

  plot_limits <- df_ind_grp %>%
    dplyr::group_by(.data[[iso3_col]], .data[["plot_group"]]) %>%
    dplyr::summarise (ymin = min(value, na.rm = TRUE),
               ymax = max(value, na.rm = TRUE)) %>%
    dplyr::mutate(ymin = pmax(0, floor(.data[["ymin"]] / 10) * 10),
           ymax = pmin(ceiling(.data[["ymax"]] / 10) * 10))

  df_ind_grp <- df_ind_grp %>%
    dplyr::left_join(plot_limits, by = c(iso3_col, "plot_group"))

  base_plot <- ggplot2::ggplot(df_ind_grp, ggplot2::aes(
    x = as.Date(paste(.data[[year_col]], 1, 1, sep = "-")),
    y = .data[[value]]
  ))

  if (length(iso3) > 1) {
    base_plot <- base_plot +
      ggplot2::facet_wrap(~ .data[[iso3_col]], ncol = 6, nrow = 9, scales = scale)
  }

  if (nrow(df_ind_grp_line) > 0) {
    df_ind_grp_line <- connect_lines(df_ind_grp_line,
                                     iso3_col = iso3_col, year_col = year_col,
                                     plot_color = "plot_color", ind_col = ind,
                                     value = value,
                                     plot_group ="plot_group")

    base_plot <- base_plot +
      ggplot2::geom_path(data = df_ind_grp_line,
                         ggplot2::aes(color = .data[["plot_color"]], group = .data[["plot_group"]]))
  }

  final_plot <- base_plot +
    ggplot2::geom_point(ggplot2::aes(colour = .data[["plot_color"]]), size = 1) +
    ggplot2::scale_color_identity(guide = "legend", labels = color_type_breaks_labels[["plot_type"]], breaks = color_type_breaks_labels[["plot_color"]]) +
    ggplot2::scale_x_date(date_labels = "%y", date_breaks = "5 years") +
    ggplot2::scale_y_continuous(breaks = integer_breaks(), expand = ) +
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
    ggplot2::geom_blank(ggplot2::aes(y = .data[["ymin"]])) +
    ggplot2::geom_blank(ggplot2::aes(y = .data[["ymax"]])) +
    ggplot2::ggtitle(plot_title) +
    theme_billionaiRe()
}
