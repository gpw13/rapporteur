#' Export plot timeseries for an indicator
#'
#' `export_plot_timeseries_indicator` exports plots in the `format` specified.
#'
#' @param indicator name of indicator to plot
#' @param format format of the export to produced. Can be either:
#' - **pdf**
#' - **png** for images
#' - **plotly** for html interactive graphs
#' Only pdf is implemented at the moment.
#' @inherit write_hep_summary_sheet
#' @inheritParams export_country_summary_xls
#'
#' @export
export_plot_timeseries_indicator <- function(df,
                                        indicator,
                                        format = c("pdf", "png", "plotly"),
                                        output_folder = "outputs",
                                        ind = "ind",
                                        scenario = "scenario",
                                        default_scenario = "default",
                                        type_col = "type",
                                        transform_value = "transform_value",
                                        start_year = 2018,
                                        year = "year",
                                        iso3 = "iso3",
                                        ind_ids = billionaiRe::billion_ind_codes("all", include_calculated = TRUE)) {
  format <- rlang::arg_match(format)

  df_ind <- df %>%
    dplyr::filter(.data[[ind]] == !!indicator)

  unique_iso3 <- sort(unique(df_ind[[iso3]]))
  iso3_groups <- split(unique_iso3, ceiling(seq_along(unique_iso3) / 54))

  all_plots <- purrr::map(iso3_groups, ~ plot_timeseries_indicator(
    df = df_ind,
    iso3 = .x,
    indicator = indicator,
    ind = ind,
    iso3_col = iso3,
    year_col = year,
    transform_value = transform_value,
    type_col = type_col,
    scenario = scenario,
    default_scenario = default_scenario,
    start_year = start_year  ))

  # pdftools::pdf_combine(c(paste0("temp/temp_",indicator,"_combined_", seq_along(iso3_groups), ".pdf")),
  #                       paste0("output/", billion,"_plots/",billion, "_acceleration_",ind, ".pdf"))
  #
  # # From group_iso3
  # ggsave(paste0("temp/temp_",ind,"_common_", group_id, ".pdf"), plot,
  #        width = 210, height = 297, units = "mm")
  #
  # ggsave(paste0("temp/temp_",ind,"_free_", group_id, ".pdf"), plot,
  #        width = 210, height = 297, units = "mm")
  #
  # pdftools::pdf_combine(c(paste0("temp/temp_",ind,"_common_", group_id, ".pdf"), paste0("temp/temp_",ind,"_free_", group_id, ".pdf")),
  #                       paste0("temp/temp_",ind,"_combined_", group_id, ".pdf"))
  # file.remove(c(paste0("temp/temp_",ind,"_common_", group_id, ".pdf"), paste0("temp/temp_",ind,"_free_", group_id, ".pdf")))
}


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
#' @param transform_value Column name of column(s) with transformed indicator
#'     values, used to calculate contributions.
#' @param scenario Column name of column with scenario identifiers. Useful for
#'     calculating contributions on data in long format rather than wide format.
#' @param type_col Column name of column with type data.
#'
#' @inherit write_hep_summary_sheet
#' @inheritParams export_country_summary_xls
#' @inheritParams export_plot_timeseries_indicator
#'
#' @return list with two `ggplot2` objects: one with a common scale for all iso3
#' and one with an individual scale for each iso3.
#'
plot_timeseries_indicator <- function(df,
                                      iso3,
                                      indicator,
                                      ind = "ind",
                                      iso3_col = "iso3",
                                      year_col = "year",
                                      transform_value = "transform_value",
                                      type_col = "type",
                                      scenario = "scenario",
                                      default_scenario = "default",
                                      start_year = 2018) {

  # TODO: sort iso3 labels
  df_ind_grp <- df %>%
    dplyr::filter(
      .data[[iso3_col]] %in% iso3,
      .data[[ind]] == indicator,
      !is.na(.data[[transform_value]])
    ) %>%
    dplyr::filter(dplyr::case_when(
      .data[[scenario]] != default_scenario & .data[["recycled"]] == FALSE ~ TRUE,
      .data[[scenario]] == default_scenario ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    dplyr::mutate(!!sym(type_col) := dplyr::case_when(
      .data[[scenario]] == default_scenario ~ stringr::str_to_sentence(.data[[type_col]]),
      TRUE ~ as.character(.data[[scenario]])
    ),
    type_line = dplyr::case_when(
      .data[[year_col]] <= start_year ~ "Imputed",
      TRUE ~ as.character(.data[[type_col]])
    ),
    color_type = get_scenario_colour(.data[[type_col]]),
    color_line = get_scenario_colour(.data[["type_line"]])
    ) %>%
    dplyr::arrange(.data[[iso3_col]], .data[[year_col]])

  if(nrow(df_ind_grp) == 0){
    warning(paste0("No values for ", indicator))

    return(NULL)
  }

  df_ind_grp_min_max <- df_ind_grp %>%
    dplyr::group_by(.data[[iso3_col]]) %>%
    dplyr::summarise (ymin = min(.data[[transform_value]], na.rm = TRUE),
               ymax = max(.data[[transform_value]], na.rm = TRUE)) %>%
    dplyr::mutate(ymin = pmax(0, floor(.data[["ymin"]] / 1) * 1),
           ymax = pmin(ceiling(.data[["ymax"]] / 1) * 1))

  df_ind_grp <- df_ind_grp %>%
    dplyr::left_join(df_ind_grp_min_max, by = iso3_col)

  iso3_labels <- df_ind_grp %>%
    dplyr::mutate(x_common = min(.data[[year_col]]) + 4, y_common = 30) %>%
    dplyr::group_by(.data[[iso3_col]]) %>%
    dplyr::summarise(
      x_common = min(.data[["x_common"]]),
      y_common = min(.data[["y_common"]]),
      x_free = min(.data[[year_col]]) + 0.2 * (max(.data[[year_col]]) - min(.data[[year_col]])),
      y_free = min(.data[[transform_value]]) + 0.2 * (max(.data[[transform_value]]) - min(.data[[transform_value]]))
    )

  line_type_name <- unique(df_ind_grp[[type_col]])
  color_type_breaks <- df_ind_grp %>%
    dplyr::select(dplyr::all_of(c(type_col,"color_type"))) %>%
    dplyr::distinct() %>%
    dplyr::pull(.data[["color_type"]])

  base_plot <- ggplot2::ggplot(df_ind_grp, ggplot2::aes(x = as.Date(paste(.data[[year_col]],1,1, sep = "-")), y = .data[[transform_value]])) +
    ggplot2::geom_path(ggplot2::aes(color = .data[["color_line"]]), alpha = 0.5) +
    ggplot2::geom_point(ggplot2::aes(colour = .data[["color_type"]]), size = 1) +
    ggplot2::scale_color_identity(guide = "legend", labels = line_type_name, breaks = color_type_breaks)+
    ggplot2::scale_x_date(date_labels = "%y", date_breaks = "5 years")

  plot_common_scale <- base_plot +
    ggplot2::facet_wrap(~ .data[[iso3_col]], ncol = 6, nrow = 9) +
    ggplot2::geom_text(data = iso3_labels ,
                       ggplot2::aes(x = as.Date(paste(.data[["x_common"]],1,1, sep = "-")),
                                    y= .data[["y_common"]],
                                    label = .data[[iso3_col]]),
              color = "grey50")+
    ggplot2::ggtitle(paste0(toupper(indicator), ": ", iso3[1], " - ", iso3[length(iso3)], ": - common scale")) +
    theme_billionaiRe()

  plot_free_scale <- base_plot +
    ggplot2::facet_wrap(~ .data[[iso3_col]], ncol = 6, nrow = 9, scales = "free") +
    ggplot2::geom_blank(ggplot2::aes(y = .data[["ymin"]])) +
    ggplot2::geom_blank(ggplot2::aes(y = .data[["ymax"]])) +
    ggplot2::geom_text(data = iso3_labels ,
                       ggplot2::aes(x = as.Date(paste(.data[["x_free"]],1,1, sep = "-")),
                                    y= .data[["y_free"]],
                                    label = .data[[iso3_col]]),
                       color = "grey50")+
    ggplot2::ggtitle(paste0(toupper(indicator), ": ", iso3[1], " - ", iso3[length(iso3)], ": - separate scale")) +
    theme_billionaiRe()

  list(common_scale = plot_common_scale, free_scale = plot_free_scale)
}
