#' Export plot timeseries for an indicator to pdf
#'
#' `export_plot_timeseries_indicator_pdf` exports plots to pdf.
#'
#' @param indicator name of indicator to plot
#' @param scale type of scale to be exported. Can be either:
#' - `common`: same scale for all iso3
#' - `free`: individual scale for each iso3
#' - `combined`: combination of common and free.
#' By default, `combined` is used
#' @param experiment name of the experiment of plots. Default to NULL. If used, a new
#' folder will be added to `output_folder` to store the results.
#' @param group_iso3 if TRUE (default), iso3s will be grouped by 54 to have more readable graphs.
#' @param version character vector identifying the version of the plot to be passed.
#' @inherit write_hep_summary_sheet
#' @inheritParams export_country_summary_xls
#'
#' @export
export_plot_timeseries_indicator_pdf <- function(df,
                                                 indicator,
                                                 scale = c("combined", "free", "fixed", "free_x", "free_y"),
                                                 output_folder = "outputs",
                                                 experiment = NULL,
                                                 group_iso3 = TRUE,
                                                 default_scenario = "default",
                                                 value_col = "value",
                                                 start_year = 2018,
                                                 ind_ids = billionaiRe::billion_ind_codes("all", include_calculated = TRUE),
                                                 version = whdh::get_formatted_timestamp()) {
  scale <- rlang::arg_match(scale)

  if (scale == "combined") {
    scales <- c("fixed", "free")
  } else {
    scales <- scale
  }

  df_ind <- df %>%
    dplyr::filter(.data[["ind"]] == !!indicator)

  unique_iso3 <- sort(unique(df_ind[["iso3"]]))

  if (group_iso3) {
    iso3_groups <- split(unique_iso3, ceiling(seq_along(unique_iso3) / 54))
  } else {
    iso3_groups <- list(unique_iso3)
  }

  names(iso3_groups) <- purrr::map_chr(iso3_groups, function(x) {
    paste0(x[[1]], "_", x[[length(x)]])
  })

  if (!is.null(experiment)) {
    output_folder <- here::here(output_folder, experiment)
  }

  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }

  temp_dir <- tempdir()

  purrr::walk(
    iso3_groups, ~ export_plot_timeseries_indicator_iso3_group(
      df = df_ind,
      iso3_group = .x,
      indicator = indicator,
      output_folder = temp_dir,
      value_col = value_col,
      default_scenario = default_scenario,
      start_year = start_year,
      scale = scales,
      experiment = experiment
    )
  )

  combined_pdf <- pdftools::pdf_combine(
    c(here::here(temp_dir, paste0(stringr::str_c("temp", experiment, indicator, scale, names(iso3_groups), sep = "_"), ".pdf"))),
    here::here(output_folder, paste0(stringr::str_c(experiment, indicator, scale, version, sep = "_"), ".pdf"))
  )
}

#' Export timeseries for a group of countries
#'
#' @param iso3_group named list of iso3 to be ploted
#' @inheritParams export_country_summary_xls
#'
#' @inherit export_plot_timeseries_indicator_pdf
export_plot_timeseries_indicator_iso3_group <- function(df,
                                                        iso3_group,
                                                        indicator,
                                                        output_folder,
                                                        scale,
                                                        value_col,
                                                        default_scenario,
                                                        start_year,
                                                        experiment) {
  group_id <- paste0(iso3_group[[1]], "_", iso3_group[[length(iso3_group)]])

  if (!is.null(experiment)) {
    experiment_index <- paste0(experiment, "_")
  } else {
    experiment_index <- NULL
  }


  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }

  plots <- purrr::map(scale, ~ plot_timeseries_indicator(
    df = df,
    iso3 = iso3_group,
    indicator = indicator,
    scale = .x,
    value_col = value_col,
    default_scenario = default_scenario,
    start_year = start_year
  ))
  names(plots) <- scale

  purrr::walk(scale, ~
    ggplot2::ggsave(here::here(output_folder, paste0("temp_", experiment_index, indicator, "_", .x, "_", group_id, ".pdf")),
      plots[[paste0(.x)]],
      width = 210, height = 297, units = "mm"
    ))

  if (length(scale) > 1) {

    pdftools::pdf_combine(
      c(here::here(output_folder, paste0("temp_", experiment_index, indicator, "_", scale, "_", group_id, ".pdf"))),
      here::here(output_folder, paste0("temp_", experiment_index, indicator, "_combined_", group_id, ".pdf"))
    )

    file.remove(c(here::here(output_folder, paste0("temp_", experiment_index, indicator, "_", scale, "_", group_id, ".pdf"))))
  }
}
