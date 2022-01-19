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
#' @param run_name name of the run of plots. Default to NULL. If used, a new
#' folder will be added to `output_folder` to store the results.
#' @param group_iso3 if TRUE (default), iso3s will be grouped by 54 to have more readable graphs.
#' @inherit write_hep_summary_sheet
#' @inheritParams export_country_summary_xls
#'
#' @export
export_plot_timeseries_indicator_pdf <- function(df,
                                                 indicator,
                                                 scale = c("combined", "free", "fixed", "free_x", "free_y"),
                                                 output_folder = "outputs",
                                                 run_name = NULL,
                                                 group_iso3 = TRUE,
                                                 ind = "ind",
                                                 scenario = "scenario",
                                                 default_scenario = "default",
                                                 type_col = "type",
                                                 transform_value = "transform_value",
                                                 start_year = 2018,
                                                 year = "year",
                                                 iso3 = "iso3",
                                                 ind_ids = billionaiRe::billion_ind_codes("all", include_calculated = TRUE)) {
  scale <- rlang::arg_match(scale)

  if(scale == "combined"){
    scales <- c("fixed", "free")
  }else{
    scales <- scale
  }

  df_ind <- df %>%
    dplyr::filter(.data[[ind]] == !!indicator)

  unique_iso3 <- sort(unique(df_ind[[iso3]]))

  if(group_iso3){
    iso3_groups <- split(unique_iso3, ceiling(seq_along(unique_iso3) / 54))
  }else{
    iso3_groups <- list(unique_iso3)
  }

  names(iso3_groups) <- purrr::map_chr(iso3_groups, function(x) {
    paste0(x[[1]], "_", x[[length(x)]])
  })

  if (!is.null(run_name)) {
    output_folder <- here::here(output_folder, run_name)
    run_name_index <- paste0(run_name, "_")
  } else {
    run_name_index <- NULL
  }

  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }

  temp_dir <- here::here(output_folder, "temp")

  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir)
  }

  purrr::walk(
    iso3_groups, ~ export_plot_timeseries_indicator_iso3_group(
      df = df_ind,
      iso3_group = .x,
      indicator = indicator,
      output_folder = temp_dir,
      ind = ind,
      iso3_col = iso3,
      year_col = year,
      transform_value = transform_value,
      type_col = type_col,
      scenario = scenario,
      default_scenario = default_scenario,
      start_year = start_year,
      scale = scales,
      run_name = run_name
    )
  )

  pdftools::pdf_combine(
    c(here::here(temp_dir, paste0("temp_", run_name_index, indicator, "_", scale, "_", names(iso3_groups), ".pdf"))),
    here::here(output_folder, paste0(run_name_index, indicator, "_", scale, ".pdf"))
  )

  unlink(temp_dir, recursive = TRUE)
}

#' Export timeseries for a group of countries
#'
#' @param iso3_group named list of iso3 to be ploted
#' @param year_col Column name of column with years.
#' @param iso3_col Column name of column with country ISO3 codes.
#' @inheritParams export_country_summary_xls
#'
#' @inherit export_plot_timeseries_indicator_pdf
export_plot_timeseries_indicator_iso3_group <- function(df,
                                                        iso3_group,
                                                        indicator,
                                                        output_folder,
                                                        scale,
                                                        ind,
                                                        iso3_col,
                                                        year_col,
                                                        transform_value,
                                                        type_col,
                                                        scenario,
                                                        default_scenario,
                                                        start_year,
                                                        run_name) {
  group_id <- paste0(iso3_group[[1]], "_", iso3_group[[length(iso3_group)]])

  if (!is.null(run_name)) {
    output_folder <- here::here(output_folder, run_name)
    run_name_index <- paste0(run_name, "_")
  } else {
    run_name_index <- NULL
  }


  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }

  plots <- purrr::map(scale, ~ plot_timeseries_indicator(
    df = df,
    iso3 = iso3_group,
    indicator = indicator,
    scale = .x,
    ind = ind,
    iso3_col = iso3_col,
    year_col = year_col,
    transform_value = transform_value,
    type_col = type_col,
    scenario = scenario,
    default_scenario = default_scenario,
    start_year = start_year
  ))
  names(plots) <- scale

  if (length(scale) > 1) {
    purrr::walk(scale, ~
                  ggplot2::ggsave(here::here(output_folder, paste0("temp_", run_name_index, indicator, "_", .x, "_", group_id, ".pdf")),
                                  plots[[paste0(.x)]],
                                  width = 210, height = 297, units = "mm"
                  )
    )

    pdftools::pdf_combine(
      c(here::here(output_folder, paste0("temp_", run_name_index, indicator, "_", scale, "_", group_id, ".pdf"))),
      here::here(output_folder, paste0("temp_", run_name_index, indicator, "_combined_", group_id, ".pdf"))
    )

    file.remove(c(here::here(output_folder, paste0("temp_", run_name_index, indicator, "_", scale, "_", group_id, ".pdf"))))
  } else {
    ggplot2::ggsave(here::here(output_folder, paste0("temp_", run_name_index, indicator, "_", scale, "_", group_id, ".pdf")),
                    plots[[paste0(scale, "_scale")]],
                    width = 210, height = 297, units = "mm"
    )
  }
}
