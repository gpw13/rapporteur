#' Export plot comparison of two billionaiRe data frames
#'
#' `export_plot_comparison_pdf` exports plots to pdf comparing two billionaiRe
#' data frames.
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
export_plot_comparison_pdf <- function(new_df,
                                       old_df,
                                       indicator,
                                       output_folder = "outputs",
                                       run_name = NULL,
                                       ind = "ind",
                                       scenario = "scenario",
                                       default_scenario = "default",
                                       type_col = "type",
                                       transform_value = "transform_value",
                                       start_year = 2018,
                                       year = "year",
                                       iso3 = "iso3",
                                       ind_ids = billionaiRe::billion_ind_codes("all", include_calculated = TRUE)) {
  df_ind <- df %>%
    dplyr::filter(.data[[ind]] == !!indicator)

  unique_ind <- sort(unique(df_ind[[ind]]))

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


  purrr::walk(
    unique_ind, ~ plot_comparison_indicator(
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
