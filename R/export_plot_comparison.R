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
export_plot_comparison_indicator_pdf <- function(new_df,
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
                                       value = "value",
                                       ind_ids = billionaiRe::billion_ind_codes("all", include_calculated = TRUE)) {
  new_df_ind <- new_df %>%
    dplyr::filter(.data[[ind]] == !!indicator)

  old_df_ind <- old_df %>%
    dplyr::filter(.data[[ind]] == !!indicator)

  unique_ind <- sort(c(unique(new_df_ind[[ind]]), unique(old_df_ind[[ind]])))

  if (!is.null(run_name)) {
    output_folder <- here::here(output_folder, run_name)
    run_name_index <- paste0(run_name, "_")
  } else {
    run_name_index <- NULL
  }

  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }

  temp_dir <- tempdir()

  purrr::walk(
    unique_ind, ~ ggplot2::ggsave(
      here::here(temp_dir, paste0("temp_", run_name_index, indicator, ".pdf")),
      plot_comparison_indicator(
        new_df = new_df_ind,
        old_df = old_df_ind,
        indicator,
        ind = ind,
        iso3_col = iso3,
        year_col = year,
        value = value,
        type_col = type_col,
        ind_ids = ind_ids
      ),
      width = 210, height = 297, units = "mm"
    )
  )

  pdftools::pdf_combine(
    c(here::here(temp_dir, paste0("temp_", run_name_index, indicator, ".pdf"))),
    here::here(output_folder, paste0("plot_comparison_dfs_",run_name_index, indicator, ".pdf"))
  )


  unlink(temp_dir, recursive = TRUE)
}
