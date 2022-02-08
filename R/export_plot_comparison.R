#' Export plot comparison of two billionaiRe data frames
#'
#' `export_plot_comparison_pdf` exports plots to pdf comparing two billionaiRe
#' data frames.
#' @param ... Name-value pair of data frames to use e.g. (new = df_1).The name
#' (if provided) gives the name of the data frame and the value is the data
#' frame itself. The name is used to appear in the legend of the plot if
#' provided.
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
export_plot_comparison_pdf <- function(...,
                                       indicator,
                                       output_folder = "outputs",
                                       run_name = NULL,
                                       ind = "ind",
                                       scenario = "scenario",
                                       default_scenario = "default",
                                       type_col = "type",
                                       value = "value",
                                       start_year = 2018,
                                       year = "year",
                                       iso3 = "iso3",
                                       ind_ids = billionaiRe::billion_ind_codes("all", include_calculated = TRUE)) {

  dfs <- rlang::list2(...)
  billionaiRe:::assert_columns(dfs[[1]], ind, iso3, year, type_col, value)
  billionaiRe:::assert_columns(dfs[[2]], ind, iso3, year, type_col, value)
  billionaiRe:::assert_unique_rows(dfs[[1]], ind, iso3, year, ind_ids = ind_ids)
  billionaiRe:::assert_unique_rows(dfs[[2]], ind, iso3, year, ind_ids = ind_ids)

  if(length(dfs) != 2){
    stop("Only two data frames are supported by comparaison plots ", call. = FALSE)
  }

  if(is.null(names(dfs))){
    old <- "old"
    new <- "new"
  }else{
    old <- ifelse(names(dfs)[1] != "", names(dfs)[1], "old")
    new <- ifelse(names(dfs)[2] != "", names(dfs)[2], "new")
  }

  dfs[[1]] <- dfs[[1]] %>%
    dplyr::filter(!is.na(.data[[value]]))

  dfs[[2]] <- dfs[[2]] %>%
    dplyr::filter(!is.na(.data[[value]]))

  unique_ind <- sort(unique(c(dfs[[1]][[ind]], dfs[[2]][[ind]])))

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
      "{new}" := dfs[[1]],
      "{old}" := dfs[[2]],
      indicator = .x,
      ind = ind,
      iso3_col = iso3,
      year_col = year,
      value = value,
      type_col = type_col,
      ind_ids = ind_ids
    )
  )

  pdftools::pdf_combine(
    c(here::here(temp_dir, paste0("temp_", run_name_index, indicator, "_", scale, "_", names(iso3_groups), ".pdf"))),
    here::here(output_folder, paste0(run_name_index, indicator, "_", scale, ".pdf"))
  )


  unlink(temp_dir, recursive = TRUE)
}
