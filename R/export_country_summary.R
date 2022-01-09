#' Export all countries summaries to Excel
#'
#' `export_all_countries_summaries_xls` Export summaries of all countries for the
#' three billions or for a specific billion.
#'
#' @param df Data frame in long format, where 1 row corresponds to a specific
#'     country, year, and indicator.
#' @param billion Billion indicator names to return, either "hep", "hpop", "uhc"
#'     , or "all".
#' @param year Column name of column with years.
#' @param iso3 Column name of column with country ISO3 codes.
#' @param ind Column name of column with indicator names.
#' @param value Column name of column with indicator values.
#' @param transform_value Column name of column(s) with transformed indicator
#'     values, used to calculate contributions.
#' @param scenario Column name of column with scenario identifiers. Useful for
#'     calculating contributions on data in long format rather than wide format.
#' @param type_col Column name of column with type data.
#' @param source_col Column name of column to store source information.
#' @param population Column name of column with population figures.
#' @param contribution Column name of column(s) to store contribution (population)
#'     values. Must be the same length as `transform_value`.
#' @param contribution_pct Column name of column(s) to store contribution (percent)
#'     values. Must be the same length as `transform_value`.
#' @param contribution_pct_total_pop Column name of column(s) to store contribution
#' (percent of total population of the country) values. Must be the same length
#' as `transform_value`.
#' @param default_scenario name of the default scenario.
#' @param start_year Base year for contribution calculation, defaults to 2018.
#' @param end_year End year(s) for contribution calculation, defaults to 2019 to
#'     2025.
#' @param output_folder Folder path to where the Excel files should be written
#'
#' @return `openxslx` Workbook object. Output file is in `output_folder`.
#'
#' @export
#'
export_all_countries_summaries_xls <- function(df,
                                               billion = c(
                                                 "hpop",
                                                 "hep",
                                                 "uhc",
                                                 "all"),
                                               year = "year",
                                               iso3 = "iso3",
                                               ind = "ind",
                                               value = "value",
                                               transform_value = "transform_value",
                                               scenario = NULL,
                                               type_col = "type",
                                               source_col = "source",
                                               population = "population",
                                               contribution = "contribution",
                                               contribution_pct = paste0(contribution, "_percent"),
                                               contribution_pct_total_pop = paste0(contribution, "_percent_total_pop"),
                                               default_scenario = "default_scenario",
                                               start_year = 2018,
                                               end_year = 2019:2025,
                                               output_folder = "outputs") {
  billion <- rlang::arg_match(billion)

  unique_iso3s <- unique(df[[iso3]])

  purrr::map(unique_iso3s, ~ export_country_summary_xls(
    df = df,
    iso = .x,
    billion = billion,
    iso3 = iso3,
    year = year,
    ind = ind,
    value = value,
    transform_value = transform_value,
    scenario = scenario,
    type_col = type_col,
    source_col = source_col,
    population = population,
    contribution = contribution,
    contribution_pct = contribution_pct,
    contribution_pct_total_pop = contribution_pct_total_pop,
    default_scenario = default_scenario,
    start_year = start_year,
    end_year = end_year,
    output_folder = output_folder
  ))
}

#' Export country summary to Excel
#'
#' `export_country_summary_xls` Export a country-specific for all three
#' billions or for a specific billion.
#' @param iso ISO3 code of country to summarize.
#' @inherit export_all_countries_summaries_xls
#'
#' @export
export_country_summary_xls <- function(df,
                                       iso,
                                       billion = c("hpop", "hep", "uhc", "all"),
                                       year = "year",
                                       iso3 = "iso3",
                                       ind = "ind",
                                       value = "value",
                                       transform_value = "transform_value",
                                       scenario = NULL,
                                       type_col = "type",
                                       source_col = "source",
                                       population = "population",
                                       contribution = "contribution",
                                       contribution_pct = paste0(contribution, "_percent"),
                                       contribution_pct_total_pop = paste0(contribution, "_percent_total_pop"),
                                       default_scenario = "default",
                                       start_year = 2018,
                                       end_year = 2019:2025,
                                       output_folder = "outputs") {
  billion <- rlang::arg_match(billion)

  billionaiRe:::assert_in_list_or_null(iso, unique(df[[iso3]]))

  wb <- write_permanent_sheets(billion, start_col = 2, start_row = 3)

  if (billion == "all") {
    export_hep_country_summary_xls(
      df = df,
      wb = wb,
      iso = iso,
      iso3 = iso3,
      year = year,
      ind = ind,
      value = value,
      transform_value = transform_value,
      scenario = scenario,
      type_col = type_col,
      source_col = source_col,
      population = population,
      contribution = contribution,
      contribution_pct = contribution_pct,
      default_scenario = default_scenario,
      start_year = start_year,
      end_year = end_year,
      sheet_prefix = "HEP",
      output_folder = output_folder,
      ind_ids = billionaiRe::billion_ind_codes("hep", include_calculated = TRUE)
    )
    export_hpop_country_summary_xls(
      df = df,
      wb = wb,
      iso = iso,
      iso3 = iso3,
      year = year,
      ind = ind,
      value = value,
      transform_value = transform_value,
      scenario = scenario,
      type_col = type_col,
      source_col = source_col,
      population = population,
      contribution = contribution,
      contribution_pct = contribution_pct,
      contribution_pct_total_pop = contribution_pct_total_pop,
      default_scenario = default_scenario,
      start_year = start_year,
      end_year = end_year,
      sheet_prefix = "HPOP",
      output_folder = output_folder,
      ind_ids = billionaiRe::billion_ind_codes("hpop", include_calculated = TRUE)
    )
    export_uhc_country_summary_xls(df,
      wb = wb,
      iso = iso,
      iso3 = iso3,
      year = year,
      ind = ind,
      value = value,
      transform_value = transform_value,
      scenario = scenario,
      type_col = type_col,
      source_col = source_col,
      population = population,
      contribution = contribution,
      default_scenario = default_scenario,
      start_year = start_year,
      end_year = end_year,
      sheet_prefix = "UHC",
      output_folder = output_folder,
      ind_ids = billionaiRe::billion_ind_codes("uhc", include_calculated = TRUE)
    )
  }

  if (billion == "hep") {
    purrr::map(openxlsx::sheets(wb)[stringr::str_detect(openxlsx::sheets(wb), "^UHC|^HPOP")], ~ openxlsx::removeWorksheet(wb, .x))

    export_hep_country_summary_xls(
      df = df,
      wb = wb,
      iso = iso,
      iso3 = iso3,
      year = year,
      ind = ind,
      value = value,
      transform_value = transform_value,
      scenario = scenario,
      type_col = type_col,
      source_col = source_col,
      population = population,
      contribution = contribution,
      contribution_pct = contribution_pct,
      default_scenario = default_scenario,
      start_year = start_year,
      end_year = end_year,
      sheet_prefix = "HEP",
      output_folder = output_folder,
      ind_ids = billionaiRe::billion_ind_codes("hep", include_calculated = TRUE)
    )
  }
  if (billion == "hpop") {
    purrr::map(openxlsx::sheets(wb)[stringr::str_detect(openxlsx::sheets(wb), "^UHC|^HEP")], ~ openxlsx::removeWorksheet(wb, .x))

    export_hpop_country_summary_xls(
      df = df,
      wb = wb,
      iso = iso,
      iso3 = iso3,
      year = year,
      ind = ind,
      value = value,
      transform_value = transform_value,
      scenario = scenario,
      type_col = type_col,
      source_col = source_col,
      population = population,
      contribution = contribution,
      contribution_pct = contribution_pct,
      contribution_pct_total_pop = contribution_pct_total_pop,
      default_scenario = default_scenario,
      start_year = start_year,
      end_year = end_year,
      sheet_prefix = "HPOP",
      output_folder = output_folder,
      ind_ids = billionaiRe::billion_ind_codes("hpop", include_calculated = TRUE)
    )
  }
  if (billion == "uhc") {
    purrr::map(openxlsx::sheets(wb)[stringr::str_detect(openxlsx::sheets(wb), "^HPOP|^HEP")], ~ openxlsx::removeWorksheet(wb, .x))

    export_uhc_country_summary_xls(df,
      wb = wb,
      iso = iso,
      iso3 = iso3,
      year = year,
      ind = ind,
      value = value,
      transform_value = transform_value,
      scenario = scenario,
      type_col = type_col,
      source_col = source_col,
      population = population,
      contribution = contribution,
      default_scenario = default_scenario,
      start_year = start_year,
      end_year = end_year,
      sheet_prefix = "UHC",
      output_folder = output_folder,
      ind_ids = billionaiRe::billion_ind_codes("uhc", include_calculated = TRUE)
    )
  }

  sheets_hidden <- grep("_Inter$", openxlsx::sheets(wb))
  for (i in sheets_hidden) {
    openxlsx::sheetVisibility(wb)[sheets_hidden] <- "hidden"
  }

  # Write workbook
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }

  openxlsx::saveWorkbook(wb,
    glue::glue("{output_folder}/GPW13_{toupper(billion)}_billion_{iso}_CountrySummary_{lubridate::today()}.xlsx"),
    overwrite = TRUE
  )
}

#' Export country summary to Excel for HEP billion$
#'
#' `export_hep_country_summary_xls` Export a country-specific for HEP billion.
#'
#' @param wb a `openxlsx` workbook to be edited.
#' @param sheet_prefix Character prefix to add in front of export sheets
#' @param ind_ids Named vector of indicator codes for input indicators to the Billion.
#'     Although separate indicator codes can be used than the standard, they must
#'     be supplied as a named vector where the names correspond to the output of
#'     `billion_ind_codes()`.
#' @inherit export_country_summary_xls
#'
export_hep_country_summary_xls <- function(df,
                                           wb,
                                           iso,
                                           iso3 = "iso3",
                                           year = "year",
                                           ind = "ind",
                                           value = "value",
                                           transform_value = "transform_value",
                                           scenario = NULL,
                                           type_col = "type",
                                           source_col = "source",
                                           population = "population",
                                           contribution = "contribution",
                                           contribution_pct = paste0(contribution, "_percent"),
                                           default_scenario = "default",
                                           start_year = 2018,
                                           end_year = 2019:2025,
                                           sheet_prefix = "HEP",
                                           output_folder = "outputs",
                                           ind_ids = billionaiRe::billion_ind_codes("hep", include_calculated = TRUE)) {
  billionaiRe:::assert_columns(df, year, iso3, ind, value, transform_value, contribution, contribution_pct, scenario, type_col, source_col)
  billionaiRe:::assert_years(start_year, end_year)
  billionaiRe:::assert_who_iso(iso)
  billionaiRe:::assert_in_list_or_null(iso, unique(df[[iso3]]))
  billionaiRe:::assert_same_length(value, transform_value)
  billionaiRe:::assert_same_length(value, contribution)
  billionaiRe:::assert_same_length(contribution, contribution_pct)

  # TODO: HEP export functions are static (length(value) == 1). If required, it would be nice to have it dynamic.
  ## Adding a stop for now to avoid issues for now.
  stopifnot("export_hep_country_summary_xls:
  value, transform_value, and contribution must be of length 1 at the moment.
  If you need to run with mutliple values, please run function multiple times" = length(value) == 1)

  # Get country specific data frame

  df_iso <- df %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data[[iso3]] == !!iso) %>%
    dplyr::group_by(dplyr::across(c(iso3, year, ind, scenario))) %>%
    dplyr::group_modify(
      ~ {
        if(nrow(.x) == 1){
          .x
        }else{
          .x %>%
            dplyr::filter(!is.na(.data[["level"]]))
        }
      }
    ) %>%
    dplyr::arrange(
      get_ind_order(.data[[ind]]),
      .data[[year]]
    ) %>%
    dplyr::mutate(dplyr::across(c(!!value, !!transform_value), ~ round(.x, digits = 2))) %>%
    dplyr::ungroup()

  df_iso <- get_df_one_scenario(df_iso, scenario, default_scenario)

  ind_df <- billionaiRe::indicator_df %>%
    dplyr::filter(
      .data[["hep"]],
      !is.na(.data[["ind"]]),
      !is.na(.data[["order"]])
    ) %>%
    dplyr::arrange(get_ind_order(.data[["ind"]]))

  # summary sheet
  summary_sheet <- glue::glue("{sheet_prefix}_summary")

  wb <- write_hep_summary_sheet(
    df = df_iso,
    wb = wb,
    sheet_name = summary_sheet,
    iso = iso,
    start_year = start_year,
    end_year = end_year,
    value = value,
    transform_value = transform_value,
    year = year,
    iso3 = iso3,
    ind = ind,
    population = population,
    type_col = type_col,
    source_col = source_col,
    scenario = scenario,
    ind_df,
    ind_ids
  )

  write_hep_timeseries_sheet(
    df = df_iso,
    wb = wb,
    sheet_name = glue::glue("{sheet_prefix}_Time Series"),
    start_row = 4,
    start_col = 2,
    transform_value,
    ind_df,
    ind,
    year,
    type_col,
    ind_ids,
    end_year,
    iso3
  )

  openxlsx::addStyle(wb,
    sheet = "HEP_Chart", rows = 22, cols = (3:(2 + nrow(ind_df))),
    style = excel_styles(
      textRotation = 90,
      fontSize = 8,
      fgFill = "white",
      wrapText = TRUE,
      halign = "center",
      valign = "center"
    )
  )
}

#' Export country summary to Excel for HPOP billion
#'
#' `export_hpop_country_summary_xls` Export a country-specific for HPOP billion.
#'
#' @inherit export_hep_country_summary_xls
#' @inheritParams export_all_countries_summaries_xls
#'
export_hpop_country_summary_xls <- function(df,
                                            wb,
                                            iso,
                                            iso3 = "iso3",
                                            year = "year",
                                            ind = "ind",
                                            value = "value",
                                            transform_value = "transform_value",
                                            scenario = NULL,
                                            type_col = "type",
                                            source_col = "source",
                                            population = "population",
                                            contribution = "contribution",
                                            contribution_pct = paste0(contribution, "_percent"),
                                            contribution_pct_total_pop = paste0(contribution, "_percent_total_pop"),
                                            default_scenario = "default",
                                            start_year = 2018,
                                            end_year = 2019:2025,
                                            sheet_prefix = "HPOP",
                                            output_folder = "outputs",
                                            ind_ids = billionaiRe::billion_ind_codes("hpop", include_calculated = TRUE)) {
  billionaiRe:::assert_columns(df, year, iso3, ind, value, transform_value, contribution, population, contribution_pct, contribution_pct_total_pop, scenario, type_col, source_col)
  billionaiRe:::assert_years(start_year, end_year)
  billionaiRe:::assert_who_iso(iso)
  billionaiRe:::assert_in_list_or_null(iso, unique(df[[iso3]]))
  billionaiRe:::assert_same_length(value, transform_value)
  billionaiRe:::assert_same_length(value, contribution)
  billionaiRe:::assert_same_length(contribution, contribution_pct)
  billionaiRe:::assert_same_length(contribution, contribution_pct_total_pop)

  # TODO: Big chunks of HPOP export functions are static (length(value) == 1). If required, it would be nice to have it dynamic.
  ## Adding a stop for now to avoid issues for now.
  stopifnot("export_hpop_country_summary_xls:
  value, transform_value, and contribution must be of length 1 at the moment.
  If you need to run with mutliple values, please run function multiple times" = length(value) == 1)

  # Get country specific data frame

  df_iso <- df %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      .data[[iso3]] == !!iso,
      stringr::str_detect(.data[[ind]], paste0(c(ind_ids, "^hpop_healthier"), collapse = "|"))
    ) %>%
    dplyr::arrange(
      get_ind_order(.data[[ind]]),
      .data[[year]]
    ) %>%
    dplyr::mutate(dplyr::across(c(!!value, !!transform_value), ~ round(.x, digits = 2))) %>%
    dplyr::group_by(.data[[ind]]) %>%
    dplyr::filter(sum(is.na(.data[[value]])) != dplyr::n() | !is.na(.data[[contribution]])) %>%
    dplyr::ungroup()

  df_iso <- get_df_one_scenario(df_iso, scenario, default_scenario)

  ind_in_df <- unique(df_iso[[ind]])

  water_ind <- ind_ids[stringr::str_detect(names(ind_ids), "^water")]

  water_ind_in_df <- water_ind[ifelse(sum(water_ind %in% ind_in_df) > 0, ind_ids["water"], water_ind %in% ind_in_df)]

  sanitation_ind <- ind_ids[stringr::str_detect(names(ind_ids), "^hpop_sanitation")]

  sanitation_ind_in_df <- sanitation_ind[ifelse(sum(sanitation_ind %in% ind_in_df) == 0, ind_ids["hpop_sanitation"], sanitation_ind %in% ind_in_df)]

  which_ind_ids <- ind_ids[!stringr::str_detect(names(ind_ids), "^water|^hpop_sanitation")]

  ind_ids <- c(which_ind_ids, water_ind_in_df, sanitation_ind_in_df)

  # indicator data frame to make sure the order of indicators is correct
  # remove wash/sanitation indicators not in data
  ind_df <- billionaiRe::indicator_df %>%
    dplyr::filter(
      .data[["hpop"]],
      !is.na(.data[["ind"]]),
      .data[["ind"]] %in% !!ind_ids,
      !is.na(.data[["order"]])
    ) %>%
    dplyr::arrange(get_ind_order(.data[["ind"]]))


  df_iso <- df_iso %>%
    dplyr::full_join(
      tidyr::expand_grid(
        ind = unique(ind_df[["ind"]]),
        year = start_year:max(end_year),
        iso3 = iso
      ),
      by = c(ind, year, iso3)
    ) %>%
    dplyr::arrange(
      get_ind_order(.data[[ind]]),
      .data[[year]]
    )


  # summary sheet
  summary_sheet <- glue::glue("{sheet_prefix}_summary")

  wb <- write_hpop_summary_sheet(
    df = df_iso,
    wb = wb,
    sheet_name = summary_sheet,
    start_year = start_year,
    end_year = end_year,
    value = value,
    year = year,
    iso3 = iso3,
    iso = iso,
    ind = ind,
    population = population,
    transform_value = transform_value,
    type_col = type_col,
    source_col = source_col,
    contribution = contribution,
    contribution_pct = contribution_pct,
    contribution_pct_total_pop = contribution_pct_total_pop,
    ind_df = ind_df,
    ind_ids = ind_ids
  )

  # Time series
  timeseries_sheet <- glue::glue("{sheet_prefix}_Time Series")

  wb <- write_hpop_timeseries_sheet(
    df = df_iso, wb,
    sheet_name = timeseries_sheet,
    start_col = 2, start_row = 4,
    value = value,
    ind_df = ind_df, ind = ind,
    year = year, type_col = type_col,
    end_year = end_year
  )

  # Flip titles graph
  openxlsx::addStyle(wb,
    sheet = "HPOP_Chart", rows = 22, cols = (3:(2 + nrow(ind_df))),
    style = excel_styles(
      textRotation = 90,
      fontSize = 8,
      fgFill = "white",
      wrapText = TRUE,
      halign = "center",
      valign = "center"
    )
  )
  return(wb)
}

#' Export country summary to Excel for UHC billion
#' `export_uhc_country_summary_xls` Export a country-specific for UHC billion.
#'
#' @inherit export_hep_country_summary_xls
#'
export_uhc_country_summary_xls <- function(df,
                                           wb,
                                           iso,
                                           iso3 = "iso3",
                                           year = "year",
                                           ind = "ind",
                                           value = "value",
                                           transform_value = "transform_value",
                                           scenario = NULL,
                                           type_col = "type",
                                           source_col = "source",
                                           population = "population",
                                           contribution = "contribution",
                                           default_scenario = "default",
                                           start_year = 2018,
                                           end_year = 2019:2025,
                                           sheet_prefix = "UHC",
                                           output_folder = "outputs",
                                           ind_ids = billionaiRe::billion_ind_codes("uhc",
                                                                                    include_calculated = TRUE)
                                           ) {
  billionaiRe:::assert_columns(df, year, iso3, ind, value, transform_value, contribution, scenario, type_col, source_col)
  billionaiRe:::assert_years(start_year, end_year)
  billionaiRe:::assert_who_iso(iso)
  billionaiRe:::assert_in_list_or_null(iso, unique(df[[iso3]]))
  billionaiRe:::assert_same_length(value, transform_value)
  billionaiRe:::assert_same_length(value, contribution)

  # TODO: The whole of UHC export functions are static (length(value) == 1). If required, it would be nice to have it dynamic.
  ## Adding a stop for now to avoid issues for now.
  stopifnot("export_uhc_country_summary_xls:
  value, transform_value, and contribution must be of length 1 at the moment.
  If you need to run with mutliple values, please run function multiple times" = length(value) == 1)

  df_iso <- df %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data[[iso3]] == iso) %>%
    dplyr::group_by(dplyr::across(c(iso3, year, ind, scenario))) %>%
    dplyr::group_modify(
      ~ {
        if(nrow(.x) == 1){
          .x
        }else{
          .x %>%
            dplyr::filter(is.na(.data[["level"]]))
        }
      }
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(
      get_ind_order(.data[[ind]]),
      .data[[year]]
    ) %>%
    dplyr::mutate(dplyr::across(c(!!value, !!transform_value), ~ round(.x, digits = 2)))

  df_iso <- get_df_one_scenario(df_iso, scenario, default_scenario)

  ind_df <- billionaiRe::indicator_df %>%
    dplyr::filter(.data[["uhc"]],
                  !is.na(.data[["order"]])
                  ) %>%
    dplyr::arrange(get_ind_order(.data[["ind"]]))

  # data sheet
  summary_sheet <- glue::glue("{sheet_prefix}_summary")

  write_uhc_summary_sheet(
    df = df_iso,
    wb = wb,
    sheet_name = summary_sheet,
    iso = iso,
    start_year = start_year,
    end_year = end_year,
    value = value,
    transform_value = transform_value,
    year = year,
    iso3 = iso3,
    ind = ind,
    population = population,
    type_col = type_col,
    source_col = source_col,
    ind_df,
    ind_ids
  )

  write_uhc_timeseries_sheet(
    df = df_iso,
    wb = wb,
    sheet_name = glue::glue("{sheet_prefix}_Time Series"),
    start_row = 4,
    start_col = 2,
    value,
    ind_df,
    ind,
    year,
    type_col,
    ind_ids,
    end_year
  )

  openxlsx::addStyle(wb,
    sheet = "UHC_Chart", rows = 22, cols = (3:(2 + nrow(ind_df))),
    style = excel_styles(
      textRotation = 90,
      fontSize = 8,
      fgFill = "white",
      wrapText = TRUE,
      halign = "center",
      valign = "center"
    )
  )


  return(wb)
}
