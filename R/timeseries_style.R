#' Change font of time series values
#'
#' `timeseries_style()` changes the font of time series values based on the
#' indicator type it is:
#'
#' * bold: reported
#' * normal: estimated
#' * faded: imputed or projected
#'
#' @inheritParams write_baseline_projection_hpop_summary
#' @inherit style_header_hpop_summary_sheet
#' @inheritParams write_hpop_timeseries_sheet
#'
#'
timeseries_style <- function(df, wb, sheet_name, start_row, start_col, ind_df) {
  wide_df <- df %>%
    dplyr::ungroup() %>%
    dplyr::select(.data[["ind"]], .data[["year"]], .data[["type"]]) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data[["year"]]) %>%
    dplyr::filter(!stringr::str_detect(.data[["ind"]], "^hpop_healthier")) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data[["ind"]]) %>%
    tidyr::pivot_wider(names_from = .data[["year"]], values_from = .data[["type"]]) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), tidyr::replace_na, ""))

  wide_df <- ind_df[, "ind"] %>%
    dplyr::left_join(wide_df, by = "ind")

  args <- list(
    "type" = list("reported", "projected", "imputed", ""),
    "fontColour" = list(NULL, "grey", "grey", NULL),
    "textDecoration" = list("bold", NULL, NULL, NULL)
  )

  purrr::pwalk(args,
    type_styler,
    wb = wb,
    df = wide_df,
    sheet_name = sheet_name,
    start_row = start_row,
    start_col = start_col
  )
}


#' Generate a style with variable color or bolding
#'
#' `add_style_wrapper()` adds a style for use within [timeseries_style()],
#' where the font color and decoration needs to vary based on data type. Wraps
#' around [openxlsx::addStyle()] and [openxlsx::createStyle()].
#'
#' @inheritParams timeseries_style
#' @param rows Rows to apply style to, passed to `openxlsx::add_style()`.
#' @param cols Columns to apply style to, passed to `openxlsx::add_style()`.
#' @param fontColour Font colour, passed to [openxlsx::createStyle()].
#' @param textDecoration Font decoration, passed to [openxlsx::createStyle()].
add_style_wrapper <- function(wb,
                              sheet_name,
                              rows,
                              cols,
                              fontColour,
                              textDecoration) {
  openxlsx::addStyle(
    wb,
    sheet = sheet_name,
    rows = rows,
    cols = cols,
    style = openxlsx::createStyle(
      fontName = "Calibri",
      fontColour = fontColour,
      textDecoration = textDecoration,
      fontSize = 8,
      halign = "right",
      wrapText = TRUE,
      numFmt = "0.00",
      border = "bottom",
      borderStyle = "thin",
      borderColour = "grey"
    )
  )
}

#' @inheritParams add_style_wrapper
#' @inheritParams timeseries_style
type_styler <- function(wb,
                        type,
                        sheet_name,
                        df,
                        start_row,
                        start_col,
                        fontColour,
                        textDecoration) {
  # map across matching row/column values
  inds <- which(df == type, arr.ind = TRUE)
  inds_list <- split(inds[, 2], inds[, 1])

  purrr::iwalk(
    inds_list,
    ~ add_style_wrapper(
      wb = wb,
      sheet_name = sheet_name,
      rows = as.numeric(.y) + start_row,
      cols = .x + start_col - 1,
      fontColour = fontColour,
      textDecoration = textDecoration
    )
  )
}

#' Styles time series sheet
#'
#' @inheritParams write_hpop_timeseries_sheet
#' @param billion Billion to be used for billion styling when relevant: either
#' "hep", "hpop", or "uhc" when no billion to be applied.
#' @inherit style_header_hpop_summary_sheet
#' @param df_wide wide version of `df` generated by `write_hpop_timeseries_sheet`
#' @param df_wide wide version of `df` generated by `write_uhc_timeseries_sheet`
#' @param this_iso3 character iditenfying the country being styled.

style_timeseries <- function(df, wb, billion = c("hep", "hpop", "uhc"), sheet_name, start_row, start_col,
                             df_wide, ind_df, this_iso3 = NULL) {
  billion <- rlang::arg_match(billion)

  mergeCellForced(wb,
    sheet = sheet_name,
    cols = start_col, rows = start_row:(start_row + 1)
  )

  mergeCellForced(wb,
    sheet = sheet_name,
    cols = (start_col + 1):(ncol(df_wide) + 1), rows = start_row
  )

  openxlsx::addStyle(wb,
    sheet = sheet_name, style = excel_styles(
      style_category = "datatable_header",
      billion = billion,
      billion_fgFill = "main"
    ),
    rows = start_row, cols = c(start_col:(ncol(df_wide) + 1))
  )
  openxlsx::addStyle(wb,
    sheet = sheet_name, style = excel_styles(
      style_category = "datatable_header",
      billion = billion,
      billion_fgFill = "main"
    ),
    rows = start_row:(start_row + 1), cols = start_col
  )
  openxlsx::addStyle(wb,
    sheet = sheet_name, style = excel_styles(
      style_category = "sub_datatable_header",
      billion = billion,
      billion_fgFill = "light",
      halign = "right"
    ),
    rows = c((start_row + 1)), cols = c((start_col + 1):(ncol(df_wide) + 1)),
    gridExpand = TRUE
  )
  openxlsx::addStyle(wb,
    sheet = sheet_name, style = excel_styles(
      style_category = "data",
      type_data = "numeric",
      border = "bottom",
      borderStyle = "thin",
      borderColour = "grey"
    ),
    rows = c((start_row + 2):(start_row + nrow(df_wide) + 1)), cols = c(start_col:(ncol(df_wide) + 1)),
    gridExpand = TRUE
  )


  timeseries_style(df, wb, sheet_name,
    start_row = start_row + 1, start_col = start_col, ind_df
  )

  if (billion == "uhc") {
    openxlsx::addStyle(wb,
      sheet = sheet_name,
      style = excel_styles(
        style_category = "normal_text",
        textDecoration = "bold"
      ),
      rows = c((start_row + nrow(df_wide)):(start_row + nrow(df_wide) + 1)),
      cols = c(start_col),
      gridExpand = TRUE
    )
  }

  if (billion == "hep") {
    affected_pathos_iso3 <- rapporteur::affected_pathogens %>%
      dplyr::filter(.data[["iso3"]] == !!this_iso3)

    if (rowSums(affected_pathos_iso3 %>% dplyr::select(-.data[["iso3"]])) > 1) {
      fade <- TRUE
      pathos_iso3 <- names(affected_pathos_iso3)[affected_pathos_iso3 == FALSE]
      medium_name_pathos <- unlist(
        ind_df[stringr::str_detect(ind_df[["ind"]], paste0(pathos_iso3, collapse = "|")), "medium_name"]
      )

      fade_rows <- grep(paste0(medium_name_pathos, collapse = "|"), df_wide$short_name)
      openxlsx::addStyle(wb,
        sheet = sheet_name,
        style = excel_styles(
          style_category = "normal_text",
          fontColour = "grey",
          border = "bottom",
          borderColour = "grey"
        ),
        rows = start_row + fade_rows + 1,
        cols = c(start_col),
        gridExpand = TRUE
      )

      short_name_indic <- unlist(
        ind_df[stringr::str_detect(ind_df$ind, paste0(c("espar$", "prevent", "detect_respond", "hep_idx"), collapse = "|")), "short_name"]
      )
      bold_rows <- grep(paste0(short_name_indic, collapse = "|"), df_wide$short_name)

      if (length(bold_rows) >= 0) {
        openxlsx::addStyle(wb,
          sheet = sheet_name,
          style = excel_styles(
            style_category = "normal_text",
            textDecoration = "bold",
            border = "bottom",
            borderColour = "grey"
          ),
          rows = as.double(bold_rows + start_row + 1),
          cols = c(start_col),
          gridExpand = TRUE
        )
      }
    }
  }

  return(wb)
}
