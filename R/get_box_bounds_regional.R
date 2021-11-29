#' Get Excel box bounds
#'
#' `get_box_bounds_regional` gets the bounds (boundaries) of Excel regional
#'    sheet boxes.
#'
#' @param sheet_name name of the sheet to get boundaries. It can be either:
#' * Intro
#' * Indicator_view
#' * Country_view
#' * uhc_regional_summary
#' * hep_regional_summary
#' *
#' @param box name of which to get the boundaries. If NULL, all boxes for
#'    `sheet_name` is returned.
#'
#' @return named list or list of list  with boxes bounds for specified
#'    `sheet_name` and `box`. For each box, returns `start_row`, `end_row`,
#'    `start_col` and `end_col
#'
get_box_bounds_regional <- function(sheet_name = c("Intro",
                                                   "Indicator_view",
                                                   "Country_view",
                                                   "uhc_regional_sumamry",
                                                   "hep_regional_summary",
                                                   "hpop_regional_summary"),
                                    box = NULL){

  sheet_name <- rlang::arg_match(sheet_name)

  Intro <- list(
    purpose = list(
      start_row = 3, end_row = 14,
      start_col = 2, end_col = 11
    ),
    interp = list(
      start_row = 15, end_row = 49,
      start_col = 2, end_col = 11
    ),
    interp0 = list(
      start_row = 16, end_row = 17,
      start_col = 2, end_col = 11
    ),
    interp1 = list(
      start_row = 18, end_row = 25,
      start_col = 3, end_col = 11
    ),
    interp2 = list(
      start_row = 26, end_row = 34,
      start_col = 3, end_col = 11
    ),
    interp3 = list(
      start_row = 35, end_row = 42,
      start_col = 3, end_col = 11
    ),
    interp4 = list(
      start_row = 43, end_row = 49,
      start_col = 3, end_col = 11
    )
  )

  Indicator_view <- list(
    param = list(
      start_row = 3, end_row = 6,
      start_col = 2, end_col = 12
    ),
    blue_notes = list(
      start_row = 7, end_row = 8,
      start_col = 2, end_col = 33
    ),
    worse = list(
      start_row = 13, end_row = 200,
      start_col = 3, end_col = 16
    ),
    better = list(
      start_row = 13, end_row = 200,
      start_col = 19, end_col = 31
    ),
    insuf = list(
      start_row = 13, end_row = 200,
      start_col = 35, end_col = 40
    )
  )

  Country_view <- list(
    blue_note = list(
      start_row = 7, end_row = 7,
      start_col = 2, end_col = 33
    ),
    worse_titles = list(
      start_row = 11, end_row = 17,
      start_col = 2, end_col = 16
    ),
    worse_uhc = list(
      start_row = 19, end_row = 33,
      start_col = 2, end_col = 16
    ),
    worse_hpop = list(
      start_row = 35, end_row = 48,
      start_col = 2, end_col = 16
    ),
    worse_hep = list(
      start_row = 50, end_row = 57,
      start_col = 2, end_col = 16
    ),
    better_titles = list(
      start_row = 11, end_row = 17,
      start_col = 19, end_col = 33
    ),
    better_uhc = list(
      start_row = 19, end_row = 33,
      start_col = 19, end_col = 33
    ),
    better_hpop = list(
      start_row = 35, end_row = 48,
      start_col = 19, end_col = 33
    ),
    better_hep = list(
      start_row = 50, end_row = 57,
      start_col = 19, end_col = 33
    )
  )

  uhc_regional_summary <- list(
    title = list(
      start_row = 1, end_row = 4,
      start_col = 2, end_col = 42
    ),
    data = list(
      start_row = 4, end_row = 198,
      start_col = 2, end_col = 42
    ),
    cntry_info = list(
      start_row = 1, end_row = 198,
      start_col = 2, end_col = 6
    ),
    contribution_billion = list(
      start_row = 1, end_row = 198,
      start_col = 2, end_col = 6
    ),
    indic_perf = list(
      start_row = 1, end_row = 1,
      start_col = 9, end_col = 14
    ),
    sm = list(
      start_row = 2, end_row = 4,
      start_col = 9, end_col = 10
    ),
    fh = list(
      start_row = 2, end_row = 4,
      start_col = 11, end_col = 12
    ),
    asc = list(
      start_row = 2, end_row = 4,
      start_col = 13, end_col = 14
    ),
    asc_tracer_title = list(
      start_row = 1, end_row = 1,
      start_col = 15, end_col = 42
    ),
    rmnch = list(
      start_row = 2, end_row = 4,
      start_col = 15, end_col = 22
    ),
    infec_diseases = list(
      start_row = 2, end_row = 4,
      start_col = 23, end_col = 30
    ),
    nd = list(
      start_row = 2, end_row = 4,
      start_col = 31, end_col = 36
    ),
    nd = list(
      start_row = 2, end_row = 4,
      start_col = 37, end_col = 42
    )
  )

  hep_regional_summary <- list(
    title = list(
      start_row = 1, end_row = 4,
      start_col = 2, end_col = 43
    ),
    data = list(
      start_row = 4, end_row = 198,
      start_col = 2, end_col = 43
    ),
    cntry_info = list(
      start_row = 1, end_row = 198,
      start_col = 2, end_col = 6
    ),
    contribution_billion = list(
      start_row = 1, end_row = 198,
      start_col = 7, end_col = 8
    ),
    billion_indic = list(
      start_row = 1, end_row = 1,
      start_col = 9, end_col = 16
    ),
    hep_idx = list(
      start_row = 2, end_row = 4,
      start_col = 9, end_col = 10
    ),
    prepare = list(
      start_row = 2, end_row = 4,
      start_col = 11, end_col = 12
    ),
    prevent = list(
      start_row = 2, end_row = 4,
      start_col = 13, end_col = 14
    ),
    dnr = list(
      start_row = 2, end_row = 4,
      start_col = 15, end_col = 16
    ),
    prepare = list(
      start_row = 1, end_row = 4,
      start_col = 17, end_col = 29
    ),
    prevent = list(
      start_row = 1, end_row = 1,
      start_col = 30, end_col = 43
    ),
    polio = list(
      start_row = 2, end_row = 4,
      start_col = 30, end_col = 31
    ),
    measles = list(
      start_row = 2, end_row = 4,
      start_col = 32, end_col = 33
    ),
    yellow_fever = list(
      start_row = 2, end_row = 4,
      start_col = 34, end_col = 35
    ),
    meningitis = list(
      start_row = 2, end_row = 4,
      start_col = 36, end_col = 37
    ),
    cholera = list(
      start_row = 2, end_row = 4,
      start_col = 38, end_col = 39
    ),
    covid = list(
      start_row = 2, end_row = 4,
      start_col = 40, end_col = 41
    ),
    ebola = list(
      start_row = 2, end_row = 4,
      start_col = 42, end_col = 43
    )
  )

  hpop_regional_summary <- list(
    title = list(
      start_row = 1, end_row = 4,
      start_col = 2, end_col = 36
    ),
    data = list(
      start_row = 4, end_row = 198,
      start_col = 2, end_col = 36
    ),
    cntry_info = list(
      start_row = 1, end_row = 198,
      start_col = 2, end_col = 6
    ),
    contribution_billion = list(
      start_row = 1, end_row = 198,
      start_col = 7, end_col = 8
    ),
    billion_indic = list(
      start_row = 1, end_row = 1,
      start_col = 9, end_col = 36
    ),
    adult_oberse = list(
      start_row = 2, end_row = 4,
      start_col = 9, end_col = 10
    ),
    children_obese = list(
      start_row = 2, end_row = 4,
      start_col = 11, end_col = 12
    ),
    children_overweight = list(
      start_row = 2, end_row = 4,
      start_col = 13, end_col = 14
    ),
    children_wasted = list(
      start_row = 2, end_row = 4,
      start_col = 15, end_col = 16
    ),
    children_stunted = list(
      start_row = 2, end_row = 4,
      start_col = 17, end_col = 18
    ),
    fuel = list(
      start_row = 1, end_row = 1,
      start_col = 19, end_col = 20
    ),
    air_quality = list(
      start_row = 2, end_row = 4,
      start_col = 21, end_col = 22
    ),
    road_safety = list(
      start_row = 2, end_row = 4,
      start_col = 23, end_col = 24
    ),
    suicide = list(
      start_row = 2, end_row = 4,
      start_col = 25, end_col = 26
    ),
    sanitation = list(
      start_row = 2, end_row = 4,
      start_col = 27, end_col = 28
    ),
    water = list(
      start_row = 2, end_row = 4,
      start_col = 29, end_col = 30
    ),
    tobacco = list(
      start_row = 2, end_row = 4,
      start_col = 31, end_col = 32
    ),
    alcohol = list(
      start_row = 2, end_row = 4,
      start_col = 33, end_col = 34
    ),
    healthy_fat = list(
      start_row = 2, end_row = 4,
      start_col = 35, end_col = 36
    )
  )

  all_sheets <- list(Intro = Intro,
                     Indicator_view = Indicator_view,
                     Country_view = Country_view,
                     uhc_regional_summary = uhc_regional_summary,
                     hep_regional_summary = hep_regional_summary,
                     hpop_regional_summary = hpop_regional_summary)
  if(is.null(box)){
    all_sheets[[sheet_name]]
  }else{
    all_sheets[[sheet_name]][box]
  }
}
