get_scenario_colour <- function(col) {
  those_col <- unique(col)

  base_colours <- c(
    "Estimated" = "grey30",
    "Reported" = "grey30",
    "Imputed" = "grey",
    "Projected" = "forestgreen",
    "Acceleration" = "Purple",
    "SDG" = "Green",
    "Pre-COVID-19 trajectories" = "lightblue",
    "COVID-19 shock" = "Red",
    "COVID-19 Optimistic" = "Green",
    "COVID-19 Pessimistic" = "Darkred"
  )

  extended_palette_no_red_no_purple <- RColorBrewer::brewer.pal(12, "Paired")[-c(4, 6, 10)]

  not_base_colour <- those_col[!those_col %in% names(base_colours)]

  if (length(not_base_colour) > length(extended_palette_no_red_no_purple)) {
    warning("More than 7 scenarios were passed to be plotted on top of acceleration and sdg. Only the 9 first will be displayed.")
  }

  for (i in seq_along(not_base_colour)) {
    this_colour <- extended_palette_no_red_no_purple[i]
    names(this_colour) <- not_base_colour[i]
    base_colours <- c(base_colours, this_colour)
  }

  for (i in seq_along(col)) {
    col[i] <- base_colours[[col[i]]]
  }

  return(col)
}

theme_billionaiRe <- function() {
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.border = ggplot2::element_rect(colour = "grey90", fill = NA),
    axis.line.x = ggplot2::element_line(colour = "grey"),
    axis.line.y = ggplot2::element_line(colour = "grey"),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(colour = "gray25", size = 15),
    axis.title = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    strip.text.x = ggplot2::element_blank(),
    strip.placement = "inside",
    axis.text.y = ggplot2::element_text(size = 6), axis.text.x = ggplot2::element_text(size = 6),
    text = ggplot2::element_text(size = 15),
    legend.key = ggplot2::element_blank(),
    legend.position = "bottom",
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(size = 25)
  )
}

integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

connect_lines <- function(df,
                          iso3_col,
                          year_col,
                          ind_col,
                          plot_group,
                          value,
                          plot_color,
                          base_scenario = "base"){

  base_df <- df %>%
    dplyr::filter(.data[[plot_group]] == base_scenario)

  df_parameters <- df %>%
    dplyr::filter(.data[[plot_group]] != base_scenario) %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::any_of(c(!!iso3_col,!!ind_col,!!plot_group)))) %>%
    dplyr::filter(.data[[year_col]] == min(.data[[year_col]])) %>%
    dplyr::select(dplyr::all_of(c(iso3_col, year_col, ind_col, plot_group, plot_color))) %>%
    dplyr::rename(last_year = !!year_col, this_ind = !!ind_col, this_plot_group = !!plot_group, this_iso3 = !! iso3_col,
                  this_plot_color = !!plot_color) %>%
    dplyr::distinct()

   purrr::pmap_dfr(df_parameters, get_previous_year_row, df = base_df) %>%
     dplyr::bind_rows(df)
}

get_previous_year_row <- function(df, last_year, this_iso3, this_ind, this_plot_group,
                                  this_plot_color, iso3_col = "iso3", year_col = "year", ind_col = "ind",
                                  plot_group = "plot_group",
                                  plot_color = "plot_color"){
  df %>%
    dplyr::filter(.data[[year_col]] %in% max(c(max(.data[[year_col]]): last_year - 1)),
                  .data[[iso3_col]] == this_iso3,
                  .data[[ind_col]] == this_ind) %>%
    dplyr::mutate(
      "{plot_group}" := this_plot_group,
      "{plot_color}" := this_plot_color
      )
}

