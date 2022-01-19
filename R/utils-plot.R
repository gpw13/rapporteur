get_scenario_colour <- function(col) {
  those_col <- unique(col)

  base_colours <- c(
    "Estimated" = "grey30",
    "Reported" = "grey30",
    "Imputed" = "grey",
    "Projected" = "Red",
    "acceleration" = "Purple",
    "sdg" = "Green"
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
get_scenario_colour <- function(col) {
  those_col <- unique(col)

  base_colours <- c(
    "Estimated" = "grey30",
    "Reported" = "grey30",
    "Imputed" = "grey",
    "Projected" = "Red",
    "acceleration" = "Purple",
    "sdg" = "Green"
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

integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}
