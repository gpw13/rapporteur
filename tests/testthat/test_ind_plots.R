library(billionaiRe)

test_data <- load_misc_data("test_data/test_data_transformed_with_scenarios.parquet")

save_png <- function(code) {
  path <- tempfile(fileext = ".png")
  plot <- code
  ggplot2::ggsave(path, height = 4.25, width = 7.35)
  path
}

test_plot <- function(df, ind) {
  testthat::test_that(paste0("plot_timeseries_indicator returns plots for ", ind, ":"), {
    df_ind <- df %>%
      dplyr::filter(.data[["ind"]] == !!ind)

    if (ind %in% c("doctors", "nurses")) {
      testthat::expect_warning(plot_timeseries_indicator(df_ind,
        iso3 = unique(df_ind[["iso3"]]),
        indicator = ind
      ))
    } else {
      test_result <- plot_timeseries_indicator(df_ind,
        iso3 = unique(df_ind[["iso3"]]),
        indicator = ind
      )

      testthat::expect_s3_class(test_result, "ggplot")
      testthat::expect_snapshot_file(save_png(plot_timeseries_indicator(df_ind,
        iso3 = unique(df_ind[["iso3"]]),
        indicator = ind,
        scale = "fixed"
      )), paste0("test_plot_", ind, "_fixed.png"))
      testthat::expect_snapshot_file(save_png(plot_timeseries_indicator(df_ind,
        iso3 = unique(df_ind[["iso3"]]),
        indicator = ind,
        scale = "free"
      )), paste0("test_plot_", ind, "_free.png"))
    }
  })
}

test_plot(test_data, "anc4")


purrr::walk(unique(test_data[["ind"]]), test_plot, df = test_data)

test_pdf_plot <- function(df, indicator, scale) {
  temp_dir <- tempdir()

  expand_df <- function(df, iso3s) {
    old_isos <- c("AFG", "AGO", "BDI", "BGD", "BOL", "UGA")
    iso3s <- setNames(iso3s, old_isos)
    new_df <- df
    new_df$iso3 <- as.character(iso3s[as.character(df$iso3)])
    new_df
  }

  iso3s <- split(unique(whoville::countries$iso3), ceiling(seq_along(unique(whoville::countries$iso3)) / 6))
  iso3s <- iso3s[1:41]

  full_df <- purrr::map_dfr(iso3s, ~ expand_df(test_data, .x))

  export_plot_timeseries_indicator_pdf(full_df, indicator = indicator, output_folder = temp_dir, scale = scale)
  paste0(temp_dir, "/", indicator, "_", scale, ".pdf")
}

testthat::test_that("export_plot_timeseries_indicator_pdf returns right format", {
  testthat::expect_snapshot_file(test_pdf_plot(test_data, indicator = "anc4", scale = "combined"), "test_pdf_anc4_combined.pdf")
  testthat::expect_snapshot_file(test_pdf_plot(test_data, indicator = "anc4", scale = "fixed"), "test_pdf_anc4_fixed.pdf")
  testthat::expect_snapshot_file(test_pdf_plot(test_data, indicator = "anc4", scale = "free"), "test_pdf_anc4_free.pdf")

  testthat::expect_snapshot_file(test_pdf_plot(test_data, indicator = "child_viol", scale = "combined"), "test_pdf_child_viol_combined.pdf")
  testthat::expect_snapshot_file(test_pdf_plot(test_data, indicator = "child_viol", scale = "fixed"), "test_pdf_child_viol_fixed.pdf")
  testthat::expect_snapshot_file(test_pdf_plot(test_data, indicator = "child_viol", scale = "free"), "test_pdf_child_viol_free.pdf")
})
