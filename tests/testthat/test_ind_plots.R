library(billionaiRe)

test_data <- load_misc_data("test_data/test_data/test_data.parquet") %>%
  dplyr::mutate(value = dplyr::case_when(
    scenario == "covid_dip_lag" & year <= 2020 ~ NA_real_,
    scenario == "covid_dip_lag" ~  dplyr::if_else(.data[["value"]] - 10.0 >0, .data[["value"]] - 10.0, 0),
    TRUE ~ .data[["value"]]
  )) %>%
  dplyr::filter(!is.na(value))

test_plot <- function(df, ind) {
  testthat::test_that(paste0("plot_timeseries_indicator returns plots for ", ind, ":"), {
    df_ind <- df %>%
      dplyr::filter(.data[["ind"]] == !!ind)

    test_result <- plot_timeseries_indicator(df_ind,
                                             iso3 = unique(df_ind[["iso3"]]),
                                             indicator = ind
    )

    testthat::expect_s3_class(test_result, "ggplot")

    vdiffr::expect_doppelganger(
      paste0("plot with fixed scale ",ind),
      plot_timeseries_indicator(df_ind,
                                iso3 = unique(df_ind[["iso3"]]),
                                indicator = ind,
                                scale = "fixed"
      ))
    vdiffr::expect_doppelganger(
      paste0("plot with free scale ", ind),
      plot_timeseries_indicator(df_ind,
                                iso3 = unique(df_ind[["iso3"]]),
                                indicator = ind,
                                scale = "free"
      ))
  })
}

those_inds <- billionaiRe::billion_ind_codes("all", include_subindicators = FALSE)[billionaiRe::billion_ind_codes("all", include_subindicators = FALSE) %in% unique(test_data[["ind"]])]

purrr::walk(those_inds, test_plot, df = test_data)

test_pdf_plot <- function(df, indicator, scale, experiment = NULL) {
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

  export_plot_timeseries_indicator_pdf(full_df, indicator = indicator, output_folder = temp_dir, scale = scale,
                                       experiment = experiment, version = NULL)
  here::here(temp_dir, paste0(indicator, "_", scale, ".pdf"))
}

testthat::test_that("export_plot_timeseries_indicator_pdf returns right format", {
  test_data_pdf <- test_data %>%
    dplyr::filter(ind %in% c("anc4", "child_viol"))

  anc4_combined_pdf <- test_pdf_plot(test_data_pdf, indicator = "anc4", scale = "combined")
  anc4_fixed_pdf <- test_pdf_plot(test_data_pdf, indicator = "anc4", scale = "fixed")
  anc4_free_pdf <- test_pdf_plot(test_data_pdf, indicator = "anc4", scale = "free")

  testthat::expect_equal(pdftools::pdf_length(anc4_combined_pdf), 10)
  testthat::expect_equal(pdftools::pdf_length(anc4_fixed_pdf), 5)
  testthat::expect_equal(pdftools::pdf_length(anc4_free_pdf), 5)

  # testthat::expect_snapshot_file(anc4_combined_pdf, "test_pdf_anc4_combined.pdf")
  # testthat::expect_snapshot_file(anc4_fixed_pdf, "test_pdf_anc4_fixed.pdf")
  # testthat::expect_snapshot_file(anc4_free_pdf, "test_pdf_anc4_free.pdf")

  child_viol_combined_pdf <- test_pdf_plot(test_data_pdf, indicator = "child_viol", scale = "combined")
  child_viol_fixed_pdf <- test_pdf_plot(test_data_pdf, indicator = "child_viol", scale = "fixed")
  child_viol_free_pdf <- test_pdf_plot(test_data_pdf, indicator = "child_viol", scale = "free")

  testthat::expect_equal(pdftools::pdf_length(child_viol_combined_pdf), 8)
  testthat::expect_equal(pdftools::pdf_length(child_viol_fixed_pdf), 4)
  testthat::expect_equal(pdftools::pdf_length(child_viol_free_pdf), 4)

  # testthat::expect_snapshot_file(child_viol_combined_pdf, "test_pdf_child_viol_combined.pdf")
  # testthat::expect_snapshot_file(child_viol_fixed_pdf, "test_pdf_child_viol_fixed.pdf")
  # testthat::expect_snapshot_file(child_viol_free_pdf, "test_pdf_child_viol_free.pdf")

  testthat::expect_error(test_pdf_plot(test_data_pdf, indicator = "child_viol", scale = "combined", experiment = "test"), NA)

})
