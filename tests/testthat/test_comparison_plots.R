library(billionaiRe)

new_test_data_calculated <- load_misc_data("test_data/test_data_calculated/test_data_calculated.parquet") %>%
  dplyr::filter(scenario == "default") %>%
  dplyr::select(-recycled)

old_test_data_calculated <- load_misc_data("test_data/test_data_calculated/test_data_calculated_2021-06-20T12-00-00.parquet") %>%
  dplyr::select(names(new_test_data_calculated))

test_plot <- function(new_df, old_df, indicator) {
  testthat::test_that(paste0("plot_timeseries_indicator returns plots for ", indicator, ":"), {
    new_df_ind <- new_df %>%
      dplyr::filter(.data[["ind"]] == !!indicator)

    old_df_ind <- old_df %>%
      dplyr::filter(.data[["ind"]] == !!indicator)

    if (indicator %in% c("doctors", "nurses")) {
      testthat::expect_warning(plot_comparison_indicator(new_df_ind,
                                                         old_df_ind,
                                                         indicator = indicator
      ))
    } else {
      test_result <- plot_comparison_indicator(new_df_ind,
                                                         old_df_ind,
                                                         indicator = indicator
      )

      testthat::expect_s3_class(test_result, "ggplot")
      vdiffr::expect_doppelganger(
        paste0("plot  ",indicator),
        plot_comparison_indicator(new_df_ind,
                                  old_df_ind,
                                  indicator = indicator)
      )
    }
  })
}

test_plot(new_df = new_test_data_calculated,
          old_df = old_test_data_calculated,
          indicator = "anc4"
)

export_plot_comparison_indicator_pdf(new_df = new_test_data_calculated,
                           old_df = old_test_data_calculated,
                           indicator = "anc4")
