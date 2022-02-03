library(billionaiRe)

new_test_data_calculated <- load_misc_data("test_data/test_data_calculated/test_data_calculated.parquet") %>%
  dplyr::filter(scenario == "default") %>%
  dplyr::select(-recycled)

old_test_data_calculated <- load_misc_data("test_data/test_data_calculated/test_data_calculated_2021-06-20T12-00-00.parquet") %>%
  dplyr::select(names(new_test_data_calculated))

test_plot <- function(new_df, old_df, ind) {
  testthat::test_that(paste0("plot_timeseries_indicator returns plots for ", ind, ":"), {
    new_df_ind <- new_df %>%
      dplyr::filter(.data[["ind"]] == !!ind)

    old_df_ind <- old_df %>%
      dplyr::filter(.data[["ind"]] == !!ind)

    if (ind %in% c("doctors", "nurses")) {
      testthat::expect_warning(plot_comparison_indicator(new_df_ind,
                                                         old_df_ind,
                                                         indicator = ind
      ))
    } else {
      test_result <- plot_comparison_indicator(new_df_ind,
                                                         old_df_ind,
                                                         indicator = ind
      )

      testthat::expect_s3_class(test_result, "ggplot")
      vdiffr::expect_doppelganger(
        paste0("plot  ",ind),
        plot_comparison_indicator(new_df_ind,
                                  old_df_ind,
                                  indicator = ind)
      )
    }
  })
}

test_plot(new_df = new_test_data_calculated,
          old_df = old_test_data_calculated,
          ind = "anc4"
)
