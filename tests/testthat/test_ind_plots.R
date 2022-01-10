
test_data <- suppressWarnings(billionaiRe::load_misc_data("test_data/test_data.parquet") %>%
  billionaiRe::make_default_scenario() %>%
  dplyr::filter(!stringr::str_detect(.data[["ind"]], "espar|detect_respond")) %>%
  billionaiRe::add_scenario("accelerate") %>%
  billionaiRe::add_scenario("halt_rise") %>%
  billionaiRe::transform_hep_data(scenario = "scenario") %>%
  billionaiRe::transform_hpop_data() %>%
  billionaiRe::transform_uhc_data()
)

test_plot <- function(df, ind) {
  testthat::test_that(paste0("plot_timeseries_indicator returns plots for ", ind,":"), {
    df_ind <- df %>%
      dplyr::filter(.data[["ind"]] == !!ind)

    if(ind %in% c("doctors", "nurses")){
      testthat::expect_warning(plot_timeseries_indicator(df_ind,
                                                         iso3 = unique(df_ind[["iso3"]]),
                                                         indicator = ind
      ))
    }else{
      test_result <- plot_timeseries_indicator(df_ind,
                                               iso3 = unique(df_ind[["iso3"]]),
                                               indicator = ind
      )

      testthat::expect_type(test_result, "list")
      testthat::expect_s3_class(test_result[[1]], "ggplot")
      testthat::expect_s3_class(test_result[[2]], "ggplot")

    }
  })
}

purrr::walk(unique(test_data[["ind"]]), test_plot, df = test_data)


testthat::test_that("export_plot_timeseries_indicator returns right format", {
  export_plot_timeseries_indicator(test_data, indicator = "anc4", format =  "pdf")
})
