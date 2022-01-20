library(tidyverse)

test_data_transformed_with_scenarios <- load_misc_data("test_data/test_data/test_data.parquet") %>%
  billionaiRe::make_default_scenario() %>%
  dplyr::filter(!stringr::str_detect(.data[["ind"]], "espar|detect_respond")) %>%
  billionaiRe::add_scenario("accelerate") %>%
  billionaiRe::add_scenario("halt_rise") %>%
  billionaiRe::transform_hep_data(scenario = "scenario") %>%
  billionaiRe::transform_hpop_data() %>%
  billionaiRe::transform_uhc_data()

arrow::write_parquet(test_data_transformed_with_scenarios, "data-raw/test_data_transformed_with_scenarios.parquet")

arrow::write_parquet(test_data_transformed_with_scenarios,
                     glue::glue("data-raw/test_data_transformed_with_scenarios{whdh::get_formatted_timestamp()}.parquet"))
