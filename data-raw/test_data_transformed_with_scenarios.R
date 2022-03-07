library(tidyverse)
library(billionaiRe)

test_data_transformed_with_scenarios <- load_misc_data("test_data/test_data/test_data.parquet") %>%
  dplyr::filter(scenario != "default") %>%
  billionaiRe::make_default_scenario() %>%
  dplyr::filter(!stringr::str_detect(.data[["ind"]], "espar|detect_respond"),
                scenario == "default") %>%
  billionaiRe::add_scenario("accelerate") %>%
  billionaiRe::add_scenario("halt_rise") %>%
  billionaiRe::transform_hep_data(scenario = "scenario") %>%
  billionaiRe::transform_hpop_data() %>%
  billionaiRe::transform_uhc_data()

arrow::write_parquet(test_data_transformed_with_scenarios, "data-raw/test_data_transformed_with_scenarios.parquet")

whdh::upload_to_data_lake(billionaiRe::get_data_lake_name(),
                          source_path = "data-raw/test_data_transformed_with_scenarios.parquet",
                          destination_path = "3B/Bronze/misc/test_data/test_data_transformed_with_scenarios/test_data_transformed_with_scenarios.parquet",
                          add_timestamp = T)
