library(tidyverse)
library(billionaiRe)

uhc_20210620 <- load_billion_data_legacy("uhc","raw_data", date_filter = "2021-06-20", auth_type = "client")
hep_20210620 <- load_billion_data_legacy("hep","raw_data", date_filter = "2021-06-20", auth_type = "client")
hpop_20210620 <- load_billion_data_legacy("hpop","raw_data", date_filter = "2021-06-20", auth_type = "client")


uhc_20210620_calculated <- uhc_20210620 %>%
  transform_uhc_data() %>%
  calculate_uhc_billion() %>%
  calculate_uhc_billion()

hep_20210620_calculated <- hep_20210620 %>%
  transform_hep_data() %>%
  calculate_hep_components() %>%
  calculate_hep_billion()

hpop_20210620_calculated <- hpop_20210620 %>%
  transform_hpop_data() %>%
  add_hpop_populations() %>%
  calculate_hpop_billion()

test_data_calculated_20210620 <- bind_rows(uhc_20210620_calculated,
                                   hep_20210620_calculated,
                                   hpop_20210620_calculated)

arrow::write_parquet(test_data_calculated_20210620, "data-raw/test_data_calculated_2021-06-20T12-00-00.parquet")


whdh::upload_to_data_lake(billionaiRe::get_data_lake_name(),
                          source_path = "data-raw/test_data_calculated_2021-06-20T12-00-00.parquet",
                          destination_path = "3B/Bronze/misc/test_data/test_data_calculated/test_data_calculated_2021-06-20T12-00-00.parquet",
                          add_timestamp = FALSE,
                          validate_user_input = FALSE
                          )

