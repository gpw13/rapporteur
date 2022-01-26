library(billionaiRe)
library(rapporteur)

temp_dir <- tempdir()

test_data <- load_misc_data("test_data/test_data_transformed_with_scenarios/test_data_transformed_with_scenarios.parquet")

test_data_hep <- test_data %>%
  dplyr::filter(ind %in% billion_ind_codes("hep")) %>%
  calculate_hep_components(scenario = "scenario") %>%
  calculate_hep_billion(scenario = "scenario")

hep_test <- export_country_summary_xls(test_data_hep, "AFG", "hep", scenario = "scenario", output_folder = temp_dir)
openxlsx::saveWorkbook(hep_test, "inst/extdata/test_scenarios_hep_afg.xlsx", overwrite = TRUE)

test_data_hpop <- test_data %>%
  dplyr::filter(ind %in% billion_ind_codes("hpop")) %>%
  add_hpop_populations() %>%
  calculate_hpop_billion(scenario = "scenario")

hpop_test <- export_country_summary_xls(test_data_hpop, "AFG", "hpop", scenario = "scenario", output_folder = temp_dir)
openxlsx::saveWorkbook(hpop_test, "inst/extdata/test_scenarios_hpop_afg.xlsx", overwrite = TRUE)

test_data_uhc <- test_data %>%
  dplyr::mutate(use_dash = TRUE) %>%
  dplyr::filter(ind %in% billion_ind_codes("uhc")) %>%
  calculate_uhc_billion(scenario = "scenario") %>%
  calculate_uhc_contribution(scenario = "scenario")

uhc_test <- export_country_summary_xls(test_data_uhc, "AFG", "uhc", scenario = "scenario", output_folder = temp_dir)
openxlsx::saveWorkbook(uhc_test, "inst/extdata/test_scenarios_uhc_afg.xlsx", overwrite = TRUE)
