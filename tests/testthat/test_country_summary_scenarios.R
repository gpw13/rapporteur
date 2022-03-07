library(billionaiRe)

test_data <- load_misc_data("test_data/test_data/test_data_2022-03-06T09-30-41.parquet") %>%
  make_default_scenario(default_scenario = "pre_covid_trajectory") %>%
  dplyr::filter(!scenario %in% c("routine", "reference_infilling", "covid_shock"))

test_data_hep <- test_data %>%
  dplyr::filter(ind %in% billion_ind_codes("hep")) %>%
  transform_hep_data(scenario = "scenario") %>%
  calculate_hep_components(scenario = "scenario") %>%
  calculate_hep_billion(scenario = "scenario")

testthat::test_that("expect_country_summary_xls produced correct scenario sheet for hep",{
  temp_dir <- tempdir()
  try_wb <- export_country_summary_xls(test_data_hep, "AFG", "hep", scenario = "scenario", output_folder = temp_dir)
  try_wb <- openxlsx::readWorkbook(try_wb, sheet = "HEP_Scenarios")

  test_wb <- system.file("extdata", "test_scenarios_hep_afg.xlsx", package = "rapporteur")
  test_wb <- openxlsx::readWorkbook(test_wb, sheet = "HEP_Scenarios")

  testthat::expect_true(all.equal(try_wb, test_wb), label = "HEP scenarios passed")
})

test_data_hpop <- test_data %>%
  dplyr::filter(ind %in% billion_ind_codes("hpop")) %>%
  transform_hpop_data() %>%
  add_hpop_populations() %>%
  calculate_hpop_billion(scenario = "scenario") %>%
  dplyr::mutate(source = "This is a source")

testthat::test_that("expect_country_summary_xls produced correct scenario sheet for hpop",{
  temp_dir <- tempdir()
  try_wb <- export_country_summary_xls(test_data_hpop, "AFG", "hpop", scenario = "scenario", output_folder = temp_dir)
  try_wb <- openxlsx::readWorkbook(try_wb, sheet = "HPOP_Scenarios")

  test_wb <- system.file("extdata", "test_scenarios_hpop_afg.xlsx", package = "rapporteur")
  test_wb <- openxlsx::readWorkbook(test_wb, sheet = "HPOP_Scenarios")

  testthat::expect_true(all.equal(try_wb, test_wb), label = "hpop scenarios passed")
})

test_data_uhc <- test_data %>%
  dplyr::mutate(use_dash = TRUE) %>%
  dplyr::filter(ind %in% billion_ind_codes("uhc")) %>%
  transform_uhc_data(recycle = TRUE) %>%
  calculate_uhc_billion(scenario = "scenario") %>%
  calculate_uhc_contribution(scenario = "scenario")

testthat::test_that("expect_country_summary_xls produced correct scenario sheet for uhc",{
  temp_dir <- tempdir()
  try_wb <- export_country_summary_xls(test_data_uhc, "AFG", "uhc", scenario = "scenario", output_folder = temp_dir)
  try_wb <- openxlsx::readWorkbook(try_wb, sheet = "UHC_Scenarios")

  test_wb <- system.file("extdata", "test_scenarios_uhc_afg.xlsx", package = "rapporteur")
  test_wb <- openxlsx::readWorkbook(test_wb, sheet = "UHC_Scenarios")

  testthat::expect_true(all.equal(try_wb, test_wb), label = "uhc scenarios passed")
})

testthat::test_that("expect_country_summary_xls produced correct scenario sheet for all",{
  temp_dir <- tempdir()

  test_data_all <- dplyr::bind_rows(test_data_hep, test_data_hpop, test_data_uhc)
  testthat::expect_error(export_country_summary_xls(test_data_all, "AFG", "all", scenario = "scenario", output_folder = temp_dir), NA)
})

