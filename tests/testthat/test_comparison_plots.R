# library(billionaiRe)
#
# old_test_data_calculated <- load_misc_data("test_data/test_data_calculated/test_data_calculated_2021-06-20T12-00-00.parquet") %>%
#   dplyr::filter(!stringr::str_detect(ind, "espar|asc|uhc"),
#                 iso3 %in% c("AFG", "BGD", "AGO", "VEN"))
#
# new_test_data_calculated <- old_test_data_calculated %>%
#   dplyr::mutate(value = value * .96)
#
# test_plot <- function(new_df, old_df, ind) {
#   testthat::test_that(paste0("plot_comparison_indicator returns plots for ", ind, ":"), {
#
#     new_df_ind <- new_df %>%
#       dplyr::filter(.data[["ind"]] == !!ind)
#
#     old_df_ind <- old_df %>%
#       dplyr::filter(.data[["ind"]] == !!ind)
#
#     if (ind %in% c("doctors", "nurses")) {
#       testthat::expect_warning(plot_comparison_indicator("df1" = new_df_ind,
#                                                          "df2" = old_df_ind,
#                                                          indicator = ind
#       ))
#     } else {
#
#       test_result <- plot_comparison_indicator(new_df_ind,
#                                                "really_this_one" = old_df_ind,
#                                                indicator = ind
#       )
#
#       testthat::expect_s3_class(test_result, "ggplot")
#
#       vdiffr::expect_doppelganger(
#         paste0("plot  ",ind),
#         plot_comparison_indicator(new_df_ind,
#                                   old_df_ind,
#                                   indicator = ind)
#       )
#
#       vdiffr::expect_doppelganger(
#         paste0("plot with named data frames",ind),
#         plot_comparison_indicator("new_data" = new_df_ind,
#                                   "old_data" = old_df_ind,
#                                   indicator = ind)
#       )
#     }
#   })
# }
#
# test_plot(new_df = new_test_data_calculated,
#           old_df = old_test_data_calculated,
#           ind = "anc4"
# )
#
#
# testthat::test_that("export_plot_comparison_pdf produces no error", {
#
#   temp_dir <- tempdir()
#   testthat::expect_error(export_plot_comparison_pdf(new_test_data_calculated,
#                                                     old_test_data_calculated,
#                                                     output_folder = temp_dir),
#                          NA)
# })
