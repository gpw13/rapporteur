
# rapporteur

<!-- badges: start -->
[![R-CMD-check](https://github.com/gpw13/rapporteur/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gpw13/rapporteur/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/gpw13/rapporteur/branch/main/graph/badge.svg?token=WK2WM2A7PU)](https://codecov.io/gh/gpw13/rapporteur)
<!-- badges: end -->

The goal of rapporteur is to facilitate reporting on the World Health
Organization’s Triple Billions framework.

## Installation

You can install rapporteur from [GitHub](https://github.com/) with:

## Demonstration dataset

A demonstration dataset is provided in the dataset. It does not reflect
actual data for the countries included. It serves only to demonstrate
the functions of the package.

``` r
library(rapporteur)

head(all_billions_example)
```

    ##   iso3 year               ind use_dash
    ## 1  CAN 2000 surviving_infants     TRUE
    ## 2  CAN 2000   measles_routine     TRUE
    ## 3  CAN 2000     polio_routine     TRUE
    ## 4  CAN 2001 surviving_infants     TRUE
    ## 5  CAN 2001   measles_routine     TRUE
    ## 6  CAN 2001     polio_routine     TRUE
    ##                                                                                                                                           source
    ## 1 United Nations, Department of Economic and Social Affairs, Population Division (2019). World Population Prospects 2019, Online Edition. Rev. 1
    ## 2                                                                                WHO/UNICEF Estimates of National Immunization Coverage (WUENIC)
    ## 3                                                                                WHO/UNICEF Estimates of National Immunization Coverage (WUENIC)
    ## 4 United Nations, Department of Economic and Social Affairs, Population Division (2019). World Population Prospects 2019, Online Edition. Rev. 1
    ## 5                                                                                WHO/UNICEF Estimates of National Immunization Coverage (WUENIC)
    ## 6                                                                                WHO/UNICEF Estimates of National Immunization Coverage (WUENIC)
    ##       type    value transform_value level contribution contribution_percent
    ## 1 reported 45.18210        45.18210    NA           NA                   NA
    ## 2 reported 28.45644        28.45644    NA           NA                   NA
    ## 3 reported 76.77938        76.77938    NA           NA                   NA
    ## 4 reported 56.95852        56.95852    NA           NA                   NA
    ## 5 reported 48.03589        48.03589    NA           NA                   NA
    ## 6 reported 87.61698        87.61698    NA           NA                   NA
    ##   population contribution_percent_total_pop
    ## 1         NA                             NA
    ## 2         NA                             NA
    ## 3         NA                             NA
    ## 4         NA                             NA
    ## 5         NA                             NA
    ## 6         NA                             NA

## Country Summary Sheets

Country summary sheets are country-specific Excel files reporting on the
Triple Billions.

Two functions exist to generate these:
* `export_country_summary_xls()` : export a
specific country 
* `export_all_countries_summaries_xls()` : export all
countries present in the dataset

The `billion` parameter allows to export a specific billion or all at
once.

``` r
# Export HEP summary for Afghanistan:
export_country_summary_xls(all_billions_example, "AFG", "hep")

# Export all billions for Canada:
export_country_summary_xls(all_billions_example, "CAN", "all")

# By default, files are saved to "outputs" folder, but this can be modified with `output_folder'.
# If the folder doesn't exists, it will be created (recursively).
export_country_summary_xls(all_billions_example, "CAN", "all", output_folder = "outputs/all/CAN")

# Exporting all billions for all countries in the dataset:
export_all_countries_summaries_xls(all_billions_example, "all", output_folder = "outputs/all")
```
