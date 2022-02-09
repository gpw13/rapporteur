# Data frame checks ------------------------------------------------------

#' Assert that columns exist in a data frame
#'
#' @param df Data frame
#' @param ... Column names
assert_columns <- function(df, ...) {
  columns <- c(...)
  bad_cols <- columns[!(columns %in% names(df))]
  if (length(bad_cols) > 0) {
    stop(sprintf(
      "Column(s) %s are not in the data frame",
      paste(bad_cols, collapse = ", ")
    ),
    call. = FALSE
    )
  }
}

#' Assert that x is in list or is NULL
#'
#' @param x value to be checked
#' @param list list of values to be checked against
assert_in_list_or_null <- function(x, list) {
  if (!is.null(x)) {
    if (!x %in% list) {
      stop(sprintf(
        "%s must be present in %s or NULL",
        x, paste(list, collapse = ", ")
      ),
      call. = FALSE
      )
    }
  }
}

#' Assert that the given data frame columns are numeric
#'
#' @param df Data frame
#' @param ... Column names
assert_numeric <- function(df, ...) {
  args <- c(...)
  nums <- sapply(args, function(x) is.numeric(df[[x]]))
  if (!all(nums)) {
    stop(sprintf(
      "%s must be numeric not %s",
      paste(args[!nums], collapse = ", "),
      paste(sapply(args[!nums], function(x) class(df[[x]])), collapse = ", ")
    ),
    call. = FALSE
    )
  }
}

#' Assert that two or more vectors are the same length
#'
#' @param ... Two or more vectors that should be the same length.
#' @param recycle (logical) Whether vectors of length one can be recycled to match the length
#'   of the other vectors.
#' @param remove_null (logical) whether NULL values should be removed from the inputs before
#'   comparison
assert_same_length <- function(..., recycle = FALSE, remove_null = FALSE) {
  # Extract just the names of the ... arguments
  arg_names <- sys.call()
  end_idx <- length(arg_names) - 2
  arg_names <- arg_names[2:end_idx]

  args <- list(...)

  # Ensure that the input has at least two vectors for comparison
  assert_min_length(args, 2)

  if (remove_null) {
    args <- args[!sapply(args, is.null)]
  }

  # If recycle = TRUE
  if (recycle) {
    length_one_vecs <- args[sapply(args, length) == 1]

    # If all the vectors are of length 1, then return immediately
    if (length(length_one_vecs) == length(args)) {
      return(invisible())
    }
    # Otherwise, remove the length one vectors from the list of vector to check
    # because they can always be replicated
    else {
      args <- args[sapply(args, length) != 1]
    }
  }

  cond <- purrr::map(args, length) %>%
    purrr::reduce(`==`)

  if (!cond) {
    stop(sprintf(
      "%s must have the same length.",
      paste(arg_names, collapse = ", ")
    ),
    call. = FALSE
    )
  }
}

#' Assert that `params` are valid formal argument to [openxlsx::createStyle()]
#'
#' @param ... character vector of parameters to [openxlsx::createStyle()]
assert_style_param <- function(...) {
  params <- list(...)
  createStylesParams <- names(formals(openxlsx::createStyle))
  bad_params <- params[!names(params) %in% createStylesParams]

  if (length(bad_params) > 0) {
    stop(sprintf(
      "Params(s) %s are not valid formal argument to openxlsx::createStyle",
      paste(bad_params, collapse = ", ")
    ),
    call. = FALSE
    )
  }
}

#' Assert unique rows of df
#'
#' Makes sure there are distinct rows for each ind, iso3, year, and scenario if
#' being used.
#'
#' @inheritParams export_all_countries_summaries_xls
#' @inheritParams export_hep_country_summary_xls
#'
assert_unique_rows <- function(df,
                               ind,
                               iso3,
                               year,
                               scenario = NULL,
                               ind_ids) {
  ind_df <- dplyr::filter(df, .data[[ind]] %in% ind_ids)
  dist_df <- dplyr::distinct(ind_df, dplyr::across(dplyr::any_of(c(ind, iso3, year, scenario))))
  if (nrow(ind_df) != nrow(dist_df)) {
    stop("`df` does not have distinct rows for each combination of `ind`, `iso3`, and `year` (by `scenario` if present), please make distinct.",
         call. = FALSE
    )
  }
}

#' Asserts that provided ISO is valid
#'
#' Checks that provided ISO code is a valid ISO3 code for a WHO member state,
#' using [whoville::is_who_member()].
#'
#' @param iso Single ISO3 code
assert_who_iso <- function(iso) {
  assert_string(iso, 1)
  if (!whoville::is_who_member(iso)) {
    stop(strwrap("`iso` must be a valid WHO member state ISO3 code.
                 All valid codes are available through `whoville::who_member_states()`."),
         call. = FALSE
    )
  }
}


#' Assert that end years are always later than start year
#'
#' @param start_year Start year
#' @param end_year End year(s)
assert_years <- function(start_year, end_year) {
  if (!all(start_year < end_year)) {
    stop("`end_year` must always be after `start_year`.",
         call. = FALSE
    )
  }
}

#' Assert that a vector has a minimum length n
#'
#' @param x (vector)
#' @param n (integer) the minimum allowed size of the vector
assert_min_length <- function(x, n) {
  l <- length(x)
  if (l < n) {
    stop(sprintf("%s must have a minimum length of %s elements", deparse(substitute(x)), n),
         call. = FALSE
    )
  }
}


#' Assert that `x` is a character vector of length n
#'
#' @param x Supposed string to test
#' @param n Length to test
assert_string <- function(x, n) {
  if (!is.null(x)) {
    lx <- length(x)
    if (!((is.character(x) & (lx == n)))) {
      stop(sprintf(
        "`%s` must be a character vector of length %d, not %s of length %d.",
        deparse(substitute(x)),
        n,
        class(x),
        lx
      ))
    }
  }
}
