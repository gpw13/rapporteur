#' Validate BorderStyle
#'
#' @param borderStyle borderStyle parameter as in openxlsx::createStyle
#' @author Philipp Schauberger
assert_valid_BorderStyle <- function(borderStyle) {
  valid <- c(
    "none", "thin", "medium", "dashed", "dotted", "thick", "double", "hair", "mediumDashed",
    "dashDot", "mediumDashDot", "dashDotDot", "mediumDashDotDot", "slantDashDot"
  )

  ind <- match(tolower(borderStyle), tolower(valid))
  if (any(is.na(ind))) {
    stop("Invalid borderStyle", call. = FALSE)
  }
}

#' Validate colour inputed
#'
#' @param colour colour as `colours()`
#' @param  errorMsg error message to be passed.
#' @author Philipp Schauberger
#'
assert_valid_Colour <- function(colour, errorMsg = "Invalid colour") {

  ## check if
  if (is.null(colour)) {
    colour <- "black"
  }

  validColours <- grDevices::colours()

  if (any(colour %in% validColours)) {
    colour[colour %in% validColours] <- col2hex(colour[colour %in% validColours])
  }

  if (any(!grepl("^#[A-Fa-f0-9]{6}$", colour))) {
    stop(errorMsg, call. = FALSE)
  }
}

#' Assert that style parameters are valid
#' @param ... list of parameters to be passed to [openxlsx::createStyle()]
#'
assert_valid_style_parameters <- function(...) {
  parameters <- c(...)

  if ("borderStyle" %in% names(parameters)) {
    assert_valid_BorderStyle(parameters["borderStyle"])
  }

  if ("halign" %in% names(parameters)) {
    halign <- tolower(parameters[["halign"]])
    if (!halign %in% c("left", "right", "center")) {
      stop("Invalid halign argument")
    }
  }

  if ("valign" %in% names(parameters)) {
    valign <- tolower(parameters[["valign"]])
    if (!valign %in% c("top", "bottom", "center")) {
      stop("Invalid valign argument")
    }
  }

  if ("wrapText" %in% names(parameters)) {
    if (!is.logical(parameters[["wrapText"]])) {
      stop("Invalid wrapText")
    }
  }

  if ("indent" %in% names(parameters)) {
    if (!is.numeric(parameters[["indent"]]) & !is.integer(parameters[["indent"]])) {
      stop("indent must be numeric")
    }
  }

  textDecoration <- tolower(parameters[["textDecoration"]])
  if (!is.null(textDecoration)) {
    if (!all(textDecoration %in% c("bold", "strikeout", "italic", "underline", "underline2", ""))) {
      stop("Invalid textDecoration")
    }
  }

  if ("borderColour" %in% names(parameters)) {
    assert_valid_Colour(parameters[["borderColour"]], "Invalid border colour")
  }

  if ("fontColour" %in% names(parameters)) {
    assert_valid_Colour(parameters[["fontColour"]], "Invalid font colour")
  }

  if ("fontSize" %in% names(parameters)) {
    if (parameters[["fontSize"]] < 1) stop("Font size must be greater than 0")
  }

  if ("locked" %in% names(parameters)) {
    if (!is.logical(parameters[["locked"]])) stop("Cell attribute locked must be TRUE or FALSE")
  }
  if ("hidden" %in% names(parameters)) {
    if (!is.logical(parameters[["hidden"]])) stop("Cell attribute hidden must be TRUE or FALSE")
  }
  if ("bgFill" %in% names(parameters)) {
    assert_valid_Colour(parameters[["bgFill"]], "Invalid bgFill colour")
  }
  if ("fgFill" %in% names(parameters)) {
    assert_valid_Colour(parameters[["fgFill"]], "Invalid fgFill colour")
  }
}

