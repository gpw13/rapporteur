#' Modify Excel Style
#'
#' `modifyStyle` modifies a [openxlsx::createStyle()] object to add the
#' parameters
#'
#' @param style a [openxlsx::createStyle()] object to be modified
#' @param ... any of  the parameters passed to [openxlsx::createStyle()]
modifyStyle <- function(style,
                        ...) {
  modifications <- list(...)
  billionaiRe:::assert_style_param(modifications)
  assert_valid_style_parameters(modifications)

  od <- getOption("OutDec")
  options("OutDec" = ".")
  on.exit(expr = options("OutDec" = od), add = TRUE)

  new_style <- style


  if ("numFmt" %in% names(modifications)) {
    numFmt_original <- modifications[["numFmt"]][[1]]
    numFmt <- tolower(numFmt_original)
    validNumFmt <- c("general", "number", "currency", "accounting", "date", "longdate", "time", "percentage", "scientific", "text", "3", "4", "comma")

    if (numFmt == "date") {
      numFmt <- openxlsx::openxlsx_getOp("dateFormat", "date")
    } else if (numFmt == "longdate") {
      numFmt <- openxlsx::openxlsx_getOp("datetimeFormat", "longdate")
    }

    numFmtMapping <- list(
      list(numFmtId = 0), # GENERAL
      list(numFmtId = 2), # NUMBER
      list(numFmtId = 164, formatCode = "&quot;$&quot;#,##0.00"), ## CURRENCY
      list(numFmtId = 44), # ACCOUNTING
      list(numFmtId = 14), # DATE
      list(numFmtId = 166, formatCode = "yyyy/mm/dd hh:mm:ss"), # LONGDATE
      list(numFmtId = 167), # TIME
      list(numFmtId = 10), # PERCENTAGE
      list(numFmtId = 11), # SCIENTIFIC
      list(numFmtId = 49), # TEXT
      list(numFmtId = 3),
      list(numFmtId = 4),
      list(numFmtId = 3)
    )

    names(numFmtMapping) <- validNumFmt

    if (numFmt != "general") {
      if (numFmt %in% validNumFmt) {
        new_style$numFmt <- numFmtMapping[[numFmt[[1]]]]
      } else {
        new_style$numFmt <- list("numFmtId" = 165, formatCode = numFmt) ## Custom numFmt
      }
    }
  }

  if ("fontName" %in% names(modifications)) {
    new_style$fontName <- list("val" = modifications$fontName)
  }

  if ("fontSize" %in% names(modifications)) {
    new_style$fontSize <- list("val" = modifications$fontSize)
  }

  if ("fontColour" %in% names(modifications)) {
    new_style$fontColour <- list("rgb" = excel_hex_colour(modifications$fontColour))
  }

  if ("textDecoration" %in% names(modifications)) {
    new_style$fontDecoration <- toupper(modifications$textDecoration)
  }

  if ("bgFill" %in% names(modifications)) {
    if ("fillBg" %in% names(new_style$fill)) {
      new_style$fill$fillBg <- NULL
    }
    new_style$fill <- append(new_style$fill, list(fillBg = list("rgb" = excel_hex_colour(modifications$bgFill))))
  }

  ## foreground fill
  if ("fgFill" %in% names(modifications)) {
    if ("fillFg" %in% names(new_style$fill)) {
      new_style$fill$fillFg <- NULL
    }
    new_style$fill <- append(new_style$fill, list(fillFg = list("rgb" = excel_hex_colour(modifications$fgFill))))
  }


  ## border
  if (sum(grepl("border", names(modifications))) > 0) {
    sides <- c("LEFT", "RIGHT", "TOP", "BOTTOM")

    if ("border" %in% names(modifications)) {
      border <- toupper(modifications[["border"]])
      border <- paste(border, collapse = "")
    } else {
      border <- sides
    }
    ## find position of each side in string
    pos <- sapply(sides, function(x) regexpr(x, border))
    pos <- pos[order(pos, decreasing = FALSE)]
    nSides <- sum(pos > 0)
    if ("borderColour" %in% names(modifications)) {
      borderColour <- rep(excel_hex_colour(modifications$borderColour), length.out = nSides)
    } else {
      borderColour <- rep(excel_hex_colour("black"), length.out = nSides)
    }
    if ("borderStyle" %in% names(modifications)) {
      borderStyle <- rep(modifications$borderStyle, length.out = nSides)
    } else {
      borderStyle <- rep("thin", length.out = nSides)
    }

    pos <- pos[pos > 0]

    if (length(pos) == 0) {
      stop("Unknown border argument")
    }
    names(borderColour) <- names(pos)

    names(borderStyle) <- names(pos)

    if ("LEFT" %in% names(pos)) {
      new_style$borderLeft <- borderStyle[["LEFT"]]
      new_style$borderLeftColour <- list("rgb" = borderColour[["LEFT"]])
    }

    if ("RIGHT" %in% names(pos)) {
      new_style$borderRight <- borderStyle[["RIGHT"]]
      new_style$borderRightColour <- list("rgb" = borderColour[["RIGHT"]])
    }

    if ("TOP" %in% names(pos)) {
      new_style$borderTop <- borderStyle[["TOP"]]
      new_style$borderTopColour <- list("rgb" = borderColour[["TOP"]])
    }

    if ("BOTTOM" %in% names(pos)) {
      new_style$borderBottom <- borderStyle[["BOTTOM"]]
      new_style$borderBottomColour <- list("rgb" = borderColour[["BOTTOM"]])
    }
  }

  ## other fields
  if ("halign" %in% names(modifications)) {
    new_style$halign <- modifications$halign
  }

  if ("valign" %in% names(modifications)) {
    new_style$valign <- modifications$valign
  }

  if ("indent" %in% names(modifications)) {
    new_style$indent <- modifications$indent
  }

  if ("wrapText" %in% names(modifications)) {
    new_style$wrapText <- modifications$wrapText
  }

  if ("textRotation" %in% names(modifications)) {
    if (!is.numeric(modifications$textRotation)) {
      stop("textRotation must be numeric.")
    }

    if (modifications$textRotation < 0 & modifications$textRotation >= -90) {
      modifications$textRotation <- (modifications$textRotation * -1) + 90
    }

    new_style$textRotation <- round(modifications$textRotation, 0)
  }

  if ("locked" %in% names(modifications)) {
    new_style$locked <- modifications$locked
  }

  if ("hidden" %in% names(modifications)) {
    new_style$hidden <- modifications$hidden
  }

  return(new_style)
}

#' Converts rgb colour to hex colour
#'
#' @param colour  colour as in `colours()`
#' @author Philipp Schauberger
col2hex <- function(colour) {
  grDevices::rgb(t(grDevices::col2rgb(colour)), maxColorValue = 255)
}

#' Converts colour to Excel hex colour
#'
#' @param colour  colour as in `colours()`
#' @param  errorMsg error message to be passed.
#'
excel_hex_colour <- function(colour, errorMsg = "Invalid colour") {
  validColours <- grDevices::colours()

  if (any(colour %in% validColours)) {
    colour[colour %in% validColours] <- col2hex(colour[colour %in% validColours])
  }

  if (any(!grepl("^#[A-Fa-f0-9]{6}$", colour))) {
    stop(errorMsg, call. = FALSE)
  }

  colour <- gsub("^#", "FF", toupper(colour))

  return(colour)
}
