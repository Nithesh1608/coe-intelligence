# Parsers for official COE extracts. These functions do not download data and
# do not drop rows. Callers decide how to treat annual totals, partial years,
# and missing tokens.

box::use(
  readr[col_character, cols, read_csv],
  readxl[excel_sheets, read_excel],
)

#' @export
blank_to_na <- function(x) {
  y <- trimws(as.character(x))
  missing <- y %in% c("", "-", "na", "NA", "n.a.", "n.a", "null", "NULL", "NaN")
  y[missing] <- NA_character_
  y
}

#' @export
singstat_month <- function(label) {
  as.Date(paste0(label, "01"), format = "%Y%b%d")
}

excel_origin <- function() {
  as.Date("1899-12-30")
}

#' @export
excel_serial_to_date <- function(x) {
  as.Date(x, origin = excel_origin())
}

empty_period <- function() {
  list(
    kind = "empty",
    month = as.Date(NA),
    year = NA_integer_,
    label = NA_character_
  )
}

period_from_text <- function(text) {
  if (grepl("^[12][0-9]{3}$", text)) {
    return(list(
      kind = "year",
      month = as.Date(NA),
      year = as.integer(text),
      label = text
    ))
  }
  if (grepl("^[12][0-9]{3}/[0-9]{2}$", text)) {
    month <- as.Date(paste0(text, "/01"), format = "%Y/%m/%d")
    return(list(kind = "month", month = month, year = NA_integer_, label = text))
  }
  if (grepl("[Jj]an|[Yy]ear to date|YTD", text)) {
    return(list(
      kind = "partial_year",
      month = as.Date(NA),
      year = NA_integer_,
      label = text
    ))
  }
  if (grepl("^[0-9]+(\\.[0-9]+)?$", text)) {
    serial <- as.numeric(text)
    if (serial > 20000 && serial < 80000) {
      day <- excel_serial_to_date(serial)
      month <- as.Date(format(day, "%Y-%m-01"))
      return(list(kind = "month", month = month, year = NA_integer_, label = text))
    }
  }
  list(kind = "other", month = as.Date(NA), year = NA_integer_, label = text)
}

#' Classify one period cell from an LTA worksheet.
#'
#' Annual totals, year-to-date labels, and monthly dates are kept distinct so
#' later code cannot add a year total on top of its months.
#'
#' @export
classify_period <- function(x) {
  if (length(x) != 1) {
    stop("classify_period() expects a single cell.")
  }
  if (is.null(x) || length(unlist(x)) == 0 || is.na(x[[1]])) {
    return(empty_period())
  }
  value <- x[[1]]
  if (inherits(value, "POSIXt") || inherits(value, "Date")) {
    day <- as.Date(value)
    return(list(
      kind = "month",
      month = as.Date(format(day, "%Y-%m-01")),
      year = NA_integer_,
      label = as.character(day)
    ))
  }
  if (is.numeric(value)) {
    if (value >= 1900 && value <= 2100 && value == floor(value)) {
      return(list(
        kind = "year",
        month = as.Date(NA),
        year = as.integer(value),
        label = as.character(value)
      ))
    }
    if (value > 20000 && value < 80000) {
      day <- excel_serial_to_date(value)
      return(list(
        kind = "month",
        month = as.Date(format(day, "%Y-%m-01")),
        year = NA_integer_,
        label = as.character(value)
      ))
    }
  }
  period_from_text(trimws(as.character(value)))
}

#' @export
period_table <- function(x) {
  parts <- lapply(seq_along(x), function(i) classify_period(x[[i]]))
  data.frame(
    kind = vapply(parts, function(p) p$kind, character(1)),
    month = as.Date(vapply(parts, function(p) {
      if (is.na(p$month)) NA_character_ else as.character(p$month)
    }, character(1))),
    year = vapply(parts, function(p) {
      if (is.na(p$year)) NA_integer_ else as.integer(p$year)
    }, integer(1)),
    label = vapply(parts, function(p) {
      if (is.na(p$label)) NA_character_ else p$label
    }, character(1)),
    stringsAsFactors = FALSE
  )
}

#' @export
parse_singstat_wide <- function(path) {
  wide <- read_csv(
    path,
    show_col_types = FALSE,
    name_repair = "minimal",
    col_types = cols(.default = col_character())
  )
  series <- trimws(as.character(wide[[1]]))
  labels <- names(wide)[-1]
  months <- singstat_month(labels)
  raw_values <- as.vector(as.matrix(wide[, -1, drop = FALSE]))
  parsed <- suppressWarnings(as.numeric(gsub(",", "", blank_to_na(raw_values))))
  n_series <- length(series)
  n_months <- length(labels)
  data.frame(
    series = rep(series, times = n_months),
    month_label = rep(labels, each = n_series),
    month = rep(months, each = n_series),
    value = parsed,
    stringsAsFactors = FALSE
  )
}

#' @export
parse_bidding_csv <- function(path) {
  bidding <- read_csv(
    path,
    show_col_types = FALSE,
    name_repair = "minimal",
    col_types = cols(.default = col_character())
  )
  names(bidding) <- tolower(names(bidding))
  bidding$month <- as.Date(paste0(trimws(bidding$month), "-01"))
  bidding$bidding_no <- trimws(bidding$bidding_no)
  bidding$vehicle_class <- trimws(bidding$vehicle_class)
  numeric_cols <- c("quota", "bids_success", "bids_received", "premium")
  for (col in numeric_cols) {
    bidding[[col]] <- suppressWarnings(
      as.numeric(gsub(",", "", blank_to_na(bidding[[col]])))
    )
  }
  bidding
}

#' @export
workbook_sheets <- function(path) {
  excel_sheets(path)
}

#' @export
read_grid <- function(path, sheet = 1) {
  read_excel(path, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
}
