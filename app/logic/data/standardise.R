# Long, standardised extracts. Raw files are read and not modified.
# `na` tokens stay missing. Zeros stay zeros.

box::use(
  app/logic/data/parse[blank_to_na, parse_bidding_csv, parse_singstat_wide],
)

#' @export
shift_month <- function(x, n) {
  x <- as.Date(x)
  year <- as.integer(format(x, "%Y"))
  month_index <- as.integer(format(x, "%m")) - 1L
  absolute <- year * 12L + month_index + as.integer(n)
  sprintf_month <- sprintf("%04d-%02d-01", absolute %/% 12L, absolute %% 12L + 1L)
  as.Date(sprintf_month)
}

#' @export
month_span <- function(from, to) {
  seq(as.Date(from), as.Date(to), by = "month")
}

category_from_label <- function(label) {
  label <- trimws(label)
  mapped <- c(
    "Category A" = "A",
    "Category A: Cars" = "A",
    "Cars Up To 1600cc And 97kW" = "A",
    "Category B" = "B",
    "Category B: Cars" = "B",
    "Cars Above 1600cc Or 97kW" = "B",
    "Category C" = "C",
    "Category C: Goods Vehicles & Buses" = "C",
    "Goods Vehicles & Buses" = "C",
    "Category D" = "D",
    "Category D: Motorcycles & Scooters" = "D",
    "Motorcycles" = "D",
    "Category E" = "E",
    "Open Category" = "E"
  )
  unname(mapped[label])
}

measure_from_series <- function(series) {
  if (grepl("Prevailing Quota Premium", series, fixed = TRUE)) {
    return(NA_character_)
  }
  if (startsWith(series, "Total ")) {
    return(NA_character_)
  }
  measures <- c(
    "Quota Premium", "Successful Bids", "Bids Received", "Quota"
  )
  hit <- measures[vapply(measures, function(item) {
    grepl(item, series, fixed = TRUE)
  }, logical(1))]
  if (!length(hit)) {
    return(NA_character_)
  }
  hit[[1]]
}

prefix_from_series <- function(series) {
  prefixes <- c(
    "Cars Up To 1600cc And 97kW",
    "Cars Above 1600cc Or 97kW",
    "Goods Vehicles & Buses",
    "Motorcycles",
    "Open Category"
  )
  vapply(series, function(one) {
    hit <- prefixes[startsWith(one, prefixes)]
    if (!length(hit)) NA_character_ else hit[[1]]
  }, character(1), USE.NAMES = FALSE)
}

#' @export
singstat_flow <- function(path, series_names) {
  long <- parse_singstat_wide(path)
  long <- long[long$series %in% series_names, , drop = FALSE]
  long$category <- category_from_label(long$series)
  long[, c("series", "category", "month", "value")]
}

#' @export
bidding_from_datamall <- function(path) {
  bidding <- parse_bidding_csv(path)
  data.frame(
    month = bidding$month,
    bidding_no = bidding$bidding_no,
    category = category_from_label(bidding$vehicle_class),
    quota = bidding$quota,
    bids_success = bidding$bids_success,
    bids_received = bidding$bids_received,
    premium = bidding$premium,
    source = "datamall_m11",
    stringsAsFactors = FALSE
  )
}

#' @export
bidding_from_singstat <- function(path) {
  long <- parse_singstat_wide(path)
  rows <- lapply(unique(long$series), function(series) {
    measure <- measure_from_series(series)
    prefix <- prefix_from_series(series)
    if (is.na(measure) || is.na(prefix)) {
      return(NULL)
    }
    exercise <- if (grepl("1st Bidding", series, fixed = TRUE)) "1" else "2"
    part <- long[long$series == series, , drop = FALSE]
    data.frame(
      month = part$month,
      bidding_no = exercise,
      category = category_from_label(prefix),
      measure = measure,
      value = part$value,
      stringsAsFactors = FALSE
    )
  })
  flat <- do.call(rbind, rows)
  keys <- unique(flat[, c("month", "bidding_no", "category")])
  take <- function(measure) {
    part <- flat[flat$measure == measure, c("month", "bidding_no", "category", "value")]
    names(part)[names(part) == "value"] <- measure
    part
  }
  out <- keys
  for (measure in c("Quota", "Successful Bids", "Bids Received", "Quota Premium")) {
    out <- merge(out, take(measure), by = c("month", "bidding_no", "category"), all.x = TRUE)
  }
  data.frame(
    month = out$month,
    bidding_no = out$bidding_no,
    category = out$category,
    quota = out$Quota,
    bids_success = out$`Successful Bids`,
    bids_received = out$`Bids Received`,
    premium = out$`Quota Premium`,
    source = "singstat_quota_premium",
    stringsAsFactors = FALSE
  )
}

#' @export
pqp_from_datamall <- function(path) {
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  data.frame(
    month = as.Date(paste0(raw$month, "-01")),
    category = category_from_label(raw$vehicle_class),
    pqp = as.numeric(gsub(",", "", blank_to_na(raw$pqp))),
    source = "datamall_pqp",
    stringsAsFactors = FALSE
  )
}

#' @export
pqp_from_singstat <- function(path) {
  long <- parse_singstat_wide(path)
  keep <- grepl("Prevailing Quota Premium", long$series, fixed = TRUE)
  part <- long[keep, , drop = FALSE]
  data.frame(
    month = shift_month(part$month, 1L),
    category = category_from_label(prefix_from_series(part$series)),
    pqp = part$value,
    source = "singstat_pqp_shifted",
    stringsAsFactors = FALSE
  )
}

#' @export
revalidations_long <- function(path) {
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  data.frame(
    month = as.Date(paste0(raw$month, "-01")),
    term = ifelse(trimws(raw$type) == "5 Year", "5y", "10y"),
    category = category_from_label(raw$category),
    value = as.numeric(gsub(",", "", blank_to_na(raw$number))),
    stringsAsFactors = FALSE
  )
}

#' @export
age_long <- function(path) {
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  band <- trimws(raw$age)
  band[band %in% c("20 & above", "20 and above")] <- "20 and above"
  number <- as.numeric(gsub(",", "", blank_to_na(raw$number)))
  data.frame(
    month = as.Date(paste0(raw$month, "-01")),
    vehicle_type = trimws(raw$type),
    age_band = band,
    number = number,
    number_status = ifelse(is.na(number), "not_published", "observed"),
    stringsAsFactors = FALSE
  )
}
