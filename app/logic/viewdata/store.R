# Load processed tables once. The app does not rebuild them or fit a model.

#' @export
project_root <- function(start = getwd()) {
  here <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in seq_len(8)) {
    if (file.exists(file.path(here, "data", "processed", "coe_monthly_panel.csv"))) {
      return(here)
    }
    parent <- dirname(here)
    if (identical(parent, here)) {
      break
    }
    here <- parent
  }
  normalizePath(start, winslash = "/", mustWork = FALSE)
}

processed_dir <- function(root = project_root()) {
  file.path(root, "data", "processed")
}

read_csv <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

#' @export
load_store <- function(root = project_root()) {
  dir <- processed_dir(root)
  panel <- read_csv(file.path(dir, "coe_monthly_panel.csv"))
  panel$month <- as.Date(panel$month)
  bidding <- read_csv(file.path(dir, "coe_bidding_history.csv"))
  bidding$month <- as.Date(bidding$month)
  age <- read_csv(file.path(dir, "vehicle_age_distribution.csv"))
  age$month <- as.Date(age$month)
  list(
    panel = panel,
    bidding = bidding,
    age = age,
    latest_dereg_month = max(panel$month[!is.na(panel$deregistrations)], na.rm = TRUE),
    latest_bid_month = max(bidding$month, na.rm = TRUE)
  )
}
