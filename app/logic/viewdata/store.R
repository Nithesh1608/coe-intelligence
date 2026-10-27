# Load processed data for the four proposal views. App does not refit models.

#' @export
project_root <- function(start = getwd()) {
  here <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in seq_len(8)) {
    if (file.exists(file.path(here, "data", "processed", "vehicle_flows.rds"))) {
      return(here)
    }
    parent <- dirname(here)
    if (identical(parent, here)) break
    here <- parent
  }
  normalizePath(start, winslash = "/", mustWork = FALSE)
}

processed_dir <- function(root = project_root()) file.path(root, "data", "processed")
experiment_dir <- function(root = project_root()) file.path(processed_dir(root), "model_experiment")

read_csv_safe <- function(path) {
  if (!file.exists(path)) return(NULL)
  df <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if ("month" %in% names(df)) {
    m <- df$month
    if (is.numeric(m) || (is.character(m) && length(m) && grepl("^[0-9]+$", m[[1]]))) {
      df$month <- as.Date(as.numeric(m), origin = "1970-01-01")
    } else {
      df$month <- as.Date(m)
    }
  }
  df
}

as_date_col <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (is.numeric(x)) return(as.Date(x, origin = "1970-01-01"))
  as.Date(x)
}

#' @export
load_store <- function(root = project_root()) {
  dir <- processed_dir(root)
  exp <- experiment_dir(root)

  flows <- readRDS(file.path(dir, "vehicle_flows.rds"))
  flows$month <- as_date_col(flows$month)
  flows$category <- as.character(flows$category)

  bids <- readRDS(file.path(dir, "coe_results.rds"))
  bids$month <- as_date_col(bids$month)
  bids$category <- as.character(bids$category)

  pqp <- readRDS(file.path(dir, "pqp.rds"))
  pqp$month <- as_date_col(pqp$month)
  pqp$category <- as.character(pqp$category)

  reval <- readRDS(file.path(dir, "revalidation.rds"))
  reval$month <- as_date_col(reval$month)
  reval$category <- as.character(reval$category)

  age <- readRDS(file.path(dir, "pop_by_age.rds"))
  age$month <- as_date_col(age$month)
  # Normalise for category view
  if (!"age_band" %in% names(age) && "age_lower" %in% names(age)) {
    age$age_band <- paste0(age$age_lower, " - < ", age$age_lower + 1L)
  }
  if (!"number" %in% names(age) && "population" %in% names(age)) {
    age$number <- age$population
  }

  bidding <- stats::aggregate(
    cbind(premium, quota, bids_received, bids_success) ~ month + category,
    data = bids[!is.na(bids$premium), ],
    FUN = function(z) if (all(is.na(z))) NA_real_ else mean(z, na.rm = TRUE)
  )
  qsum <- stats::aggregate(
    quota ~ month + category,
    data = bids[!is.na(bids$quota), ],
    FUN = sum, na.rm = TRUE
  )
  bidding$quota <- NULL
  bidding <- merge(bidding, qsum, by = c("month", "category"), all.x = TRUE)
  bidding <- bidding[order(bidding$month, bidding$category), ]

  # Panel for feedback: flows + PQP + revalidations
  reval_tot <- stats::aggregate(
    revalidations ~ month + category,
    data = reval,
    FUN = sum,
    na.rm = TRUE
  )
  names(reval_tot)[3] <- "revalidations_total"
  pqp_m <- stats::aggregate(pqp ~ month + category, data = pqp, FUN = function(z) mean(z, na.rm = TRUE))

  panel <- flows[, c("month", "category", "registrations", "deregistrations", "population")]
  panel <- merge(panel, pqp_m, by = c("month", "category"), all.x = TRUE)
  panel <- merge(panel, reval_tot, by = c("month", "category"), all.x = TRUE)
  panel <- panel[order(panel$month, panel$category), ]

  list(
    panel = panel,
    bidding = bidding,
    bids_raw = bids,
    age = age,
    pqp = pqp,
    reval = reval,
    two_monthly = read_csv_safe(file.path(exp, "two_models_monthly.csv")),
    two_rollup = read_csv_safe(file.path(exp, "two_models_rollup.csv")),
    two_hits = read_csv_safe(file.path(exp, "two_models_hit_rates.csv")),
    two_quota = read_csv_safe(file.path(exp, "two_models_quota_outlook.csv")),
    two_quota_scores = read_csv_safe(file.path(exp, "two_models_quota_scores.csv")),
    meta = list(
      models = c("core_forecast", "momentum_forecast"),
      train_end = as.Date("2024-08-01"),
      test_start = as.Date("2024-09-01")
    ),
    latest_dereg_month = max(panel$month[!is.na(panel$deregistrations)], na.rm = TRUE),
    latest_bid_month = max(bidding$month, na.rm = TRUE)
  )
}
