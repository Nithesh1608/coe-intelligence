# Load processed RDS + two-model showcase outputs. App does not refit models.

#' @export
project_root <- function(start = getwd()) {
  here <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in seq_len(8)) {
    if (
      file.exists(file.path(here, "data", "processed", "vehicle_flows.rds")) ||
        file.exists(file.path(here, "data", "processed", "coe_monthly_panel.csv"))
    ) {
      return(here)
    }
    parent <- dirname(here)
    if (identical(parent, here)) break
    here <- parent
  }
  normalizePath(start, winslash = "/", mustWork = FALSE)
}

processed_dir <- function(root = project_root()) {
  file.path(root, "data", "processed")
}

experiment_dir <- function(root = project_root()) {
  file.path(processed_dir(root), "model_experiment")
}

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

cat_label <- function(code) {
  switch(as.character(code),
    A = "Category A",
    B = "Category B",
    C = "Category C",
    D = "Category D",
    E = "Category E",
    as.character(code)
  )
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

  bidding <- stats::aggregate(
    cbind(premium, quota, bids_received, bids_success) ~ month + category,
    data = bids[!is.na(bids$premium), ],
    FUN = function(z) if (all(is.na(z))) NA_real_ else mean(z, na.rm = TRUE)
  )
  qsum <- stats::aggregate(
    quota ~ month + category,
    data = bids[!is.na(bids$quota), ],
    FUN = sum,
    na.rm = TRUE
  )
  bidding$quota <- NULL
  bidding <- merge(bidding, qsum, by = c("month", "category"), all.x = TRUE)
  bidding <- bidding[order(bidding$month, bidding$category), ]

  panel <- flows[, c("month", "category", "registrations", "deregistrations", "population")]

  read_rds_safe <- function(name) {
    p <- file.path(dir, name)
    if (!file.exists(p)) return(NULL)
    x <- readRDS(p)
    if ("month" %in% names(x)) x$month <- as_date_col(x$month)
    if ("category" %in% names(x)) x$category <- as.character(x$category)
    x
  }

  pop_by_age <- read_rds_safe("pop_by_age.rds")
  dereg_lta <- read_rds_safe("dereg_lta.rds")
  renewal_rate <- read_rds_safe("renewal_rate.rds")
  revalidation <- read_rds_safe("revalidation.rds")
  pqp <- read_rds_safe("pqp.rds")

  two_monthly <- read_csv_safe(file.path(exp, "two_models_monthly.csv"))
  two_rollup <- read_csv_safe(file.path(exp, "two_models_rollup.csv"))
  two_hits <- read_csv_safe(file.path(exp, "two_models_hit_rates.csv"))
  two_quota <- read_csv_safe(file.path(exp, "two_models_quota_outlook.csv"))
  two_quota_scores <- read_csv_safe(file.path(exp, "two_models_quota_scores.csv"))
  quota_outlook <- read_csv_safe(file.path(exp, "quota_outlook_calibrated.csv"))
  quota_scores <- read_csv_safe(file.path(exp, "quota_outlook_scores.csv"))

  meta <- list(
    models = c("core_forecast", "momentum_forecast"),
    model_labels = c(
      core_forecast = "Elastic net",
      momentum_forecast = "Elastic net + momentum",
      simple_average = "12-month average"
    ),
    model_blurb = c(
      core_forecast = "Predicts monthly exits from due-stock and recent flow features, then maps to quota.",
      momentum_forecast = "Same elastic net, plus year-on-year due-stock change and short-run exit acceleration.",
      simple_average = "Naive baseline: average of the last twelve months of exits or quota."
    ),
    train_end = as.Date("2024-08-01"),
    test_start = as.Date("2024-09-01")
  )

  list(
    panel = panel,
    bidding = bidding,
    bids_raw = bids,
    pop_by_age = pop_by_age,
    dereg_lta = dereg_lta,
    renewal_rate = renewal_rate,
    revalidation = revalidation,
    pqp = pqp,
    two_monthly = two_monthly,
    two_rollup = two_rollup,
    two_hits = two_hits,
    two_quota = two_quota,
    two_quota_scores = two_quota_scores,
    quota_outlook = quota_outlook,
    quota_scores = quota_scores,
    meta = meta,
    cat_label = cat_label,
    latest_dereg_month = max(panel$month[!is.na(panel$deregistrations)], na.rm = TRUE),
    latest_bid_month = max(bidding$month, na.rm = TRUE)
  )
}
