# Load processed RDS + model artefacts. App does not refit models.

`%||%` <- function(a, b) {
  if (is.null(a) || !length(a) || (length(a) == 1L && is.na(a))) b else a
}

#' @export
project_root <- function(start = getwd()) {
  starts <- c(start, getOption("box.path", ""))
  starts <- unique(starts[nzchar(as.character(starts))])
  for (s in starts) {
    here <- normalizePath(s, winslash = "/", mustWork = FALSE)
    for (i in seq_len(12)) {
      if (
        file.exists(file.path(here, "data", "processed", "vehicle_flows.rds")) ||
          file.exists(file.path(here, "data", "processed", "coe_monthly_panel.csv")) ||
          file.exists(file.path(here, "rhino.yml"))
      ) {
        return(here)
      }
      parent <- dirname(here)
      if (identical(parent, here)) break
      here <- parent
    }
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
  # A month with only one of its two exercises in is partial: its summed quota
  # is half a month and must not be compared with forecasts.
  rounds <- stats::aggregate(bidding_no ~ month, data = bids[!is.na(bids$quota), ], FUN = function(z) length(unique(z)))
  partial_months <- rounds$month[rounds$bidding_no < 2L & rounds$month == max(rounds$month)]
  bidding$partial <- bidding$month %in% partial_months
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

  drop_momentum <- function(df) {
    if (is.null(df) || !"model" %in% names(df)) return(df)
    df[df$model != "momentum_forecast", , drop = FALSE]
  }

  two_monthly <- drop_momentum(read_csv_safe(file.path(exp, "two_models_monthly.csv")))
  two_rollup <- drop_momentum(read_csv_safe(file.path(exp, "two_models_rollup.csv")))
  two_hits <- drop_momentum(read_csv_safe(file.path(exp, "two_models_hit_rates.csv")))
  two_quota <- drop_momentum(read_csv_safe(file.path(exp, "two_models_quota_outlook.csv")))
  two_quota_scores <- drop_momentum(read_csv_safe(file.path(exp, "two_models_quota_scores.csv")))
  quota_outlook <- read_csv_safe(file.path(exp, "quota_outlook_calibrated.csv"))
  quota_scores <- read_csv_safe(file.path(exp, "quota_outlook_scores.csv"))
  quota_train_test <- read_csv_safe(file.path(exp, "quota_train_test_mape.csv"))

  # Forward path (production fit rolled 36 months ahead with bootstrap bands),
  # recursive backtest, scenario presets and the renewal response.
  forward_quota <- read_csv_safe(file.path(exp, "forward_quota.csv"))
  forward_dereg <- read_csv_safe(file.path(exp, "forward_dereg.csv"))
  forward_scenarios <- read_csv_safe(file.path(exp, "forward_scenarios.csv"))
  if (!is.null(forward_scenarios) && "quarter" %in% names(forward_scenarios)) {
    forward_scenarios$quarter <- as_date_col(forward_scenarios$quarter)
  }
  if (!is.null(forward_quota) && "quarter" %in% names(forward_quota)) {
    forward_quota$quarter <- as_date_col(forward_quota$quarter)
  }
  backtest <- read_csv_safe(file.path(exp, "backtest_recursive.csv"))
  calibration <- read_csv_safe(file.path(exp, "backtest_calibration.csv"))
  renewal_response <- read_csv_safe(file.path(exp, "renewal_response.csv"))
  alphas <- read_csv_safe(file.path(exp, "quota_calibration_alpha.csv"))
  coefficients <- read_csv_safe(file.path(exp, "enet_coefficients_production.csv"))
  spec_df <- read_csv_safe(file.path(exp, "model_spec.csv"))
  model_spec <- if (is.null(spec_df)) list() else stats::setNames(as.list(spec_df$value), spec_df$key)

  meta <- list(
    live_model = "core_forecast",
    models = c("core_forecast"),
    model_labels = c(
      core_forecast = "Elastic net",
      simple_average = "12-month average"
    ),
    model_blurb = c(
      core_forecast = "Predicts monthly deregistrations from registration cohorts, age stock, renewals and PQP, then maps to quota.",
      simple_average = "Naive baseline: average of the last twelve months of deregistrations or quota."
    ),
    train_end = as.Date(model_spec$train_end %||% "2024-08-01"),
    test_start = as.Date(model_spec$test_start %||% "2024-09-01"),
    spec = model_spec
  )

  list(
    root = root,
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
    quota_train_test = quota_train_test,
    forward_quota = forward_quota,
    forward_dereg = forward_dereg,
    forward_scenarios = forward_scenarios,
    backtest = backtest,
    calibration = calibration,
    renewal_response = renewal_response,
    alphas = alphas,
    coefficients = coefficients,
    meta = meta,
    cat_label = cat_label,
    latest_dereg_month = max(panel$month[!is.na(panel$deregistrations)], na.rm = TRUE),
    latest_bid_month = max(bidding$month, na.rm = TRUE),
    latest_complete_bid_month = max(bidding$month[!bidding$partial], na.rm = TRUE)
  )
}
