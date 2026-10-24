# Load processed RDS + model experiment outputs once. App does not refit models.

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

  # Monthly premium / quota for charts (mean premium, sum quota per month×cat)
  bidding <- stats::aggregate(
    cbind(premium, quota, bids_received, bids_success) ~ month + category,
    data = bids[!is.na(bids$premium), ],
    FUN = function(z) if (all(is.na(z))) NA_real_ else mean(z, na.rm = TRUE)
  )
  # Re-aggregate quota as sum of exercises in the month
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

  primary <- read_csv_safe(file.path(exp, "primary_test_forecast.csv"))
  dereg_monthly <- read_csv_safe(file.path(exp, "dereg_actual_vs_predicted_monthly.csv"))
  dereg_examples <- read_csv_safe(file.path(exp, "dereg_first_page_examples.csv"))
  dereg_rollup <- read_csv_safe(file.path(exp, "dereg_actual_vs_predicted_rollup.csv"))
  quota_test <- read_csv_safe(file.path(exp, "level2_quota_test.csv"))
  quota_outlook <- read_csv_safe(file.path(exp, "quota_outlook_calibrated.csv"))
  quota_alpha <- read_csv_safe(file.path(exp, "quota_calibration_alpha.csv"))
  quota_scores <- read_csv_safe(file.path(exp, "quota_outlook_scores.csv"))
  selection <- read_csv_safe(file.path(exp, "selection_summary.csv"))
  shortlist <- read_csv_safe(file.path(exp, "models_beat_baseline_both_categories.csv"))
  lasso_feat <- read_csv_safe(file.path(exp, "lasso_selected_features.csv"))

  meta <- list(
    primary_model = if (!is.null(primary) && nrow(primary)) primary$model[[1]] else "elastic_net",
    train_end = as.Date("2024-08-01"),
    test_start = as.Date("2024-09-01"),
    shortlist = if (!is.null(shortlist)) shortlist$model else c("elastic_net", "ols_due", "lasso")
  )

  list(
    panel = panel,
    bidding = bidding,
    bids_raw = bids,
    primary = primary,
    dereg_monthly = dereg_monthly,
    dereg_examples = dereg_examples,
    dereg_rollup = dereg_rollup,
    quota_test = quota_test,
    quota_outlook = quota_outlook,
    quota_alpha = quota_alpha,
    quota_scores = quota_scores,
    selection = selection,
    shortlist = shortlist,
    lasso_feat = lasso_feat,
    meta = meta,
    latest_dereg_month = max(panel$month[!is.na(panel$deregistrations)], na.rm = TRUE),
    latest_bid_month = max(bidding$month, na.rm = TRUE)
  )
}
