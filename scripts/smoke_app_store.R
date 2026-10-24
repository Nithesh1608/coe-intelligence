# Smoke-test load_store() the same way the app resolves the project root
root <- normalizePath(".", winslash = "/", mustWork = TRUE)
setwd(root)

# Evaluate store.R without box (strip box::use lines) — fragile; instead duplicate
# the critical path inline matching store.R
flows <- readRDS(file.path(root, "data/processed/vehicle_flows.rds"))
flows$month <- as.Date(flows$month)
flows$category <- as.character(flows$category)

read_csv_safe <- function(path) {
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

exp <- file.path(root, "data/processed/model_experiment")
primary <- read_csv_safe(file.path(exp, "primary_test_forecast.csv"))
dereg <- read_csv_safe(file.path(exp, "dereg_actual_vs_predicted_monthly.csv"))
quota <- read_csv_safe(file.path(exp, "level2_quota_test.csv"))
roll <- read_csv_safe(file.path(exp, "dereg_actual_vs_predicted_rollup.csv"))

stopifnot(nrow(primary) > 0, nrow(dereg) > 0, nrow(quota) > 0, nrow(roll) > 0)
stopifnot(inherits(primary$month[[1]], "Date"))
stopifnot(all(c("A", "B") %in% unique(flows$category)))

cat("Smoke OK\n")
cat("  primary:", primary$model[[1]], "rows", nrow(primary), "\n")
cat("  dereg monthly:", nrow(dereg), "\n")
cat("  quota rows:", nrow(quota), "range", as.character(min(quota$month)), "→", as.character(max(quota$month)), "\n")
cat("  rollup periods:", paste(unique(roll$period), collapse = " | "), "\n")
