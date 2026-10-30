# Calibrated monthly quota outlook from exits.
# Textbook 25% identity is annual-scale; monthly tenders need a fitted link:
#   published_monthly ≈ alpha * trailing_12m_exits
# alpha is fit on train-era months only (before test), then applied to predicted exits.

suppressPackageStartupMessages({
  library(dplyr)
})

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
proc <- file.path(root, "data", "processed")
exp <- file.path(proc, "model_experiment")
TRAIN_END <- as.Date("2024-08-01")
TEST_START <- as.Date("2024-09-01")

flows <- readRDS(file.path(proc, "vehicle_flows.rds"))
bids <- readRDS(file.path(proc, "coe_results.rds"))
pred <- read.csv(file.path(exp, "primary_test_forecast.csv"), stringsAsFactors = FALSE)
pred$month <- as.Date(pred$month)

flows$month <- as.Date(flows$month)
flows$category <- as.character(flows$category)
bids$month <- as.Date(bids$month)
bids$category <- as.character(bids$category)

add_months <- function(month, n) {
  m <- as.Date(month)
  y <- as.integer(format(m, "%Y"))
  mo <- as.integer(format(m, "%m")) + as.integer(n)
  y <- y + (mo - 1L) %/% 12L
  mo <- (mo - 1L) %% 12L + 1L
  as.Date(sprintf("%04d-%02d-01", y, mo))
}

# Monthly published tender quota
pub <- bids %>%
  filter(category %in% c("A", "B"), !is.na(quota)) %>%
  group_by(month, category) %>%
  summarise(published = sum(quota, na.rm = TRUE), .groups = "drop")

# Actual exits
y_act <- flows %>%
  filter(category %in% c("A", "B"), !is.na(deregistrations)) %>%
  transmute(month, category, exits = as.numeric(deregistrations))

# Predicted exits on test; actual before
lookup_exit <- function(cat, m, use_pred = TRUE) {
  if (use_pred && m >= TEST_START) {
    hit <- pred$category == cat & pred$month == m
    if (any(hit)) return(pred$mid[hit][[1]])
  }
  hit <- y_act$category == cat & y_act$month == m
  if (any(hit)) y_act$exits[hit][[1]] else NA_real_
}

trailing_12 <- function(cat, end_m, use_pred = TRUE) {
  months <- add_months(end_m, -11:0)
  sum(vapply(months, function(m) lookup_exit(cat, m, use_pred), numeric(1)), na.rm = TRUE)
}

# Build train panel for alpha: published ~ alpha * d12_actual
train_months <- sort(unique(pub$month[pub$month <= TRAIN_END & pub$month >= as.Date("2018-01-01")]))
train_rows <- list()
for (tm in train_months) {
  for (cat in c("A", "B")) {
    d12 <- trailing_12(cat, tm, use_pred = FALSE)
    p <- pub$published[pub$month == tm & pub$category == cat]
    if (!length(p) || is.na(d12) || d12 <= 0) next
    train_rows[[length(train_rows) + 1]] <- data.frame(
      month = tm, category = cat, published = p[[1]], d12 = d12,
      stringsAsFactors = FALSE
    )
  }
}
train_df <- bind_rows(train_rows)

# alpha by category = median(published / d12) on train
alphas <- train_df %>%
  group_by(category) %>%
  summarise(
    alpha = median(published / d12, na.rm = TRUE),
    n_months = n(),
    .groups = "drop"
  )

# Also keep textbook annual for reference notes
alphas$textbook_monthly_share <- 0.9 * 0.25 / 12

write.csv(alphas, file.path(exp, "quota_calibration_alpha.csv"), row.names = FALSE)

# Outlook on test window
test_months <- sort(unique(pred$month))
out <- list()
for (tm in test_months) {
  for (cat in c("A", "B")) {
    a <- alphas$alpha[alphas$category == cat][[1]]
    d12_pred <- trailing_12(cat, tm, use_pred = TRUE)
    d12_act <- trailing_12(cat, tm, use_pred = FALSE)
    p <- pub$published[pub$month == tm & pub$category == cat]
    out[[length(out) + 1]] <- data.frame(
      month = tm,
      category = cat,
      published = if (length(p)) p[[1]] else NA_real_,
      outlook_from_forecast = a * d12_pred,
      outlook_from_actual_exits = a * d12_act,
      alpha = a,
      d12_pred = d12_pred,
      d12_act = d12_act,
      stringsAsFactors = FALSE
    )
  }
}
outlook <- bind_rows(out)
outlook$month <- as.character(as.Date(outlook$month))
write.csv(outlook, file.path(exp, "quota_outlook_calibrated.csv"), row.names = FALSE)

# Scores
scores <- outlook %>%
  filter(!is.na(published)) %>%
  group_by(category) %>%
  summarise(
    n = n(),
    MAE = mean(abs(outlook_from_forecast - published)),
    MAPE = mean(abs(outlook_from_forecast - published) / pmax(published, 1)),
    MAE_oracle = mean(abs(outlook_from_actual_exits - published)),
    .groups = "drop"
  )
write.csv(scores, file.path(exp, "quota_outlook_scores.csv"), row.names = FALSE)

cat("Calibration alpha (train ≤ 2024-08):\n")
print(alphas)
cat("\nCalibrated outlook vs published (test):\n")
print(scores)
cat("\nWrote quota_outlook_calibrated.csv\n")
