# Audience-friendly dereg actual vs predicted (no quota yet)
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
p <- read.csv(file.path(root, "data/processed/model_experiment/test_predictions.csv"))
p$month <- as.Date(p$month)

keep <- c("historic_avg", "elastic_net", "ols_due", "lasso")

out_dir <- file.path(root, "data/processed/model_experiment")

# Side-by-side monthly table for primary + baseline
monthly <- p %>%
  filter(model %in% c("elastic_net", "historic_avg")) %>%
  transmute(
    month, category, model,
    actual = y,
    predicted = round(pred),
    gap = round(pred - y),
    pct_off = round(100 * (pred - y) / pmax(y, 1), 1)
  )

write.csv(monthly, file.path(out_dir, "dereg_actual_vs_predicted_monthly.csv"), row.names = FALSE)

# Compact “first page” sample months
samp <- as.Date(c(
  "2024-09-01", "2025-01-01", "2025-05-01", "2025-08-01",
  "2026-01-01", "2026-03-01", "2026-05-01", "2026-08-01"
))

first_page <- monthly %>%
  filter(month %in% samp) %>%
  select(month, category, model, actual, predicted, gap, pct_off) %>%
  arrange(category, month, model)

write.csv(first_page, file.path(out_dir, "dereg_first_page_examples.csv"), row.names = FALSE)

cat("=== FIRST-PAGE STYLE: Cat A (cars up to 1600cc / 97kW) ===\n")
cat("Month        Actual   Model forecast   Old baseline   Model gap   Baseline gap\n")
for (m in samp) {
  a <- monthly %>% filter(month == m, category == "A", model == "elastic_net")
  b <- monthly %>% filter(month == m, category == "A", model == "historic_avg")
  if (nrow(a) == 0) next
  lab <- strftime(as.Date(m), "%b %Y")
  cat(sprintf(
    "%-12s %6d   %6d           %6d           %+5d        %+5d\n",
    lab, a$actual[[1]], a$predicted[[1]], b$predicted[[1]], a$gap[[1]], b$gap[[1]]
  ))
}

cat("\n=== FIRST-PAGE STYLE: Cat B ===\n")
cat("Month        Actual   Model forecast   Old baseline   Model gap   Baseline gap\n")
for (m in samp) {
  a <- monthly %>% filter(month == m, category == "B", model == "elastic_net")
  b <- monthly %>% filter(month == m, category == "B", model == "historic_avg")
  if (nrow(a) == 0) next
  lab <- strftime(as.Date(m), "%b %Y")
  cat(sprintf(
    "%-12s %6d   %6d           %6d           %+5d        %+5d\n",
    lab, a$actual[[1]], a$predicted[[1]], b$predicted[[1]], a$gap[[1]], b$gap[[1]]
  ))
}

# Year / half-year rollups (still dereg only)
cat("\n=== ROLL-UPS (sum of monthly deregistrations on test months) ===\n")
roll <- monthly %>%
  mutate(
    period = case_when(
      month < as.Date("2025-01-01") ~ "2024 Q4 (Sep–Dec)",
      month < as.Date("2026-01-01") ~ "2025 full year",
      TRUE ~ "2026 YTD (Jan–Aug)"
    )
  ) %>%
  group_by(category, period, model) %>%
  summarise(
    months = n(),
    actual = sum(actual),
    predicted = sum(predicted),
    gap = sum(gap),
    .groups = "drop"
  ) %>%
  mutate(pct_off = round(100 * gap / pmax(actual, 1), 1))

write.csv(roll, file.path(out_dir, "dereg_actual_vs_predicted_rollup.csv"), row.names = FALSE)

for (cat in c("A", "B")) {
  cat("\nCat", cat, ":\n")
  d <- roll %>% filter(category == cat)
  for (per in unique(d$period)) {
    m <- d %>% filter(period == per, model == "elastic_net")
    b <- d %>% filter(period == per, model == "historic_avg")
    cat(sprintf(
      "  %s | actual %d | model %d (%+.1f%%) | baseline %d (%+.1f%%)\n",
      per, m$actual, m$predicted, m$pct_off, b$predicted, b$pct_off
    ))
  }
}

# Hit rates in plain English
cat("\n=== HOW OFTEN IS THE MONTH CLOSE? ===\n")
hr <- p %>%
  filter(model %in% keep) %>%
  mutate(pct = abs(pred - y) / pmax(y, 1)) %>%
  group_by(model, category) %>%
  summarise(
    months = n(),
    within_10pct = paste0(round(100 * mean(pct <= 0.10)), "%"),
    within_15pct = paste0(round(100 * mean(pct <= 0.15)), "%"),
    .groups = "drop"
  )
print(as.data.frame(hr), row.names = FALSE)

# Why fast
panel <- read.csv(file.path(out_dir, "feature_panel.csv"))
cat("\n=== WHY THE RUN WAS FAST ===\n")
cat("Rows total:", nrow(panel),
    "| train:", sum(panel$window == "train"),
    "| test:", sum(panel$window == "test"), "\n")
cat("This is monthly A/B data (~10 years), ~12 features, linear/glmnet models.\n")
cat("Not images / neural nets — fitting finishes in seconds on a laptop.\n")

cat("\nWrote:\n")
cat("  dereg_actual_vs_predicted_monthly.csv\n")
cat("  dereg_first_page_examples.csv\n")
cat("  dereg_actual_vs_predicted_rollup.csv\n")
