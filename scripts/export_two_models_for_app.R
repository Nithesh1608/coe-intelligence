# Export the two production showcases + simple average for the app (plain labels)
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(glmnet)
})

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
exp <- file.path(root, "data", "processed", "model_experiment")
TRAIN_END <- as.Date("2024-08-01")
TEST_START <- as.Date("2024-09-01")
set.seed(3702L)

panel <- read.csv(file.path(exp, "feature_panel.csv"), stringsAsFactors = FALSE)
panel$month <- as.Date(panel$month)
panel$category <- as.character(panel$category)

panel <- panel %>%
  arrange(category, month) %>%
  group_by(category) %>%
  mutate(
    due10_yoy = due10 - dplyr::lag(due10, 12L),
    due10_mom3 = due10 - dplyr::lag(due10, 3L),
    y_mom3 = y_lag2 - dplyr::lag(y_lag2, 3L),
    exit_accel = y_lag2 - y_lag12
  ) %>%
  ungroup()

base_cols <- c(
  "due10", "due15", "due_near", "recent12", "y_lag12", "y_lag2",
  "reval5_lag12", "reval5_lag24", "reval5_lag60", "lpqp",
  "post_2022", "moy_sin", "moy_cos"
)
mom_cols <- c(base_cols, "due10_yoy", "due10_mom3", "y_mom3", "exit_accel")

panel <- panel %>%
  group_by(category) %>%
  mutate(across(all_of(mom_cols), ~ {
    med <- median(.x[month <= TRAIN_END], na.rm = TRUE)
    ifelse(is.na(.x), med, .x)
  })) %>%
  ungroup() %>%
  filter(month >= as.Date("2018-01-01"))

x_mat <- function(df, cols) as.matrix(df[, cols, drop = FALSE])

fit_pred <- function(tr, te, cols) {
  cv <- cv.glmnet(
    x_mat(tr, cols), tr$y, alpha = 0.5,
    nfolds = min(5L, max(3L, nrow(tr) %/% 12L)),
    type.measure = "mae"
  )
  pmax(as.numeric(predict(cv, newx = x_mat(te, cols), s = "lambda.1se")), 0.2 * te$recent12)
}

rows <- list()
for (cat in c("A", "B")) {
  tr <- panel %>% filter(category == cat, month <= TRAIN_END)
  te <- panel %>% filter(category == cat, month >= TEST_START)
  rows[[length(rows) + 1]] <- tibble(
    month = te$month, category = cat, model = "core_forecast",
    label = "Core supply forecast",
    actual = te$y, predicted = round(fit_pred(tr, te, base_cols))
  )
  rows[[length(rows) + 1]] <- tibble(
    month = te$month, category = cat, model = "momentum_forecast",
    label = "Momentum supply forecast",
    actual = te$y, predicted = round(fit_pred(tr, te, mom_cols))
  )
  rows[[length(rows) + 1]] <- tibble(
    month = te$month, category = cat, model = "simple_average",
    label = "Simple average of the last 12 months",
    actual = te$y, predicted = round(te$recent12)
  )
}
out <- bind_rows(rows)
out$month <- as.character(as.Date(out$month))
out$gap <- out$predicted - out$actual
out$pct_off <- round(100 * out$gap / pmax(out$actual, 1), 1)

write.csv(out, file.path(exp, "two_models_monthly.csv"), row.names = FALSE)

# Rollups for Desk
roll <- out %>%
  mutate(
    period = dplyr::case_when(
      as.Date(month) < as.Date("2025-01-01") ~ "Late 2024 (September to December)",
      as.Date(month) < as.Date("2026-01-01") ~ "Full year 2025",
      TRUE ~ "2026 so far (January to August)"
    )
  ) %>%
  group_by(category, period, model, label) %>%
  summarise(
    months = n(),
    actual = sum(actual),
    predicted = sum(predicted),
    gap = sum(gap),
    .groups = "drop"
  ) %>%
  mutate(pct_off = round(100 * gap / pmax(actual, 1), 1))

write.csv(roll, file.path(exp, "two_models_rollup.csv"), row.names = FALSE)

# Hit rates
hit <- out %>%
  mutate(pct = abs(predicted - actual) / pmax(actual, 1)) %>%
  group_by(model, label, category) %>%
  summarise(
    months = n(),
    within_10pct = mean(pct <= 0.10),
    within_15pct = mean(pct <= 0.15),
    .groups = "drop"
  )
write.csv(hit, file.path(exp, "two_models_hit_rates.csv"), row.names = FALSE)

cat("Exported two_models_monthly.csv / rollup / hit_rates\n")
print(hit)
