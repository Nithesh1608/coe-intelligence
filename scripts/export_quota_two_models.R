# Quota outlook for both showcased exit engines (train-only alpha, locked test)
suppressPackageStartupMessages({
  library(dplyr)
  library(glmnet)
})

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
proc <- file.path(root, "data", "processed")
exp <- file.path(proc, "model_experiment")
TRAIN_END <- as.Date("2024-08-01")
TEST_START <- as.Date("2024-09-01")
set.seed(3702L)

add_months <- function(month, n) {
  m <- as.Date(month)
  y <- as.integer(format(m, "%Y"))
  mo <- as.integer(format(m, "%m")) + as.integer(n)
  y <- y + (mo - 1L) %/% 12L
  mo <- (mo - 1L) %% 12L + 1L
  as.Date(sprintf("%04d-%02d-01", y, mo))
}

flows <- readRDS(file.path(proc, "vehicle_flows.rds"))
bids <- readRDS(file.path(proc, "coe_results.rds"))
two <- read.csv(file.path(exp, "two_models_monthly.csv"), stringsAsFactors = FALSE)
two$month <- as.Date(two$month)

flows$month <- as.Date(flows$month)
flows$category <- as.character(flows$category)
bids$month <- as.Date(bids$month)
bids$category <- as.character(bids$category)

pub <- bids %>%
  filter(category %in% c("A", "B"), !is.na(quota)) %>%
  group_by(month, category) %>%
  summarise(published = sum(quota, na.rm = TRUE), .groups = "drop")

y_act <- flows %>%
  filter(category %in% c("A", "B"), !is.na(deregistrations)) %>%
  transmute(month, category, exits = as.numeric(deregistrations))

alphas <- read.csv(file.path(exp, "quota_calibration_alpha.csv"), stringsAsFactors = FALSE)

lookup_exit <- function(cat, m, model) {
  if (m >= TEST_START) {
    hit <- two$category == cat & two$month == m & two$model == model
    if (any(hit)) return(two$predicted[hit][[1]])
  }
  hit <- y_act$category == cat & y_act$month == m
  if (any(hit)) y_act$exits[hit][[1]] else NA_real_
}

trailing_12 <- function(cat, end_m, model) {
  months <- add_months(end_m, -11:0)
  sum(vapply(months, function(m) lookup_exit(cat, m, model), numeric(1)), na.rm = TRUE)
}

test_months <- sort(unique(two$month[two$model == "core_forecast"]))
label_of <- c(
  core_forecast = "Core supply forecast",
  momentum_forecast = "Momentum supply forecast",
  simple_average = "Simple 12-month average"
)
out <- list()
for (model in c("core_forecast", "momentum_forecast", "simple_average")) {
  for (tm in test_months) {
    for (cat in c("A", "B")) {
      a <- alphas$alpha[alphas$category == cat][[1]]
      d12 <- trailing_12(cat, tm, model)
      p <- pub$published[pub$month == tm & pub$category == cat]
      out[[length(out) + 1]] <- data.frame(
        month = as.character(as.Date(tm)),
        category = cat,
        model = model,
        label = unname(label_of[[model]]),
        published = if (length(p)) p[[1]] else NA_real_,
        outlook = a * d12,
        stringsAsFactors = FALSE
      )
    }
  }
}
outlook <- bind_rows(out)
write.csv(outlook, file.path(exp, "two_models_quota_outlook.csv"), row.names = FALSE)

scores <- outlook %>%
  filter(!is.na(published)) %>%
  group_by(model, label, category) %>%
  summarise(
    MAE = mean(abs(outlook - published)),
    MAPE = mean(abs(outlook - published) / pmax(published, 1)),
    .groups = "drop"
  )
write.csv(scores, file.path(exp, "two_models_quota_scores.csv"), row.names = FALSE)
print(scores)
cat("Wrote two_models_quota_outlook.csv\n")
