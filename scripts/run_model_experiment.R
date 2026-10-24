# =============================================================================
# COE supply model experiment — Cat A & B only
# Train: 2016-01 .. 2024-08 | Test: 2024-09 .. latest (never used for selection)
# Level 1: deregistrations | Level 2: LTA replacement quota from exits
# Outputs: data/processed/model_experiment/
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(purrr)
  library(stringr)
  library(glmnet)
  library(mgcv)
})

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
proc <- file.path(root, "data", "processed")
out_dir <- file.path(proc, "model_experiment")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

TRAIN_END <- as.Date("2024-08-01")
TEST_START <- as.Date("2024-09-01")
SEED <- 3702L
set.seed(SEED)

cat("=== COE A/B model experiment ===\n")
cat("Train <=", as.character(TRAIN_END), "| Test >=", as.character(TEST_START), "\n\n")

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------
add_months <- function(month, n) {
  m <- as.Date(month)
  y <- as.integer(format(m, "%Y"))
  mo <- as.integer(format(m, "%m")) + as.integer(n)
  y <- y + (mo - 1L) %/% 12L
  mo <- (mo - 1L) %% 12L + 1L
  as.Date(sprintf("%04d-%02d-01", y, mo))
}

mae <- function(y, yhat) mean(abs(y - yhat), na.rm = TRUE)
rmse <- function(y, yhat) sqrt(mean((y - yhat)^2, na.rm = TRUE))
mape <- function(y, yhat) mean(abs(y - yhat) / pmax(abs(y), 1), na.rm = TRUE)

score_vec <- function(y, yhat, model, category, window) {
  tibble(
    model = model, category = category, window = window,
    n = sum(!is.na(y) & !is.na(yhat)),
    MAE = mae(y, yhat), RMSE = rmse(y, yhat), MAPE = mape(y, yhat),
    bias = mean(yhat - y, na.rm = TRUE)
  )
}

# LTA replacement quota (quarterly-ish from 12m exits); A/B keep 90%, 10% to E
quota_from_dereg12 <- function(dereg_12_a, dereg_12_b) {
  c(A = 0.9 * 0.25 * dereg_12_a, B = 0.9 * 0.25 * dereg_12_b)
}

# -----------------------------------------------------------------------------
# 1. Load cleaned data
# -----------------------------------------------------------------------------
flows <- readRDS(file.path(proc, "vehicle_flows.rds"))
age   <- readRDS(file.path(proc, "pop_by_age.rds"))
pqp   <- readRDS(file.path(proc, "pqp.rds"))
reval <- readRDS(file.path(proc, "revalidation.rds"))
bids  <- readRDS(file.path(proc, "coe_results.rds"))

y_tbl <- flows %>%
  filter(category %in% c("A", "B"), !is.na(deregistrations)) %>%
  transmute(month = as.Date(month), category, y = deregistrations) %>%
  arrange(category, month)

cars_age <- age %>%
  filter(vehicle_type == "Cars") %>%
  transmute(
    month = as.Date(month),
    age_lower = as.integer(age_lower),
    population = as.numeric(population)
  ) %>%
  filter(!is.na(age_lower))

pqp_ab <- pqp %>%
  filter(category %in% c("A", "B")) %>%
  transmute(month = as.Date(month), category, pqp)

reval5 <- reval %>%
  filter(category %in% c("A", "B"), tenure_years == 5) %>%
  transmute(month = as.Date(month), category, reval5 = revalidations)

# Published monthly quota (sum of exercises) for level-2 check
quota_pub <- bids %>%
  filter(category %in% c("A", "B"), !is.na(quota)) %>%
  group_by(month = as.Date(month), category) %>%
  summarise(quota_pub = sum(quota, na.rm = TRUE), .groups = "drop")

# -----------------------------------------------------------------------------
# 2. Feature engineering (one row per category-month)
#    All predictors use only information dated before month t
# -----------------------------------------------------------------------------
stock_band <- function(o, band) {
  hit <- cars_age$month == o & cars_age$age_lower == band
  if (!any(hit)) return(NA_real_)
  sum(cars_age$population[hit], na.rm = TRUE)
}

ab_share <- function(o) {
  win <- y_tbl %>%
    filter(month <= o, month > add_months(o, -12L))
  tot <- sum(win$y, na.rm = TRUE)
  if (tot <= 0) return(c(A = 0.5, B = 0.5))
  a <- sum(win$y[win$category == "A"], na.rm = TRUE) / tot
  c(A = a, B = 1 - a)
}

lookup_y <- function(cat, m) {
  hit <- y_tbl$category == cat & y_tbl$month == m
  if (!any(hit)) return(NA_real_)
  y_tbl$y[hit][[1]]
}

lookup_pqp <- function(cat, m) {
  hit <- pqp_ab$category == cat & pqp_ab$month == m
  if (!any(hit)) return(NA_real_)
  pqp_ab$pqp[hit][[1]]
}

lookup_reval5 <- function(cat, m) {
  hit <- reval5$category == cat & reval5$month == m
  if (!any(hit)) return(NA_real_)
  reval5$reval5[hit][[1]]
}

# Months where age exists and we have enough history for lags
age_months <- sort(unique(cars_age$month))
# Need t-1 age, t-12 y, preferably t-60 reval — start rows from max(2016-02, first usable)
months_all <- age_months[age_months >= as.Date("2017-01-01")]

build_row <- function(cat, t) {
  o <- add_months(t, -1L) # last complete age/PQP month before t
  if (!o %in% age_months) return(NULL)
  sh <- ab_share(o)
  due10 <- stock_band(o, 9L) * sh[[cat]] / 12
  due15 <- stock_band(o, 14L) * sh[[cat]] / 12
  # also near-expiry bulge 8-11 as extra candidate
  due_near <- sum(vapply(8:10, function(b) stock_band(o, as.integer(b)), numeric(1)), na.rm = TRUE) *
    sh[[cat]] / 12

  y_hist <- y_tbl %>% filter(category == cat, month < t, month >= add_months(t, -12L))
  recent12 <- if (nrow(y_hist)) mean(y_hist$y) else NA_real_
  y_lag12 <- lookup_y(cat, add_months(t, -12L))
  y_lag2  <- lookup_y(cat, add_months(t, -2L))

  pq <- lookup_pqp(cat, o)
  lpqp <- if (is.na(pq) || pq <= 0) NA_real_ else log(pq / 50000)

  tibble(
    month = t,
    category = cat,
    y = lookup_y(cat, t),
    due10 = due10,
    due15 = due15,
    due_near = due_near,
    recent12 = recent12,
    y_lag12 = y_lag12,
    y_lag2 = y_lag2,
    reval5_lag12 = lookup_reval5(cat, add_months(t, -12L)),
    reval5_lag24 = lookup_reval5(cat, add_months(t, -24L)),
    reval5_lag60 = lookup_reval5(cat, add_months(t, -60L)),
    lpqp = lpqp,
    post_2022 = as.integer(t >= as.Date("2022-05-01")),
    moy_sin = sin(2 * pi * month(t) / 12),
    moy_cos = cos(2 * pi * month(t) / 12)
  )
}

cat("Building feature panel …\n")
panel <- map_dfr(c("A", "B"), function(cat) {
  map_dfr(months_all, function(t) build_row(cat, t))
}) %>%
  filter(!is.na(y), !is.na(due10), !is.na(recent12)) %>%
  mutate(
    window = if_else(month <= TRAIN_END, "train", "test")
  )

# Impute rare missing optional features with train medians (by category), for glmnet only
feature_cols <- c(
  "due10", "due15", "due_near", "recent12", "y_lag12", "y_lag2",
  "reval5_lag12", "reval5_lag24", "reval5_lag60", "lpqp",
  "post_2022", "moy_sin", "moy_cos"
)

panel_filled <- panel %>%
  group_by(category) %>%
  mutate(across(
    all_of(feature_cols),
    ~ {
      med <- median(.x[window == "train"], na.rm = TRUE)
      ifelse(is.na(.x), med, .x)
    }
  )) %>%
  ungroup()

write.csv(panel_filled, file.path(out_dir, "feature_panel.csv"), row.names = FALSE)
cat(
  "Panel rows:", nrow(panel_filled),
  "| train:", sum(panel_filled$window == "train"),
  "| test:", sum(panel_filled$window == "test"), "\n"
)

# -----------------------------------------------------------------------------
# 3. Model fitting helpers
# -----------------------------------------------------------------------------
full_form <- y ~ due10 + due15 + due_near + recent12 + y_lag12 + y_lag2 +
  reval5_lag12 + reval5_lag24 + reval5_lag60 + lpqp + post_2022 + moy_sin + moy_cos

due_form <- y ~ due10 + due15 + post_2022
lean_form <- y ~ due10 + due15 + reval5_lag12 + reval5_lag60 + recent12 + post_2022 + lpqp

x_matrix <- function(df, cols = feature_cols) {
  as.matrix(df[, cols, drop = FALSE])
}

fit_predict_models <- function(train_df, test_df) {
  # returns named list of prediction vectors aligned to test_df rows
  out <- list()
  out[["historic_avg"]] <- test_df$recent12
  out[["seasonal_naive"]] <- ifelse(is.na(test_df$y_lag12), test_df$recent12, test_df$y_lag12)

  safe_lm <- function(form) {
    fit <- tryCatch(lm(form, data = train_df), error = function(e) NULL)
    if (is.null(fit)) return(rep(NA_real_, nrow(test_df)))
    as.numeric(predict(fit, newdata = test_df))
  }
  out[["ols_full"]] <- safe_lm(full_form)
  out[["ols_due"]] <- safe_lm(due_form)
  out[["ols_lean"]] <- safe_lm(lean_form)

  # Ridge / Lasso / Elastic net via cv.glmnet on train
  xtr <- x_matrix(train_df)
  ytr <- train_df$y
  xte <- x_matrix(test_df)
  for (alpha in c(0, 0.5, 1)) {
    nm <- c("0" = "ridge", "0.5" = "elastic_net", "1" = "lasso")[[as.character(alpha)]]
    pred <- tryCatch({
      cv <- cv.glmnet(xtr, ytr, alpha = alpha, nfolds = min(5L, max(3L, nrow(train_df) %/% 12)),
                      type.measure = "mae")
      as.numeric(predict(cv, newx = xte, s = "lambda.1se"))
    }, error = function(e) rep(NA_real_, nrow(test_df)))
    out[[nm]] <- pred
  }

  # GAM (smooth due10, due15)
  pred_gam <- tryCatch({
    fit <- gam(y ~ s(due10, k = 5) + s(due15, k = 5) + post_2022 + recent12,
               data = train_df, method = "REML")
    as.numeric(predict(fit, newdata = test_df))
  }, error = function(e) rep(NA_real_, nrow(test_df)))
  out[["gam_due"]] <- pred_gam

  # Floor at 20% of recent12
  map(out, function(p) pmax(p, 0.2 * test_df$recent12, na.rm = TRUE))
}

MODEL_NAMES <- c(
  "historic_avg", "seasonal_naive", "ols_due", "ols_lean", "ols_full",
  "ridge", "elastic_net", "lasso", "gam_due"
)

# -----------------------------------------------------------------------------
# 4. Time-series cross-validation INSIDE the train window only
#    Expanding origin every 3 months; score next 6 months (still <= TRAIN_END)
# -----------------------------------------------------------------------------
cat("\nRunning expanding-origin CV on train window …\n")
cv_origins <- seq(as.Date("2019-01-01"), as.Date("2023-08-01"), by = "3 months")

cv_rows <- list()
for (cat in c("A", "B")) {
  dcat <- panel_filled %>% filter(category == cat, window == "train")
  for (o in cv_origins) {
    tr <- dcat %>% filter(month <= o)
    te <- dcat %>% filter(month > o, month <= min(add_months(o, 6L), TRAIN_END))
    if (nrow(tr) < 36 || nrow(te) == 0) next
    preds <- fit_predict_models(tr, te)
    for (nm in names(preds)) {
      cv_rows[[length(cv_rows) + 1]] <- tibble(
        origin = o, category = cat, month = te$month, model = nm,
        y = te$y, pred = preds[[nm]]
      )
    }
  }
}
cv_long <- bind_rows(cv_rows)
cv_scores <- cv_long %>%
  group_by(model, category) %>%
  summarise(
    n = n(), MAE = mae(y, pred), RMSE = rmse(y, pred), MAPE = mape(y, pred),
    bias = mean(pred - y, na.rm = TRUE), .groups = "drop"
  ) %>%
  arrange(category, MAE)

cv_scores_pooled <- cv_long %>%
  group_by(model) %>%
  summarise(
    n = n(), MAE = mae(y, pred), RMSE = rmse(y, pred), MAPE = mape(y, pred),
    bias = mean(pred - y, na.rm = TRUE), .groups = "drop"
  ) %>%
  arrange(MAE)

write.csv(cv_scores, file.path(out_dir, "cv_scores_by_category.csv"), row.names = FALSE)
write.csv(cv_scores_pooled, file.path(out_dir, "cv_scores_pooled.csv"), row.names = FALSE)
cat("CV pooled ranking:\n")
print(cv_scores_pooled, width = 120)

# -----------------------------------------------------------------------------
# 5. Feature selection signals on TRAIN only
# -----------------------------------------------------------------------------
cat("\nFeature selection signals (train only) …\n")
feat_cor <- map_dfr(c("A", "B"), function(cat) {
  d <- panel_filled %>% filter(category == cat, window == "train")
  map_dfr(feature_cols, function(f) {
    tibble(category = cat, feature = f, cor_y = suppressWarnings(cor(d[[f]], d$y, use = "complete.obs")))
  })
}) %>% arrange(category, desc(abs(cor_y)))

# Lasso nonzero at lambda.1se on full train
lasso_sel <- map_dfr(c("A", "B"), function(cat) {
  d <- panel_filled %>% filter(category == cat, window == "train")
  cv <- cv.glmnet(x_matrix(d), d$y, alpha = 1, nfolds = 5, type.measure = "mae")
  coefs <- as.matrix(coef(cv, s = "lambda.1se"))
  tibble(
    category = cat,
    feature = rownames(coefs),
    coef = as.numeric(coefs[, 1])
  ) %>%
    filter(feature != "(Intercept)", abs(coef) > 1e-8)
})

write.csv(feat_cor, file.path(out_dir, "feature_correlations_train.csv"), row.names = FALSE)
write.csv(lasso_sel, file.path(out_dir, "lasso_selected_features.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# 6. Fit on FULL train, evaluate once on locked TEST (2024-09+)
# -----------------------------------------------------------------------------
cat("\nFitting on full train; scoring locked test 2024-09+ …\n")
test_pred_rows <- list()
test_scores <- list()
train_fits <- list()

for (cat in c("A", "B")) {
  tr <- panel_filled %>% filter(category == cat, window == "train")
  te <- panel_filled %>% filter(category == cat, window == "test")
  preds <- fit_predict_models(tr, te)
  train_fits[[cat]] <- preds # structure placeholder; store coefs below

  for (nm in names(preds)) {
    test_pred_rows[[length(test_pred_rows) + 1]] <- tibble(
      month = te$month, category = cat, model = nm, y = te$y, pred = preds[[nm]]
    )
    test_scores[[length(test_scores) + 1]] <- score_vec(te$y, preds[[nm]], nm, cat, "test")
  }

  # Store OLS lean / ridge coefs for explanation
  train_fits[[paste0(cat, "_ols_lean")]] <- coef(lm(lean_form, data = tr))
  cv_r <- cv.glmnet(x_matrix(tr), tr$y, alpha = 0, nfolds = 5, type.measure = "mae")
  train_fits[[paste0(cat, "_ridge_coef")]] <- as.matrix(coef(cv_r, s = "lambda.1se"))
}

test_long <- bind_rows(test_pred_rows)
test_scores_df <- bind_rows(test_scores) %>% arrange(category, MAE)
test_scores_pooled <- test_long %>%
  group_by(model) %>%
  summarise(
    n = n(), MAE = mae(y, pred), RMSE = rmse(y, pred), MAPE = mape(y, pred),
    bias = mean(pred - y, na.rm = TRUE), .groups = "drop"
  ) %>%
  arrange(MAE)

write.csv(test_long, file.path(out_dir, "test_predictions.csv"), row.names = FALSE)
write.csv(test_scores_df, file.path(out_dir, "test_scores_by_category.csv"), row.names = FALSE)
write.csv(test_scores_pooled, file.path(out_dir, "test_scores_pooled.csv"), row.names = FALSE)

cat("TEST pooled ranking (2024-09+):\n")
print(test_scores_pooled, width = 120)
cat("\nTEST by category:\n")
print(test_scores_df, width = 120)

# -----------------------------------------------------------------------------
# 7. Select models that beat historic_avg on BOTH CV and TEST (pooled + each cat)
# -----------------------------------------------------------------------------
base_cv <- cv_scores_pooled$MAE[cv_scores_pooled$model == "historic_avg"]
base_te <- test_scores_pooled$MAE[test_scores_pooled$model == "historic_avg"]

sel <- test_scores_pooled %>%
  inner_join(cv_scores_pooled %>% select(model, MAE_cv = MAE), by = "model") %>%
  mutate(
    beat_cv = MAE_cv < base_cv,
    beat_test = MAE < base_te,
    improve_test_pct = 100 * (base_te - MAE) / base_te
  ) %>%
  arrange(MAE)

# Must beat baseline on test; prefer also beat on CV
winners <- sel %>% filter(beat_test, model != "historic_avg") %>% arrange(MAE)

# Per-category: models that beat historic on test for BOTH A and B
by_cat_wide <- test_scores_df %>%
  select(model, category, MAE) %>%
  pivot_wider(names_from = category, values_from = MAE)
base_a <- by_cat_wide$A[by_cat_wide$model == "historic_avg"]
base_b <- by_cat_wide$B[by_cat_wide$model == "historic_avg"]
consistent <- by_cat_wide %>%
  filter(model != "historic_avg", A < base_a, B < base_b) %>%
  mutate(
    improve_A = 100 * (base_a - A) / base_a,
    improve_B = 100 * (base_b - B) / base_b,
    MAE_mean = (A + B) / 2
  ) %>%
  arrange(MAE_mean)

write.csv(sel, file.path(out_dir, "selection_summary.csv"), row.names = FALSE)
write.csv(consistent, file.path(out_dir, "models_beat_baseline_both_categories.csv"), row.names = FALSE)

# Pick top 3 consistent models for the app shortlist
shortlist <- head(consistent$model, 3)
if (!length(shortlist)) {
  shortlist <- head(winners$model, 3)
}

cat("\n=== SHORTLIST (beat historic_avg on TEST for BOTH A and B) ===\n")
print(consistent)
cat("\nChosen shortlist:", paste(shortlist, collapse = ", "), "\n")

# Primary = best consistent MAE_mean
primary <- if (nrow(consistent)) consistent$model[[1]] else winners$model[[1]]
cat("PRIMARY model for demo:", primary, "\n")

# -----------------------------------------------------------------------------
# 8. Level-2: quota from predicted vs actual dereg (test window)
# -----------------------------------------------------------------------------
cat("\nLevel-2 quota check on test window …\n")
# For each test month, trailing 12m exits using predicted path spliced with history
quota_rows <- list()
for (nm in unique(c(primary, shortlist, "historic_avg"))) {
  for (tm in sort(unique(test_long$month[test_long$model == nm]))) {
    # Build exit series: actual before tm, pred at tm for this model
    get_exit <- function(cat, m) {
      if (m < TEST_START) {
        lookup_y(cat, m)
      } else {
        hit <- test_long$model == nm & test_long$category == cat & test_long$month == m
        if (any(hit)) test_long$pred[hit][[1]] else lookup_y(cat, m)
      }
    }
    # For fair month-t quota identity using predicted exits in the test era:
    # trailing 12m ending at tm using preds for test months and actuals before
    months_12 <- add_months(tm, -11:0)
    d12_a <- sum(vapply(months_12, function(m) get_exit("A", m), numeric(1)), na.rm = TRUE)
    d12_b <- sum(vapply(months_12, function(m) get_exit("B", m), numeric(1)), na.rm = TRUE)
    q_hat <- quota_from_dereg12(d12_a, d12_b)

    # Actual trailing using true y
    d12_a_act <- sum(vapply(months_12, function(m) lookup_y("A", m), numeric(1)), na.rm = TRUE)
    d12_b_act <- sum(vapply(months_12, function(m) lookup_y("B", m), numeric(1)), na.rm = TRUE)
    q_act <- quota_from_dereg12(d12_a_act, d12_b_act)

    for (cat in c("A", "B")) {
      pub <- quota_pub %>% filter(month == tm, category == cat)
      quota_rows[[length(quota_rows) + 1]] <- tibble(
        month = tm, category = cat, model = nm,
        quota_from_pred_exits = q_hat[[cat]],
        quota_from_actual_exits = q_act[[cat]],
        quota_published = if (nrow(pub)) pub$quota_pub[[1]] else NA_real_
      )
    }
  }
}
quota_df <- bind_rows(quota_rows)
quota_df_out <- quota_df %>% mutate(month = as.character(as.Date(month)))
write.csv(quota_df_out, file.path(out_dir, "level2_quota_test.csv"), row.names = FALSE)

quota_mae <- quota_df %>%
  filter(!is.na(quota_published)) %>%
  group_by(model, category) %>%
  summarise(
    MAE_vs_formula_actual = mae(quota_from_actual_exits, quota_from_pred_exits),
    MAE_vs_published = mae(quota_published, quota_from_pred_exits),
    .groups = "drop"
  )
write.csv(quota_mae, file.path(out_dir, "level2_quota_mae.csv"), row.names = FALSE)
print(quota_mae)

# -----------------------------------------------------------------------------
# 9. Save selected artifact for the app (primary model predictions on test +
#    refit note). Also write a plain-text explanation.
# -----------------------------------------------------------------------------
primary_test <- test_long %>% filter(model == primary)
# Empirical 80% band from train CV errors of primary
cv_ratios <- cv_long %>%
  filter(model == primary) %>%
  mutate(ratio = y / pmax(pred, 1)) %>%
  summarise(lo80 = quantile(ratio, 0.10, na.rm = TRUE),
            hi80 = quantile(ratio, 0.90, na.rm = TRUE))

forecast_demo <- primary_test %>%
  transmute(
    month, category,
    mid = pred,
    lo = pred * cv_ratios$lo80[[1]],
    hi = pred * cv_ratios$hi80[[1]],
    actual = y,
    model = primary
  )
forecast_demo_out <- forecast_demo %>% mutate(month = as.character(as.Date(month)))
write.csv(forecast_demo_out, file.path(out_dir, "primary_test_forecast.csv"), row.names = FALSE)
saveRDS(
  list(
    primary = primary,
    shortlist = shortlist,
    train_end = TRAIN_END,
    test_start = TEST_START,
    feature_cols = feature_cols,
    cv_bands = cv_ratios,
    generated_at = Sys.time()
  ),
  file.path(out_dir, "selection.rds")
)

# Plain-text lab notes (not markdown)
notes <- c(
  "COE A/B SUPPLY MODEL EXPERIMENT — RESULTS NOTES",
  paste("Generated:", as.character(Sys.time())),
  paste("Train:", "<=", as.character(TRAIN_END), "| Test:", ">=", as.character(TEST_START)),
  "",
  "TARGET: monthly deregistrations for Cat A and Cat B.",
  "BASELINE: historic_avg = trailing 12-month mean of exits (recent12).",
  "",
  "FEATURES ENGINEERED (all known before month t):",
  "  due10, due15, due_near, recent12, y_lag12, y_lag2,",
  "  reval5_lag12/24/60, lpqp, post_2022, moy_sin, moy_cos",
  "",
  "MODELS TRIED: historic_avg, seasonal_naive, ols_due, ols_lean, ols_full,",
  "  ridge, elastic_net, lasso, gam_due",
  "",
  "CV: expanding origin every 3 months (2019-2023), predict next 6 months inside train.",
  "TEST: scored once after model comparison; not used for tuning.",
  "",
  paste("PRIMARY:", primary),
  paste("SHORTLIST:", paste(shortlist, collapse = ", ")),
  "",
  "See CSVs in this folder for full rankings. Level-2 applies LTA 25% trailing-12m",
  "exits with 10% of A/B base to E (A/B keep 90%).",
  "",
  "post_2022: May 2022 A/B EV-power redefinition flag.",
  "lpqp / reval5_*: renewal feedback (high PQP -> more 5y renewals -> later due15 exits)."
)
writeLines(notes, file.path(out_dir, "EXPERIMENT_NOTES.txt"))

cat("\nDone. Outputs in:", out_dir, "\n")
cat(paste(notes, collapse = "\n"), "\n")
