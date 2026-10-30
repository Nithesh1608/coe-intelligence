# Rebuild every model artefact the app reads, end to end:
#   1. feature panel from the processed RDS tables
#   2. evaluation fit (train <= 2024-08) scored one-step on 2024-09 onward
#   3. calibrated alpha and the held-out quota outlook
#   4. recursive 24-month backtest from the training end with bootstrap bands
#   5. production fit on all observed months, rolled 36 months forward
#   6. renewal response and the scenario presets
#
# Run from the repo root:  Rscript scripts/fit_and_forecast.R
# Needs: glmnet, box (everything else is base R).

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(root, "rhino.yml"))) stop("Run from the repo root.")
options(box.path = root)

box::use(
  app/logic/model/features[
    CATS, BASE_COLS, CORE_COLS, add_months, quarter_start, build_feature_panel, impute_panel
  ],
  app/logic/model/forecast[
    TRAIN_END, TEST_START, fit_enet, predict_enet, enet_coefficients, one_step_eval,
    published_monthly_quota, trailing12, calibrate_alpha, quota_from_dereg, quota_bands,
    mapping_residuals, roll_forward, band_summary, forward_inputs, recursive_backtest, calibration_table,
    bootstrap_fits, pqp_paths
  ],
  app/logic/model/scenarios[
    renewal_response, scenario_presets, apply_scenario, rollup_quarters, injection_split, brought_forward
  ],
)

proc <- file.path(root, "data", "processed")
exp <- file.path(proc, "model_experiment")
dir.create(exp, showWarnings = FALSE, recursive = TRUE)
# Model specification: restore the desk recipe that won one-step held-out quota
# MAPE (due-stock + short memory + revals, lambda.1se, train from 2018). Keep the
# realignment scaffolding: recursive bootstrap bands, scenarios, forward path.
PRODUCTION_START <- as.Date("2018-01-01")
MODEL_COLS <- CORE_COLS
LAMBDA <- "lambda.1se"
FORWARD_MONTHS <- 40L  # 12 full quarters from any starting month
N_SIMS <- 150L
N_REFITS <- 25L   # parameter-uncertainty refits on block-resampled training rows
set.seed(3702L)

rds <- function(name) {
  x <- readRDS(file.path(proc, name))
  if ("month" %in% names(x)) x$month <- as.Date(x$month)
  if ("category" %in% names(x)) x$category <- as.character(x$category)
  x
}
write_out <- function(df, name) {
  if ("month" %in% names(df)) df$month <- as.character(as.Date(df$month))
  if ("quarter" %in% names(df)) df$quarter <- as.character(as.Date(df$quarter))
  utils::write.csv(df, file.path(exp, name), row.names = FALSE)
  cat("  wrote", name, "(", nrow(df), "rows )\n")
}

flows <- rds("vehicle_flows.rds")
pop_by_age <- rds("pop_by_age.rds")
revalidation <- rds("revalidation.rds")
pqp <- rds("pqp.rds")
coe_results <- rds("coe_results.rds")
renewal_rate <- rds("renewal_rate.rds")

# ---- 1. Feature panel --------------------------------------------------------
cat("1. Feature panel\n")
panel <- build_feature_panel(flows, pop_by_age, revalidation, pqp)
panel <- panel[panel$month >= PRODUCTION_START, ]
panel <- impute_panel(panel, TRAIN_END, cols = union(BASE_COLS, "cohort"))
write_out(panel, "feature_panel.csv")
write_out(data.frame(
  key = c("predictors", "lambda_rule", "alpha_mixing", "loss", "train_start", "train_end", "test_start",
          "forward_months", "bootstrap_sims", "bootstrap_block", "bootstrap_refits"),
  value = c(paste(MODEL_COLS, collapse = " "), LAMBDA, "0.5", "mae", format(PRODUCTION_START),
            format(TRAIN_END), format(TEST_START), as.character(FORWARD_MONTHS),
            as.character(N_SIMS), "3", as.character(N_REFITS)),
  stringsAsFactors = FALSE
), "model_spec.csv")
last_obs <- max(panel$month)
cat("  panel:", format(min(panel$month)), "to", format(last_obs), "\n")

# ---- 2. Evaluation fit, one-step held-out ------------------------------------
cat("2. Evaluation fit (train <=", format(TRAIN_END), ")\n")
ev <- one_step_eval(panel, TRAIN_END, TEST_START, cols = MODEL_COLS, lambda = LAMBDA)
monthly <- ev$monthly
write_out(monthly, "two_models_monthly.csv")

period_of <- function(m) {
  m <- as.Date(m)
  y <- as.integer(format(m, "%Y"))
  last_y <- as.integer(format(last_obs, "%Y"))
  first_y <- as.integer(format(TEST_START, "%Y"))
  ifelse(y == first_y, sprintf("Late %d (%s to December)", first_y, format(TEST_START, "%B")),
    ifelse(y == last_y, sprintf("%d so far (January to %s)", last_y, format(last_obs, "%B")),
      sprintf("Full year %d", y)))
}
monthly$period <- period_of(monthly$month)
roll <- do.call(rbind, lapply(split(monthly, list(monthly$category, monthly$period, monthly$model), drop = TRUE), function(d) {
  data.frame(category = d$category[1], period = d$period[1], model = d$model[1], label = d$label[1],
             months = nrow(d), actual = sum(d$actual), predicted = sum(d$predicted),
             gap = sum(d$gap), pct_off = round(100 * sum(d$gap) / max(sum(d$actual), 1), 1),
             stringsAsFactors = FALSE)
}))
roll <- roll[order(roll$category, roll$period, roll$model), ]
rownames(roll) <- NULL
write_out(roll, "two_models_rollup.csv")

hit <- do.call(rbind, lapply(split(monthly, list(monthly$model, monthly$category), drop = TRUE), function(d) {
  pct <- abs(d$predicted - d$actual) / pmax(d$actual, 1)
  data.frame(model = d$model[1], label = d$label[1], category = d$category[1], months = nrow(d),
             within_10pct = mean(pct <= 0.10), within_15pct = mean(pct <= 0.15), stringsAsFactors = FALSE)
}))
rownames(hit) <- NULL
write_out(hit, "two_models_hit_rates.csv")

coefs <- do.call(rbind, lapply(CATS, function(cat) enet_coefficients(ev$fits[[cat]], cat)))
write_out(coefs, "enet_coefficients_eval.csv")

# ---- 3. Alpha and held-out quota outlook -------------------------------------
cat("3. Quota calibration\n")
alphas <- calibrate_alpha(flows, coe_results, PRODUCTION_START, TRAIN_END)
write_out(alphas, "quota_calibration_alpha.csv")
print(alphas)

pub <- published_monthly_quota(coe_results)
y_series <- lapply(CATS, function(cat) {
  fc <- flows[flows$category == cat & !is.na(flows$deregistrations), ]
  stats::setNames(as.numeric(fc$deregistrations), as.character(fc$month))
})
names(y_series) <- CATS

test_months <- sort(unique(monthly$month))
outlook_rows <- list()
two_rows <- list()
for (cat in CATS) {
  a <- alphas$alpha[alphas$category == cat]
  for (model in c("core_forecast", "simple_average")) {
    pred <- monthly[monthly$category == cat & monthly$model == model, ]
    spliced <- y_series[[cat]]
    spliced[as.character(pred$month)] <- pred$predicted
    for (tm in as.list(test_months)) {
      d12_pred <- trailing12(spliced, tm)
      d12_act <- trailing12(y_series[[cat]], tm)
      p <- pub$published[pub$category == cat & pub$month == tm]
      p <- if (length(p)) p[[1]] else NA_real_
      two_rows[[length(two_rows) + 1L]] <- data.frame(
        month = tm, category = cat, model = model, label = pred$label[1],
        published = p, outlook = a * d12_pred, stringsAsFactors = FALSE
      )
      if (model == "core_forecast") {
        outlook_rows[[length(outlook_rows) + 1L]] <- data.frame(
          month = tm, category = cat, published = p,
          outlook_from_forecast = a * d12_pred, outlook_from_actual_exits = a * d12_act,
          alpha = a, d12_pred = d12_pred, d12_act = d12_act, stringsAsFactors = FALSE
        )
      }
    }
  }
}
two_quota <- do.call(rbind, two_rows)
two_quota$label <- ifelse(two_quota$model == "core_forecast", "Elastic net", "Simple 12-month average")
write_out(two_quota, "two_models_quota_outlook.csv")
outlook <- do.call(rbind, outlook_rows)
write_out(outlook, "quota_outlook_calibrated.csv")

score <- function(d, pred_col) {
  d <- d[is.finite(d$published), ]
  data.frame(MAE = mean(abs(d[[pred_col]] - d$published)),
             MAPE = mean(abs(d[[pred_col]] - d$published) / pmax(d$published, 1)))
}
two_scores <- do.call(rbind, lapply(split(two_quota, list(two_quota$model, two_quota$category), drop = TRUE), function(d) {
  cbind(data.frame(model = d$model[1], label = d$label[1], category = d$category[1], stringsAsFactors = FALSE),
        score(d, "outlook"))
}))
rownames(two_scores) <- NULL
write_out(two_scores, "two_models_quota_scores.csv")
q_scores <- do.call(rbind, lapply(split(outlook, outlook$category), function(d) {
  s <- score(d, "outlook_from_forecast")
  data.frame(category = d$category[1], n = sum(is.finite(d$published)), MAE = s$MAE, MAPE = s$MAPE,
             MAE_oracle = score(d, "outlook_from_actual_exits")$MAE, stringsAsFactors = FALSE)
}))
rownames(q_scores) <- NULL
write_out(q_scores, "quota_outlook_scores.csv")
print(two_scores)

# Relative one-step residuals (actual / predicted - 1) feed the bootstrap.
resid_by_cat <- lapply(CATS, function(cat) {
  d <- monthly[monthly$category == cat & monthly$model == "core_forecast", ]
  d$actual / pmax(d$predicted, 1) - 1
})
names(resid_by_cat) <- CATS

# Quota-mapping residuals (LTA discretion over the formula) on the training window.
map_resid <- mapping_residuals(flows, coe_results, alphas, PRODUCTION_START, TRAIN_END)
cat(sprintf("  mapping residual sd: A %.3f, B %.3f (n = %d, %d)\n",
            stats::sd(map_resid$A), stats::sd(map_resid$B), length(map_resid$A), length(map_resid$B)))

# ---- 4. Recursive backtest ---------------------------------------------------
cat("4. Recursive backtest from", format(TRAIN_END), "\n")
eval_boot <- lapply(CATS, function(cat) {
  bootstrap_fits(panel[panel$category == cat & panel$month <= TRAIN_END, ], MODEL_COLS,
                 lambda = LAMBDA, n_fits = N_REFITS)
})
names(eval_boot) <- CATS
bt <- recursive_backtest(ev$fits, panel, flows, pop_by_age, revalidation, pqp, coe_results,
                         alphas, origin = TRAIN_END, horizon = 24L,
                         resid_by_cat = resid_by_cat, n_sims = N_SIMS, cols = MODEL_COLS,
                         map_resid = map_resid, fits_boot = eval_boot)
write_out(bt, "backtest_recursive.csv")
cal <- calibration_table(bt)
write_out(cal, "backtest_calibration.csv")
print(cal, digits = 3)

# ---- 5. Production fit and forward path --------------------------------------
cat("5. Production fit (all months through", format(last_obs), ") and", FORWARD_MONTHS, "months forward\n")
prod_fits <- lapply(CATS, function(cat) fit_enet(panel[panel$category == cat, ], MODEL_COLS, lambda = LAMBDA))
names(prod_fits) <- CATS
prod_boot <- lapply(CATS, function(cat) bootstrap_fits(panel[panel$category == cat, ], MODEL_COLS, lambda = LAMBDA, n_fits = N_REFITS))
names(prod_boot) <- CATS
prod_coefs <- do.call(rbind, lapply(CATS, function(cat) enet_coefficients(prod_fits[[cat]], cat)))
write_out(prod_coefs, "enet_coefficients_production.csv")

inp <- forward_inputs(flows, pop_by_age, revalidation, pqp, last_obs, FORWARD_MONTHS)
fwd_dereg <- list()
fwd_quota <- list()
for (cat in CATS) {
  pc <- inp$per_cat[[cat]]
  imp_cols <- union(MODEL_COLS, "cohort")
  imp <- lapply(imp_cols, function(col) stats::median(panel[[col]][panel$category == cat], na.rm = TRUE))
  names(imp) <- imp_cols
  rf <- roll_forward(prod_fits[[cat]], cat, last_obs, FORWARD_MONTHS, pc$y_hist, inp$stock_at,
                     pc$share, pc$reval5, pc$pqp_base, pc$reg, resid = resid_by_cat[[cat]],
                     n_sims = N_SIMS, impute = imp, fits_boot = prod_boot[[cat]],
                     pqp_sims = pqp_paths(pc$pqp_hist, FORWARD_MONTHS, N_SIMS))
  a <- alphas$alpha[alphas$category == cat]
  db <- band_summary(rf$point, rf$sims)
  fwd_dereg[[cat]] <- data.frame(
    month = rf$months, category = cat, h = seq_len(FORWARD_MONTHS),
    mid = db$mid, lo50 = db$lo50, hi50 = db$hi50, lo80 = db$lo80, hi80 = db$hi80,
    due_flow = rf$due_flow, pqp_base = as.numeric(pc$pqp_base), share = pc$share,
    stringsAsFactors = FALSE
  )
  q_point <- as.numeric(quota_from_dereg(pc$y_hist, rf$months, rf$point, a))
  qb <- band_summary(q_point, quota_bands(pc$y_hist, rf$months, rf$sims, a, map_resid[[cat]]))
  published <- pub$published[match(paste(cat, rf$months), paste(pub$category, pub$month))]
  fwd_quota[[cat]] <- data.frame(
    month = rf$months, category = cat, quarter = quarter_start(rf$months), h = seq_len(FORWARD_MONTHS),
    published = as.numeric(published),
    mid = qb$mid, lo50 = qb$lo50, hi50 = qb$hi50, lo80 = qb$lo80, hi80 = qb$hi80, alpha = a,
    stringsAsFactors = FALSE
  )
}
fwd_dereg <- do.call(rbind, fwd_dereg)
fwd_quota <- do.call(rbind, fwd_quota)
write_out(fwd_dereg, "forward_dereg.csv")
write_out(fwd_quota, "forward_quota.csv")

age_proj <- do.call(rbind, lapply(seq_len(FORWARD_MONTHS), function(h) {
  m <- add_months(last_obs, h)
  row <- inp$stock_at(m)
  data.frame(month = m, age = 0:20, population = row, stringsAsFactors = FALSE)
}))
write_out(age_proj, "forward_age_stock.csv")

# ---- 6. Renewal response and scenario presets --------------------------------
cat("6. Renewal response and scenarios\n")
resp <- renewal_response(renewal_rate)
write_out(resp, "renewal_response.csv")
print(resp, digits = 3)

bf <- brought_forward(pub, y_series, alphas)
cat(sprintf("  certificates published above formula since Feb 2023: %s\n", format(round(bf), big.mark = ",")))
spec <- utils::read.csv(file.path(exp, "model_spec.csv"), stringsAsFactors = FALSE)
spec <- rbind(spec, data.frame(key = "brought_forward_since_2023", value = as.character(round(bf)), stringsAsFactors = FALSE))
spec <- rbind(spec, data.frame(key = "last_observed_month", value = format(last_obs), stringsAsFactors = FALSE))
spec <- rbind(spec, data.frame(key = "mapping_resid_sd_A", value = sprintf("%.3f", stats::sd(map_resid$A)), stringsAsFactors = FALSE))
spec <- rbind(spec, data.frame(key = "mapping_resid_sd_B", value = sprintf("%.3f", stats::sd(map_resid$B)), stringsAsFactors = FALSE))
write_out(spec, "model_spec.csv")

scen_rows <- list()
for (preset in scenario_presets(bf)) {
  split <- injection_split(pub, preset$injection_per_qtr)
  for (cat in CATS) {
    fw <- fwd_dereg[fwd_dereg$category == cat, ]
    m <- apply_scenario(fw, inp$per_cat[[cat]]$y_hist, alphas$alpha[alphas$category == cat],
                        resp[resp$category == cat, ], pqp_target = preset$pqp_target,
                        renewal_shift = preset$renewal_shift, injection_per_qtr = split[[cat]])
    q <- rollup_quarters(m)
    q$scenario_id <- preset$id
    q$scenario <- preset$label
    q$category <- cat
    scen_rows[[length(scen_rows) + 1L]] <- q
  }
}
scen <- do.call(rbind, scen_rows)
scen <- scen[, c("scenario_id", "scenario", "category", "quarter", "months", setdiff(names(scen), c("scenario_id", "scenario", "category", "quarter", "months")))]
write_out(scen, "forward_scenarios.csv")

cat("\nForward quota (quarterly, baseline):\n")
base_q <- scen[scen$scenario_id == "baseline", c("category", "quarter", "quota_base", "quota_base_lo80", "quota_base_hi80")]
base_q[, 3:5] <- round(base_q[, 3:5]); print(base_q, row.names = FALSE)
cat("\nDone.\n")
