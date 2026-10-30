# Rebuild forward path / scenarios / bands from the SHIPPED old feature panel
# and the restored held-out monthly predictions. Does NOT overwrite:
#   two_models_*.csv, quota_outlook_*.csv
#
# Run from repo root with glmnet on .libPaths().

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(root, "rhino.yml"))) stop("Run from the repo root.")
options(box.path = root)

box::use(
  app/logic/model/features[CATS, CORE_COLS, add_months, quarter_start],
  app/logic/model/forecast[
    TRAIN_END, TEST_START, fit_enet, enet_coefficients, published_monthly_quota,
    calibrate_alpha, quota_from_dereg, quota_bands, mapping_residuals,
    roll_forward, band_summary, forward_inputs, recursive_backtest, calibration_table,
    bootstrap_fits, pqp_paths
  ],
  app/logic/model/scenarios[
    renewal_response, scenario_presets, apply_scenario, rollup_quarters,
    brought_forward, injection_split
  ],
)

proc <- file.path(root, "data", "processed")
exp <- file.path(proc, "model_experiment")
MODEL_COLS <- CORE_COLS
LAMBDA <- "lambda.1se"
PRODUCTION_START <- as.Date("2018-01-01")
FORWARD_MONTHS <- 40L
N_SIMS <- 150L
N_REFITS <- 25L
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

panel <- utils::read.csv(file.path(exp, "feature_panel.csv"), stringsAsFactors = FALSE)
panel$month <- as.Date(panel$month)
panel$category <- as.character(panel$category)
need <- c("y", MODEL_COLS)
miss <- setdiff(need, names(panel))
if (length(miss)) stop("feature_panel missing columns: ", paste(miss, collapse = ", "))

for (cat in CATS) {
  idx <- panel$category == cat
  for (col in MODEL_COLS) {
    med <- stats::median(panel[[col]][idx & panel$month <= TRAIN_END], na.rm = TRUE)
    bad <- idx & is.na(panel[[col]])
    if (any(bad)) panel[[col]][bad] <- med
  }
}
panel <- panel[panel$month >= PRODUCTION_START, ]
if (!("cohort" %in% names(panel))) panel$cohort <- pmax(panel$due10 * 12, 1)

monthly <- utils::read.csv(file.path(exp, "two_models_monthly.csv"), stringsAsFactors = FALSE)
monthly$month <- as.Date(monthly$month)
monthly$category <- as.character(monthly$category)

cat("Shipped panel", format(min(panel$month)), "->", format(max(panel$month)),
    "| locked held-out monthly rows:", nrow(monthly), "\n")
cat("Locked quota scores (must stay):\n")
print(utils::read.csv(file.path(exp, "two_models_quota_scores.csv")))

write_out(data.frame(
  key = c("predictors", "lambda_rule", "alpha_mixing", "loss", "train_start", "train_end",
          "test_start", "forward_months", "bootstrap_sims", "bootstrap_block", "bootstrap_refits",
          "eval_source"),
  value = c(paste(MODEL_COLS, collapse = " "), LAMBDA, "0.5", "mae", format(PRODUCTION_START),
            format(TRAIN_END), format(TEST_START), as.character(FORWARD_MONTHS),
            as.character(N_SIMS), "3", as.character(N_REFITS),
            "shipped_artefacts_9d13593"),
  stringsAsFactors = FALSE
), "model_spec.csv")

alphas <- calibrate_alpha(flows, coe_results, PRODUCTION_START, TRAIN_END)
write_out(alphas, "quota_calibration_alpha.csv")
print(alphas)

resid_by_cat <- lapply(CATS, function(cat) {
  d <- monthly[monthly$category == cat & monthly$model == "core_forecast", ]
  d$actual / pmax(d$predicted, 1) - 1
})
names(resid_by_cat) <- CATS
map_resid <- mapping_residuals(flows, coe_results, alphas, PRODUCTION_START, TRAIN_END)

cat("Eval fits for backtest/coefs\n")
eval_fits <- lapply(CATS, function(cat) {
  fit_enet(panel[panel$category == cat & panel$month <= TRAIN_END, ], MODEL_COLS, lambda = LAMBDA)
})
names(eval_fits) <- CATS
write_out(do.call(rbind, lapply(CATS, function(cat) enet_coefficients(eval_fits[[cat]], cat))),
          "enet_coefficients_eval.csv")
eval_boot <- lapply(CATS, function(cat) {
  bootstrap_fits(panel[panel$category == cat & panel$month <= TRAIN_END, ], MODEL_COLS,
                 lambda = LAMBDA, n_fits = N_REFITS)
})
names(eval_boot) <- CATS

cat("Recursive backtest\n")
bt <- recursive_backtest(eval_fits, panel, flows, pop_by_age, revalidation, pqp, coe_results,
                         alphas, origin = TRAIN_END, horizon = 24L,
                         resid_by_cat = resid_by_cat, n_sims = N_SIMS, cols = MODEL_COLS,
                         map_resid = map_resid, fits_boot = eval_boot)
write_out(bt, "backtest_recursive.csv")
write_out(calibration_table(bt), "backtest_calibration.csv")

last_obs <- max(panel$month)
cat("Production forward from", format(last_obs), "\n")
prod_fits <- lapply(CATS, function(cat) fit_enet(panel[panel$category == cat, ], MODEL_COLS, lambda = LAMBDA))
names(prod_fits) <- CATS
prod_boot <- lapply(CATS, function(cat) {
  bootstrap_fits(panel[panel$category == cat, ], MODEL_COLS, lambda = LAMBDA, n_fits = N_REFITS)
})
names(prod_boot) <- CATS
write_out(do.call(rbind, lapply(CATS, function(cat) enet_coefficients(prod_fits[[cat]], cat))),
          "enet_coefficients_production.csv")

inp <- forward_inputs(flows, pop_by_age, revalidation, pqp, last_obs, FORWARD_MONTHS)
pub <- published_monthly_quota(coe_results)
y_series <- lapply(CATS, function(cat) {
  fc <- flows[flows$category == cat & !is.na(flows$deregistrations), ]
  stats::setNames(as.numeric(fc$deregistrations), as.character(as.Date(fc$month)))
})
names(y_series) <- CATS

fwd_dereg <- list()
fwd_quota <- list()
for (cat in CATS) {
  pc <- inp$per_cat[[cat]]
  imp <- lapply(MODEL_COLS, function(col) stats::median(panel[[col]][panel$category == cat], na.rm = TRUE))
  names(imp) <- MODEL_COLS
  rf <- roll_forward(prod_fits[[cat]], cat, last_obs, FORWARD_MONTHS, pc$y_hist, inp$stock_at,
                     pc$share, pc$reval5, pc$pqp_base, pc$reg, resid = resid_by_cat[[cat]],
                     n_sims = N_SIMS, cols = MODEL_COLS, impute = imp, fits_boot = prod_boot[[cat]],
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
fwd_dereg_df <- do.call(rbind, fwd_dereg)
fwd_quota_df <- do.call(rbind, fwd_quota)
write_out(fwd_dereg_df, "forward_dereg.csv")
write_out(fwd_quota_df, "forward_quota.csv")

age_proj <- do.call(rbind, lapply(seq_len(FORWARD_MONTHS), function(h) {
  m <- add_months(last_obs, h)
  row <- inp$stock_at(m)
  data.frame(month = m, age = 0:20, population = as.numeric(row), stringsAsFactors = FALSE)
}))
write_out(age_proj, "forward_age_stock.csv")

cat("Renewal response + scenarios\n")
resp <- renewal_response(renewal_rate)
write_out(resp, "renewal_response.csv")
bf <- brought_forward(pub, y_series, alphas)
cat(sprintf("  brought-forward stock since Feb 2023: %s\n", format(round(bf), big.mark = ",")))

scen_rows <- list()
for (preset in scenario_presets(bf)) {
  split <- injection_split(pub, preset$injection_per_qtr)
  for (cat in CATS) {
    fw <- fwd_dereg_df[fwd_dereg_df$category == cat, ]
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
keep <- c("scenario_id", "scenario", "category", "quarter", "months")
scen <- scen[, c(keep, setdiff(names(scen), keep))]
write_out(scen, "forward_scenarios.csv")

spec <- utils::read.csv(file.path(exp, "model_spec.csv"), stringsAsFactors = FALSE)
spec <- rbind(
  spec,
  data.frame(key = "brought_forward_since_2023", value = as.character(round(bf)), stringsAsFactors = FALSE),
  data.frame(key = "last_observed_month", value = format(last_obs), stringsAsFactors = FALSE),
  data.frame(key = "mapping_resid_sd_A", value = sprintf("%.3f", stats::sd(map_resid$A)), stringsAsFactors = FALSE),
  data.frame(key = "mapping_resid_sd_B", value = sprintf("%.3f", stats::sd(map_resid$B)), stringsAsFactors = FALSE)
)
write_out(spec, "model_spec.csv")

cat("\nLocked scores still present:\n")
print(utils::read.csv(file.path(exp, "two_models_quota_scores.csv")))
cat("\nDone.\n")
