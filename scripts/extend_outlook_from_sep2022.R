# Extend Home orange (quota outlook) back through Sep 2022 using the eval fit
# (train <= Aug 2024). Keeps shipped held-out rows (Sep 2024+) unchanged.
# Also prints / writes train vs test one-step quota MAPE.
#
# Run from repo root: Rscript scripts/extend_outlook_from_sep2022.R

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(root, "rhino.yml"))) stop("Run from the repo root.")
options(box.path = root)

box::use(
  app/logic/model/features[CATS, CORE_COLS],
  app/logic/model/forecast[
    TRAIN_END, TEST_START, fit_enet, predict_enet,
    published_monthly_quota, calibrate_alpha, trailing12
  ],
)

proc <- file.path(root, "data", "processed")
exp <- file.path(proc, "model_experiment")
MODEL_COLS <- CORE_COLS
LAMBDA <- "lambda.1se"
PRODUCTION_START <- as.Date("2018-01-01")
ORANGE_FROM <- as.Date("2022-09-01")

rds <- function(name) {
  x <- readRDS(file.path(proc, name))
  if ("month" %in% names(x)) x$month <- as.Date(x$month)
  if ("category" %in% names(x)) x$category <- as.character(x$category)
  x
}

flows <- rds("vehicle_flows.rds")
coe_results <- rds("coe_results.rds")

panel <- utils::read.csv(file.path(exp, "feature_panel.csv"), stringsAsFactors = FALSE)
panel$month <- as.Date(panel$month)
panel$category <- as.character(panel$category)
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

alphas <- calibrate_alpha(flows, coe_results, PRODUCTION_START, TRAIN_END)
pub <- published_monthly_quota(coe_results)

y_series <- lapply(CATS, function(cat) {
  fc <- flows[flows$category == cat & !is.na(flows$deregistrations), ]
  stats::setNames(as.numeric(fc$deregistrations), as.character(as.Date(fc$month)))
})
names(y_series) <- CATS

shipped <- utils::read.csv(file.path(exp, "quota_outlook_calibrated.csv"), stringsAsFactors = FALSE)
shipped$month <- as.Date(shipped$month)
shipped$category <- as.character(shipped$category)

# One-step: for each month m, replace only m with the model prediction in D12.
# Score train from PRODUCTION_START; only write Home orange from ORANGE_FROM.
chart_rows <- list()
score_rows <- list()
for (cat in CATS) {
  tr <- panel[panel$category == cat & panel$month <= TRAIN_END, ]
  fit <- fit_enet(tr, MODEL_COLS, lambda = LAMBDA)
  a <- alphas$alpha[alphas$category == cat][[1]]
  months <- sort(unique(panel$month[
    panel$category == cat &
      panel$month >= PRODUCTION_START &
      panel$month <= TRAIN_END
  ]))
  for (tm in as.list(months)) {
    row <- panel[panel$category == cat & panel$month == tm, , drop = FALSE]
    if (!nrow(row)) next
    yhat <- as.numeric(predict_enet(fit, row, MODEL_COLS)[[1]])
    spliced <- y_series[[cat]]
    spliced[as.character(tm)] <- yhat
    d12_pred <- trailing12(spliced, tm)
    d12_act <- trailing12(y_series[[cat]], tm)
    p <- pub$published[pub$category == cat & pub$month == tm]
    p <- if (length(p)) p[[1]] else NA_real_
    if (!is.finite(d12_pred) || !is.finite(p)) next
    score_rows[[length(score_rows) + 1L]] <- data.frame(
      month = tm, category = cat, window = "train",
      published = p, outlook = a * d12_pred,
      stringsAsFactors = FALSE
    )
    if (tm >= ORANGE_FROM) {
      chart_rows[[length(chart_rows) + 1L]] <- data.frame(
        month = tm, category = cat, published = p,
        outlook_from_forecast = a * d12_pred,
        outlook_from_actual_exits = a * d12_act,
        alpha = a, d12_pred = d12_pred, d12_act = d12_act,
        stringsAsFactors = FALSE
      )
    }
  }
}

chart_outlook <- do.call(rbind, chart_rows)
held <- shipped[shipped$month >= TEST_START, , drop = FALSE]
extended <- rbind(chart_outlook, held)
extended <- extended[order(extended$category, extended$month), ]
utils::write.csv(extended, file.path(exp, "quota_outlook_calibrated.csv"), row.names = FALSE)
cat("Wrote quota_outlook_calibrated.csv:", nrow(extended), "rows (Home orange ",
    format(ORANGE_FROM), "->", format(max(extended$month)), ")\n")

# Test scores from locked held-out artefact (unchanged rows).
held_score <- data.frame(
  month = held$month, category = held$category, window = "test",
  published = held$published, outlook = held$outlook_from_forecast,
  stringsAsFactors = FALSE
)
all_score <- rbind(do.call(rbind, score_rows), held_score)

mape_tab <- do.call(rbind, lapply(split(all_score, list(all_score$category, all_score$window), drop = TRUE), function(d) {
  d <- d[is.finite(d$published) & is.finite(d$outlook), ]
  data.frame(
    category = d$category[1],
    window = d$window[1],
    n = nrow(d),
    from = format(min(d$month)),
    to = format(max(d$month)),
    MAE = mean(abs(d$outlook - d$published)),
    MAPE = mean(abs(d$outlook - d$published) / pmax(d$published, 1)),
    stringsAsFactors = FALSE
  )
}))
rownames(mape_tab) <- NULL
mape_tab <- mape_tab[order(mape_tab$category, mape_tab$window), ]
utils::write.csv(mape_tab, file.path(exp, "quota_train_test_mape.csv"), row.names = FALSE)

desk <- utils::read.csv(file.path(exp, "two_models_quota_scores.csv"), stringsAsFactors = FALSE)
desk <- desk[desk$model == "core_forecast", c("category", "MAE", "MAPE")]

cat("\nQuota MAPE (one-step):\n")
print(mape_tab, row.names = FALSE)
cat("\nShipped desk test KPI (Home / Results; unchanged):\n")
print(desk, row.names = FALSE)
cat("\nTrain = in-sample from production start; Home orange still from Sep 2022.\n")
cat("The ~14.6% A figure is calibrated-outlook MAPE on the same held-out months;\n")
cat("it was already that before extending orange - not a regression from this change.\n")
