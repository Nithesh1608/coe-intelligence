# Elastic-net deregistration model, recursive forward paths with a residual
# block bootstrap, and the calibrated quota mapping.
#
# Two fits are used:
#   evaluation fit  - trained through TRAIN_END, scored one-step on the locked
#                     held-out window and backtested recursively from TRAIN_END;
#   production fit  - trained on every observed month, rolled forward 36 months.

box::use(
  glmnet[cv.glmnet],
  stats[predict, quantile, median, setNames],
  app/logic/model/features[
    CATS, BASE_COLS, add_months, quarter_start, month_seq, feature_row,
    make_stock_lookup, age_stock_matrix, cohort_survival, reg_total_series, dereg_share,
    reval5_series, pqp_series, reg_series, impute_panel
  ],
)

#' @export
TRAIN_END <- as.Date("2024-08-01")

#' @export
TEST_START <- as.Date("2024-09-01")

#' @export
FLOOR_FRAC <- 0.2

x_mat <- function(df, cols = BASE_COLS) as.matrix(df[, cols, drop = FALSE])

#' Fit the elastic net (alpha = 0.5, MAE loss) on one category. The penalty
#' used for prediction (`lambda.1se` by default) is stored on the fit.
#' @export
fit_enet <- function(train, cols = BASE_COLS, seed = 3702L, lambda = "lambda.1se",
                     target = c("level", "hazard")) {
  target <- match.arg(target)
  set.seed(seed)
  y <- if (target == "hazard") train$y / pmax(train$cohort, 1) else train$y
  fit <- cv.glmnet(
    x_mat(train, cols), y, alpha = 0.5,
    nfolds = min(5L, max(3L, nrow(train) %/% 12L)),
    type.measure = "mae"
  )
  attr(fit, "lambda_rule") <- lambda
  attr(fit, "cols") <- cols
  attr(fit, "target") <- target
  fit
}

#' @export
fit_target <- function(fit) {
  t <- attr(fit, "target")
  if (is.null(t)) "level" else t
}

lambda_rule <- function(fit) {
  s <- attr(fit, "lambda_rule")
  if (is.null(s)) "lambda.1se" else s
}

#' Predict deregistrations, floored at FLOOR_FRAC x recent12.
#' @export
predict_enet <- function(fit, newdata, cols = NULL) {
  if (is.null(cols)) cols <- attr(fit, "cols")
  if (is.null(cols)) cols <- BASE_COLS
  p <- as.numeric(predict(fit, newx = x_mat(newdata, cols), s = lambda_rule(fit)))
  if (fit_target(fit) == "hazard") p <- p * pmax(as.numeric(newdata$cohort), 1)
  pmax(p, FLOOR_FRAC * as.numeric(newdata$recent12))
}

#' Non-zero coefficients at the chosen penalty, for the About page and the report.
#' @export
enet_coefficients <- function(fit, category) {
  cf <- as.matrix(stats::coef(fit, s = lambda_rule(fit)))
  data.frame(
    category = category,
    term = rownames(cf),
    coefficient = as.numeric(cf[, 1]),
    stringsAsFactors = FALSE
  )
}

#' One-step-ahead held-out predictions for the elastic net and the trailing
#' 12-month average baseline.
#' @export
one_step_eval <- function(panel, train_end = TRAIN_END, test_start = TEST_START, cols = BASE_COLS,
                          target = "level", lambda = "lambda.1se") {
  rows <- list()
  fits <- list()
  for (cat in CATS) {
    tr <- panel[panel$category == cat & panel$month <= train_end, ]
    te <- panel[panel$category == cat & panel$month >= test_start, ]
    fit <- fit_enet(tr, cols, lambda = lambda, target = target)
    fits[[cat]] <- fit
    rows[[length(rows) + 1L]] <- data.frame(
      month = te$month, category = cat, model = "core_forecast", label = "Elastic net",
      actual = te$y, predicted = round(predict_enet(fit, te, cols)), stringsAsFactors = FALSE
    )
    rows[[length(rows) + 1L]] <- data.frame(
      month = te$month, category = cat, model = "simple_average",
      label = "Simple average of the last 12 months",
      actual = te$y, predicted = round(te$recent12), stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  out$gap <- out$predicted - out$actual
  out$pct_off <- round(100 * out$gap / pmax(out$actual, 1), 1)
  list(monthly = out, fits = fits)
}

# ---- Quota mapping -----------------------------------------------------------

#' Published monthly A/B tender quota (sum over the two exercises).
#' The latest month is dropped while only one of its two exercises is in.
#' @export
published_monthly_quota <- function(coe_results) {
  b <- coe_results[coe_results$category %in% CATS & !is.na(coe_results$quota), ]
  b$month <- as.Date(b$month)
  agg <- stats::aggregate(quota ~ month + category, data = b, FUN = sum)
  names(agg)[names(agg) == "quota"] <- "published"
  rounds <- stats::aggregate(quota ~ month, data = b, FUN = length)
  last <- max(rounds$month)
  if (rounds$quota[rounds$month == last] < 2L * length(CATS)) agg <- agg[agg$month != last, ]
  agg[order(agg$category, agg$month), ]
}

#' Relative quota-mapping residuals: published / (alpha x trailing-12 actual
#' exits) - 1 over a window. These carry LTA's discretion over the formula
#' and are resampled into the quota bands.
#' @export
mapping_residuals <- function(flows, coe_results, alphas, from = as.Date("2017-02-01"),
                              to = TRAIN_END) {
  pub <- published_monthly_quota(coe_results)
  out <- list()
  for (cat in CATS) {
    fc <- flows[flows$category == cat & !is.na(flows$deregistrations), ]
    y <- setNames(as.numeric(fc$deregistrations), as.character(as.Date(fc$month)))
    p <- pub[pub$category == cat & pub$month >= from & pub$month <= to, ]
    p <- p[order(p$month), ]
    d12 <- vapply(as.list(p$month), function(m) trailing12(y, m), numeric(1))
    a <- alphas$alpha[alphas$category == cat][[1]]
    r <- p$published / (a * d12) - 1
    out[[cat]] <- r[is.finite(r)]
  }
  out
}

#' Quota paths with mapping noise: each bootstrap path's alpha x D12 is
#' multiplied by (1 + u), u block-resampled from mapping_residuals() with
#' 6-month blocks so persistent discretion stays persistent.
#' @export
quota_bands <- function(y_hist, fwd_months, sims, alpha, map_resid, block = 6L, seed = 3702L) {
  q <- quota_from_dereg(y_hist, fwd_months, sims, alpha)
  if (is.null(map_resid) || !length(map_resid)) return(q)
  set.seed(seed + 1L)
  x <- map_resid[is.finite(map_resid)]
  n_sims <- nrow(q)
  horizon <- ncol(q)
  nb <- ceiling(horizon / block)
  starts <- sample(seq_len(max(1L, length(x) - block + 1L)), n_sims * nb, replace = TRUE)
  idx <- pmin(as.vector(outer(0:(block - 1L), starts, `+`)), length(x))
  u <- matrix(x[idx], nrow = n_sims, byrow = TRUE)[, seq_len(horizon), drop = FALSE]
  q * (1 + u)
}

#' Trailing 12-month sum of a named series ending at `end`; NA if incomplete.
#' @export
trailing12 <- function(series, end) {
  keys <- as.character(add_months(end, -11:0))
  v <- series[keys]
  if (any(is.na(v))) return(NA_real_)
  sum(as.numeric(v))
}

#' Calibrate alpha = median(published monthly quota / trailing-12m deregistrations)
#' over the training window, per category.
#' @export
calibrate_alpha <- function(flows, coe_results, train_start = as.Date("2018-01-01"),
                            train_end = TRAIN_END) {
  pub <- published_monthly_quota(coe_results)
  out <- list()
  for (cat in CATS) {
    fc <- flows[flows$category == cat & !is.na(flows$deregistrations), ]
    y <- setNames(as.numeric(fc$deregistrations), as.character(as.Date(fc$month)))
    p <- pub[pub$category == cat & pub$month >= train_start & pub$month <= train_end, ]
    d12 <- vapply(as.list(p$month), function(m) trailing12(y, m), numeric(1))
    ok <- is.finite(d12) & d12 > 0
    out[[cat]] <- data.frame(
      category = cat,
      alpha = median(p$published[ok] / d12[ok]),
      n_months = sum(ok),
      textbook_monthly_share = 0.9 * 0.25 / 12,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out)
}

#' Quota path from a deregistration path: alpha x trailing-12 of the spliced
#' (actual then forecast) series. `fwd` is a vector or a sims x horizon matrix.
#' @export
quota_from_dereg <- function(y_hist, fwd_months, fwd, alpha) {
  if (is.null(dim(fwd))) fwd <- matrix(fwd, nrow = 1L)
  n_sims <- nrow(fwd)
  horizon <- length(fwd_months)
  hist_keys <- names(y_hist)
  out <- matrix(NA_real_, n_sims, horizon)
  for (h in seq_len(horizon)) {
    keys <- as.character(add_months(fwd_months[h], -11:0))
    in_hist <- keys %in% hist_keys
    base <- sum(as.numeric(y_hist[keys[in_hist]]))
    fwd_idx <- match(keys[!in_hist], as.character(fwd_months))
    fwd_idx <- fwd_idx[!is.na(fwd_idx)]
    add <- if (length(fwd_idx)) rowSums(fwd[, fwd_idx, drop = FALSE]) else 0
    out[, h] <- alpha * (base + add)
  }
  out
}

# ---- Recursive paths ---------------------------------------------------------

#' Block-bootstrap sampler over centred relative residuals.
block_sample <- function(resid, n_sims, horizon, block = 3L) {
  x <- resid[is.finite(resid)]
  x <- x - mean(x)
  nb <- ceiling(horizon / block)
  starts <- sample(seq_len(max(1L, length(x) - block + 1L)), n_sims * nb, replace = TRUE)
  idx <- as.vector(outer(0:(block - 1L), starts, `+`))
  idx <- pmin(idx, length(x))
  matrix(x[idx], nrow = n_sims, byrow = TRUE)[, seq_len(horizon), drop = FALSE]
}

#' Parameter uncertainty: refit the elastic net on moving-block resamples of
#' the training rows (12-month blocks). Returns a list of fits.
#' @export
bootstrap_fits <- function(train, cols = BASE_COLS, lambda = "lambda.1se", target = "level",
                           n_fits = 100L, block = 12L, seed = 3702L) {
  train <- train[order(train$month), ]
  n <- nrow(train)
  set.seed(seed + 2L)
  lapply(seq_len(n_fits), function(i) {
    nb <- ceiling(n / block)
    starts <- sample(seq_len(max(1L, n - block + 1L)), nb, replace = TRUE)
    idx <- as.vector(outer(0:(block - 1L), starts, `+`))
    idx <- idx[idx <= n][seq_len(n)]
    fit_enet(train[idx, ], cols, seed = seed + i, lambda = lambda, target = target)
  })
}

#' PQP uncertainty: block-bootstrapped monthly log changes (last `years` of
#' history), demeaned and accumulated from the last PQP. n_sims x horizon.
#' @export
pqp_paths <- function(pqp_series, horizon, n_sims, years = 8L, block = 3L, seed = 3702L) {
  v <- as.numeric(pqp_series)
  v <- utils::tail(v[is.finite(v) & v > 0], 12L * years + 1L)
  dl <- diff(log(v))
  dl <- dl - mean(dl)
  set.seed(seed + 3L)
  nb <- ceiling(horizon / block)
  starts <- sample(seq_len(max(1L, length(dl) - block + 1L)), n_sims * nb, replace = TRUE)
  idx <- pmin(as.vector(outer(0:(block - 1L), starts, `+`)), length(dl))
  shocks <- matrix(dl[idx], nrow = n_sims, byrow = TRUE)[, seq_len(horizon), drop = FALSE]
  last <- utils::tail(v, 1L)
  exp(log(last) + t(apply(shocks, 1, cumsum)))
}

#' Roll the model forward `horizon` months from `origin` for one category.
#'
#' @param fit        cv.glmnet fit for the category
#' @param y_hist     named vector month -> actual deregistrations up to origin
#' @param stock_at   function(month) -> 21-band stock row (actual or projected)
#' @param share      category share of A+B deregistrations (held constant)
#' @param reval5     named vector of 5-year revalidations
#' @param pqp_path   named vector month -> PQP at t-1 for every forward month
#' @param reg        named vector of the category's monthly registrations
#' @param resid      relative held-out residuals (actual / predicted - 1)
#' @param n_sims     bootstrap paths (0 for the point path only)
#' @param fits_boot  optional list of refits (parameter uncertainty); path i uses fit i mod length
#' @param pqp_sims   optional n_sims x horizon matrix of PQP paths (PQP uncertainty)
#' @return list(months, point, sims (n_sims x horizon), due_flow)
#' @export
roll_forward <- function(fit, cat, origin, horizon, y_hist, stock_at, share, reval5,
                         pqp_path, reg, resid = NULL, n_sims = 0L, block = 3L, seed = 3702L,
                         cols = BASE_COLS, impute = NULL, fits_boot = NULL, pqp_sims = NULL) {
  months <- add_months(origin, seq_len(horizon))
  shocks <- if (n_sims > 0L && length(resid)) {
    set.seed(seed)
    block_sample(resid, n_sims, horizon, block)
  } else {
    NULL
  }
  one_path <- function(shock, use_fit = fit, pqp_row = NULL) {
    y <- y_hist
    out <- numeric(horizon)
    for (h in seq_len(horizon)) {
      t <- months[h]
      prev <- add_months(t, -1L)
      pqp_prev <- if (is.null(pqp_row)) as.numeric(pqp_path[[as.character(t)]]) else pqp_row[h]
      f <- feature_row(cat, t, y, stock_at, share, reval5, pqp_prev, reg)
      row <- as.data.frame(as.list(f))
      if (!is.null(impute)) {
        for (col in names(impute)) if (col %in% names(row) && is.na(row[[col]])) row[[col]] <- impute[[col]]
      }
      p <- predict_enet(use_fit, row)
      if (!is.null(shock)) p <- max(p * (1 + shock[h]), 0)
      out[h] <- p
      y[[as.character(t)]] <- p
    }
    out
  }
  point <- one_path(NULL)
  sims <- NULL
  if (!is.null(shocks)) {
    sims <- t(vapply(seq_len(n_sims), function(i) {
      use_fit <- if (length(fits_boot)) fits_boot[[(i - 1L) %% length(fits_boot) + 1L]] else fit
      pqp_row <- if (!is.null(pqp_sims)) pqp_sims[i, ] else NULL
      one_path(shocks[i, ], use_fit, pqp_row)
    }, numeric(horizon)))
  }
  due_flow <- vapply(as.list(months), function(t) {
    srow <- stock_at(add_months(t, -1L))
    srow[10] * share / 12
  }, numeric(1))
  list(months = months, point = point, sims = sims, due_flow = due_flow)
}

#' Summarise a sims matrix into mid / 50% / 80% bands.
#' @export
band_summary <- function(point, sims) {
  if (is.null(sims)) {
    return(data.frame(mid = point, lo50 = point, hi50 = point, lo80 = point, hi80 = point))
  }
  q <- function(p) apply(sims, 2, quantile, probs = p, na.rm = TRUE)
  data.frame(
    mid = point,
    lo50 = pmin(q(0.25), point), hi50 = pmax(q(0.75), point),
    lo80 = pmin(q(0.10), point), hi80 = pmax(q(0.90), point)
  )
}

#' Inputs shared by the backtest and the production forward run.
#' @export
forward_inputs <- function(flows, pop_by_age, revalidation, pqp, origin, horizon) {
  flows$month <- as.Date(flows$month)
  pop_by_age$month <- as.Date(pop_by_age$month)
  stock <- age_stock_matrix(pop_by_age[pop_by_age$month <= origin, ])
  reg_total <- reg_total_series(flows[flows$month <= origin, ])
  survival <- cohort_survival(stock, reg_total)
  # The stock lookup must cover origin+h-1 for h up to horizon; the last stock
  # month may precede the dereg origin, so project a little further.
  gap <- (as.integer(format(origin, "%Y")) - as.integer(format(max(stock$months), "%Y"))) * 12L +
    as.integer(format(origin, "%m")) - as.integer(format(max(stock$months), "%m"))
  stock_at <- make_stock_lookup(stock, horizon + max(gap, 0L) + 1L, reg_total, survival)
  per_cat <- list()
  for (cat in CATS) {
    fc <- flows[flows$category == cat & !is.na(flows$deregistrations) & flows$month <= origin, ]
    y_hist <- setNames(as.numeric(fc$deregistrations), as.character(fc$month))
    rv <- reval5_series(revalidation[revalidation$month <= origin, ], cat)
    rv_mean <- mean(utils::tail(rv, 12L))
    pq <- pqp_series(pqp[pqp$month <= origin, ], cat)
    pqp_last <- as.numeric(utils::tail(pq, 1))
    pqp_hist <- pq
    months <- add_months(origin, seq_len(horizon))
    # Revalidation lags beyond the observed series fall back to the trailing mean.
    for (k in c(12L, 24L, 60L)) {
      need <- add_months(months, -k)
      miss <- setdiff(as.character(need), names(rv))
      if (length(miss)) rv[miss] <- rv_mean
    }
    per_cat[[cat]] <- list(
      y_hist = y_hist, reval5 = rv, share = dereg_share(flows[flows$month <= origin, ], cat, origin),
      reg = reg_series(flows[flows$month <= origin, ], cat),
      pqp_last = pqp_last,
      pqp_hist = pqp_hist,
      pqp_base = setNames(rep(pqp_last, horizon), as.character(months))
    )
  }
  list(stock_at = stock_at, survival = survival, per_cat = per_cat,
       months = add_months(origin, seq_len(horizon)))
}

#' Recursive multi-step backtest from `origin` against realised deregistrations
#' and published quota. Uses the evaluation fit and projected (not realised)
#' age stock, so it tests the whole forward method.
#' @export
recursive_backtest <- function(fits, panel, flows, pop_by_age, revalidation, pqp, coe_results,
                               alphas, origin = TRAIN_END, horizon = 24L, resid_by_cat,
                               n_sims = 400L, cols = BASE_COLS, map_resid = NULL,
                               fits_boot = NULL, pqp_uncertainty = TRUE) {
  inp <- forward_inputs(flows, pop_by_age, revalidation, pqp, origin, horizon)
  pub <- published_monthly_quota(coe_results)
  rows <- list()
  for (cat in CATS) {
    pc <- inp$per_cat[[cat]]
    imp_cols <- union(cols, "cohort")
    imp <- lapply(imp_cols, function(col) {
      median(panel[[col]][panel$category == cat & panel$month <= origin], na.rm = TRUE)
    })
    names(imp) <- imp_cols
    pq_sims <- if (isTRUE(pqp_uncertainty) && n_sims > 0L) pqp_paths(pc$pqp_hist, horizon, n_sims) else NULL
    rf <- roll_forward(fits[[cat]], cat, origin, horizon, pc$y_hist, inp$stock_at, pc$share,
                       pc$reval5, pc$pqp_base, pc$reg, resid = resid_by_cat[[cat]], n_sims = n_sims,
                       cols = cols, impute = imp, fits_boot = fits_boot[[cat]], pqp_sims = pq_sims)
    alpha <- alphas$alpha[alphas$category == cat][[1]]
    db <- band_summary(rf$point, rf$sims)
    q_point <- as.numeric(quota_from_dereg(pc$y_hist, rf$months, rf$point, alpha))
    q_sims <- if (is.null(rf$sims)) NULL else quota_bands(pc$y_hist, rf$months, rf$sims, alpha, map_resid[[cat]])
    qb <- band_summary(q_point, q_sims)
    actual <- flows$deregistrations[match(paste(cat, rf$months), paste(flows$category, flows$month))]
    published <- pub$published[match(paste(cat, rf$months), paste(pub$category, pub$month))]
    rows[[cat]] <- data.frame(
      month = rf$months, category = cat, h = seq_len(horizon),
      dereg_actual = as.numeric(actual), dereg_mid = db$mid, dereg_lo80 = db$lo80, dereg_hi80 = db$hi80,
      quota_published = as.numeric(published), quota_mid = qb$mid, quota_lo80 = qb$lo80, quota_hi80 = qb$hi80,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

#' Calibration table by horizon bucket: MAPE, bias and 80% coverage for
#' deregistrations and quota.
#' @export
calibration_table <- function(bt) {
  bt$bucket <- cut(bt$h, breaks = c(0, 6, 12, 24, 36),
                   labels = c("1-6 months", "7-12 months", "13-24 months", "25-36 months"))
  rows <- list()
  for (cat in unique(bt$category)) {
    for (b in levels(bt$bucket)) {
      d <- bt[bt$category == cat & bt$bucket == b, ]
      if (!nrow(d)) next
      dd <- d[is.finite(d$dereg_actual), ]
      dq <- d[is.finite(d$quota_published), ]
      rows[[length(rows) + 1L]] <- data.frame(
        category = cat, horizon = b, months = nrow(dd),
        dereg_mape = if (nrow(dd)) mean(abs(dd$dereg_mid - dd$dereg_actual) / pmax(dd$dereg_actual, 1)) else NA_real_,
        dereg_bias_pct = if (nrow(dd)) 100 * sum(dd$dereg_mid - dd$dereg_actual) / sum(dd$dereg_actual) else NA_real_,
        dereg_cover80 = if (nrow(dd)) mean(dd$dereg_actual >= dd$dereg_lo80 & dd$dereg_actual <= dd$dereg_hi80) else NA_real_,
        quota_mape = if (nrow(dq)) mean(abs(dq$quota_mid - dq$quota_published) / pmax(dq$quota_published, 1)) else NA_real_,
        quota_bias_pct = if (nrow(dq)) 100 * sum(dq$quota_mid - dq$quota_published) / sum(dq$quota_published) else NA_real_,
        quota_cover80 = if (nrow(dq)) mean(dq$quota_published >= dq$quota_lo80 & dq$quota_published <= dq$quota_hi80) else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}
