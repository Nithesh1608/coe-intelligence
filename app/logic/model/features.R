# Feature engineering for the Category A/B deregistration model.
#
# Unit of prediction: one category-month of SingStat VQS deregistrations.
# Every predictor is dated strictly before month t (age stock and PQP at t-1).
# The same functions build historical rows (training) and forward rows
# (recursive forecasting), so the forecast cannot use features the model
# never saw.

#' @export
CATS <- c("A", "B")

#' @export
REF_PQP <- 50000

#' Full predictor set. reg_lag120/180/240 are the category's own registrations
#' 10, 15 and 20 years earlier: the cohorts reaching the year-10 PARF exit, the
#' end of a 5-year renewal, and the year-20 decision.
#' @export
BASE_COLS <- c(
  "due10", "due15", "due_near", "pop_young", "pop_renewed",
  "reg_lag120", "reg_lag180", "reg_lag240",
  "recent12", "y_lag12", "y_lag2",
  "reval5_lag12", "reval5_lag24", "reval5_lag60", "lpqp",
  "post_2022", "moy_sin", "moy_cos"
)

#' Predictors for the hazard (exit-rate) model: the level comes from the due
#' cohort, so the model learns the rate from lagged rates, renewals, PQP and
#' seasonality. hz_* are lagged exits divided by the lagged cohort.
#' @export
HAZARD_COLS <- c(
  "hz_recent12", "hz_lag2", "hz_lag12", "due_share15", "due_share_near",
  "reval5_lag12", "reval5_lag24", "reval5_lag60", "lpqp",
  "post_2022", "moy_sin", "moy_cos"
)

#' Cohort-only subset: no autoregressive terms, so the forward path is driven
#' by registrations and age stock already in the public record.
#' @export
COHORT_COLS <- setdiff(BASE_COLS, c("recent12", "y_lag2"))

#' @export
POST_2022_START <- as.Date("2022-05-01")

#' Shift first-of-month dates by n months (vectorised over month or n).
#' @export
add_months <- function(month, n) {
  m <- as.Date(month)
  y <- as.integer(format(m, "%Y"))
  mo <- as.integer(format(m, "%m")) + as.integer(n)
  y <- y + (mo - 1L) %/% 12L
  mo <- (mo - 1L) %% 12L + 1L
  as.Date(sprintf("%04d-%02d-01", y, mo))
}

#' @export
quarter_start <- function(month) {
  m <- as.Date(month)
  q <- ((as.integer(format(m, "%m")) - 1L) %/% 3L) * 3L + 1L
  as.Date(sprintf("%04d-%02d-01", as.integer(format(m, "%Y")), q))
}

#' @export
month_seq <- function(from, to) {
  from <- as.Date(from)
  to <- as.Date(to)
  n <- (as.integer(format(to, "%Y")) - as.integer(format(from, "%Y"))) * 12L +
    as.integer(format(to, "%m")) - as.integer(format(from, "%m"))
  add_months(from, seq.int(0L, max(n, 0L)))
}

# ---- Age stock ---------------------------------------------------------------

#' Monthly A+B car stock by single-year age band as a months x 21 matrix.
#' Column k holds cars aged (k-1) to <k years; column 21 is "20 and above".
#' @export
age_stock_matrix <- function(pop_by_age) {
  d <- pop_by_age[pop_by_age$vehicle_type == "Cars" & pop_by_age$coe_group == "A+B", ]
  d$month <- as.Date(d$month)
  months <- sort(unique(d$month))
  mat <- matrix(0, nrow = length(months), ncol = 21L,
                dimnames = list(as.character(months), paste0("age", 0:20)))
  idx_m <- match(d$month, months)
  idx_a <- pmin(as.integer(d$age_lower), 20L) + 1L
  for (i in seq_len(nrow(d))) {
    mat[idx_m[i], idx_a[i]] <- mat[idx_m[i], idx_a[i]] + as.numeric(d$population[i])
  }
  list(months = months, mat = mat)
}

#' Registration-cohort survival by age band.
#' Cars aged k to <k+1 at month M were registered in the 12 months ending
#' M - 12k - 1. S_k = observed stock in band k / registrations in that window,
#' measured on the latest observed stock row (median over the last `months`
#' rows). Band 20+ is held at its observed level.
#' @export
cohort_survival <- function(stock, reg_total, months = 6L) {
  n <- nrow(stock$mat)
  rows <- seq.int(max(1L, n - months + 1L), n)
  s <- matrix(NA_real_, length(rows), 20L)
  for (i in seq_along(rows)) {
    m <- stock$months[rows[i]]
    for (k in 0:19) {
      regs <- cohort_regs(reg_total, m, k)
      s[i, k + 1L] <- if (is.finite(regs) && regs > 0) stock$mat[rows[i], k + 1L] / regs else NA_real_
    }
  }
  out <- apply(s, 2, stats::median, na.rm = TRUE)
  out[!is.finite(out)] <- 0
  pmin(pmax(out, 0), 1.2)
}

#' Registrations in the 12-month window that is aged k to <k+1 at month M.
#' Future months (window not yet observed) use the trailing 12-month mean.
#' @export
cohort_regs <- function(reg_total, month, k) {
  keys <- as.character(add_months(as.Date(month), -(12L * k + 1L):-(12L * k + 12L)))
  v <- reg_total[keys]
  if (anyNA(v)) {
    last <- utils::tail(reg_total[!is.na(reg_total)], 12L)
    v[is.na(v)] <- mean(last)
  }
  sum(as.numeric(v))
}

#' Project the age stock `horizon` months past the last observed row using
#' registration cohorts and the calibrated survival factors.
#' @export
project_age_stock <- function(stock, horizon, reg_total, survival = NULL) {
  if (is.null(survival)) survival <- cohort_survival(stock, reg_total)
  origin <- stock$months[length(stock$months)]
  last20 <- stock$mat[nrow(stock$mat), 21L]
  out <- matrix(NA_real_, nrow = horizon, ncol = 21L, dimnames = list(NULL, colnames(stock$mat)))
  for (h in seq_len(horizon)) {
    m <- add_months(origin, h)
    for (k in 0:19) out[h, k + 1L] <- survival[k + 1L] * cohort_regs(reg_total, m, k)
    out[h, 21L] <- last20
  }
  out
}

#' Lookup for the stock row at a given month: actual if observed, projected after.
#' @export
make_stock_lookup <- function(stock, horizon, reg_total, survival = NULL) {
  proj <- project_age_stock(stock, horizon, reg_total, survival)
  origin <- stock$months[length(stock$months)]
  function(month) {
    month <- as.Date(month)
    key <- as.character(month)
    if (key %in% rownames(stock$mat)) return(as.numeric(stock$mat[key, ]))
    h <- (as.integer(format(month, "%Y")) - as.integer(format(origin, "%Y"))) * 12L +
      as.integer(format(month, "%m")) - as.integer(format(origin, "%m"))
    if (h < 1L || h > nrow(proj)) return(rep(NA_real_, 21L))
    as.numeric(proj[h, ])
  }
}

#' Total A+B monthly registrations as a named vector.
#' @export
reg_total_series <- function(flows) {
  d <- flows[flows$category %in% CATS & is.finite(flows$registrations), ]
  agg <- stats::aggregate(registrations ~ month, data = d, FUN = sum)
  agg <- agg[order(agg$month), ]
  stats::setNames(as.numeric(agg$registrations), as.character(as.Date(agg$month)))
}

#' Due-stock features from the stock row at t-1 and the category share.
#' due10: cars aged 9-<10 that reach the year-10 decision within a year.
#' due15: cars aged 14-<15 whose 5-year renewal runs out within a year.
#' due_near: neighbouring bands 8-<9 and 10-<11.
#' All scaled by the category share of A+B deregistrations and spread over 12 months.
#' @export
due_features <- function(stock_row, share) {
  due10 <- stock_row[10] * share / 12
  due15 <- stock_row[15] * share / 12
  due_near <- (stock_row[9] + stock_row[11]) * share / 12
  c(
    due10 = unname(due10),
    due15 = unname(due15),
    due_near = unname(due_near),
    # Fleet that can leave early (before the year-10 decision) or after a
    # 10-year renewal: bands 0-8 and 10-19, same monthly scaling.
    pop_young = unname(sum(stock_row[1:9]) * share / 12),
    pop_renewed = unname(sum(stock_row[c(11:14, 16:20)]) * share / 12),
    # Cohort at risk of leaving this month: year-10 PARF exits plus cars whose
    # 5-year renewal runs out. This is the denominator of the hazard model.
    cohort = unname(due10 + due15),
    due_share15 = unname(due15 / pmax(due10 + due15, 1)),
    due_share_near = unname(due_near / pmax(due10 + due15, 1))
  )
}

# ---- Panel -------------------------------------------------------------------

series_lookup <- function(df, value_col) {
  function(cat, month) {
    hit <- df$category == cat & df$month == as.Date(month)
    v <- df[[value_col]][hit]
    if (length(v) && is.finite(v[[1]])) as.numeric(v[[1]]) else NA_real_
  }
}

#' Category share of A+B deregistrations over the 12 months ending at `end`.
#' @export
dereg_share <- function(flows, cat, end) {
  months <- add_months(end, -11:0)
  d <- flows[flows$category %in% CATS & flows$month %in% months, ]
  tot <- sum(d$deregistrations, na.rm = TRUE)
  own <- sum(d$deregistrations[d$category == cat], na.rm = TRUE)
  if (!is.finite(tot) || tot <= 0) return(0.5)
  own / tot
}

#' Monthly 5-year revalidations for one category as a named vector.
#' @export
reval5_series <- function(revalidation, cat) {
  d <- revalidation[revalidation$category == cat & revalidation$tenure_years == 5, ]
  d <- d[order(d$month), ]
  stats::setNames(as.numeric(d$revalidations), as.character(as.Date(d$month)))
}

#' Monthly new registrations for one category as a named vector.
#' @export
reg_series <- function(flows, cat) {
  d <- flows[flows$category == cat & is.finite(flows$registrations), ]
  d <- d[order(d$month), ]
  stats::setNames(as.numeric(d$registrations), as.character(as.Date(d$month)))
}

#' Centred 3-month mean of a series around month - k (smooths exercise timing).
reg_lag <- function(series, month, k) {
  v <- vapply(c(-1L, 0L, 1L), function(j) lag_or_na(series, add_months(month, -k + j)), numeric(1))
  if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE)
}

#' Monthly PQP for one category as a named vector.
#' @export
pqp_series <- function(pqp, cat) {
  d <- pqp[pqp$category == cat & is.finite(pqp$pqp), ]
  d <- d[order(d$month), ]
  stats::setNames(as.numeric(d$pqp), as.character(as.Date(d$month)))
}

lag_or_na <- function(series, month) {
  key <- as.character(as.Date(month))
  if (key %in% names(series)) as.numeric(series[[key]]) else NA_real_
}

#' Build one feature row for category `cat` and month `t`.
#' `y_hist` is a named vector (month -> deregistrations) that may include
#' simulated values when called recursively.
#' @export
feature_row <- function(cat, t, y_hist, stock_at, share, reval5, pqp_prev, reg) {
  t <- as.Date(t)
  lag <- function(k) lag_or_na(y_hist, add_months(t, -k))
  recent <- vapply(1:12, lag, numeric(1))
  due <- due_features(stock_at(add_months(t, -1L)), share)
  cohort_lag <- function(k) {
    srow <- stock_at(add_months(t, -k - 1L))
    (srow[10] + srow[15]) * share / 12
  }
  hz <- vapply(1:12, function(k) lag(k) / pmax(cohort_lag(k), 1), numeric(1))
  moy <- as.integer(format(t, "%m"))
  c(
    due,
    reg_lag120 = reg_lag(reg, t, 120L),
    reg_lag180 = reg_lag(reg, t, 180L),
    reg_lag240 = reg_lag(reg, t, 240L),
    recent12 = mean(recent, na.rm = TRUE),
    y_lag12 = lag(12L),
    y_lag2 = lag(2L),
    hz_recent12 = mean(hz, na.rm = TRUE),
    hz_lag2 = hz[2],
    hz_lag12 = hz[12],
    reval5_lag12 = lag_or_na(reval5, add_months(t, -12L)),
    reval5_lag24 = lag_or_na(reval5, add_months(t, -24L)),
    reval5_lag60 = lag_or_na(reval5, add_months(t, -60L)),
    lpqp = log(pqp_prev / REF_PQP),
    post_2022 = as.numeric(t >= POST_2022_START),
    moy_sin = sin(2 * pi * moy / 12),
    moy_cos = cos(2 * pi * moy / 12)
  )
}

#' Build the historical feature panel for Categories A and B.
#' @export
build_feature_panel <- function(flows, pop_by_age, revalidation, pqp,
                                from = as.Date("2016-02-01")) {
  flows$month <- as.Date(flows$month)
  revalidation$month <- as.Date(revalidation$month)
  pqp$month <- as.Date(pqp$month)
  stock <- age_stock_matrix(pop_by_age)
  stock_at <- function(m) {
    key <- as.character(as.Date(m))
    if (key %in% rownames(stock$mat)) as.numeric(stock$mat[key, ]) else rep(NA_real_, 21L)
  }
  rows <- list()
  for (cat in CATS) {
    fc <- flows[flows$category == cat & !is.na(flows$deregistrations), ]
    fc <- fc[order(fc$month), ]
    y_hist <- stats::setNames(as.numeric(fc$deregistrations), as.character(fc$month))
    rv <- reval5_series(revalidation, cat)
    pq <- pqp_series(pqp, cat)
    rg <- reg_series(flows, cat)
    months <- fc$month[fc$month >= as.Date(from)]
    for (t in as.list(months)) {
      prev <- add_months(t, -1L)
      pqp_prev <- lag_or_na(pq, prev)
      if (!is.finite(pqp_prev)) {
        # Bidding suspended months: carry the last PQP forward.
        back <- pq[names(pq) < as.character(prev)]
        pqp_prev <- if (length(back)) as.numeric(utils::tail(back, 1)) else NA_real_
      }
      f <- feature_row(cat, t, y_hist, stock_at, dereg_share(flows, cat, prev), rv, pqp_prev, rg)
      rows[[length(rows) + 1L]] <- data.frame(
        month = as.Date(t), category = cat, y = as.numeric(y_hist[[as.character(t)]]),
        as.list(f), stringsAsFactors = FALSE
      )
    }
  }
  panel <- do.call(rbind, rows)
  panel[order(panel$category, panel$month), ]
}

#' Replace NA predictors with the training-era median per category.
#' @export
impute_panel <- function(panel, train_end, cols = BASE_COLS) {
  for (cat in unique(panel$category)) {
    idx <- panel$category == cat
    for (col in cols) {
      v <- panel[[col]][idx]
      med <- stats::median(v[panel$month[idx] <= as.Date(train_end)], na.rm = TRUE)
      v[is.na(v)] <- med
      panel[[col]][idx] <- v
    }
  }
  panel
}
