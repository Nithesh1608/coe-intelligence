# Feedback and scenario layer on top of the mechanical forward path.
#
# The elastic net gives a baseline deregistration path with PQP held at its
# last value. This layer adds behaviour and policy on top:
#   - renewal response: renewal share as a fitted function of log PQP
#     (LTA revalidation data, 2015 onward), applied to the cars reaching the
#     year-10 decision each month (due_flow);
#   - a direct renewal-rate lever in percentage points;
#   - a premium path (demand shock) that moves PQP and therefore renewals;
#   - a policy injection of extra certificates per quarter.
# Quota is rebuilt as alpha x trailing-12 deregistrations after the change, so
# a renewal shift today propagates into quota over the following twelve months.

box::use(
  stats[lm, coef, setNames],
  app/logic/model/features[CATS, add_months, quarter_start],
)

#' Fit renewal share on log PQP per category.
#' @export
renewal_response <- function(renewal_rate) {
  d <- renewal_rate[renewal_rate$category %in% CATS & is.finite(renewal_rate$renewal_rate) &
                      is.finite(renewal_rate$pqp) & renewal_rate$pqp > 0, ]
  rows <- lapply(CATS, function(cat) {
    dc <- d[d$category == cat, ]
    m <- lm(renewal_rate ~ log(pqp), data = dc)
    s <- summary(m)
    data.frame(
      category = cat,
      intercept = unname(coef(m)[1]),
      slope_log_pqp = unname(coef(m)[2]),
      slope_se = s$coefficients[2, 2],
      r2 = s$r.squared,
      n = nrow(dc),
      mean_renewal_rate = mean(dc$renewal_rate),
      from = format(min(dc$month), "%Y-%m"),
      to = format(max(dc$month), "%Y-%m"),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Scenario presets shown in the app and exported for the report.
#' `clawback_total` is the stock of certificates LTA brought forward since
#' Feb 2023 (published minus formula quota); the clawback preset removes it
#' evenly over eight quarters.
#' @export
scenario_presets <- function(clawback_total = NA_real_) {
  claw <- if (is.finite(clawback_total)) -abs(clawback_total) / 8 else 0
  list(
    list(id = "baseline", label = "Baseline: PQP flat, no injection",
         pqp_target = NA_real_, injection_per_qtr = 0, renewal_shift = 0,
         note = "Mechanical path from the elastic net with PQP held at its last value."),
    list(id = "inject_750", label = "LTA injects 750 car COEs per quarter",
         pqp_target = NA_real_, injection_per_qtr = 750, renewal_shift = 0,
         note = "Extra certificates added to A/B quota in proportion to recent published shares."),
    list(id = "premium_80k", label = "Premiums fall to $80k by the end of the horizon",
         pqp_target = 80000, injection_per_qtr = 0, renewal_shift = 0,
         note = "PQP glides down; the fitted renewal response raises renewals, cutting deregistrations."),
    list(id = "premium_160k", label = "Premiums rise to $160k by the end of the horizon",
         pqp_target = 160000, injection_per_qtr = 0, renewal_shift = 0,
         note = "PQP glides up; fewer owners renew, so more cars leave and quota loosens."),
    list(id = "clawback", label = "LTA claws back brought-forward certificates over 8 quarters",
         pqp_target = NA_real_, injection_per_qtr = claw, renewal_shift = 0,
         note = "Since 2023 LTA has released more than the formula implies; this preset takes that stock back out of the next eight quarters."),
    list(id = "renew_plus5", label = "Renewal rate +5 points (lever only)",
         pqp_target = NA_real_, injection_per_qtr = 0, renewal_shift = 0.05,
         note = "Pure behavioural shift with no premium or policy change.")
  )
}

#' Certificates published above the formula since `from`: the brought-forward
#' stock that a clawback scenario removes.
#' @export
brought_forward <- function(published, y_series, alphas, from = as.Date("2023-02-01")) {
  box::use(app/logic/model/forecast[trailing12])
  total <- 0
  for (cat in CATS) {
    p <- published[published$category == cat & published$month >= from, ]
    a <- alphas$alpha[alphas$category == cat][[1]]
    d12 <- vapply(as.list(p$month), function(m) trailing12(y_series[[cat]], m), numeric(1))
    ok <- is.finite(d12)
    total <- total + sum(p$published[ok] - a * d12[ok])
  }
  total
}

#' Linear PQP glide from the current level to `target` over the horizon.
#' @export
pqp_glide <- function(pqp_base, target = NA_real_) {
  if (!is.finite(target)) return(pqp_base)
  n <- length(pqp_base)
  pqp_base[1] + (target - pqp_base[1]) * seq_len(n) / n
}

#' Apply a scenario to one category's forward path.
#'
#' @param fwd   data.frame with month, mid, lo80, hi80, lo50, hi50, due_flow, pqp_base
#' @param y_hist named vector of actual deregistrations before the forward window
#' @param alpha quota scale for the category
#' @param response one row of renewal_response() for the category
#' @param injection_per_qtr extra certificates for this category per quarter
#' @return data.frame with baseline and scenario deregistrations and quota by month
#' @export
apply_scenario <- function(fwd, y_hist, alpha, response, pqp_target = NA_real_,
                           renewal_shift = 0, injection_per_qtr = 0) {
  fwd <- fwd[order(as.Date(fwd$month)), ]
  months <- as.Date(fwd$month)
  pqp_scn <- pqp_glide(as.numeric(fwd$pqp_base), pqp_target)
  slope <- if (is.null(response) || !nrow(response)) 0 else as.numeric(response$slope_log_pqp[[1]])
  delta_share <- slope * (log(pqp_scn) - log(as.numeric(fwd$pqp_base))) + as.numeric(renewal_shift)
  delta_dereg <- -delta_share * as.numeric(fwd$due_flow)
  cols <- c("mid", "lo50", "hi50", "lo80", "hi80")
  scn <- fwd
  for (col in cols) scn[[col]] <- pmax(as.numeric(fwd[[col]]) + delta_dereg, 0)

  quota_path <- function(path) {
    vapply(seq_along(months), function(h) {
      keys <- as.character(add_months(months[h], -11:0))
      in_hist <- keys %in% names(y_hist)
      base <- sum(as.numeric(y_hist[keys[in_hist]]))
      idx <- match(keys[!in_hist], as.character(months))
      idx <- idx[!is.na(idx)]
      alpha * (base + sum(path[idx]))
    }, numeric(1))
  }
  inj_month <- as.numeric(injection_per_qtr) / 3
  out <- data.frame(
    month = months,
    quarter = quarter_start(months),
    pqp_base = as.numeric(fwd$pqp_base),
    pqp_scenario = pqp_scn,
    renewal_share_delta = delta_share,
    due_flow = as.numeric(fwd$due_flow),
    dereg_base = as.numeric(fwd$mid),
    dereg_scenario = scn$mid,
    dereg_scenario_lo80 = scn$lo80,
    dereg_scenario_hi80 = scn$hi80,
    quota_base = quota_path(as.numeric(fwd$mid)),
    quota_base_lo80 = quota_path(as.numeric(fwd$lo80)),
    quota_base_hi80 = quota_path(as.numeric(fwd$hi80)),
    quota_scenario = quota_path(scn$mid) + inj_month,
    quota_scenario_lo80 = quota_path(scn$lo80) + inj_month,
    quota_scenario_hi80 = quota_path(scn$hi80) + inj_month,
    stringsAsFactors = FALSE
  )
  out
}

#' Quarterly rollup of apply_scenario() output.
#' @export
rollup_quarters <- function(monthly) {
  numeric_cols <- names(monthly)[vapply(monthly, is.numeric, logical(1))]
  cols <- setdiff(numeric_cols, c("pqp_base", "pqp_scenario", "renewal_share_delta"))
  q <- unique(monthly$quarter)
  rows <- lapply(q, function(qs) {
    d <- monthly[monthly$quarter == qs, ]
    r <- data.frame(quarter = qs, months = nrow(d), stringsAsFactors = FALSE)
    for (col in cols) r[[col]] <- sum(as.numeric(d[[col]]))
    r$pqp_scenario <- mean(d$pqp_scenario)
    r$renewal_share_delta <- mean(d$renewal_share_delta)
    r
  })
  do.call(rbind, rows)
}

#' Split a car-COE injection across A and B by recent published quota share.
#' @export
injection_split <- function(published, total_per_qtr, months_back = 12L) {
  p <- published[order(published$month), ]
  last <- max(p$month)
  recent <- p[p$month > add_months(last, -months_back), ]
  tot <- sum(recent$published)
  setNames(vapply(CATS, function(cat) {
    total_per_qtr * sum(recent$published[recent$category == cat]) / max(tot, 1)
  }, numeric(1)), CATS)
}
