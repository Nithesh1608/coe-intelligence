# =============================================================================
# exposure_functions.R — premium simulator, dealer and fleet calculators
# Everything is passed in as arguments; nothing here reads files.
# =============================================================================

EX_PER_QTR <- 6

# Next n exercise dates (1st/3rd Wednesday approximation) after a given month
next_exercises <- function(from_month, n) {
  m <- rep(from_month %m+% months(0:ceiling(n / 2)), each = 2)
  d <- m + days(rep(c(2, 16), length.out = length(m)))
  d <- d + days((4 - wday(d) + 7) %% 7)
  d[d >= from_month %m+% months(1)][seq_len(n)]
}

# -----------------------------------------------------------------------------
# simulate_premiums
#   dlog_hist   historical exercise-to-exercise log changes for the category
#   p0, pqp0    starting premium and PQP ($)
#   elasticity  dlogP / dlogQ from price_sensitivity (used only with tilt)
#   quota_path  calibrated quarterly quota path (length >= 2) for the tilt
#   Returns list(qp, pqp): n_sims x n_ex matrices, plus exercise dates.
# -----------------------------------------------------------------------------
simulate_premiums <- function(dlog_hist, p0, pqp0, start_month, elasticity = 0,
                              n_ex = 24, n_sims = 5000, vol_mult = 1,
                              supply_tilt = TRUE, quota_path = NULL, block = 2) {
  x <- dlog_hist[is.finite(dlog_hist)]
  drift <- rep(0, n_ex)
  if (supply_tilt && !is.null(quota_path) && length(quota_path) >= 2) {
    dq    <- c(0, diff(log(quota_path)))
    drift <- rep(elasticity * dq / EX_PER_QTR, each = EX_PER_QTR)[seq_len(n_ex)]
    drift[is.na(drift)] <- 0
  }
  nb     <- ceiling(n_ex / block)
  starts <- sample(seq_len(length(x) - block + 1), n_sims * nb, replace = TRUE)
  idx    <- as.vector(outer(0:(block - 1), starts, `+`))          # block-major
  shocks <- matrix(x[idx], nrow = n_sims, byrow = TRUE)[, seq_len(n_ex), drop = FALSE]
  shocks <- (shocks - mean(x)) * vol_mult + matrix(drift, n_sims, n_ex, byrow = TRUE)
  logp   <- log(p0) + t(apply(shocks, 1, cumsum))
  qp     <- exp(logp)
  # PQP in force at exercise j = mean of the 6 exercises before j; seed the
  # window with the current PQP (vectorised trailing mean)
  full   <- cbind(matrix(pqp0, n_sims, 6), qp)
  cs     <- t(apply(full, 1, cumsum))
  # mean of columns j..j+5 of `full` = (cs[, j+5] - cs[, j-1]) / 6
  pqp_m  <- sapply(seq_len(n_ex), function(j) (cs[, j + 5] - (if (j > 1) cs[, j - 1] else 0)) / 6)
  if (is.null(dim(pqp_m))) pqp_m <- matrix(pqp_m, nrow = n_sims)
  list(qp = qp, pqp = pqp_m, dates = next_exercises(start_month, n_ex))
}

# -----------------------------------------------------------------------------
# dealer_exposure
#   order_book: order_id, category, quoted_premium, target_exercise,
#               max_attempts, contract (guaranteed|rebate|capped), cap_topup
#   sims:       list by category from simulate_premiums()
# -----------------------------------------------------------------------------
dealer_exposure <- function(order_book, sims, p_success = 0.70, shift = 0,
                            allow_cat_E = FALSE, sims_E = NULL, e_switch_cost = 2000) {
  n_sims <- nrow(sims[[1]]$qp); n_ex <- ncol(sims[[1]]$qp)
  per_order <- pmap_dfr(order_book, function(order_id, category, quoted_premium, target_exercise,
                                             max_attempts, contract, cap_topup, ...) {
    qp <- sims[[category]]$qp
    t0 <- pmin(pmax(target_exercise + shift, 1), n_ex)
    attempts <- matrix(runif(n_sims * max_attempts) < p_success, nrow = n_sims)
    first_ok <- apply(attempts, 1, function(a) { w <- which(a); if (length(w)) w[1] else NA })
    ex_idx   <- pmin(t0 + coalesce(first_ok, max_attempts) - 1, n_ex)
    realised <- qp[cbind(seq_len(n_sims), ex_idx)]
    filled_E <- rep(FALSE, n_sims)
    if (allow_cat_E && !is.null(sims_E)) {
      e_px <- sims_E$qp[cbind(seq_len(n_sims), ex_idx)]
      filled_E <- e_px + e_switch_cost < realised
      realised <- ifelse(filled_E, e_px + e_switch_cost, realised)
    }
    unfilled <- is.na(first_ok)
    diff <- realised - quoted_premium
    dealer_loss <- switch(contract,
                          guaranteed = diff,
                          rebate     = rep(0, n_sims),
                          capped     = pmin(pmax(diff, 0), cap_topup) + pmin(diff, 0),
                          diff)
    tibble(order_id, category, contract, sim = seq_len(n_sims),
           realised_premium = realised, exercise_idx = ex_idx,
           attempts_used = coalesce(first_ok, max_attempts),
           unfilled, filled_E, dealer_loss, customer_topup = diff - dealer_loss)
  })
  portfolio <- per_order |> group_by(sim) |>
    summarise(total_cost = sum(realised_premium), dealer_pnl = -sum(dealer_loss),
              customer_topup = sum(customer_topup), n_unfilled = sum(unfilled),
              n_E = sum(filled_E), .groups = "drop")
  summary <- portfolio |> summarise(
    expected_cost = mean(total_cost), cost_p10 = quantile(total_cost, .1),
    cost_p50 = median(total_cost), cost_p90 = quantile(total_cost, .9),
    expected_dealer_pnl = mean(dealer_pnl), pnl_p10 = quantile(dealer_pnl, .1),
    var95_loss = -quantile(dealer_pnl, .05),
    cvar95_loss = -mean(dealer_pnl[dealer_pnl <= quantile(dealer_pnl, .05)]),
    prob_loss = mean(dealer_pnl < 0), expected_customer_topup = mean(customer_topup),
    expected_unfilled = mean(n_unfilled), share_filled_E = mean(n_E) / nrow(order_book)) |>
    mutate(shift = shift)
  list(per_order = per_order, portfolio = portfolio, summary = summary)
}

timing_shift <- function(order_book, sims, shifts = c(0, 3, 6, 12), ...) {
  map_dfr(shifts, function(s) dealer_exposure(order_book, sims, shift = s, ...)$summary) |>
    mutate(shift_quarters = shift / EX_PER_QTR)
}

# -----------------------------------------------------------------------------
# fleet_decision  (per COE-year, with optional vehicle capital cost)
#   schedule: quarter, category, n_vehicles, parf_rebate,
#             optional new_vehicle_cost (ex-COE capex of the replacement) and
#             old_vehicle_residual (what the old car is worth if sold/scrapped)
#   Capex is amortised over the COE life so all options are per vehicle-year.
# -----------------------------------------------------------------------------
fleet_decision <- function(schedule, sims, shift_quarters = 0, include_capex = FALSE) {
  n_sims <- nrow(sims[[1]]$qp)
  if (!"new_vehicle_cost" %in% names(schedule))  schedule$new_vehicle_cost  <- 0
  if (!"old_vehicle_residual" %in% names(schedule)) schedule$old_vehicle_residual <- 0
  pmap_dfr(schedule, function(quarter, category, n_vehicles, parf_rebate,
                              new_vehicle_cost, old_vehicle_residual, ...) {
    q  <- quarter + shift_quarters
    ex <- pmin(max(1, (q - 1) * EX_PER_QTR + 3), ncol(sims[[category]]$qp))
    qp <- sims[[category]]$qp[, ex]; pq <- sims[[category]]$pqp[, ex]
    capex_new <- if (include_capex) new_vehicle_cost - old_vehicle_residual else 0
    tibble(quarter, category, n_vehicles, sim = seq_len(n_sims),
           bid_new_cash   = n_vehicles * (qp + capex_new),
           renew_10y_cash = n_vehicles * (pq + parf_rebate),
           renew_5y_cash  = n_vehicles * (0.5 * pq + parf_rebate),
           bid_new_py     = bid_new_cash / 10,
           renew_10y_py   = renew_10y_cash / 10,
           renew_5y_py    = renew_5y_cash / 5,
           best_py        = pmin(bid_new_py, renew_10y_py, renew_5y_py),
           best_option    = c("bid_new", "renew_10y", "renew_5y")[
             max.col(-cbind(bid_new_py, renew_10y_py, renew_5y_py))])
  }) |>
    group_by(quarter, category, n_vehicles) |>
    summarise(across(c(bid_new_cash, renew_10y_cash, renew_5y_cash,
                       bid_new_py, renew_10y_py, renew_5y_py, best_py),
                     list(mean = mean, p10 = ~quantile(.x, .1), p90 = ~quantile(.x, .9))),
              share_bid_best = mean(best_option == "bid_new"),
              share_10y_best = mean(best_option == "renew_10y"),
              share_5y_best  = mean(best_option == "renew_5y"), .groups = "drop") |>
    mutate(shift_quarters = shift_quarters)
}

# -----------------------------------------------------------------------------
# total_vehicle_cost from the Car Cost Update table
# -----------------------------------------------------------------------------
total_vehicle_cost <- function(car_cost, make, model, coe_premium) {
  row <- car_cost |> filter(.data$make == !!make, .data$model == !!model) |> slice(1)
  if (nrow(row) == 0) return(NULL)
  row |> transmute(make, model, coe_category, omv_avg, arf_tiered, gst_excise, ves_amount,
                   eeai_amount, regn_fee, coe_premium = !!coe_premium,
                   basic_cost_incl_coe = omv_avg + gst_excise + arf_tiered + ves_amount +
                     eeai_amount + regn_fee + coe_premium,
                   coe_share = coe_premium / basic_cost_incl_coe)
}