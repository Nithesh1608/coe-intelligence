# Curated EDA story panels — mixed chart types from the EDA notebooks (Cat A/B).

box::use(
  echarts4r[
    e_charts, e_bar, e_scatter, e_color, e_datazoom, e_y_axis,
    e_x_axis, e_labels
  ],
  app/logic/viewdata/charts[style_chart],
)

shift_months <- function(dates, n) {
  dates <- as.Date(dates)
  as.Date(vapply(seq_along(dates), function(i) {
    as.numeric(seq(dates[[i]], by = sprintf("%d months", as.integer(n)), length.out = 2L)[[2]])
  }, numeric(1)), origin = "1970-01-01")
}

quota_quarter_start <- function(m) {
  m <- as.Date(m)
  shifted <- shift_months(m, -1L)
  q <- as.Date(cut(shifted, breaks = "quarter"))
  shift_months(q, 1L)
}

#' @export
fmt_int <- function(x) {
  format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
}

# ---- 1 Age cliff (bars) ----------------------------------------------------

#' @export
prep_age_wave <- function(pop_by_age) {
  age <- pop_by_age
  age$month <- as.Date(age$month)
  latest <- max(age$month[age$vehicle_type == "Cars"], na.rm = TRUE)
  cars <- age[age$vehicle_type == "Cars" & age$month == latest, ]
  cars <- cars[order(cars$age_lower), ]
  cars$age_label <- ifelse(cars$age_lower >= 20, "20+", as.character(cars$age_lower))
  cars$band <- ifelse(
    cars$age_lower %in% 7:9, "Due by ~2029",
    ifelse(cars$age_lower %in% 2:4, "Thin cohort behind", "Other")
  )
  list(
    latest = latest,
    due = sum(cars$population[cars$age_lower %in% 7:9], na.rm = TRUE),
    thin = sum(cars$population[cars$age_lower %in% 2:4], na.rm = TRUE),
    df = cars
  )
}

#' @export
chart_age_wave <- function(prep) {
  d <- prep$df
  d$due <- ifelse(d$band == "Due by ~2029", d$population, 0)
  d$thin <- ifelse(d$band == "Thin cohort behind", d$population, 0)
  d$other <- ifelse(d$band == "Other", d$population, 0)
  d |>
    e_charts(age_label) |>
    e_bar(other, name = "Other ages", stack = "age") |>
    e_bar(due, name = "Due by ~2029", stack = "age") |>
    e_bar(thin, name = "Thin cohort behind", stack = "age") |>
    e_color(c("#525252", "#ff5c1a", "#c8f542")) |>
    style_chart()
}

# ---- 2 Locked-in 5y deregistrations (grouped bars) -------------------------

#' @export
prep_locked_exits <- function(revalidation) {
  r <- revalidation
  r$month <- as.Date(r$month)
  r <- r[r$category %in% c("A", "B") & r$tenure_years == 5, ]
  r$renew_year <- as.integer(format(r$month, "%Y"))
  agg <- stats::aggregate(revalidations ~ renew_year + category, r, sum)
  # keep years with 12 months of data
  n_mo <- stats::aggregate(month ~ renew_year + category, r, function(z) length(unique(z)))
  names(n_mo)[3] <- "n_months"
  agg <- merge(agg, n_mo, by = c("renew_year", "category"))
  agg <- agg[agg$n_months == 12, ]
  agg$exit_year <- agg$renew_year + 5L
  agg <- agg[agg$exit_year >= 2020, ]
  wide <- stats::reshape(
    agg[, c("exit_year", "category", "revalidations")],
    idvar = "exit_year", timevar = "category", direction = "wide"
  )
  names(wide) <- gsub("revalidations.", "", names(wide), fixed = TRUE)
  if (!("A" %in% names(wide))) wide$A <- 0
  if (!("B" %in% names(wide))) wide$B <- 0
  wide <- wide[order(wide$exit_year), ]
  wide$label <- as.character(wide$exit_year)
  wide$A[is.na(wide$A)] <- 0
  wide$B[is.na(wide$B)] <- 0
  tot <- wide$A + wide$B
  list(
    df = wide,
    avg_past = mean(tot[wide$exit_year %in% 2021:2024], na.rm = TRUE),
    avg_fut = mean(tot[wide$exit_year %in% 2026:2028], na.rm = TRUE)
  )
}

#' @export
chart_locked_exits <- function(prep) {
  prep$df |>
    e_charts(label) |>
    e_bar(A, name = "Category A") |>
    e_bar(B, name = "Category B") |>
    e_color(c("#ff5c1a", "#e4e4e7")) |>
    style_chart()
}

# ---- 3 Formula vs LTA discretion (stacked bars) ----------------------------

#' @export
prep_quota_discretion <- function(bids_raw, dereg_lta, e_share = 0.10) {
  d <- bids_raw
  d$month <- as.Date(d$month)
  d <- d[d$category %in% c("A", "B") & !is.na(d$quota), ]
  if ("suspended" %in% names(d)) {
    sus <- d$suspended
    d <- d[is.na(sus) | !as.logical(sus), ]
  }
  d$q_start <- quota_quarter_start(d$month)
  actual <- stats::aggregate(quota ~ q_start + category, d, sum)
  n_row <- stats::aggregate(quota ~ q_start + category, d, length)
  names(n_row)[3] <- "n"
  actual <- merge(actual, n_row, by = c("q_start", "category"))
  actual <- actual[actual$n >= 6, ]
  names(actual)[names(actual) == "quota"] <- "actual"

  dereg <- dereg_lta[dereg_lta$category %in% c("A", "B"), ]
  dereg$month <- as.Date(dereg$month)
  dereg <- dereg[order(dereg$category, dereg$month), ]
  dereg$last12 <- stats::ave(
    dereg$deregistrations_lta, dereg$category,
    FUN = function(x) as.numeric(stats::filter(x, rep(1, 12), sides = 1))
  )
  dereg$q_start <- shift_months(dereg$month, 2L)
  last12 <- dereg[, c("q_start", "category", "last12")]

  m <- merge(actual, last12, by = c("q_start", "category"))
  m <- m[m$q_start >= as.Date("2023-02-01") & is.finite(m$last12), ]
  m$rule <- (1 - e_share) * 0.25 * m$last12
  m$extra <- m$actual - m$rule
  m$formula <- pmin(m$actual, m$rule)
  m$added <- ifelse(m$extra > 0, m$extra, 0)
  m$held <- ifelse(m$extra < 0, -m$extra, 0)
  m <- m[order(m$category, m$q_start), ]
  m$label <- format(m$q_start, "%b %Y")
  share <- sum(m$extra) / sum(m$actual)
  list(df = m, share = share)
}

#' @export
chart_quota_discretion <- function(prep, category = "A") {
  d <- prep$df[prep$df$category == category, ]
  d |>
    e_charts(label) |>
    e_bar(formula, name = "25% rule", stack = "q") |>
    e_bar(added, name = "LTA added", stack = "q") |>
    e_bar(held, name = "LTA held back", stack = "q") |>
    e_color(c("#737373", "#3b82f6", "#ff5c1a")) |>
    style_chart() |>
    e_datazoom(type = "inside")
}

# ---- 4 Renewal vs PQP (scatter) --------------------------------------------

#' @export
prep_renewal_scatter <- function(revalidation, pop_by_age, pqp) {
  r <- revalidation
  r$month <- as.Date(r$month)
  r <- r[r$category %in% c("A", "B"), ]
  r$year <- as.integer(format(r$month, "%Y"))
  renew <- do.call(rbind, lapply(split(r, r$year), function(g) {
    data.frame(
      year = g$year[[1]],
      renew_total = sum(g$revalidations, na.rm = TRUE),
      n_months = length(unique(g$month)),
      stringsAsFactors = FALSE
    )
  }))
  renew <- renew[renew$n_months == 12, ]

  age <- pop_by_age
  age$month <- as.Date(age$month)
  turn10 <- age[age$vehicle_type == "Cars" & age$age_lower == 9 & as.integer(format(age$month, "%m")) == 12, ]
  turn10 <- stats::aggregate(population ~ month, turn10, sum)
  turn10$year <- as.integer(format(turn10$month, "%Y")) + 1L
  names(turn10)[names(turn10) == "population"] <- "cars_turning_10"

  pq <- pqp[pqp$category %in% c("A", "B") & !is.na(pqp$pqp), ]
  pq$month <- as.Date(pq$month)
  pq$year <- as.integer(format(pq$month, "%Y"))
  pqy <- stats::aggregate(pqp ~ year, pq, mean)

  out <- merge(renew, turn10[, c("year", "cars_turning_10")], by = "year")
  out <- merge(out, pqy, by = "year")
  out <- out[out$cars_turning_10 > 0, ]
  out$rate_pct <- 100 * out$renew_total / out$cars_turning_10
  out$pqp_k <- out$pqp / 1000
  out <- out[order(out$year), ]
  out$label <- as.character(out$year)
  ct <- if (nrow(out) >= 4) stats::cor(out$rate_pct, out$pqp_k, use = "complete.obs") else NA_real_
  list(df = out, cor = ct)
}

#' @export
chart_renewal_scatter2 <- function(prep) {
  d <- prep$df[, c("pqp_k", "rate_pct", "label")]
  names(d) <- c("pqp_k", "rate_pct", "year")
  d |>
    e_charts(pqp_k) |>
    e_scatter(rate_pct, bind = year, symbolSize = 16, name = "Year") |>
    e_labels(formatter = "{b}", position = "top", color = "#a3a3a3", fontSize = 11) |>
    e_x_axis(name = "Avg PQP (S$'000)") |>
    e_y_axis(name = "Renewal share of cars turning 10 (%)") |>
    e_color("#ff5c1a") |>
    style_chart()
}

# ---- 5 Quota Δ vs premium Δ (scatter) --------------------------------------

#' @export
prep_supply_price_scatter <- function(bids_raw) {
  d <- bids_raw
  d$month <- as.Date(d$month)
  d <- d[d$category %in% c("A", "B") & !is.na(d$premium) & !is.na(d$quota), ]
  d$q_start <- quota_quarter_start(d$month)
  q_sum <- stats::aggregate(quota ~ q_start + category, d, sum)
  p_mean <- stats::aggregate(premium ~ q_start + category, d, mean)
  q <- merge(q_sum, p_mean, by = c("q_start", "category"))
  q <- q[order(q$category, q$q_start), ]
  q$quota_yoy <- NA_real_
  q$prem_yoy <- NA_real_
  for (i in seq_len(nrow(q))) {
    hit <- which(
      q$category == q$category[i] &
        q$q_start == shift_months(q$q_start[i], -12L)
    )
    if (length(hit) == 1L) {
      q$quota_yoy[i] <- q$quota[i] - q$quota[hit]
      q$prem_yoy[i] <- q$premium[i] - q$premium[hit]
    }
  }
  q <- q[is.finite(q$quota_yoy) & is.finite(q$prem_yoy), ]
  q$regime <- ifelse(q$q_start < as.Date("2022-05-01"), "pre-R3", "R3")
  q$prem_yoy_k <- q$prem_yoy / 1000
  q3 <- q[q$regime == "R3", ]
  list(df = q3, all = q)
}

#' @export
chart_supply_price_scatter <- function(prep) {
  d <- prep$df
  plot_df <- data.frame(
    quota_yoy = d$quota_yoy,
    prem = d$prem_yoy_k,
    cat = d$category,
    stringsAsFactors = FALSE
  )
  plot_df |>
    e_charts(quota_yoy) |>
    e_scatter(prem, bind = cat, symbolSize = 12, name = "Quarter YoY") |>
    e_x_axis(name = "YoY change in quarterly certificates") |>
    e_y_axis(name = "YoY change in avg premium (S$'000)") |>
    e_color("#ff5c1a") |>
    style_chart()
}
