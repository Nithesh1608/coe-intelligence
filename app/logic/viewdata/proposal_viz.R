# Proposal visualisations: fan, cohort flow, within-quarter swing,
# spread/shares, exposure waterfall. The feedback/scenario explorer lives in
# app/logic/viewdata/scenario_viz.R.

box::use(
  echarts4r[
    e_charts, e_line, e_bar, e_area, e_band2,
    e_tooltip, e_legend, e_grid, e_color, e_datazoom,
    e_x_axis, e_y_axis, e_labels
  ],
  app/logic/viewdata/charts[style_chart],
  app/logic/viewdata/pipeline[add_months],
)

`%||%` <- function(a, b) {
  if (is.null(a) || !length(a) || (length(a) == 1L && is.na(a))) b else a
}

# ---- 1 Fan chart of projected quota -----------------------------------------

#' History (published monthly quota + held-out outlook) followed by the
#' production forward path with bootstrap 50% / 80% bands.
#' @export
prep_quota_fan <- function(store, category = "A", horizon_months = 36L, history_months = 36L) {
  fq <- store$forward_quota
  if (is.null(fq) || !nrow(fq)) return(NULL)
  fwd <- fq[as.character(fq$category) == category, , drop = FALSE]
  fwd <- fwd[order(as.Date(fwd$month)), , drop = FALSE]
  fwd <- fwd[seq_len(min(nrow(fwd), as.integer(horizon_months))), , drop = FALSE]
  if (!nrow(fwd)) return(NULL)

  # Published monthly quota (sum of the two exercises) for history.
  b <- store$bidding
  if ("partial" %in% names(b)) b <- b[!b$partial, ]
  b <- b[as.character(b$category) == category & is.finite(b$quota), c("month", "quota")]
  b <- b[order(b$month), ]
  # Held-out one-step outlook where available, to show the fit on history.
  q <- store$quota_outlook
  outlook <- if (!is.null(q)) q[as.character(q$category) == category, c("month", "outlook_from_forecast")] else NULL

  first_fwd <- min(as.Date(fwd$month))
  # Keep recent history so the forward fan is readable, not a stub on a decade chart.
  hist_from <- add_months(first_fwd, -as.integer(history_months))
  hist <- data.frame(month = as.Date(b$month), actual = as.numeric(b$quota), stringsAsFactors = FALSE)
  hist <- hist[hist$month >= hist_from & hist$month < first_fwd, ]
  hist$mid <- if (!is.null(outlook)) as.numeric(outlook$outlook_from_forecast[match(hist$month, as.Date(outlook$month))]) else NA_real_
  hist$lo80 <- NA_real_
  hist$hi80 <- NA_real_
  hist$lo50 <- NA_real_
  hist$hi50 <- NA_real_
  hist$phase <- "history"

  fwd_df <- data.frame(
    month = as.Date(fwd$month),
    actual = as.numeric(fwd$published),
    mid = as.numeric(fwd$mid),
    lo80 = as.numeric(fwd$lo80), hi80 = as.numeric(fwd$hi80),
    lo50 = as.numeric(fwd$lo50), hi50 = as.numeric(fwd$hi50),
    phase = "forecast",
    stringsAsFactors = FALSE
  )
  both <- rbind(hist, fwd_df)
  both$label <- format(both$month, "%b %Y")

  cal <- store$calibration
  cover <- NA_real_
  if (!is.null(cal) && nrow(cal)) {
    cc <- cal[as.character(cal$category) == category, ]
    if (nrow(cc)) cover <- stats::weighted.mean(cc$quota_cover80, cc$months, na.rm = TRUE)
  }
  list(
    df = both, category = category,
    as_of = max(hist$month, na.rm = TRUE),
    horizon = nrow(fwd_df),
    last_published = max(fwd_df$month[is.finite(fwd_df$actual)], hist$month, na.rm = TRUE),
    cover80 = cover
  )
}

#' @export
chart_quota_fan <- function(prep) {
  d <- prep$df
  if (is.null(d) || !nrow(d)) return(NULL)

  # Keep true gaps so Released stops when publications end.
  d$released <- ifelse(is.finite(d$actual), as.numeric(d$actual), NA_real_)
  d$lo80 <- ifelse(is.finite(d$lo80), as.numeric(d$lo80), NA_real_)
  d$hi80 <- ifelse(is.finite(d$hi80), as.numeric(d$hi80), NA_real_)
  d$lo50 <- ifelse(is.finite(d$lo50), as.numeric(d$lo50), NA_real_)
  d$hi50 <- ifelse(is.finite(d$hi50), as.numeric(d$hi50), NA_real_)

  # Outlook + bands start only after Released ends (anchor at the join for continuity).
  d$outlook <- NA_real_
  idx <- seq_len(nrow(d))
  last_rel <- suppressWarnings(max(idx[is.finite(d$released)]))
  if (is.finite(last_rel)) {
    keep_hist <- idx <= last_rel
    d$lo80[keep_hist] <- NA_real_
    d$hi80[keep_hist] <- NA_real_
    d$lo50[keep_hist] <- NA_real_
    d$hi50[keep_hist] <- NA_real_
    d$outlook[last_rel] <- d$released[last_rel]
    after <- idx > last_rel & is.finite(d$mid)
    d$outlook[after] <- as.numeric(d$mid[after])
  } else {
    d$outlook <- ifelse(is.finite(d$mid), as.numeric(d$mid), NA_real_)
  }

  y_vals <- c(d$released, d$outlook, d$lo80, d$hi80, d$lo50, d$hi50)
  y_vals <- y_vals[is.finite(y_vals)]
  y_min <- if (length(y_vals)) max(0, floor(min(y_vals) * 0.85 / 100) * 100) else 0
  y_max <- if (length(y_vals)) ceiling(max(y_vals) * 1.08 / 100) * 100 else 4000

  d |>
    e_charts(label) |>
    e_band2(
      lo80, hi80,
      name = "80% range",
      itemStyle = list(color = "rgba(255, 92, 26, 0.14)", borderWidth = 0)
    ) |>
    e_band2(
      lo50, hi50,
      name = "50% range",
      itemStyle = list(color = "rgba(200, 245, 66, 0.22)", borderWidth = 0)
    ) |>
    e_line(
      released, name = "Released",
      symbol = "circle", symbolSize = 4,
      connectNulls = FALSE,
      lineStyle = list(width = 2.6, color = "#fafafa"),
      itemStyle = list(color = "#fafafa")
    ) |>
    e_line(
      outlook, name = "Outlook",
      symbol = "none",
      connectNulls = FALSE,
      lineStyle = list(width = 2.8, color = "#ff5c1a"),
      itemStyle = list(color = "#ff5c1a")
    ) |>
    e_grid(left = 8, right = 16, top = 48, bottom = 20, containLabel = TRUE) |>
    e_tooltip(trigger = "axis") |>
    e_legend(
      data = list("Released", "Outlook", "50% range", "80% range"),
      right = 8, top = 4,
      textStyle = list(color = "#a3a3a3", fontSize = 11)
    ) |>
    e_x_axis(
      axisLine = list(show = FALSE),
      axisTick = list(show = FALSE),
      axisLabel = list(color = "#a3a3a3", fontFamily = "IBM Plex Mono", hideOverlap = TRUE),
      splitLine = list(show = FALSE)
    ) |>
    e_y_axis(
      name = "Certificates / month",
      min = y_min,
      max = y_max,
      nameTextStyle = list(color = "#737373", fontSize = 11),
      axisLine = list(show = FALSE),
      axisTick = list(show = FALSE),
      axisLabel = list(color = "#a3a3a3", fontFamily = "IBM Plex Mono"),
      splitLine = list(show = FALSE)
    ) |>
    e_datazoom(type = "inside")
}

#' Quarterly forward quota table for a category (for Home and the deck).
#' @export
forward_quarters <- function(store, category = "A", n = 12L) {
  fq <- store$forward_quota
  if (is.null(fq) || !nrow(fq)) return(NULL)
  d <- fq[as.character(fq$category) == category, ]
  d$quarter <- as.Date(d$quarter)
  full <- stats::aggregate(cbind(mid, lo80, hi80, published) ~ quarter, data = d,
                           FUN = function(z) sum(z), na.action = stats::na.pass)
  n_m <- stats::aggregate(month ~ quarter, data = d, FUN = length)
  full$months <- n_m$month[match(full$quarter, n_m$quarter)]
  full <- full[full$months == 3L, ]
  full <- full[order(full$quarter), ]
  utils::head(full, n)
}

# ---- 2 Cohort flow ----------------------------------------------------------

#' Trace one registration cohort into renewals, deregistrations, and quota.
#' @export
prep_cohort_flow <- function(store, exit_year = NULL) {
  panel <- store$panel
  rr <- store$renewal_rate
  if (is.null(panel) || is.null(rr) || !nrow(rr)) return(NULL)

  rr$year <- as.integer(format(as.Date(rr$month), "%Y"))
  yearly <- stats::aggregate(
    cbind(revalidations, deregistrations_lta, coes_expiring) ~ year,
    data = rr[rr$category %in% c("A", "B"), ],
    FUN = sum,
    na.rm = TRUE
  )
  yearly <- yearly[yearly$coes_expiring > 0, ]
  if (!nrow(yearly)) return(NULL)

  if (is.null(exit_year)) {
    n_mo <- stats::aggregate(month ~ year, rr[rr$category %in% c("A", "B"), ], function(z) length(unique(z)))
    full <- n_mo$year[n_mo$month >= 12]
    exit_year <- if (length(full)) max(full) else max(yearly$year)
  }
  exit_year <- as.integer(exit_year)
  row <- yearly[yearly$year == exit_year, , drop = FALSE]
  if (!nrow(row)) {
    exit_year <- max(yearly$year)
    row <- yearly[yearly$year == exit_year, , drop = FALSE]
  }

  reg_year <- exit_year - 10L
  panel$year <- as.integer(format(as.Date(panel$month), "%Y"))
  regs <- sum(panel$registrations[panel$year == reg_year & panel$category %in% c("A", "B")], na.rm = TRUE)
  if (!is.finite(regs) || regs <= 0) {
    regs <- as.numeric(row$coes_expiring[[1]])
  }

  due <- as.numeric(row$coes_expiring[[1]])
  renewed <- as.numeric(row$revalidations[[1]])
  dereg <- as.numeric(row$deregistrations_lta[[1]])
  lost <- max(0, regs - due)
  # Calibrated A/B quota scale (alpha x trailing-12 deregistrations, monthly).
  alpha <- 0.08
  if (!is.null(store$alphas) && nrow(store$alphas)) alpha <- mean(store$alphas$alpha, na.rm = TRUE)
  into_ab <- alpha * dereg

  links <- data.frame(
    source = c(
      sprintf("Registered %s", reg_year),
      sprintf("Registered %s", reg_year),
      sprintf("Due in %s", exit_year),
      sprintf("Due in %s", exit_year),
      "Deregistered"
    ),
    target = c(
      sprintf("Due in %s", exit_year),
      "Left earlier / other",
      "Renewed (PQP)",
      "Deregistered",
      "Cat A/B quota"
    ),
    value = c(due, lost, renewed, dereg, into_ab),
    stringsAsFactors = FALSE
  )
  links <- links[links$value > 0 & is.finite(links$value), ]
  list(
    links = links,
    exit_year = exit_year,
    reg_year = reg_year,
    years = sort(unique(yearly$year)),
    renewed = renewed,
    dereg = dereg,
    due = due
  )
}

#' Available exit years for the cohort control (no full store required).
#' @export
cohort_year_choices <- function(root) {
  p <- file.path(root, "data", "processed", "renewal_rate.rds")
  fallback <- list(choices = stats::setNames(2024L, "2024"), selected = 2024L)
  if (!file.exists(p)) return(fallback)
  rr <- tryCatch(readRDS(p), error = function(e) NULL)
  if (is.null(rr) || !nrow(rr)) return(fallback)
  rr$category <- as.character(rr$category)
  rr$month <- if (inherits(rr$month, "Date")) rr$month else as.Date(rr$month)
  rr <- rr[rr$category %in% c("A", "B"), , drop = FALSE]
  if (!nrow(rr)) return(fallback)
  rr$year <- as.integer(format(rr$month, "%Y"))
  n_mo <- stats::aggregate(month ~ year, rr, function(z) length(unique(z)))
  full <- n_mo$year[n_mo$month >= 12]
  years <- sort(unique(rr$year))
  if (!length(years)) return(fallback)
  selected <- if (length(full)) max(as.integer(full)) else max(years)
  list(
    choices = stats::setNames(as.integer(years), as.character(years)),
    selected = as.integer(selected)
  )
}

#' @export
chart_cohort_flow <- function(prep) {
  stages <- data.frame(
    stage = c(
      sprintf("Registered %s", prep$reg_year),
      sprintf("Due in %s", prep$exit_year),
      "Renewed (PQP)",
      "Deregistered",
      "Into Cat A/B quota"
    ),
    value = c(
      prep$due + max(0, sum(prep$links$value[prep$links$target == "Left earlier / other"], na.rm = TRUE)),
      prep$due,
      prep$renewed,
      prep$dereg,
      sum(prep$links$value[prep$links$target == "Cat A/B quota"], na.rm = TRUE)
    ),
    stringsAsFactors = FALSE
  )
  reg_v <- sum(prep$links$value[grepl("^Registered", prep$links$source)], na.rm = TRUE)
  if (is.finite(reg_v) && reg_v > 0) stages$value[1] <- reg_v
  stages$stage <- factor(stages$stage, levels = stages$stage)
  stages |>
    e_charts(stage) |>
    e_bar(value, name = "Vehicles / certificates", itemStyle = list(color = "#ff5c1a")) |>
    e_tooltip(trigger = "axis") |>
    e_legend(show = FALSE) |>
    style_chart()
}

# ---- 3 Within-quarter volatility --------------------------------------------

#' @export
load_premium_moves <- function(root) {
  p <- file.path(root, "data", "processed", "exposure", "premium_moves.rds")
  if (!file.exists(p)) return(NULL)
  readRDS(p)
}

#' @export
prep_within_quarter_swing <- function(premium_moves, category = "A") {
  if (is.null(premium_moves) || is.null(premium_moves$within_quarter_detail)) return(NULL)
  d <- premium_moves$within_quarter_detail
  d <- d[as.character(d$category) == category, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  d <- d[order(as.Date(d$qtr_start)), ]
  d$label <- format(as.Date(d$qtr_start), "%b %Y")
  d$swing_k <- as.numeric(d$swing) / 1000
  summary <- premium_moves$within_quarter
  summary <- summary[as.character(summary$category) == category, , drop = FALSE]
  list(df = d, summary = summary, category = category)
}

#' @export
chart_within_quarter_swing <- function(prep) {
  prep$df |>
    e_charts(label) |>
    e_bar(swing_k, name = "Max premium swing ($k)") |>
    e_color("#ff5c1a") |>
    style_chart() |>
    e_datazoom(type = "inside")
}

# ---- 4 Spread + volume shares -----------------------------------------------

#' Policy markers for A/B premium history.
#' @export
policy_events <- function() {
  data.frame(
    date = as.Date(c("2020-04-01", "2022-05-01")),
    label = c("Bidding suspended (COVID)", "Cat A/B redefined (EV power)"),
    stringsAsFactors = FALSE
  )
}

#' @export
prep_spread_and_shares <- function(bidding, from = as.Date("2016-01-01")) {
  b <- bidding[bidding$category %in% c("A", "B") & !is.na(bidding$premium), ]
  b$month <- as.Date(b$month)
  b <- b[b$month >= as.Date(from), ]
  if (!nrow(b)) return(NULL)

  prem <- stats::reshape(
    b[, c("month", "category", "premium")],
    idvar = "month", timevar = "category", direction = "wide"
  )
  names(prem) <- gsub("premium.", "", names(prem), fixed = TRUE)
  for (nm in c("A", "B")) if (!nm %in% names(prem)) prem[[nm]] <- NA_real_
  prem <- prem[order(prem$month), ]
  prem$label <- format(prem$month, "%b %Y")
  prem$spread_ab <- prem$B - prem$A

  q <- b[!is.na(b$quota), c("month", "category", "quota")]
  qw <- stats::reshape(q, idvar = "month", timevar = "category", direction = "wide")
  names(qw) <- gsub("quota.", "", names(qw), fixed = TRUE)
  for (nm in c("A", "B")) if (!nm %in% names(qw)) qw[[nm]] <- 0
  qw$total <- rowSums(qw[, c("A", "B")], na.rm = TRUE)
  qw <- qw[qw$total > 0, ]
  qw$share_A <- 100 * qw$A / qw$total
  qw$share_B <- 100 * qw$B / qw$total
  qw <- qw[order(qw$month), ]
  qw$label <- format(qw$month, "%b %Y")

  list(premiums = prem, shares = qw, events = policy_events())
}

#' @export
chart_spread <- function(prep) {
  d <- prep$premiums
  # No e_mark_line: category-axis markLines crash echarts on some builds.
  d |>
    e_charts(label) |>
    e_line(A, name = "Cat A", symbol = "none", lineStyle = list(width = 2)) |>
    e_line(B, name = "Cat B", symbol = "none", lineStyle = list(width = 2)) |>
    e_color(c("#ff5c1a", "#e4e4e7")) |>
    style_chart() |>
    e_datazoom(type = "inside")
}

#' @export
chart_volume_shares <- function(prep) {
  prep$shares |>
    e_charts(label) |>
    e_area(share_A, name = "Cat A %", stack = "share", smooth = TRUE) |>
    e_area(share_B, name = "Cat B %", stack = "share", smooth = TRUE) |>
    e_color(c("#ff5c1a", "#a3a3a3")) |>
    style_chart() |>
    e_datazoom(type = "inside")
}

# ---- 5 Exposure waterfall ---------------------------------------------------

#' Dealer book cost build-up from exposure summary + timing shifts.
#' @export
prep_exposure_waterfall <- function(base_summary, shifts, quoted_total) {
  s <- as.list(base_summary)
  sh <- as.data.frame(shifts)
  exp_cost <- as.numeric(s$expected_cost)
  p10 <- as.numeric(s$cost_p10)
  p90 <- as.numeric(s$cost_p90)
  quoted_total <- as.numeric(quoted_total)

  gap <- exp_cost - quoted_total
  adverse <- p90 - exp_cost
  favourable <- exp_cost - p10

  shift1 <- sh[as.numeric(sh$shift_quarters) == 1, , drop = FALSE]
  shift2 <- sh[as.numeric(sh$shift_quarters) == 2, , drop = FALSE]
  if (!nrow(shift1) && "shift" %in% names(sh)) {
    shift1 <- sh[as.numeric(sh$shift) == 6, , drop = FALSE]
    shift2 <- sh[as.numeric(sh$shift) == 12, , drop = FALSE]
  }

  timing_1 <- if (nrow(shift1)) as.numeric(shift1$expected_cost[[1]]) - exp_cost else NA_real_
  timing_2 <- if (nrow(shift2)) as.numeric(shift2$expected_cost[[1]]) - exp_cost else NA_real_

  steps <- data.frame(
    step = c(
      "Quoted book",
      "Clearing gap",
      "Expected cost",
      "Upside to p10",
      "Downside to p90",
      "If delay +1Q",
      "If delay +2Q"
    ),
    value = c(quoted_total, gap, exp_cost, -favourable, adverse, timing_1, timing_2),
    kind = c("total", "delta", "total", "delta", "delta", "delta", "delta"),
    stringsAsFactors = FALSE
  )
  steps <- steps[is.finite(steps$value), ]
  list(
    steps = steps,
    expected = exp_cost,
    p10 = p10,
    p90 = p90,
    quoted = quoted_total
  )
}

#' Waterfall via stacked helper bars (invisible base + visible delta).
#' @export
chart_exposure_waterfall <- function(prep) {
  st <- prep$steps
  n <- nrow(st)
  base <- numeric(n)
  rise <- numeric(n)
  fall <- numeric(n)
  running <- 0
  for (i in seq_len(n)) {
    v <- st$value[[i]]
    if (identical(st$kind[[i]], "total")) {
      base[[i]] <- 0
      if (v >= 0) {
        rise[[i]] <- v
        fall[[i]] <- 0
      } else {
        rise[[i]] <- 0
        fall[[i]] <- -v
      }
      running <- v
    } else {
      if (v >= 0) {
        base[[i]] <- running
        rise[[i]] <- v
        fall[[i]] <- 0
        running <- running + v
      } else {
        base[[i]] <- running + v
        rise[[i]] <- 0
        fall[[i]] <- -v
        running <- running + v
      }
    }
  }
  df <- data.frame(
    step = st$step,
    base = base / 1000,
    up = rise / 1000,
    down = fall / 1000,
    stringsAsFactors = FALSE
  )
  df |>
    e_charts(step) |>
    e_bar(base, name = "base", stack = "wf",
          itemStyle = list(color = "rgba(0,0,0,0)", borderColor = "rgba(0,0,0,0)")) |>
    e_bar(up, name = "Increase ($k)", stack = "wf", itemStyle = list(color = "#ff5c1a")) |>
    e_bar(down, name = "Decrease ($k)", stack = "wf", itemStyle = list(color = "#c8f542")) |>
    e_tooltip(trigger = "axis") |>
    e_legend(show = FALSE) |>
    style_chart()
}
