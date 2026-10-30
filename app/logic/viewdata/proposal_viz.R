# Proposal visualisations: fan, cohort, swing, spread/shares, feedback, waterfall.

box::use(
  echarts4r[
    e_charts, e_line, e_bar, e_area, e_tooltip, e_legend,
    e_grid, e_color, e_datazoom, e_x_axis, e_y_axis, e_labels
  ],
  app/logic/viewdata/charts[style_chart],
  app/logic/viewdata/pipeline[naive_path, quota_path, year_end_pop_c, add_months],
)

`%||%` <- function(a, b) {
  if (is.null(a) || !length(a) || (length(a) == 1L && is.na(a))) b else a
}

# ---- 1 Fan chart of projected quota -----------------------------------------

#' Build history + forward path with widening prediction intervals.
#' @export
prep_quota_fan <- function(store, category = "A", horizon_months = 24L) {
  live <- store$meta$live_model %||% "core_forecast"
  mape <- 0.12
  sc <- store$two_quota_scores
  if (!is.null(sc) && nrow(sc)) {
    row <- sc[sc$model == live & sc$category == category, , drop = FALSE]
    if (nrow(row) && is.finite(row$MAPE[[1]])) mape <- as.numeric(row$MAPE[[1]])
  }

  q <- store$quota_outlook
  if (!is.null(q) && nrow(q) && "outlook_from_forecast" %in% names(q)) {
    d <- q[as.character(q$category) == category, , drop = FALSE]
    d <- d[order(d$month), ]
    mid_col <- "outlook_from_forecast"
  } else {
    tq <- store$two_quota
    d <- tq[tq$model == live & tq$category == category, , drop = FALSE]
    d <- d[order(d$month), ]
    mid_col <- "outlook"
  }
  if (!nrow(d)) return(NULL)

  d$month <- as.Date(d$month)
  d$mid <- as.numeric(d[[mid_col]])
  d$published <- if ("published" %in% names(d)) as.numeric(d$published) else NA_real_

  latest_pub <- max(d$month[is.finite(d$published)], na.rm = TRUE)
  if (!is.finite(latest_pub)) latest_pub <- max(d$month, na.rm = TRUE)

  hist <- d[d$month <= latest_pub, c("month", "published", "mid"), drop = FALSE]
  names(hist)[names(hist) == "published"] <- "actual"
  # Collapse the band on history so e_band2 stays continuous into the forecast.
  hist$lo <- hist$mid
  hist$hi <- hist$mid
  hist$phase <- "history"

  # Prefer calibrated forward months after last published; else extend mid with bands.
  fwd <- d[d$month > latest_pub, , drop = FALSE]
  if (!nrow(fwd)) {
    last_mid <- utils::tail(hist$mid[is.finite(hist$mid)], 1)
    if (!length(last_mid)) last_mid <- utils::tail(hist$actual[is.finite(hist$actual)], 1)
    fwd <- data.frame(
      month = add_months(latest_pub, seq_len(as.integer(horizon_months))),
      mid = as.numeric(last_mid),
      stringsAsFactors = FALSE
    )
  } else {
    fwd <- fwd[seq_len(min(nrow(fwd), as.integer(horizon_months))), ]
    fwd <- fwd[, c("month", "mid"), drop = FALSE]
  }

  h <- seq_len(nrow(fwd))
  width <- pmax(fwd$mid, 1) * mape * 1.96 * sqrt(h / 12)
  fwd$actual <- NA_real_
  fwd$lo <- pmax(0, fwd$mid - width)
  fwd$hi <- fwd$mid + width
  fwd$phase <- "forecast"

  both <- rbind(
    hist[, c("month", "actual", "mid", "lo", "hi", "phase")],
    fwd[, c("month", "actual", "mid", "lo", "hi", "phase")]
  )
  both$label <- format(as.Date(both$month), "%b %Y")
  list(df = both, category = category, mape = mape, as_of = latest_pub)
}

#' @export
chart_quota_fan <- function(prep) {
  d <- prep$df
  if (is.null(d) || !nrow(d)) return(NULL)
  # Lo/hi envelope lines - portable across echarts4r versions (no e_band2).
  d |>
    e_charts(label) |>
    e_line(lo, name = "Low", symbol = "none",
           lineStyle = list(width = 1.2, opacity = 0.4, color = "#ff5c1a")) |>
    e_line(hi, name = "High", symbol = "none",
           lineStyle = list(width = 1.2, opacity = 0.4, color = "#ff5c1a")) |>
    e_line(actual, name = "Certificates released", symbol = "circle", symbolSize = 5,
           lineStyle = list(width = 2.4, color = "#fafafa")) |>
    e_line(mid, name = "Central supply path", symbol = "none",
           lineStyle = list(width = 2.2, color = "#ff5c1a")) |>
    e_color(c("#ff5c1a", "#ff5c1a", "#fafafa", "#ff5c1a")) |>
    style_chart() |>
    e_datazoom(type = "inside")
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
    # Prefer latest year with a full 12 months of renewal rows.
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
  # Attrition / early exits between registration and the year-10 decision.
  lost <- max(0, regs - due)
  # Quota identity (approx): 90% of 25% of A+B deregistrations land in A/B quota; 10% to E.
  into_ab <- 0.9 * 0.25 * dereg
  into_e <- 0.1 * 0.25 * dereg

  links <- data.frame(
    source = c(
      sprintf("Registered %s", reg_year),
      sprintf("Registered %s", reg_year),
      sprintf("Due in %s", exit_year),
      sprintf("Due in %s", exit_year),
      "Deregistered",
      "Deregistered"
    ),
    target = c(
      sprintf("Due in %s", exit_year),
      "Left earlier / other",
      "Renewed (PQP)",
      "Deregistered",
      "Cat A/B quota",
      "Cat E pool"
    ),
    value = c(due, lost, renewed, dereg, into_ab, into_e),
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
  # Stage bars render reliably on first paint; sankey often stays blank until resize.
  stages <- data.frame(
    stage = c(
      sprintf("Registered %s", prep$reg_year),
      sprintf("Due in %s", prep$exit_year),
      "Renewed (PQP)",
      "Deregistered",
      "Into Cat A/B quota",
      "Into Cat E pool"
    ),
    value = c(
      prep$due + max(0, sum(prep$links$value[prep$links$target == "Left earlier / other"], na.rm = TRUE)),
      prep$due,
      prep$renewed,
      prep$dereg,
      sum(prep$links$value[prep$links$target == "Cat A/B quota"], na.rm = TRUE),
      sum(prep$links$value[prep$links$target == "Cat E pool"], na.rm = TRUE)
    ),
    stringsAsFactors = FALSE
  )
  # Prefer exact registered total from links when present.
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

#' Policy markers for A/B/E premium history.
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
  b <- bidding[bidding$category %in% c("A", "B", "E") & !is.na(bidding$premium), ]
  b$month <- as.Date(b$month)
  b <- b[b$month >= as.Date(from), ]
  if (!nrow(b)) return(NULL)

  prem <- stats::reshape(
    b[, c("month", "category", "premium")],
    idvar = "month", timevar = "category", direction = "wide"
  )
  names(prem) <- gsub("premium.", "", names(prem), fixed = TRUE)
  for (nm in c("A", "B", "E")) if (!nm %in% names(prem)) prem[[nm]] <- NA_real_
  prem <- prem[order(prem$month), ]
  prem$label <- format(prem$month, "%b %Y")
  prem$spread_ab <- prem$B - prem$A

  q <- b[!is.na(b$quota), c("month", "category", "quota")]
  qw <- stats::reshape(q, idvar = "month", timevar = "category", direction = "wide")
  names(qw) <- gsub("quota.", "", names(qw), fixed = TRUE)
  for (nm in c("A", "B", "E")) if (!nm %in% names(qw)) qw[[nm]] <- 0
  qw$total <- rowSums(qw[, c("A", "B", "E")], na.rm = TRUE)
  qw <- qw[qw$total > 0, ]
  qw$share_A <- 100 * qw$A / qw$total
  qw$share_B <- 100 * qw$B / qw$total
  qw$share_E <- 100 * qw$E / qw$total
  qw <- qw[order(qw$month), ]
  qw$label <- format(qw$month, "%b %Y")

  list(premiums = prem, shares = qw, events = policy_events())
}

#' @export
chart_spread <- function(prep) {
  d <- prep$premiums
  # No e_mark_line: category-axis markLines crash echarts ("Cannot read ... 'coord'")
  # on some Posit/browser builds and can blank every chart on the page.
  d |>
    e_charts(label) |>
    e_line(A, name = "Cat A", symbol = "none", lineStyle = list(width = 2)) |>
    e_line(B, name = "Cat B", symbol = "none", lineStyle = list(width = 2)) |>
    e_line(E, name = "Cat E", symbol = "none", lineStyle = list(width = 1.8, type = "dashed")) |>
    e_color(c("#ff5c1a", "#e4e4e7", "#fb7185")) |>
    style_chart() |>
    e_datazoom(type = "inside")
}

#' @export
chart_volume_shares <- function(prep) {
  prep$shares |>
    e_charts(label) |>
    e_area(share_A, name = "Cat A %", stack = "share", smooth = TRUE) |>
    e_area(share_B, name = "Cat B %", stack = "share", smooth = TRUE) |>
    e_area(share_E, name = "Cat E %", stack = "share", smooth = TRUE) |>
    e_color(c("#ff5c1a", "#a3a3a3", "#fb7185")) |>
    style_chart() |>
    e_datazoom(type = "inside")
}

# ---- 5 Feedback loop explorer -----------------------------------------------

#' Renewal shift ? deregistration path ? replacement quota path.
#' @export
prep_feedback_scenario <- function(store, renewal_shift = 0, category = "A", horizon_months = 36L) {
  cats <- c("A", "B", "C", "D")
  naive_list <- lapply(cats, function(cat) {
    naive_path(store, cat, horizon_months = as.integer(horizon_months), renewal_shift = as.numeric(renewal_shift))
  })
  names(naive_list) <- cats
  pop_c <- year_end_pop_c(store)
  q <- quota_path(store, naive_list, pop_c)
  q <- q[q$category == category, , drop = FALSE]
  q <- q[order(q$month), ]
  # Quarterly rollup for a cleaner explorer line.
  q$qtr <- as.character(cut(as.Date(q$month), breaks = "quarter"))
  agg <- stats::aggregate(quota ~ qtr, q, sum)
  agg <- agg[order(agg$qtr), ]
  agg$label <- format(as.Date(agg$qtr), "%b %Y")
  base <- prep_feedback_scenario_base_cache(store, category, horizon_months)
  list(
    df = agg,
    base = base,
    category = category,
    renewal_shift = as.numeric(renewal_shift)
  )
}

# Baseline (shift = 0) cached per session via enclosing env would be nicer;
# compute inline when shift != 0 by calling again  keep simple helper:
prep_feedback_scenario_base_cache <- function(store, category, horizon_months) {
  cats <- c("A", "B", "C", "D")
  naive_list <- lapply(cats, function(cat) {
    naive_path(store, cat, horizon_months = as.integer(horizon_months), renewal_shift = 0)
  })
  names(naive_list) <- cats
  q <- quota_path(store, naive_list, year_end_pop_c(store))
  q <- q[q$category == category, ]
  q$qtr <- as.character(cut(as.Date(q$month), breaks = "quarter"))
  agg <- stats::aggregate(quota ~ qtr, q, sum)
  agg <- agg[order(agg$qtr), ]
  agg$label <- format(as.Date(agg$qtr), "%b %Y")
  agg
}

#' @export
chart_feedback_scenario <- function(prep) {
  d <- merge(
    prep$base[, c("label", "quota")],
    prep$df[, c("label", "quota")],
    by = "label",
    all = TRUE,
    suffixes = c("_base", "_scen")
  )
  # Keep scenario month order.
  ord <- match(prep$df$label, d$label)
  d <- d[ord[!is.na(ord)], ]
  names(d)[names(d) == "quota_base"] <- "baseline"
  names(d)[names(d) == "quota_scen"] <- "scenario"
  d |>
    e_charts(label) |>
    e_line(baseline, name = "Baseline renewal", symbol = "none",
           lineStyle = list(width = 1.6, type = "dashed", color = "#737373")) |>
    e_line(scenario, name = "With your renewal shift", symbol = "none",
           lineStyle = list(width = 2.4, color = "#ff5c1a")) |>
    e_color(c("#737373", "#ff5c1a")) |>
    style_chart() |>
    e_datazoom(type = "inside")
}

# ---- 6 Exposure waterfall ---------------------------------------------------

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
  # timing_shift uses exercise shifts 0,6,12 ? 0,1,2 quarters when EX_PER_QTR=6
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
