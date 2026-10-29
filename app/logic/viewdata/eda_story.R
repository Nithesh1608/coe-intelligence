# Story panels for the EDA page — curated Cat A/B supply narrative.

box::use(
  echarts4r[e_charts, e_line, e_bar, e_color, e_datazoom, e_y_axis],
  app/logic/viewdata/charts[style_chart],
)

shift_months <- function(dates, n) {
  dates <- as.Date(dates)
  as.Date(vapply(seq_along(dates), function(i) {
    as.numeric(seq(dates[[i]], by = sprintf("%d months", as.integer(n)), length.out = 2L)[[2]])
  }, numeric(1)), origin = "1970-01-01")
}

quota_quarter_start <- function(m) {
  # LTA quota quarters: Feb–Apr, May–Jul, Aug–Oct, Nov–Jan
  m <- as.Date(m)
  shifted <- shift_months(m, -1L)
  q <- as.Date(cut(shifted, breaks = "quarter"))
  shift_months(q, 1L)
}

monthly_quota_ab <- function(bids_raw) {
  d <- bids_raw[bids_raw$category %in% c("A", "B") & !is.na(bids_raw$quota), ]
  d$month <- as.Date(d$month)
  agg <- stats::aggregate(quota ~ month + category, d, sum, na.rm = TRUE)
  nbid <- stats::aggregate(
    bidding_no ~ month + category, d,
    function(z) length(unique(z))
  )
  names(nbid)[3] <- "n_rounds"
  m <- merge(agg, nbid, by = c("month", "category"), all.x = TRUE)
  m$quota[m$n_rounds < 2] <- NA_real_
  m
}

#' Beat 1 — certificates released over time
#' @export
prep_supply_swings <- function(bids_raw) {
  m <- monthly_quota_ab(bids_raw)
  wide <- stats::reshape(
    m[, c("month", "category", "quota")],
    idvar = "month", timevar = "category", direction = "wide"
  )
  names(wide) <- gsub("quota.", "", names(wide), fixed = TRUE)
  wide <- wide[order(wide$month), ]
  wide$label <- format(wide$month, "%b %Y")
  wide
}

#' @export
chart_supply_swings <- function(df) {
  df |>
    e_charts(label) |>
    e_line(A, name = "Category A", symbol = "none", lineStyle = list(width = 2.2)) |>
    e_line(B, name = "Category B", symbol = "none", lineStyle = list(width = 2.2)) |>
    e_color(c("#ff5c1a", "#e4e4e7")) |>
    style_chart() |>
    e_datazoom(type = "inside")
}

#' Beat 2 — 12-month exits vs next-quarter quota
#' @export
prep_exits_to_quota <- function(dereg_lta, bids_raw) {
  mq <- monthly_quota_ab(bids_raw)
  mq$q_start <- quota_quarter_start(mq$month)
  qtr <- stats::aggregate(
    quota ~ q_start + category,
    mq[!is.na(mq$quota), ],
    function(z) if (length(z) == 3L) sum(z) else NA_real_
  )
  qtr <- qtr[!is.na(qtr$quota) & qtr$q_start >= as.Date("2018-02-01"), ]

  dereg <- dereg_lta[dereg_lta$category %in% c("A", "B"), ]
  dereg$month <- as.Date(dereg$month)
  dereg <- dereg[order(dereg$category, dereg$month), ]
  dereg$dereg_12m <- stats::ave(
    dereg$deregistrations_lta,
    dereg$category,
    FUN = function(x) as.numeric(stats::filter(x, rep(1, 12), sides = 1))
  )
  dereg$window_end <- dereg$month
  qtr$window_end <- shift_months(qtr$q_start, -2L)
  out <- merge(
    qtr,
    dereg[, c("category", "window_end", "dereg_12m")],
    by = c("category", "window_end"),
    all.x = TRUE
  )
  out <- out[!is.na(out$dereg_12m) & !is.na(out$quota), ]
  out <- out[order(out$category, out$q_start), ]
  out$label <- format(out$q_start, "%b %Y")
  out
}

#' @export
chart_exits_to_quota <- function(df, category = "A") {
  d <- df[df$category == category, ]
  d |>
    e_charts(label) |>
    e_line(
      dereg_12m, name = "Exits in prior 12 months", symbol = "none",
      lineStyle = list(width = 2.2)
    ) |>
    e_line(
      quota, name = "Next-quarter certificates", symbol = "circle", symbolSize = 5,
      lineStyle = list(width = 2), y_index = 1
    ) |>
    e_y_axis(
      index = 1, splitLine = list(show = FALSE),
      axisLabel = list(color = "#737373", fontFamily = "IBM Plex Mono")
    ) |>
    e_color(c("#e4e4e7", "#ff5c1a")) |>
    style_chart() |>
    e_datazoom(type = "inside")
}

#' Beat 3 — age profile / due-stock wave
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

#' Beat 4 — renewals vs PQP (yearly A+B)
#' @export
prep_renewal_price <- function(renewal_rate) {
  r <- renewal_rate[renewal_rate$category %in% c("A", "B"), ]
  r$month <- as.Date(r$month)
  r$year <- as.integer(format(r$month, "%Y"))
  yr <- do.call(rbind, lapply(split(r, r$year), function(g) {
    data.frame(
      year = g$year[[1]],
      renewals = sum(g$revalidations, na.rm = TRUE),
      expiring = sum(g$coes_expiring, na.rm = TRUE),
      pqp = mean(g$pqp, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  yr <- yr[yr$expiring > 0 & is.finite(yr$pqp), ]
  yr$rate_pct <- 100 * yr$renewals / yr$expiring
  yr$pqp_k <- yr$pqp / 1000
  yr$label <- as.character(yr$year)
  yr[order(yr$year), ]
}

#' @export
chart_renewal_price <- function(df) {
  df |>
    e_charts(label) |>
    e_line(
      rate_pct, name = "Renewal rate (%)", symbol = "circle", symbolSize = 6,
      lineStyle = list(width = 2.2)
    ) |>
    e_line(
      pqp_k, name = "Avg PQP (S$'000)", symbol = "none",
      lineStyle = list(width = 2, type = "dashed"), y_index = 1
    ) |>
    e_y_axis(
      index = 1, splitLine = list(show = FALSE),
      axisLabel = list(color = "#737373", fontFamily = "IBM Plex Mono")
    ) |>
    e_color(c("#ff5c1a", "#a3a3a3")) |>
    style_chart()
}

#' Beat 5 — supply vs premium
#' @export
prep_supply_premium <- function(bids_raw, category = "A") {
  d <- bids_raw[bids_raw$category == category, ]
  d$month <- as.Date(d$month)
  q <- stats::aggregate(quota ~ month, d[!is.na(d$quota), ], sum, na.rm = TRUE)
  p <- stats::aggregate(premium ~ month, d[!is.na(d$premium), ], mean, na.rm = TRUE)
  out <- merge(q, p, by = "month", all = TRUE)
  out <- out[order(out$month), ]
  out <- out[out$month >= as.Date("2016-01-01"), ]
  out$label <- format(out$month, "%b %Y")
  out$premium_k <- out$premium / 1000
  out
}

#' @export
chart_supply_premium <- function(df) {
  df |>
    e_charts(label) |>
    e_line(quota, name = "Monthly certificates", symbol = "none", lineStyle = list(width = 2)) |>
    e_line(
      premium_k, name = "Avg premium (S$'000)", symbol = "none",
      lineStyle = list(width = 2.2), y_index = 1
    ) |>
    e_y_axis(
      index = 1, splitLine = list(show = FALSE),
      axisLabel = list(color = "#737373", fontFamily = "IBM Plex Mono")
    ) |>
    e_color(c("#e4e4e7", "#ff5c1a")) |>
    style_chart() |>
    e_datazoom(type = "inside")
}

#' @export
fmt_int <- function(x) {
  format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
}
