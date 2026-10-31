# Black/orange echarts twins of Market Deeper-context gallery charts.
# Prep pipelines are copied from app/logic/eda/render_gallery.R (same values).
# PNGs stay; these sit beside them for comparison.

box::use(
  dplyr[
    bind_rows, filter, select, mutate, if_else, arrange, group_by, summarise,
    ungroup, inner_join, left_join, transmute, slice_tail, n, lag, lead, rename
  ],
  tidyr[pivot_longer, pivot_wider],
  lubridate[days, floor_date, `%m+%`, `%m-%`, interval],
  echarts4r[
    e_charts, e_line, e_area, e_scatter, e_heatmap,
    e_color, e_visual_map, e_datazoom, e_x_axis, e_y_axis,
    e_tooltip, e_legend, e_grid, e_lm, e_labels
  ],
  app/logic/viewdata/charts[style_chart],
  app/logic/viewdata/store[project_root],
)

`%||%` <- function(a, b) {
  if (is.null(a) || !length(a) || (length(a) == 1L && is.na(a))) b else a
}

pal <- c(A = "#ff5c1a", B = "#e4e4e7", rule = "#737373", topup = "#ff5c1a", lime = "#c8f542")

shift_mo <- function(dates, n) {
  dates <- as.Date(dates)
  as.Date(vapply(seq_along(dates), function(i) {
    as.numeric(seq(dates[[i]], by = sprintf("%d months", as.integer(n)), length.out = 2L)[[2]])
  }, numeric(1)), origin = "1970-01-01")
}

quota_quarter <- function(m) {
  # Same as render_gallery: floor_date(m %m-% months(1), "quarter") %m+% months(1)
  floor_date(shift_mo(m, -1L), "quarter") |> shift_mo(1L)
}

#' Load the same objects render_gallery.R uses and build the same frames.
#' @export
prep_gallery <- function(root = project_root()) {
  data_dir <- file.path(root, "data", "processed")
  rds <- function(name) readRDS(file.path(data_dir, name))
  as_d <- function(x) {
    if (is.null(x)) return(x)
    if ("month" %in% names(x)) x$month <- as.Date(x$month)
    if ("category" %in% names(x)) x$category <- as.character(x$category)
    x
  }
  coe      <- as_d(rds("coe_results.rds"))
  singstat <- as_d(rds("singstat_coe.rds"))
  dereg    <- as_d(rds("dereg_lta.rds"))
  age      <- as_d(rds("pop_by_age.rds"))
  pqp      <- as_d(rds("pqp.rds"))

  regimes <- tibble::tibble(
    start  = as.Date(c("2002-01-01", "2014-02-01", "2022-05-01")),
    end    = as.Date(c("2014-01-31", "2022-04-30", "2026-12-31")),
    regime = c("R1: Cat A = up to 1,600cc", "R2: plus a 97 kW power cap", "R3: EVs sorted by a 110 kW cap")
  )
  policy <- tibble::tibble(
    date  = as.Date(c("2013-03-01", "2018-02-01", "2020-04-01", "2023-05-01", "2025-02-01")),
    label = c("Loan curbs", "0% car growth", "COVID pause", "Cut and fill", "Extra COEs")
  )
  r3_start <- as.Date("2022-05-01")

  # ---- g0 premiums (render_gallery) ----
  premiums <- bind_rows(
    singstat |> filter(category %in% c("A", "B"), month < as.Date("2010-01-01")) |>
      select(month, bidding_no, category, premium),
    coe |> filter(category %in% c("A", "B")) |> select(month, bidding_no, category, premium)
  ) |>
    filter(!is.na(premium)) |>
    mutate(round_date = month + days(if_else(bidding_no == 1, 0L, 14L)))

  # ---- quarters / rule (render_gallery) ----
  quarters <- coe |>
    filter(category %in% c("A", "B"), !is.na(premium)) |>
    mutate(q_start = quota_quarter(month)) |>
    group_by(category, q_start) |>
    summarise(
      quota = sum(quota), bids = sum(bids_received), premium = mean(premium),
      rounds = n(), .groups = "drop"
    ) |>
    filter(rounds == 6) |>
    mutate(bids_per_coe = bids / quota)

  last12 <- dereg |>
    filter(category %in% c("A", "B")) |>
    arrange(category, month) |>
    group_by(category) |>
    mutate(last12 = cumsum(deregistrations_lta) - lag(cumsum(deregistrations_lta), 12)) |>
    ungroup() |>
    mutate(q_start = shift_mo(month, 2L)) |>
    select(q_start, category, last12)

  rule <- quarters |>
    inner_join(last12, by = c("q_start", "category")) |>
    filter(q_start >= as.Date("2023-02-01")) |>
    mutate(rule = 0.9 * 0.25 * last12)

  area <- rule |>
    transmute(
      category, q_start,
      `From the 25% rule` = pmin(quota, rule),
      `LTA top-up` = pmax(quota - rule, 0)
    ) |>
    pivot_longer(-c(category, q_start), names_to = "part", values_to = "coes")

  # ---- g2 age heat (render_gallery) ----
  heat <- age |> filter(vehicle_type == "Cars", age_lower <= 15)
  latest_month <- max(age$month)
  cars_age <- age |>
    filter(vehicle_type == "Cars", month == latest_month) |>
    arrange(age_lower)
  aged_7_10 <- sum(cars_age$population[cars_age$age_lower %in% 7:9])
  aged_2_5  <- sum(cars_age$population[cars_age$age_lower %in% 2:4])

  # ---- g4 owner response (render_gallery) ----
  aug <- age |>
    filter(vehicle_type == "Cars", lubridate::month(month) == 8) |>
    transmute(year = lubridate::year(month), a = pmin(age_lower, 20), population) |>
    group_by(year, a) |> summarise(pop = sum(population), .groups = "drop")
  prem_sa <- coe |>
    filter(category %in% c("A", "B"), !is.na(premium)) |>
    mutate(year = lubridate::year(shift_mo(month, 4L))) |>
    group_by(year) |> summarise(premium = mean(premium), quota = sum(quota), .groups = "drop")
  pqp_sa <- pqp |>
    filter(category %in% c("A", "B"), !is.na(pqp)) |>
    mutate(year = lubridate::year(shift_mo(month, 4L))) |>
    group_by(year) |> summarise(pqp = mean(pqp), .groups = "drop")
  drivers <- aug |>
    mutate(year = year + 1, a_next = pmin(a + 1, 20)) |>
    group_by(year, a_next) |> summarise(start = sum(pop), .groups = "drop") |>
    inner_join(aug |> rename(a_next = a, end = pop), by = c("year", "a_next")) |>
    group_by(year) |>
    summarise(
      early = sum(start[a_next %in% 1:9] - end[a_next %in% 1:9]) / sum(start[a_next %in% 1:9]),
      y10 = 1 - end[a_next == 10] / start[a_next == 10],
      .groups = "drop"
    ) |>
    inner_join(prem_sa, by = "year") |>
    inner_join(pqp_sa, by = "year")

  # ---- g7 % YoY (render_gallery) ----
  changes <- quarters |>
    arrange(category, q_start) |>
    group_by(category) |>
    mutate(
      q_lag = lag(q_start, 4),
      quota_change = quota / lag(quota, 4) - 1,
      premium_change = premium / lag(premium, 4) - 1
    ) |>
    ungroup() |>
    filter(!is.na(premium_change), q_lag == shift_mo(q_start, -12L))

  # ---- g5 bands (render_gallery) ----
  bands <- quarters |>
    mutate(
      band = cut(
        premium, c(0, 50000, 100000, Inf),
        labels = c("Under S$50k", "S$50k to 100k", "Over S$100k")
      )
    )
  band_med <- bands |>
    group_by(band) |>
    summarise(med = stats::median(bids_per_coe), n = n(), .groups = "drop")

  # ---- g6 moves (render_gallery) ----
  all_rounds <- coe |>
    filter(category %in% c("A", "B"), !is.na(premium)) |>
    arrange(category, month, bidding_no)
  now_prem <- all_rounds |> group_by(category) |> slice_tail(n = 1) |> select(category, now = premium)
  moves_all <- lapply(c(2, 6, 12), function(h) {
    all_rounds |>
      group_by(category) |>
      mutate(future = lead(premium, h), future_month = lead(month, h)) |>
      ungroup() |>
      filter(
        !is.na(future),
        lubridate::time_length(interval(month, future_month), "month") <= h / 2 + 1
      ) |>
      transmute(
        category, month,
        horizon = ifelse(h == 2, "1 month", paste(h / 2, "months")),
        pct = future / premium - 1
      )
  }) |>
    bind_rows() |>
    filter(month >= r3_start) |>
    left_join(now_prem, by = "category") |>
    mutate(
      change = pct * now,
      horizon = factor(horizon, levels = c("6 months", "3 months", "1 month"))
    )
  ridge_q <- moves_all |>
    group_by(category, horizon) |>
    summarise(
      p50 = stats::median(change), p90 = stats::quantile(change, 0.9),
      lo = stats::quantile(change, 0.01), n = n(), .groups = "drop"
    )
  moves_plot <- moves_all |>
    left_join(ridge_q |> select(category, horizon, lo), by = c("category", "horizon")) |>
    filter(change >= lo)

  list(
    premiums = premiums,
    regimes = regimes,
    policy = policy,
    area = area,
    heat = heat,
    aged_7_10 = aged_7_10,
    aged_2_5 = aged_2_5,
    drivers = drivers,
    changes = changes,
    bands = bands,
    band_med = band_med,
    moves_plot = moves_plot,
    ridge_q = ridge_q,
    quarters = quarters
  )
}

# ---- charts -----------------------------------------------------------------

#' @export
chart_g0_premium <- function(prep) {
  d <- prep$premiums
  wide <- pivot_wider(
    d |> transmute(label = format(round_date, "%Y-%m-%d"), category, premium),
    names_from = category, values_from = premium, values_fn = mean
  )
  wide <- wide[order(as.Date(wide$label)), ]
  p <- wide |>
    e_charts(label) |>
    e_line(A, name = "Cat A", symbol = "none", lineStyle = list(width = 1.6)) |>
    e_line(B, name = "Cat B", symbol = "none", lineStyle = list(width = 1.6)) |>
    e_color(c(pal[["A"]], pal[["B"]])) |>
    style_chart() |>
    e_datazoom(type = "inside") |>
    e_datazoom(type = "slider", bottom = 4, height = 18)
  # Policy guides on the A series
  marks <- lapply(seq_len(nrow(prep$policy)), function(i) {
    list(xAxis = format(prep$policy$date[[i]], "%Y-%m-%d"), name = prep$policy$label[[i]])
  })
  series <- p$x$opts$series
  if (length(series)) {
    series[[1]]$markLine <- list(
      symbol = "none",
      lineStyle = list(type = "dotted", color = "rgba(255,255,255,0.35)", width = 1),
      label = list(color = "#a3a3a3", fontSize = 10, formatter = "{b}"),
      data = marks
    )
    p$x$opts$series <- series
  }
  p
}

#' @export
chart_g1_topup <- function(prep, category = "A") {
  d <- prep$area |>
    filter(category == !!category) |>
    mutate(
      label = format(q_start, "%b %Y"),
      rule = ifelse(part == "From the 25% rule", coes, 0),
      topup = ifelse(part == "LTA top-up", coes, 0)
    )
  # one row per quarter
  q <- d |>
    group_by(label, q_start) |>
    summarise(rule = sum(rule), topup = sum(topup), .groups = "drop") |>
    arrange(q_start)
  q |>
    e_charts(label) |>
    e_area(rule, name = "From the 25% rule", stack = "q") |>
    e_area(topup, name = "LTA top-up", stack = "q") |>
    e_color(c(pal[["rule"]], pal[["topup"]])) |>
    style_chart() |>
    e_datazoom(type = "inside")
}

#' @export
chart_g2_heatmap <- function(prep) {
  d <- prep$heat |>
    transmute(
      month_lab = format(month, "%Y-%m"),
      age = as.character(age_lower),
      age_n = age_lower,
      value = round(population / 1000, 2),
      month_ord = month
    ) |>
    arrange(month_ord, age_n)
  ages <- as.character(0:15)
  d$age <- factor(d$age, levels = ages)
  d |>
    e_charts(month_lab) |>
    e_heatmap(age, value, name = "Cars (thousands)") |>
    e_visual_map(
      min = 0,
      max = max(d$value, na.rm = TRUE),
      calculable = TRUE,
      orient = "vertical",
      right = 0,
      top = 40,
      inRange = list(color = c("#1a1a1a", "#7c2d12", "#ff5c1a", "#c8f542")),
      textStyle = list(color = "#a3a3a3")
    ) |>
    e_grid(left = 48, right = 72, top = 40, bottom = 48) |>
    e_tooltip() |>
    e_x_axis(axisLabel = list(color = "#a3a3a3", fontFamily = "IBM Plex Mono", interval = 11)) |>
    e_y_axis(
      type = "category",
      data = ages,
      name = "Age (years)",
      axisLabel = list(color = "#a3a3a3")
    ) |>
    e_datazoom(type = "inside", x_index = 0)
}

#' @export
chart_g4_owner <- function(prep, which = c("early", "y10")) {
  which <- match.arg(which)
  d <- prep$drivers
  if (which == "early") {
    plot_df <- data.frame(
      x = d$premium / 1000,
      y = 100 * d$early,
      year = as.character(d$year),
      stringsAsFactors = FALSE
    )
    xlab <- "Avg COE premium (S$'000)"
    ylab <- "Share aged 1-9 scrapped (%)"
  } else {
    plot_df <- data.frame(
      x = d$pqp / 1000,
      y = 100 * d$y10,
      year = as.character(d$year),
      stringsAsFactors = FALSE
    )
    xlab <- "Avg PQP (S$'000)"
    ylab <- "Share scrapped at year 10 (%)"
  }
  plot_df |>
    e_charts(x) |>
    e_scatter(y, bind = year, symbolSize = 14, name = "Year") |>
    e_lm(y ~ x, name = "Trend", lineStyle = list(width = 2, color = pal[["A"]])) |>
    e_labels(formatter = "{b}", position = "top", color = "#a3a3a3", fontSize = 11) |>
    e_x_axis(name = xlab, nameTextStyle = list(color = "#a3a3a3")) |>
    e_y_axis(name = ylab, nameTextStyle = list(color = "#a3a3a3")) |>
    e_color(pal[["A"]]) |>
    style_chart()
}

#' @export
chart_g7_supply_price <- function(prep) {
  d <- prep$changes |>
    transmute(
      x = round(100 * quota_change, 2),
      y = round(100 * premium_change, 2),
      category,
      label = paste0(category, " · ", format(q_start, "%b %Y"))
    )
  # Two series via group_by so each gets its own lm
  d |>
    group_by(category) |>
    e_charts(x) |>
    e_scatter(y, bind = label, symbolSize = 10) |>
    e_lm(y ~ x, lineStyle = list(width = 2)) |>
    e_color(c(pal[["A"]], pal[["B"]])) |>
    e_x_axis(name = "Quota change vs year earlier (%)", nameTextStyle = list(color = "#a3a3a3")) |>
    e_y_axis(name = "Premium change vs year earlier (%)", nameTextStyle = list(color = "#a3a3a3")) |>
    style_chart()
}

#' @export
chart_g5_bids <- function(prep) {
  # Same points as the violin; show as box-like strip: band category + scatter
  d <- prep$bands |>
    filter(!is.na(band)) |>
    mutate(
      band = as.character(band),
      # light jitter in x for readability (same values as ggplot jitter intent)
      x = as.numeric(factor(band, levels = c("Under S$50k", "S$50k to 100k", "Over S$100k"))) +
        stats::runif(n(), -0.12, 0.12),
      band_i = as.numeric(factor(band, levels = c("Under S$50k", "S$50k to 100k", "Over S$100k")))
    )
  med <- prep$band_med |>
    mutate(
      band_i = as.numeric(factor(band, levels = c("Under S$50k", "S$50k to 100k", "Over S$100k"))),
      label = paste0("median ", sprintf("%.2f", med))
    )
  # Build scatter on numeric x, then category labels
  p <- d |>
    e_charts(x) |>
    e_scatter(bids_per_coe, symbolSize = 8, name = "Quarter") |>
    e_color(pal[["A"]]) |>
    e_x_axis(
      min = 0.5, max = 3.5,
      axisLabel = list(
        color = "#a3a3a3",
        formatter = htmlwidgets::JS(
          "function(v){var m={1:'Under S$50k',2:'S$50k to 100k',3:'Over S$100k'}; return m[Math.round(v)] || '';}"
        )
      )
    ) |>
    e_y_axis(name = "Bids per COE", nameTextStyle = list(color = "#a3a3a3")) |>
    style_chart()
  # Median markers as a second series
  med_df <- data.frame(x = med$band_i, y = med$med, label = med$label)
  # Append via e_data is awkward; overlay with markLine at 1.5
  series <- p$x$opts$series
  if (length(series)) {
    series[[1]]$markLine <- list(
      symbol = "none",
      data = list(list(yAxis = 1.5, name = "1.5")),
      lineStyle = list(type = "dashed", color = "rgba(255,255,255,0.4)"),
      label = list(color = "#a3a3a3", formatter = "1.5")
    )
    # median as markPoint
    series[[1]]$markPoint <- list(
      data = lapply(seq_len(nrow(med)), function(i) {
        list(coord = c(med$band_i[[i]], med$med[[i]]), value = med$label[[i]])
      }),
      symbol = "rect",
      symbolSize = c(28, 4),
      itemStyle = list(color = "#fafafa"),
      label = list(show = TRUE, formatter = "{c}", color = "#fafafa", fontSize = 10, position = "right")
    )
    p$x$opts$series <- series
  }
  p
}

#' Density curves standing in for ridges (same moves_plot values).
#' @export
chart_g6_risk <- function(prep, category = "A") {
  mp <- prep$moves_plot |> filter(category == !!category)
  dens <- lapply(levels(mp$horizon), function(hz) {
    x <- mp$change[mp$horizon == hz]
    if (!length(x) || sum(is.finite(x)) < 5) return(NULL)
    d <- stats::density(x[is.finite(x)], n = 128)
    # scale density to 0-1 then offset by horizon index for ridge stacking
    yi <- match(hz, levels(mp$horizon))
    data.frame(
      x = d$x,
      y = d$y / max(d$y) * 0.85 + (yi - 1),
      horizon = hz,
      stringsAsFactors = FALSE
    )
  })
  dens <- bind_rows(dens)
  if (!nrow(dens)) return(NULL)
  dens |>
    group_by(horizon) |>
    e_charts(x) |>
    e_area(y, name = NULL, stack = NULL) |>
    e_color(c(pal[["A"]], "#fb923c", "#fdba74")[seq_along(unique(dens$horizon))]) |>
    e_x_axis(name = "Change per car at today's premium", nameTextStyle = list(color = "#a3a3a3")) |>
    e_y_axis(
      show = TRUE,
      axisLabel = list(
        color = "#a3a3a3",
        formatter = htmlwidgets::JS(
          "function(v){var m={0:'6 months',1:'3 months',2:'1 month'}; return m[Math.round(v)] || '';}"
        )
      )
    ) |>
    style_chart() |>
    e_legend(show = TRUE)
}
