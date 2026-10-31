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
    e_charts, e_line, e_area, e_scatter, e_heatmap, e_band2,
    e_color, e_visual_map, e_datazoom, e_x_axis, e_y_axis,
    e_tooltip, e_legend, e_grid, e_lm, e_labels, e_mark_line, e_mark_area,
    e_mark_point
  ],
  app/logic/viewdata/store[project_root],
)

dl <- function(x) paste0("S$", format(round(x), big.mark = ",", trim = TRUE))
dk <- function(x) {
  if (!is.finite(x)) return("S$0")
  if (round(abs(x) / 1000) == 0) return("S$0")
  paste0(ifelse(x < 0, "-", "+"), "S$", round(abs(x) / 1000), "k")
}
pc <- function(x) paste0(round(100 * x), "%")

# Room for axis names + titles sitting above in the card header.
gallery_grid <- function(p, top = 36, bottom = 56, left = 16, right = 20) {
  p |>
    e_grid(left = left, right = right, top = top, bottom = bottom, containLabel = TRUE) |>
    e_tooltip(trigger = "item") |>
    e_legend(right = 8, top = 4, textStyle = list(color = "#a3a3a3", fontSize = 11))
}

axis_named <- function(p, xlab, ylab) {
  p |>
    e_x_axis(
      name = xlab,
      nameLocation = "middle",
      nameGap = 32,
      nameTextStyle = list(color = "#d4d4d4", fontSize = 12),
      axisLabel = list(color = "#a3a3a3", fontFamily = "IBM Plex Mono", fontSize = 11),
      axisLine = list(show = TRUE, lineStyle = list(color = "rgba(255,255,255,0.25)")),
      axisTick = list(show = TRUE, lineStyle = list(color = "rgba(255,255,255,0.25)")),
      splitLine = list(show = TRUE, lineStyle = list(color = "rgba(255,255,255,0.06)"))
    ) |>
    e_y_axis(
      name = ylab,
      nameLocation = "middle",
      nameGap = 48,
      nameTextStyle = list(color = "#d4d4d4", fontSize = 12),
      axisLabel = list(color = "#a3a3a3", fontFamily = "IBM Plex Mono", fontSize = 11),
      axisLine = list(show = TRUE, lineStyle = list(color = "rgba(255,255,255,0.25)")),
      axisTick = list(show = TRUE, lineStyle = list(color = "rgba(255,255,255,0.25)")),
      splitLine = list(show = TRUE, lineStyle = list(color = "rgba(255,255,255,0.06)"))
    )
}

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

  lo <- premiums |> filter(category == "A") |> dplyr::slice_min(premium, n = 1, with_ties = FALSE)
  hi <- premiums |> filter(category == "B") |> dplyr::slice_max(premium, n = 1, with_ties = FALSE)
  rule_q <- rule |>
    group_by(q_start) |>
    summarise(actual = sum(quota), rule = sum(rule), .groups = "drop") |>
    mutate(extra_share = (actual - rule) / actual)
  peak_extra <- rule_q |> dplyr::slice_max(extra_share, n = 1)
  last_extra <- rule_q |> dplyr::slice_max(q_start, n = 1)
  ct_early <- stats::cor.test(drivers$early, drivers$premium)
  ct_y10 <- stats::cor.test(drivers$y10, drivers$pqp)
  fmt_r <- function(ct) {
    paste0("r = ", sprintf("%.2f", unname(ct$estimate)), ", p = ", signif(ct$p.value, 2))
  }
  rq <- function(cat, hz, v) {
    ridge_q[[v]][ridge_q$category == cat & ridge_q$horizon == hz]
  }

  titles <- list(
    g0 = paste0(
      "Premiums have ranged from ", dl(lo$premium), " (Cat A, ", lubridate::year(lo$month), ") to ",
      dl(hi$premium), " (Cat B, ", lubridate::year(hi$month), ")"
    ),
    g1 = paste0(
      "LTA top-ups peaked at ", pc(peak_extra$extra_share),
      " of Cat A + B supply and are now ", pc(last_extra$extra_share)
    ),
    g2 = paste0(
      format(round(aged_7_10), big.mark = ",", trim = TRUE),
      " cars bought in 2017-19 reach year 10 by 2029; half as many follow"
    ),
    g2_note = paste0(
      "Year 10 = COE expires. Bought 2017-19: ",
      format(round(aged_7_10), big.mark = ",", trim = TRUE),
      " cars. Bought 2022-24: ",
      format(round(aged_2_5), big.mark = ",", trim = TRUE),
      " cars."
    ),
    g4 = "When COEs are dear, owners keep young cars; at year 10, a dear renewal pushes them to scrap",
    g4a = paste0("Young cars scrapped vs COE premium (", fmt_r(ct_early), ")"),
    g4b = paste0("Cars scrapped at year 10 vs renewal price (", fmt_r(ct_y10), ")"),
    g7 = "When the quota rises, do premiums fall? Only partly",
    g5 = paste0(
      "Bids per COE sits near ", round(stats::median(quarters$bids_per_coe), 1),
      " whether a COE costs S$30k or S$130k"
    ),
    g6 = paste0(
      "The longer the wait, the bigger the bad case: ",
      dk(rq("A", "6 months", "p90")), " per Cat A car after 6 months"
    )
  )

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
    quarters = quarters,
    titles = titles
  )
}

# ---- charts -----------------------------------------------------------------

snap_label <- function(labels, date) {
  dates <- as.Date(labels)
  labels[[which.min(abs(as.numeric(dates - as.Date(date))))]]
}

#' @export
chart_g0_premium <- function(prep) {
  d <- prep$premiums
  wide <- pivot_wider(
    d |> transmute(label = format(round_date, "%Y-%m-%d"), category, premium),
    names_from = category, values_from = premium, values_fn = mean
  )
  wide <- wide[order(as.Date(wide$label)), ]
  labs <- wide$label
  # Regime bands (R1 / R2 / R3) — same windows as render_gallery
  areas <- lapply(seq_len(nrow(prep$regimes)), function(i) {
    list(
      list(
        xAxis = snap_label(labs, prep$regimes$start[[i]]),
        itemStyle = list(
          color = c(
            "rgba(115,115,115,0.22)",
            "rgba(255,92,26,0.14)",
            "rgba(200,245,66,0.10)"
          )[[i]]
        ),
        name = c("R1", "R2", "R3")[[i]]
      ),
      list(xAxis = snap_label(labs, prep$regimes$end[[i]]))
    )
  })
  policy_marks <- lapply(seq_len(nrow(prep$policy)), function(i) {
    list(
      xAxis = snap_label(labs, prep$policy$date[[i]]),
      name = prep$policy$label[[i]]
    )
  })
  p <- wide |>
    e_charts(label) |>
    e_line(A, name = "Cat A", symbol = "none", lineStyle = list(width = 1.6)) |>
    e_line(B, name = "Cat B", symbol = "none", lineStyle = list(width = 1.6)) |>
    e_color(c(pal[["A"]], pal[["B"]])) |>
    gallery_grid(bottom = 72) |>
    axis_named("Bidding round", "COE premium (S$)") |>
    e_datazoom(type = "inside") |>
    e_datazoom(type = "slider", bottom = 8, height = 18)
  series <- p$x$opts$series
  if (length(series)) {
    series[[1]]$markArea <- list(
      silent = TRUE,
      label = list(
        show = TRUE, color = "#fafafa", fontSize = 11, fontWeight = "bold",
        position = "insideTop"
      ),
      data = areas
    )
    series[[1]]$markLine <- list(
      symbol = "none",
      lineStyle = list(type = "dotted", color = "rgba(255,255,255,0.45)", width = 1),
      label = list(color = "#d4d4d4", fontSize = 10, formatter = "{b}"),
      data = policy_marks
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
  q <- d |>
    group_by(label, q_start) |>
    summarise(rule = sum(rule), topup = sum(topup), .groups = "drop") |>
    arrange(q_start)
  # Same cut lines as render_gallery: Cut and fill / Extra COEs
  policy_dates <- as.Date(c("2023-05-01", "2025-02-01"))
  policy_labs <- c("Cut and fill", "Extra COEs")
  snap_q <- function(dt) {
    i <- which.min(abs(as.numeric(q$q_start - dt)))
    q$label[[i]]
  }
  p <- q |>
    e_charts(label) |>
    e_area(rule, name = "From the 25% rule", stack = "q") |>
    e_area(topup, name = "LTA top-up", stack = "q") |>
    e_color(c(pal[["rule"]], pal[["topup"]])) |>
    gallery_grid(top = 44) |>
    axis_named("Quota quarter", "COEs per quarter") |>
    e_datazoom(type = "inside")
  series <- p$x$opts$series
  if (length(series)) {
    series[[1]]$markLine <- list(
      symbol = "none",
      lineStyle = list(type = "dotted", color = "rgba(255,255,255,0.55)", width = 1.5),
      label = list(
        color = "#fafafa",
        fontSize = 11,
        fontWeight = "bold",
        formatter = "{b}",
        position = "insideEndTop"
      ),
      data = lapply(seq_along(policy_dates), function(i) {
        list(xAxis = snap_q(policy_dates[[i]]), name = policy_labs[[i]])
      })
    )
    p$x$opts$series <- series
  }
  p
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
  d$age <- as.character(d$age)
  month_labs <- unique(as.character(d$month_lab))
  snap_m <- function(ym) {
    target <- as.Date(paste0(ym, "-01"))
    dates <- as.Date(paste0(month_labs, "-01"))
    month_labs[[which.min(abs(as.numeric(dates - target)))]]
  }
  lab_big <- paste0(
    "Bought 2017-19: ",
    format(round(prep$aged_7_10), big.mark = ",", trim = TRUE),
    " cars"
  )
  lab_small <- paste0(
    "Bought 2022-24: ",
    format(round(prep$aged_2_5), big.mark = ",", trim = TRUE),
    " cars"
  )
  # Same annotate endpoints as render_gallery.R (segment + text)
  a1 <- list(x = snap_m("2018-01"), y = "1", x2 = snap_m("2026-04"), y2 = "9")
  a2 <- list(x = snap_m("2023-01"), y = "1", x2 = snap_m("2026-04"), y2 = "4")
  t1 <- list(x = snap_m("2021-09"), y = "6")
  t2 <- list(x = snap_m("2024-02"), y = "3")
  vmax <- max(d$value, na.rm = TRUE)
  vmin <- max(0, as.numeric(stats::quantile(d$value, 0.02, na.rm = TRUE)))

  p <- d |>
    e_charts(month_lab) |>
    e_heatmap(age, value, name = "Cars (thousands)") |>
    e_visual_map(
      min = vmin,
      max = vmax,
      calculable = FALSE,
      orient = "vertical",
      right = 8,
      top = 80,
      bottom = 80,
      inRange = list(color = c(
        "#0b0920", "#3b0f70", "#8c2981", "#de4968", "#fe9f6d", "#fcfdbf"
      )),
      text = list("Cars", "(thousands)"),
      textStyle = list(color = "#a3a3a3")
    ) |>
    e_grid(left = 56, right = 88, top = 24, bottom = 48, containLabel = TRUE) |>
    e_tooltip() |>
    e_x_axis(
      type = "category",
      data = month_labs,
      axisLabel = list(
        color = "#a3a3a3",
        fontFamily = "IBM Plex Mono",
        fontSize = 12,
        formatter = htmlwidgets::JS(
          "function(v){return (v&&v.slice(5)==='01') ? v.slice(0,4) : '';}"
        ),
        interval = 0
      ),
      axisTick = list(show = FALSE),
      axisLine = list(show = FALSE),
      splitLine = list(show = FALSE)
    ) |>
    e_y_axis(
      type = "category",
      data = ages,
      name = "Age of car (years)",
      nameLocation = "middle",
      nameGap = 40,
      nameTextStyle = list(color = "#d4d4d4"),
      axisLabel = list(color = "#a3a3a3"),
      axisTick = list(show = FALSE),
      axisLine = list(show = FALSE),
      splitLine = list(show = FALSE)
    )

  series <- p$x$opts$series
  if (!length(series)) return(p)

  # Year-10 = plain red line (no arrow). Cohort arrows = solid white with tip only at end.
  series[[1]]$markLine <- list(
    silent = TRUE,
    animation = FALSE,
    # Default none so the Year-10 guide is not an arrow; tips set per endpoint below
    symbol = "none",
    symbolSize = 0,
    lineStyle = list(type = "solid"),
    data = list(
      list(
        yAxis = "10",
        symbol = "none",
        symbolSize = 0,
        label = list(
          formatter = "Year 10: COE expires",
          color = "#ef4444",
          fontWeight = "bold",
          fontSize = 13,
          position = "insideStartTop"
        ),
        lineStyle = list(color = "#ef4444", width = 3, type = "solid")
      ),
      list(
        list(
          coord = list(a1$x, a1$y),
          symbol = "none",
          symbolSize = 0,
          lineStyle = list(color = "#fafafa", width = 2.4, type = "solid")
        ),
        list(
          coord = list(a1$x2, a1$y2),
          symbol = "arrow",
          symbolSize = 12,
          lineStyle = list(color = "#fafafa", width = 2.4, type = "solid")
        )
      ),
      list(
        list(
          coord = list(a2$x, a2$y),
          symbol = "none",
          symbolSize = 0,
          lineStyle = list(color = "#fafafa", width = 2.2, type = "solid")
        ),
        list(
          coord = list(a2$x2, a2$y2),
          symbol = "arrow",
          symbolSize = 11,
          lineStyle = list(color = "#fafafa", width = 2.2, type = "solid")
        )
      )
    )
  )

  # Bare white text on the heatmap (no dark label boxes)
  label_series <- list(
    type = "scatter",
    name = "cohort_labels",
    z = 20,
    silent = TRUE,
    tooltip = list(show = FALSE),
    legendHoverLink = FALSE,
    data = list(
      list(
        value = list(t1$x, t1$y),
        symbolSize = 1,
        itemStyle = list(color = "rgba(0,0,0,0)"),
        label = list(
          show = TRUE,
          formatter = lab_big,
          color = "#fafafa",
          fontWeight = "bold",
          fontSize = 14,
          rotate = 21,
          backgroundColor = "transparent",
          padding = 0,
          textBorderColor = "rgba(0,0,0,0.65)",
          textBorderWidth = 2,
          position = "top",
          distance = 2
        )
      ),
      list(
        value = list(t2$x, t2$y),
        symbolSize = 1,
        itemStyle = list(color = "rgba(0,0,0,0)"),
        label = list(
          show = TRUE,
          formatter = lab_small,
          color = "#fafafa",
          fontWeight = "bold",
          fontSize = 13,
          rotate = 21,
          backgroundColor = "transparent",
          padding = 0,
          textBorderColor = "rgba(0,0,0,0.65)",
          textBorderWidth = 2,
          position = "top",
          distance = 2
        )
      )
    )
  )

  p$x$opts$series <- c(series, list(label_series))
  # visualMap already shows the scale — hide the duplicate series legend
  p$x$opts$legend <- list(show = FALSE)
  p
}

#' @export
chart_g4_owner <- function(prep, which = c("early", "y10")) {
  which <- match.arg(which)
  d <- prep$drivers
  if (which == "early") {
    pts <- data.frame(
      x = d$premium / 1000,
      y = 100 * d$early,
      year = as.character(d$year),
      stringsAsFactors = FALSE
    )
    xlab <- "Average COE premium (S$ thousands)"
    ylab <- "Share of cars aged 1-9 scrapped (%)"
  } else {
    pts <- data.frame(
      x = d$pqp / 1000,
      y = 100 * d$y10,
      year = as.character(d$year),
      stringsAsFactors = FALSE
    )
    xlab <- "Average PQP, the price to renew (S$ thousands)"
    ylab <- "Share scrapped at year 10 (%)"
  }
  fit <- stats::lm(y ~ x, data = pts)
  xs <- seq(min(pts$x, na.rm = TRUE), max(pts$x, na.rm = TRUE), length.out = 40)
  pr <- stats::predict(fit, newdata = data.frame(x = xs), interval = "confidence", level = 0.95)
  band <- data.frame(
    x = xs, lo = pr[, "lwr"], hi = pr[, "upr"],
    y = NA_real_, year = NA_character_,
    stringsAsFactors = FALSE
  )
  pts$lo <- NA_real_
  pts$hi <- NA_real_
  both <- rbind(band, pts)
  both <- both[order(both$x, is.na(both$y)), ]
  both |>
    e_charts(x) |>
    e_band2(
      lo, hi,
      name = "95% band",
      itemStyle = list(color = "rgba(255, 92, 26, 0.22)", borderWidth = 0)
    ) |>
    e_scatter(y, bind = year, symbolSize = 14, name = "Year") |>
    e_lm(y ~ x, name = "Trend", lineStyle = list(width = 2, color = pal[["A"]])) |>
    e_labels(formatter = "{b}", position = "top", color = "#d4d4d4", fontSize = 11) |>
    e_color(pal[["A"]]) |>
    gallery_grid(top = 40, bottom = 60, left = 8, right = 16) |>
    axis_named(xlab, ylab)
}

#' @export
chart_g7_supply_price <- function(prep) {
  d <- prep$changes |>
    transmute(
      x = round(100 * quota_change, 2),
      y = round(100 * premium_change, 2),
      category = paste0("Cat ", category),
      label = paste0(category, " · ", format(q_start, "%b %Y"))
    )
  p <- d |>
    group_by(category) |>
    e_charts(x) |>
    e_scatter(y, bind = label, symbolSize = 11) |>
    e_lm(y ~ x, lineStyle = list(width = 2)) |>
    e_color(c(pal[["A"]], pal[["B"]])) |>
    gallery_grid(top = 40, bottom = 64, left = 12, right = 20) |>
    axis_named(
      "Change in number of COEs vs a year earlier (%)",
      "Change in COE price vs a year earlier (%)"
    )
  series <- p$x$opts$series
  if (length(series)) {
    series[[1]]$markLine <- list(
      symbol = "none",
      lineStyle = list(color = "rgba(255,255,255,0.35)", width = 1),
      label = list(show = FALSE),
      data = list(list(xAxis = 0), list(yAxis = 0))
    )
    p$x$opts$series <- series
  }
  p
}

# Density violin polygon around a band centre (same points as ggplot violin).
violin_poly <- function(vals, center, width = 0.28) {
  vals <- vals[is.finite(vals)]
  if (length(vals) < 3L) return(NULL)
  # from/to so the shape covers the full point range (esp. sparse Over S$100k)
  d <- stats::density(
    vals, n = 128,
    from = min(vals), to = max(vals),
    adjust = if (length(vals) < 30L) 0.85 else 1
  )
  xs <- d$y / max(d$y) * width
  ys <- d$x
  data.frame(
    x = c(center + xs, rev(center - xs), center + xs[[1]]),
    y = c(ys, rev(ys), ys[[1]])
  )
}

#' @export
chart_g5_bids <- function(prep) {
  levels_b <- c("Under S$50k", "S$50k to 100k", "Over S$100k")
  set.seed(1L) # stable jitter for compare
  d <- prep$bands |>
    filter(!is.na(band)) |>
    mutate(
      band = as.character(band),
      band_i = as.numeric(factor(band, levels = levels_b)),
      x = band_i + stats::runif(n(), -0.08, 0.08)
    )
  med <- prep$band_med |>
    mutate(
      band = as.character(band),
      band_i = as.numeric(factor(band, levels = levels_b)),
      label = paste0("median ", sprintf("%.2f", med), "\n(", n, " quarters)")
    )
  spikes <- d |> filter(bids_per_coe > 2.2)
  ymin <- floor(min(d$bids_per_coe, na.rm = TRUE) * 20) / 20
  ymax <- ceiling(max(d$bids_per_coe, na.rm = TRUE) * 20) / 20
  # Tall/narrow plot area so violins read vertically (not flat)
  p <- d |>
    e_charts(x) |>
    e_scatter(bids_per_coe, symbolSize = 8, name = "Quarter") |>
    e_color(pal[["A"]]) |>
    e_grid(left = "16%", right = "20%", top = 40, bottom = 88, containLabel = TRUE) |>
    e_tooltip(trigger = "item") |>
    e_x_axis(
      type = "value",
      min = 0.5, max = 3.5,
      # Force ticks at 1/2/3 — interval=1 from 0.55 was landing on 1.55/2.55 (blank labels)
      axisTick = list(show = TRUE, customValues = c(1, 2, 3)),
      axisLabel = list(
        color = "#a3a3a3",
        fontSize = 12,
        customValues = c(1, 2, 3),
        formatter = htmlwidgets::JS(
          "function(v){var m={1:'Under S$50k',2:'S$50k to 100k',3:'Over S$100k'}; return m[v] || '';}"
        )
      ),
      name = "Average premium in the quarter",
      nameLocation = "middle",
      nameGap = 42,
      nameTextStyle = list(color = "#d4d4d4", fontSize = 12),
      axisLine = list(show = TRUE, lineStyle = list(color = "rgba(255,255,255,0.25)")),
      splitLine = list(show = FALSE)
    ) |>
    e_y_axis(
      min = ymin,
      max = ymax,
      interval = 0.25,
      name = "Bids per COE",
      nameLocation = "middle",
      nameGap = 48,
      nameTextStyle = list(color = "#d4d4d4", fontSize = 12),
      axisLabel = list(
        color = "#a3a3a3",
        fontFamily = "IBM Plex Mono",
        formatter = htmlwidgets::JS("function(v){return Number(v).toFixed(2);}")
      ),
      axisLine = list(show = TRUE, lineStyle = list(color = "rgba(255,255,255,0.25)")),
      splitLine = list(show = TRUE, lineStyle = list(color = "rgba(255,255,255,0.06)"))
    )
  violins <- lapply(seq_along(levels_b), function(i) {
    vals <- d$bids_per_coe[d$band_i == i]
    poly <- violin_poly(vals, center = i, width = 0.28)
    if (is.null(poly)) return(NULL)
    list(
      type = "line",
      name = paste0("Range · ", levels_b[[i]]),
      data = Map(function(xx, yy) c(xx, yy), poly$x, poly$y),
      showSymbol = FALSE,
      smooth = TRUE,
      lineStyle = list(width = 1.4, color = "rgba(255,92,26,0.9)"),
      areaStyle = list(color = "rgba(255,92,26,0.28)"),
      z = 1,
      tooltip = list(show = FALSE),
      legendHoverLink = FALSE
    )
  })
  violins <- Filter(Negate(is.null), violins)
  series <- p$x$opts$series
  series[[1]]$z <- 3
  series[[1]]$markLine <- list(
    symbol = "none",
    data = list(list(yAxis = 1.5, name = "1.5")),
    lineStyle = list(type = "dashed", color = "rgba(255,255,255,0.45)"),
    label = list(color = "#d4d4d4", formatter = "1.5")
  )
  # Median bar at band centre; label nudged right so it clears the violin
  med_pts <- lapply(seq_len(nrow(med)), function(i) {
    list(
      coord = c(med$band_i[[i]], med$med[[i]]),
      value = med$label[[i]],
      name = med$label[[i]],
      symbol = "rect",
      symbolSize = c(36, 4),
      itemStyle = list(color = "#fafafa"),
      label = list(
        show = TRUE, formatter = "{b}", color = "#fafafa",
        fontSize = 11, position = "right", distance = 18,
        lineHeight = 14,
        backgroundColor = "transparent"
      )
    )
  })
  if (nrow(spikes)) {
    sp_y <- max(spikes$bids_per_coe, na.rm = TRUE)
    med_pts <- c(med_pts, list(list(
      coord = c(2.25, sp_y),
      name = "Early 2013: rush before\nthe car-loan curbs",
      symbol = "circle",
      symbolSize = 5,
      itemStyle = list(color = "#fafafa"),
      label = list(
        show = TRUE,
        formatter = "Early 2013: rush before\nthe car-loan curbs",
        color = "#d4d4d4",
        fontSize = 11,
        position = "right",
        distance = 12,
        lineHeight = 14,
        backgroundColor = "transparent"
      )
    )))
  }
  series[[1]]$markPoint <- list(data = med_pts)
  p$x$opts$series <- c(violins, series)
  p$x$opts$legend <- list(
    right = 8, top = 4,
    textStyle = list(color = "#a3a3a3", fontSize = 11),
    data = list("Quarter")
  )
  p
}

#' Density ridges for one category (ggplot twin). 6 months on top, like ggridges.
#' @export
chart_g6_risk <- function(prep, category = "A") {
  hz_levels <- c("1 month", "3 months", "6 months") # bottom -> top in echarts y
  mp <- prep$moves_plot |>
    filter(category == !!category) |>
    mutate(horizon = factor(as.character(horizon), levels = hz_levels))
  rq <- prep$ridge_q |>
    filter(category == !!category) |>
    mutate(horizon = factor(as.character(horizon), levels = hz_levels))
  fill <- if (identical(as.character(category), "A")) "rgba(255,92,26,0.45)" else "rgba(228,228,231,0.35)"
  stroke <- if (identical(as.character(category), "A")) pal[["A"]] else "#e4e4e7"

  # Closed ridge polygons (fill only under each hill, not down to y=0)
  series <- list()
  for (hz in hz_levels) {
    x <- mp$change[mp$horizon == hz]
    if (!length(x) || sum(is.finite(x)) < 5) next
    d <- stats::density(x[is.finite(x)], n = 128)
    yi <- match(hz, hz_levels) - 1
    xs <- d$x
    ys <- d$y / max(d$y) * 0.82 + yi
    # up the ridge, back along the baseline
    poly_x <- c(xs, rev(xs), xs[[1]])
    poly_y <- c(ys, rep(yi, length(xs)), ys[[1]])
    series[[length(series) + 1]] <- list(
      type = "line",
      name = hz,
      data = Map(function(xx, yy) c(xx, yy), poly_x, poly_y),
      showSymbol = FALSE,
      smooth = FALSE,
      lineStyle = list(width = 1.2, color = stroke, type = "solid"),
      areaStyle = list(color = fill, opacity = 1),
      z = 2 + yi,
      silent = TRUE,
      tooltip = list(show = FALSE),
      legendHoverLink = FALSE
    )
  }
  if (!length(series)) return(NULL)

  seed <- data.frame(x = 0, y = 0)
  p <- seed |>
    e_charts(x) |>
    e_grid(left = 72, right = 28, top = 36, bottom = 56, containLabel = TRUE) |>
    e_tooltip(show = FALSE) |>
    e_x_axis(
      name = "Change per car (right of zero: dealer pays more)",
      nameLocation = "middle",
      nameGap = 36,
      nameTextStyle = list(color = "#d4d4d4", fontSize = 12),
      axisLabel = list(
        color = "#a3a3a3",
        formatter = htmlwidgets::JS(
          "function(v){var s=v<0?'-':'+'; var a=Math.abs(v)/1000; return s+'S$'+Math.round(a)+'k';}"
        )
      ),
      axisLine = list(show = TRUE, lineStyle = list(color = "rgba(255,255,255,0.25)")),
      splitLine = list(show = TRUE, lineStyle = list(color = "rgba(255,255,255,0.06)"))
    ) |>
    e_y_axis(
      name = "Order to COE",
      nameLocation = "middle",
      nameGap = 52,
      nameTextStyle = list(color = "#d4d4d4"),
      min = -0.15,
      max = 2.95,
      axisTick = list(show = TRUE, customValues = c(0, 1, 2)),
      axisLabel = list(
        color = "#a3a3a3",
        customValues = c(0, 1, 2),
        formatter = htmlwidgets::JS(
          "function(v){var m={0:'1 month',1:'3 months',2:'6 months'}; return m[v] != null ? m[v] : '';}"
        )
      ),
      splitLine = list(show = FALSE)
    ) |>
    e_legend(show = FALSE)

  # Zero line once; typical / 1-in-10 as points on each ridge (not full-height lines)
  mark_pts <- list()
  for (i in seq_len(nrow(rq))) {
    yi <- match(as.character(rq$horizon[[i]]), hz_levels) - 1
    if (is.na(yi)) next
    mark_pts <- c(mark_pts, list(
      list(
        coord = c(rq$p50[[i]], yi + 0.35),
        name = paste0("typical: ", dk(rq$p50[[i]])),
        symbol = "rect",
        symbolSize = c(2, 28),
        itemStyle = list(color = "#fafafa"),
        label = list(
          show = TRUE, formatter = "{b}", color = "#fafafa",
          fontSize = 11, position = "right", distance = 6
        )
      ),
      list(
        coord = c(rq$p90[[i]], yi + 0.55),
        name = paste0("1 in 10: ", dk(rq$p90[[i]])),
        symbol = "rect",
        symbolSize = c(2, 28),
        itemStyle = list(color = pal[["A"]]),
        label = list(
          show = TRUE, formatter = "{b}", color = "#fafafa",
          fontSize = 11, position = "right", distance = 6
        )
      )
    ))
  }
  series[[length(series)]]$markLine <- list(
    symbol = "none",
    silent = TRUE,
    data = list(list(
      xAxis = 0,
      lineStyle = list(color = "rgba(255,255,255,0.45)", width = 1, type = "solid"),
      label = list(show = FALSE)
    ))
  )
  series[[length(series)]]$markPoint <- list(data = mark_pts, silent = TRUE)
  p$x$opts$series <- series
  p$x$opts$title <- list(
    text = paste0("Cat ", category),
    left = "center",
    top = 4,
    textStyle = list(color = "#d4d4d4", fontSize = 13, fontWeight = "600")
  )
  p
}
