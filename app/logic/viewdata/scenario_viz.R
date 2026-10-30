# Feedback and scenarios view data: apply a scenario to the forward path
# live in the app (no refit) and chart quarterly quota against the baseline.

box::use(
  echarts4r[e_charts, e_line, e_bar, e_color, e_datazoom, e_grid, e_tooltip, e_legend, e_x_axis, e_y_axis],
  app/logic/viewdata/pipeline[quarter_label],
  app/logic/model/scenarios[scenario_presets, apply_scenario, rollup_quarters, injection_split],
)

`%||%` <- function(a, b) {
  if (is.null(a) || !length(a) || (length(a) == 1L && is.na(a))) b else a
}

#' Named choices for the scenario selector.
#' @export
preset_choices <- function() {
  p <- scenario_presets()
  stats::setNames(vapply(p, function(x) x$id, character(1)), vapply(p, function(x) x$label, character(1)))
}

#' @export
preset_by_id <- function(id, clawback_total = NA_real_) {
  presets <- scenario_presets(clawback_total)
  for (p in presets) if (identical(p$id, id)) return(p)
  presets[[1]]
}

clawback_from_store <- function(store) {
  v <- suppressWarnings(as.numeric(store$meta$spec$brought_forward_since_2023 %||% NA))
  if (length(v) && is.finite(v)) v else NA_real_
}

#' Published monthly A/B quota from the bidding store.
published_from_store <- function(store) {
  b <- store$bidding
  b <- b[as.character(b$category) %in% c("A", "B") & is.finite(b$quota), c("month", "category", "quota")]
  names(b)[names(b) == "quota"] <- "published"
  b$month <- as.Date(b$month)
  b
}

#' Actual deregistrations before the forward window for one category.
hist_series <- function(store, cat, before) {
  p <- store$panel
  p <- p[as.character(p$category) == cat & !is.na(p$deregistrations) & as.Date(p$month) < as.Date(before), ]
  stats::setNames(as.numeric(p$deregistrations), as.character(as.Date(p$month)))
}

#' Build baseline vs scenario quarterly paths for A, B or merged A+B.
#'
#' @param category "A", "B" or "AB" (merged car category)
#' @param preset_id id from scenario_presets()
#' @param renewal_shift extra renewal-rate shift in share points (e.g. 0.05)
#' @param injection_per_qtr extra car COEs per quarter on top of the preset
#' @param horizon_quarters number of full quarters to show
#' @export
prep_scenario <- function(store, category = "A", preset_id = "baseline", renewal_shift = 0,
                          injection_per_qtr = 0, horizon_quarters = 12L) {
  fd <- store$forward_dereg
  if (is.null(fd) || !nrow(fd) || is.null(store$alphas)) return(NULL)
  preset <- preset_by_id(preset_id, clawback_from_store(store))
  cats <- if (identical(category, "AB")) c("A", "B") else category
  total_inj <- as.numeric(preset$injection_per_qtr) + as.numeric(injection_per_qtr)
  split <- injection_split(published_from_store(store), total_inj)
  shift <- as.numeric(preset$renewal_shift) + as.numeric(renewal_shift)

  monthly <- NULL
  for (cat in cats) {
    fw <- fd[as.character(fd$category) == cat, ]
    fw <- fw[order(as.Date(fw$month)), ]
    first <- min(as.Date(fw$month))
    resp <- store$renewal_response
    resp <- if (is.null(resp)) NULL else resp[as.character(resp$category) == cat, ]
    alpha <- as.numeric(store$alphas$alpha[as.character(store$alphas$category) == cat][[1]])
    m <- apply_scenario(fw, hist_series(store, cat, first), alpha, resp,
                        pqp_target = preset$pqp_target, renewal_shift = shift,
                        injection_per_qtr = split[[cat]])
    m$category <- cat
    monthly <- rbind(monthly, m)
  }
  if (is.null(monthly)) return(NULL)

  # Merge categories by summing the month rows.
  if (length(cats) > 1L) {
    num <- setdiff(names(monthly), c("month", "quarter", "category", "pqp_base", "pqp_scenario", "renewal_share_delta"))
    agg <- stats::aggregate(monthly[, num], by = list(month = monthly$month), FUN = sum)
    agg$quarter <- monthly$quarter[match(agg$month, monthly$month)]
    agg$pqp_base <- stats::aggregate(pqp_base ~ month, monthly, mean)$pqp_base
    agg$pqp_scenario <- stats::aggregate(pqp_scenario ~ month, monthly, mean)$pqp_scenario
    agg$renewal_share_delta <- stats::aggregate(renewal_share_delta ~ month, monthly, mean)$renewal_share_delta
    monthly <- agg
  }
  q <- rollup_quarters(monthly)
  q <- q[q$months == 3L, ]
  q <- q[order(as.Date(q$quarter)), ]
  q <- utils::head(q, horizon_quarters)
  q$label <- quarter_label(q$quarter)
  q$delta <- q$quota_scenario - q$quota_base
  q$delta_pct <- 100 * q$delta / pmax(q$quota_base, 1)

  list(
    quarters = q,
    monthly = monthly,
    category = category,
    preset = preset,
    renewal_shift = shift,
    injection_per_qtr = total_inj,
    slope = if (identical(category, "AB")) NA_real_ else {
      r <- store$renewal_response
      if (is.null(r)) NA_real_ else as.numeric(r$slope_log_pqp[as.character(r$category) == category][[1]])
    }
  )
}

as_plot_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

# Local style — skip e_theme("dark") purple canvas; keep the desk black.
style_scenario_chart <- function(p) {
  axis <- list(
    axisLine = list(show = FALSE),
    axisTick = list(show = FALSE),
    axisLabel = list(color = "#a3a3a3", fontFamily = "IBM Plex Mono"),
    splitLine = list(lineStyle = list(color = "rgba(255,255,255,0.06)"))
  )
  p |>
    e_grid(left = 12, right = 16, top = 48, bottom = 16, containLabel = TRUE) |>
    e_tooltip(trigger = "axis") |>
    e_legend(right = 8, top = 4, textStyle = list(color = "#a3a3a3", fontSize = 11)) |>
    e_x_axis(
      axisLine = axis$axisLine,
      axisTick = axis$axisTick,
      axisLabel = axis$axisLabel,
      splitLine = list(show = FALSE)
    ) |>
    e_y_axis(
      axisLine = axis$axisLine,
      axisTick = axis$axisTick,
      axisLabel = axis$axisLabel,
      splitLine = axis$splitLine
    )
}

#' @export
chart_scenario_quota <- function(prep) {
  d <- as.data.frame(prep$quarters, stringsAsFactors = FALSE)
  d$label <- as.character(d$label)
  d$quota_base <- as_plot_num(d$quota_base)
  d$quota_base_lo80 <- as_plot_num(d$quota_base_lo80)
  d$quota_base_hi80 <- as_plot_num(d$quota_base_hi80)
  d$quota_scenario <- as_plot_num(d$quota_scenario)
  d |>
    e_charts(label) |>
    e_line(quota_base_lo80, name = "Baseline 80% low", symbol = "none",
           lineStyle = list(width = 1, opacity = 0.45, color = "#a3a3a3")) |>
    e_line(quota_base_hi80, name = "Baseline 80% high", symbol = "none",
           lineStyle = list(width = 1, opacity = 0.45, color = "#a3a3a3")) |>
    e_line(quota_base, name = "Baseline quota", symbol = "none",
           lineStyle = list(width = 1.8, type = "dashed", color = "#d4d4d4")) |>
    e_line(quota_scenario, name = "Scenario quota", symbol = "circle", symbolSize = 5,
           lineStyle = list(width = 2.4, color = "#ff5c1a")) |>
    e_color(c("#a3a3a3", "#a3a3a3", "#d4d4d4", "#ff5c1a")) |>
    style_scenario_chart() |>
    e_datazoom(type = "inside")
}

#' @export
chart_scenario_dereg <- function(prep) {
  d <- as.data.frame(prep$quarters, stringsAsFactors = FALSE)
  d$label <- as.character(d$label)
  d$dereg_base_k <- as_plot_num(d$dereg_base) / 1000
  d$dereg_scenario_k <- as_plot_num(d$dereg_scenario) / 1000
  d |>
    e_charts(label) |>
    e_bar(dereg_base_k, name = "Baseline deregistrations (k)") |>
    e_bar(dereg_scenario_k, name = "Scenario deregistrations (k)") |>
    e_color(c("#737373", "#ff5c1a")) |>
    style_scenario_chart()
}
