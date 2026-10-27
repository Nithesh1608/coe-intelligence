box::use(
  echarts4r[
    e_charts,
    e_line,
    e_bar,
    e_tooltip,
    e_legend,
    e_grid,
    e_color,
    e_datazoom,
    e_x_axis,
    e_y_axis,
    e_theme
  ],
)

palette <- c(A = "#ff5c1a", B = "#e4e4e7", C = "#c8f542", D = "#fbbf24", E = "#fb7185")

axis <- list(
  axisLine = list(show = FALSE),
  axisTick = list(show = FALSE),
  axisLabel = list(color = "#737373", fontFamily = "IBM Plex Mono"),
  splitLine = list(lineStyle = list(color = "rgba(255,255,255,0.06)"))
)

#' @export
style_chart <- function(p) {
  p |>
    e_theme("dark") |>
    e_grid(left = 12, right = 16, top = 52, bottom = 16, containLabel = TRUE) |>
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

#' Actual vs two forecasts vs simple average
#' expects: month, actual, core, momentum, simple
#' @export
two_model_chart <- function(df) {
  plot_df <- df
  plot_df$month <- format(as.Date(plot_df$month), "%b %Y")
  plot_df |>
    e_charts(month) |>
    e_line(actual, name = "What actually happened", symbol = "circle", symbolSize = 6,
           lineStyle = list(width = 2.8)) |>
    e_line(core, name = "Core supply forecast", symbol = "none",
           lineStyle = list(width = 2)) |>
    e_line(momentum, name = "Momentum supply forecast", symbol = "none",
           lineStyle = list(width = 2)) |>
    e_line(simple, name = "Simple 12-month average", symbol = "none",
           lineStyle = list(width = 1.5, type = "dashed", opacity = 0.75)) |>
    e_color(c("#fafafa", "#ff5c1a", "#c8f542", "#737373")) |>
    style_chart() |>
    e_datazoom(type = "inside")
}

#' @export
fan_chart <- function(hist, fwd) {
  hist$lo <- NA_real_
  hist$hi <- NA_real_
  keep <- c("month", "mid", "lo", "hi")
  hist$month <- as.Date(hist$month)
  fwd$month <- as.Date(fwd$month)
  both <- rbind(hist[, keep], fwd[, keep])
  both$month <- format(both$month, "%Y-%m")
  both |>
    e_charts(month) |>
    e_line(lo, name = "Low", symbol = "none", lineStyle = list(opacity = 0.35)) |>
    e_line(hi, name = "High", symbol = "none", lineStyle = list(opacity = 0.35)) |>
    e_line(mid, name = "Central", smooth = TRUE, symbol = "none") |>
    e_color(c("#737373", "#737373", "#ff5c1a")) |>
    style_chart() |>
    e_datazoom(type = "inside")
}

#' @export
actual_vs_pred_chart <- function(df) {
  # legacy single-model helper
  plot_df <- df
  plot_df$month <- format(as.Date(plot_df$month), "%b %Y")
  plot_df |>
    e_charts(month) |>
    e_line(actual, name = "What actually happened", symbol = "circle", symbolSize = 6,
           lineStyle = list(width = 2.5)) |>
    e_line(model, name = "Our forecast", symbol = "none",
           lineStyle = list(width = 2)) |>
    e_line(baseline, name = "Simple 12-month average", symbol = "none",
           lineStyle = list(width = 1.5, type = "dashed", opacity = 0.75)) |>
    e_color(c("#fafafa", "#ff5c1a", "#737373")) |>
    style_chart() |>
    e_datazoom(type = "inside")
}

#' @export
quota_compare_chart <- function(df) {
  plot_df <- df
  plot_df$month <- format(as.Date(plot_df$month), "%b %Y")
  plot_df |>
    e_charts(month) |>
    e_line(published, name = "Certificates LTA released", symbol = "circle",
           symbolSize = 5, lineStyle = list(width = 2.5)) |>
    e_line(from_pred, name = "Our quota outlook", symbol = "none",
           lineStyle = list(width = 2)) |>
    e_line(from_actual, name = "Outlook if exits were known", symbol = "none",
           lineStyle = list(width = 1.5, type = "dashed", opacity = 0.8)) |>
    e_color(c("#fafafa", "#c8f542", "#737373")) |>
    style_chart() |>
    e_datazoom(type = "inside")
}

#' @export
spread_chart <- function(df) {
  df$month <- format(as.Date(df$month), "%Y-%m")
  df |>
    e_charts(month) |>
    e_line(A, smooth = TRUE, symbol = "none") |>
    e_line(B, smooth = TRUE, symbol = "none") |>
    e_line(E, smooth = TRUE, symbol = "none") |>
    e_color(c(palette[["A"]], palette[["B"]], palette[["E"]])) |>
    style_chart() |>
    e_datazoom(type = "inside")
}

#' @export
bar_chart <- function(df, x, y, name = NULL) {
  plot_df <- df[, c(x, y), drop = FALSE]
  names(plot_df) <- c("x", "value")
  nm <- if (is.null(name)) y else name
  plot_df |>
    e_charts(x) |>
    e_bar(value, name = nm, itemStyle = list(color = "#ff5c1a")) |>
    style_chart()
}

#' @export
exposure_bars_chart <- function(df) {
  df |>
    e_charts(quarter) |>
    e_bar(at_last_print, name = "At last print") |>
    e_bar(with_stress, name = "With your stress") |>
    e_color(c("#a3a3a3", "#ff5c1a")) |>
    style_chart()
}
