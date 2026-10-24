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
    e_grid(left = 12, right = 16, top = 44, bottom = 16, containLabel = TRUE) |>
    e_tooltip(trigger = "axis") |>
    e_legend(right = 8, top = 4, textStyle = list(color = "#a3a3a3")) |>
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

#' Actual vs model vs baseline for dereg (audience chart)
#' @export
actual_vs_pred_chart <- function(df) {
  # expects columns: month, actual, model, baseline
  plot_df <- df
  plot_df$month <- format(as.Date(plot_df$month), "%b %Y")
  plot_df |>
    e_charts(month) |>
    e_line(actual, name = "What happened", symbol = "circle", symbolSize = 6,
           lineStyle = list(width = 2.5)) |>
    e_line(model, name = "Our forecast", symbol = "none",
           lineStyle = list(width = 2, type = "solid")) |>
    e_line(baseline, name = "Old baseline", symbol = "none",
           lineStyle = list(width = 1.5, type = "dashed", opacity = 0.75)) |>
    e_color(c("#fafafa", "#ff5c1a", "#737373")) |>
    style_chart() |>
    e_datazoom(type = "inside")
}

#' Quota: formula from predicted exits vs formula from actual exits
#' @export
quota_compare_chart <- function(df) {
  plot_df <- df
  plot_df$month <- format(as.Date(plot_df$month), "%b %Y")
  plot_df |>
    e_charts(month) |>
    e_line(from_actual, name = "Quota if exits known", symbol = "none",
           lineStyle = list(width = 2.2)) |>
    e_line(from_pred, name = "Quota from our exit forecast", symbol = "none",
           lineStyle = list(width = 2)) |>
    e_color(c("#fafafa", "#c8f542")) |>
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
bar_chart <- function(df, x, y) {
  plot_df <- df[, c(x, y), drop = FALSE]
  names(plot_df) <- c("x", "value")
  plot_df |>
    e_charts(x) |>
    e_bar(value, name = y, itemStyle = list(color = "#ff5c1a")) |>
    style_chart()
}

#' Side-by-side grouped bars for example months
#' @export
example_bars_chart <- function(df) {
  # long: label, series, value
  df |>
    e_charts(label) |>
    e_bar(actual, name = "Actual", stack = NULL) |>
    e_bar(forecast, name = "Forecast") |>
    e_color(c("#fafafa", "#ff5c1a")) |>
    style_chart()
}
