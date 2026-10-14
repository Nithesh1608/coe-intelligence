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
