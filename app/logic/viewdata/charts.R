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
    e_y_axis
  ],
)

palette <- c(A = "#ff5c1a", B = "#e4e4e7")

axis <- list(
  axisLine = list(show = FALSE),
  axisTick = list(show = FALSE),
  axisLabel = list(color = "#a3a3a3", fontFamily = "IBM Plex Mono"),
  splitLine = list(lineStyle = list(color = "rgba(255,255,255,0.06)"))
)

# Skip e_theme("dark") — it paints a purple canvas that looks empty on the desk.
#' @export
style_chart <- function(p) {
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

#' Published quota vs selected forecast vs simple-average baseline
#' expects: month, published, outlook, baseline
#' @export
quota_track_chart <- function(df, forecast_name = "Our supply forecast") {
  plot_df <- df
  plot_df$month <- format(as.Date(plot_df$month), "%b %Y")
  names(plot_df)[names(plot_df) == "outlook"] <- "forecast"
  plot_df |>
    e_charts(month) |>
    e_line(published, name = "Certificates released", symbol = "circle",
           symbolSize = 6, lineStyle = list(width = 2.6)) |>
    e_line(forecast, name = forecast_name, symbol = "none",
           lineStyle = list(width = 2.2)) |>
    e_line(baseline, name = "Simple 12-month average", symbol = "none",
           lineStyle = list(width = 1.5, type = "dashed", opacity = 0.75)) |>
    e_color(c("#fafafa", "#ff5c1a", "#737373")) |>
    style_chart() |>
    e_datazoom(type = "inside")
}

#' Deregistrations: actual vs one forecast vs simple average
#' expects: month, actual, forecast, simple
#' @export
exit_track_chart <- function(df, forecast_name = "Our forecast") {
  plot_df <- df
  plot_df$month <- format(as.Date(plot_df$month), "%b %Y")
  plot_df |>
    e_charts(month) |>
    e_line(actual, name = "Deregistrations", symbol = "circle", symbolSize = 5,
           lineStyle = list(width = 2.4)) |>
    e_line(forecast, name = forecast_name, symbol = "none",
           lineStyle = list(width = 2)) |>
    e_line(simple, name = "Simple 12-month average", symbol = "none",
           lineStyle = list(width = 1.4, type = "dashed", opacity = 0.7)) |>
    e_color(c("#fafafa", "#ff5c1a", "#737373")) |>
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
  exit_track_chart(
    data.frame(
      month = df$month, actual = df$actual,
      forecast = df$model, simple = df$baseline,
      stringsAsFactors = FALSE
    ),
    "Our forecast"
  )
}

#' @export
quota_compare_chart <- function(df) {
  # expects published, from_pred; optional from_actual unused on Home
  plot_df <- data.frame(
    month = df$month,
    published = df$published,
    outlook = df$from_pred,
    baseline = if (!is.null(df$from_actual)) df$from_actual else df$from_pred,
    stringsAsFactors = FALSE
  )
  quota_track_chart(plot_df, "Our supply forecast")
}

#' @export
spread_chart <- function(df) {
  df$month <- format(as.Date(df$month), "%Y-%m")
  df |>
    e_charts(month) |>
    e_line(A, smooth = TRUE, symbol = "none") |>
    e_line(B, smooth = TRUE, symbol = "none") |>
    e_color(c(palette[["A"]], palette[["B"]])) |>
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
