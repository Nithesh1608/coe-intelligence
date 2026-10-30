box::use(
  shiny[NS, moduleServer, div, tags, selectInput, renderUI, uiOutput, req],
  bslib[card, card_header, card_body],
  echarts4r[renderEcharts4r, echarts4rOutput, e_charts, e_line, e_color, e_datazoom],
)
box::use(
  app/logic/viewdata/charts[exit_track_chart, style_chart],
)

fmt_pct <- function(x, digits = 0) {
  if (!is.finite(x)) return("-")
  paste0(format(round(100 * as.numeric(x), digits), nsmall = digits), "%")
}

LIVE_MODEL <- "core_forecast"

fmt_int <- function(x) {
  format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
}

#' Results — deregistrations behind the quota
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$p(class = "eyebrow", "Results"),
    tags$h2("How supply is built"),
    tags$p(
      class = "lede",
      "Quota follows deregistrations. Live model is an elastic net; pick a category."
    ),
    div(
      class = "desk-controls desk-controls--row",
      selectInput(
        ns("cat"),
        "Category",
        choices = c("Category A" = "A", "Category B" = "B"),
        selected = "A",
        width = "180px"
      )
    ),
    tags$p(
      class = "footnote",
      "Predicts monthly deregistrations from due-stock and recent flow features, then maps to quota."
    ),
    uiOutput(ns("example_grid")),
    card(
      class = "desk-card",
      full_screen = TRUE,
      card_header("Deregistrations each month"),
      card_body(
        class = "chart-frame chart-frame--tall",
        echarts4rOutput(ns("exits"), height = "400px")
      )
    ),
    tags$p(
      class = "footnote",
      "White = actual deregistrations. Orange = elastic net, one step ahead with actual lags. Grey dashed = 12-month average."
    ),

    tags$section(
      class = "viz-explain",
      tags$span(class = "nx-kicker", "Where it misses"),
      tags$h2("The honest view: where the model was wrong"),
      tags$p(
        class = "lede lede--wide",
        "Totals by period against actual deregistrations. A negative gap means the model under-called supply. ",
        "The 2025-26 surge is the 2016-17 registration boom reaching year ten; the cohort features carry it, ",
        "but the exit rate also rose as PQP climbed, and that part the model only partly caught."
      )
    ),
    uiOutput(ns("miss_table")),

    tags$section(
      class = "viz-explain",
      tags$span(class = "nx-kicker", "Recursive backtest"),
      tags$h2("Rolled forward 24 months from August 2024, no peeking"),
      tags$p(
        class = "lede lede--wide",
        "This is the test that matters for a planning horizon. The evaluation model was rolled forward month by ",
        "month from the training end, feeding its own predictions back as lags and projecting age stock from ",
        "registration cohorts. Bands come from 500 bootstrap paths of its held-out errors."
      ),
      tags$ul(
        class = "viz-explain__list",
        tags$li(tags$strong("MAPE - "), "mean absolute percentage error of the central path."),
        tags$li(tags$strong("Bias - "), "signed total error; negative means supply was under-called."),
        tags$li(tags$strong("80% coverage - "), "share of months inside the 80% band; 80% is the target, below it the band is too narrow.")
      )
    ),
    card(
      class = "desk-card",
      full_screen = TRUE,
      card_header("Recursive path versus actual deregistrations"),
      card_body(class = "chart-frame chart-frame--tall", echarts4rOutput(ns("bt_chart"), height = "380px"))
    ),
    uiOutput(ns("cal_table")),
    tags$p(
      class = "footnote",
      "Specification was chosen on this held-out window (cohort-only predictors, lambda.min), so these numbers ",
      "are the fairest we can report, not an independent test. The next twelve months of releases will be."
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    live_label <- store$meta$model_labels[[LIVE_MODEL]]
    if (is.null(live_label) || is.na(live_label)) live_label <- "Elastic net"

    output$example_grid <- renderUI({
      dm <- store$two_monthly
      if (is.null(dm)) return(NULL)
      months <- as.Date(c(
        "2024-09-01", "2025-01-01", "2025-05-01",
        "2025-08-01", "2026-01-01", "2026-05-01"
      ))
      cards <- lapply(months, function(m) {
        f <- dm[dm$category == input$cat & dm$model == LIVE_MODEL & dm$month == m, ]
        s <- dm[dm$category == input$cat & dm$model == "simple_average" & dm$month == m, ]
        if (!nrow(f)) return(NULL)
        better <- abs(f$predicted[[1]] - f$actual[[1]]) < abs(s$predicted[[1]] - s$actual[[1]])
        div(
          class = paste("ex-tile", if (better) "ex-tile--win" else "ex-tile--miss"),
          tags$span(class = "ex-month", strftime(as.Date(m), "%b %Y")),
          tags$div(class = "ex-row", tags$span("Actual"), tags$strong(fmt_int(f$actual[[1]]))),
          tags$div(
            class = "ex-row ex-row--accent",
            tags$span("Forecast"),
            tags$strong(fmt_int(f$predicted[[1]]))
          ),
          tags$div(
            class = "ex-row muted",
            tags$span("Average"),
            tags$strong(fmt_int(s$predicted[[1]]))
          )
        )
      })
      div(class = "ex-grid", cards)
    })

    output$exits <- renderEcharts4r({
      dm <- store$two_monthly
      f <- dm[dm$category == input$cat & dm$model == LIVE_MODEL, ]
      s <- dm[dm$category == input$cat & dm$model == "simple_average", ]
      df <- data.frame(
        month = f$month,
        actual = f$actual,
        forecast = f$predicted,
        simple = s$predicted[match(f$month, s$month)],
        stringsAsFactors = FALSE
      )
      exit_track_chart(df, live_label)
    })

    output$miss_table <- renderUI({
      r <- store$two_rollup
      if (is.null(r) || !nrow(r)) return(NULL)
      r <- r[as.character(r$category) == input$cat, ]
      r <- r[order(r$period, r$model), ]
      rows <- lapply(seq_len(nrow(r)), function(i) {
        off <- as.numeric(r$pct_off[i])
        cls <- if (abs(off) <= 10) "ex-row--green" else if (abs(off) <= 20) "" else "ex-row--accent"
        tags$tr(
          class = cls,
          tags$td(r$period[i]),
          tags$td(if (r$model[i] == LIVE_MODEL) live_label else "12-month average"),
          tags$td(fmt_int(r$actual[i])),
          tags$td(fmt_int(r$predicted[i])),
          tags$td(sprintf("%s%s", if (off >= 0) "+" else "", fmt_int(r$gap[i]))),
          tags$td(sprintf("%+.1f%%", off))
        )
      })
      tags$table(
        class = "table table-sm",
        style = "color:#fafafa;",
        tags$thead(tags$tr(
          tags$th("Period"), tags$th("Model"), tags$th("Actual exits"), tags$th("Predicted"),
          tags$th("Gap"), tags$th("Off by")
        )),
        tags$tbody(rows)
      )
    })

    output$bt_chart <- renderEcharts4r({
      bt <- store$backtest
      req(!is.null(bt), nrow(bt) > 0)
      d <- bt[as.character(bt$category) == input$cat, ]
      d <- d[order(as.Date(d$month)), ]
      d$label <- format(as.Date(d$month), "%b %Y")
      d |>
        e_charts(label) |>
        e_line(dereg_lo80, name = "80% low", symbol = "none", lineStyle = list(width = 1, opacity = 0.4)) |>
        e_line(dereg_hi80, name = "80% high", symbol = "none", lineStyle = list(width = 1, opacity = 0.4)) |>
        e_line(dereg_actual, name = "Actual deregistrations", symbol = "circle", symbolSize = 5, lineStyle = list(width = 2.4)) |>
        e_line(dereg_mid, name = "Recursive forecast", symbol = "none", lineStyle = list(width = 2.2)) |>
        e_color(c("#ff5c1a", "#ff5c1a", "#fafafa", "#ff5c1a")) |>
        style_chart() |>
        e_datazoom(type = "inside")
    })

    output$cal_table <- renderUI({
      cal <- store$calibration
      if (is.null(cal) || !nrow(cal)) {
        return(tags$p(class = "footnote", "Recursive backtest not available - run scripts/fit_and_forecast.R."))
      }
      d <- cal[as.character(cal$category) == input$cat, ]
      rows <- lapply(seq_len(nrow(d)), function(i) {
        tags$tr(
          tags$td(d$horizon[i]),
          tags$td(d$months[i]),
          tags$td(fmt_pct(d$dereg_mape[i])),
          tags$td(sprintf("%+.0f%%", d$dereg_bias_pct[i])),
          tags$td(fmt_pct(d$dereg_cover80[i])),
          tags$td(fmt_pct(d$quota_mape[i])),
          tags$td(sprintf("%+.0f%%", d$quota_bias_pct[i])),
          tags$td(fmt_pct(d$quota_cover80[i]))
        )
      })
      tags$table(
        class = "table table-sm",
        style = "color:#fafafa;",
        tags$thead(tags$tr(
          tags$th("Horizon"), tags$th("Months"),
          tags$th("Exits MAPE"), tags$th("Exits bias"), tags$th("Exits 80% cover"),
          tags$th("Quota MAPE"), tags$th("Quota bias"), tags$th("Quota 80% cover")
        )),
        tags$tbody(rows)
      )
    })
  })
}
