box::use(
  shiny[NS, moduleServer, div, tags, selectInput, renderUI, uiOutput, req],
  bslib[card, card_header, card_body],
  echarts4r[renderEcharts4r, echarts4rOutput],
)
box::use(
  app/logic/viewdata/charts[exit_track_chart, quota_track_chart],
)

fmt_pct <- function(x, digits = 0) {
  if (!is.finite(x)) return("-")
  paste0(format(round(100 * as.numeric(x), digits), nsmall = digits), "%")
}

LIVE_MODEL <- "core_forecast"

fmt_int <- function(x) {
  format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
}

#' Results - one-step track record for exits and quota
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$p(class = "eyebrow", "Results"),
    tags$h2("How the outlook held up against real releases"),
    tags$p(
      class = "lede lede--wide",
      "Held-out months after training: predicted exits, then quota from those exits with LTA's replacement-style rule ",
      "(share of the last 12 months of deregistrations). Pick a category."
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
    uiOutput(ns("example_grid")),
    card(
      class = "desk-card",
      full_screen = TRUE,
      card_header("Monthly deregistrations: actual versus outlook"),
      card_body(
        class = "chart-frame chart-frame--tall",
        echarts4rOutput(ns("exits"), height = "400px")
      )
    ),
    card(
      class = "desk-card",
      full_screen = TRUE,
      card_header("Monthly quota: released versus outlook"),
      card_body(
        class = "chart-frame chart-frame--tall",
        echarts4rOutput(ns("quota"), height = "400px")
      )
    ),

    tags$section(
      class = "viz-explain",
      tags$span(class = "nx-kicker", "By period"),
      tags$h2("Our outlook vs a plain 12-month average"),
      tags$p(
        class = "lede lede--wide",
        "Same periods, same actuals. Green = closer to reality. One glance: who was more accurate."
      )
    ),
    uiOutput(ns("miss_table"))
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    live_label <- "Our outlook"

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
      req(!is.null(dm), nrow(dm) > 0)
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

    output$quota <- renderEcharts4r({
      q <- store$two_quota
      req(!is.null(q), nrow(q) > 0)
      f <- q[as.character(q$category) == input$cat & as.character(q$model) == LIVE_MODEL, ]
      s <- q[as.character(q$category) == input$cat & as.character(q$model) == "simple_average", ]
      f <- f[order(as.Date(f$month)), ]
      df <- data.frame(
        month = as.Date(f$month),
        published = as.numeric(f$published),
        outlook = as.numeric(f$outlook),
        baseline = as.numeric(s$outlook[match(as.Date(f$month), as.Date(s$month))]),
        stringsAsFactors = FALSE
      )
      quota_track_chart(df, live_label)
    })

    output$miss_table <- renderUI({
      r <- store$two_rollup
      if (is.null(r) || !nrow(r)) return(NULL)
      r <- r[as.character(r$category) == input$cat, ]
      periods <- unique(as.character(r$period))
      # Chronological-ish: late 2024, then 2025, then 2026
      ord <- order(vapply(periods, function(p) {
        if (grepl("2024", p)) 1L else if (grepl("2025", p)) 2L else 3L
      }, integer(1)))
      periods <- periods[ord]

      wins <- 0L
      rows <- lapply(periods, function(per) {
        ours <- r[as.character(r$period) == per & as.character(r$model) == LIVE_MODEL, ]
        base <- r[as.character(r$period) == per & as.character(r$model) == "simple_average", ]
        if (!nrow(ours) || !nrow(base)) return(NULL)
        off_o <- abs(as.numeric(ours$pct_off[[1]]))
        off_b <- abs(as.numeric(base$pct_off[[1]]))
        we_win <- off_o < off_b
        if (we_win) wins <<- wins + 1L
        tags$tr(
          class = if (we_win) "cmp-row cmp-row--win" else "cmp-row cmp-row--lose",
          tags$td(per),
          tags$td(fmt_int(ours$actual[[1]])),
          tags$td(
            class = if (we_win) "cmp-cell cmp-cell--win" else "cmp-cell",
            tags$div(class = "cmp-pred", fmt_int(ours$predicted[[1]])),
            tags$div(class = "cmp-off", sprintf("%+.1f%%", as.numeric(ours$pct_off[[1]])))
          ),
          tags$td(
            class = if (!we_win) "cmp-cell cmp-cell--win" else "cmp-cell cmp-cell--dim",
            tags$div(class = "cmp-pred", fmt_int(base$predicted[[1]])),
            tags$div(class = "cmp-off", sprintf("%+.1f%%", as.numeric(base$pct_off[[1]])))
          ),
          tags$td(
            class = if (we_win) "cmp-verdict cmp-verdict--win" else "cmp-verdict cmp-verdict--lose",
            if (we_win) "We win" else "Average closer"
          )
        )
      })
      rows <- Filter(Negate(is.null), rows)
      n <- length(rows)
      div(
        class = "cmp-wrap",
        tags$p(
          class = "cmp-summary",
          sprintf(
            "Our outlook closer in %d of %d periods for Cat %s.",
            wins, n, input$cat
          )
        ),
        tags$table(
          class = "desk-table cmp-table",
          tags$thead(tags$tr(
            tags$th("Period"),
            tags$th("Actual exits"),
            tags$th("Our outlook"),
            tags$th("12-month average"),
            tags$th("Closer?")
          )),
          tags$tbody(rows)
        )
      )
    })
  })
}
