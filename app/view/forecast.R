box::use(
  shiny[NS, moduleServer, div, tags, selectInput, renderUI, uiOutput, renderText],
  bslib[layout_columns, card, card_header, card_body],
  echarts4r[renderEcharts4r, echarts4rOutput],
)
box::use(
  app/logic/viewdata/charts[actual_vs_pred_chart, quota_compare_chart],
)

fmt_int <- function(x) {
  format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_pct <- function(x) {
  sprintf("%.0f%%", 100 * as.numeric(x))
}

#' Results: dereg track record + calibrated quota outlook
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$p(class = "eyebrow", "Forecast"),
    tags$h2("Exits first — then quota"),
    tags$p(
      class = "lede lede--wide",
      "Held-out months from September 2024. Official deregistrations versus our forecast ",
      "and a trailing-average baseline. Quota below compares what LTA actually released ",
      "to an exit-driven outlook scaled to tender size."
    ),
    selectInput(
      ns("cat"),
      "Category",
      choices = c("Category A" = "A", "Category B" = "B"),
      selected = "A",
      width = "200px"
    ),
    uiOutput(ns("example_grid")),
    layout_columns(
      col_widths = c(12),
      card(
        class = "desk-card",
        full_screen = TRUE,
        card_header("Deregistrations — held-out window"),
        card_body(
          class = "chart-frame chart-frame--tall",
          echarts4rOutput(ns("dereg_chart"), height = "400px")
        )
      )
    ),
    tags$hr(class = "desk-rule"),
    tags$p(class = "eyebrow", "Quota"),
    tags$h2("Published tender quota vs our outlook"),
    tags$p(
      class = "lede lede--wide",
      "White = certificates LTA put up for bidding that month. ",
      "Green = our outlook: predicted exits, mapped to monthly tender scale. ",
      "Grey dashed = the same map using actual exits (isolates exit-forecast error from the quota link)."
    ),
    div(
      class = "nx-metrics",
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "Typical miss vs published"),
        div(class = "nx-num", shiny::textOutput(ns("kpi_mape"), inline = TRUE)),
        tags$span(class = "nx-hint", "Calibrated outlook · held-out months")
      ),
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "Built from"),
        div(class = "nx-num nx-num--sm", "Our exit forecast"),
        tags$span(class = "nx-hint", "Then mapped to monthly tender size")
      ),
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "We do not claim"),
        div(class = "nx-num nx-num--sm", "LTA’s internal rule"),
        tags$span(class = "nx-hint", "Policy can still move releases")
      ),
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "Compared to"),
        div(class = "nx-num nx-num--sm", "Published tenders"),
        tags$span(class = "nx-hint", "What was actually put up for sale")
      )
    ),
    div(
      class = "formula-card",
      tags$p(class = "formula-label", "In plain words"),
      tags$code("quota outlook = (how many cars left recently) × (usual monthly release size from history)")
    ),
    layout_columns(
      col_widths = c(12),
      card(
        class = "desk-card",
        full_screen = TRUE,
        card_header("Monthly quota — published vs calibrated outlook"),
        card_body(
          class = "chart-frame chart-frame--tall",
          echarts4rOutput(ns("quota_chart"), height = "400px")
        )
      )
    ),
    tags$p(
      class = "footnote",
      "The exit forecast is the product. Quota is the same exits expressed at tender scale ",
      "so you can read them next to what LTA published."
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    output$example_grid <- renderUI({
      dm <- store$dereg_monthly
      cat <- input$cat
      if (is.null(dm)) return(NULL)
      months <- as.Date(c(
        "2024-09-01", "2025-01-01", "2025-05-01", "2025-08-01",
        "2026-01-01", "2026-05-01"
      ))
      cards <- lapply(months, function(m) {
        a <- dm[dm$category == cat & dm$model == "elastic_net" & dm$month == m, ]
        b <- dm[dm$category == cat & dm$model == "historic_avg" & dm$month == m, ]
        if (!nrow(a)) return(NULL)
        better <- abs(a$predicted[[1]] - a$actual[[1]]) <
          abs(b$predicted[[1]] - b$actual[[1]])
        div(
          class = paste("ex-tile", if (better) "ex-tile--win" else "ex-tile--miss"),
          tags$span(class = "ex-month", strftime(as.Date(m), "%b %Y")),
          tags$div(
            class = "ex-row",
            tags$span("Actual"),
            tags$strong(fmt_int(a$actual[[1]]))
          ),
          tags$div(
            class = "ex-row ex-row--accent",
            tags$span("Forecast"),
            tags$strong(fmt_int(a$predicted[[1]]))
          ),
          tags$div(
            class = "ex-row muted",
            tags$span("Rule of thumb"),
            tags$strong(fmt_int(b$predicted[[1]]))
          )
        )
      })
      div(class = "ex-grid", cards)
    })

    output$kpi_mape <- renderText({
      sc <- store$quota_scores
      if (is.null(sc)) return("—")
      row <- sc[sc$category == input$cat, ]
      if (!nrow(row)) return("—")
      fmt_pct(row$MAPE[[1]])
    })

    output$dereg_chart <- renderEcharts4r({
      dm <- store$dereg_monthly
      cat <- input$cat
      m <- dm[dm$category == cat & dm$model == "elastic_net", ]
      b <- dm[dm$category == cat & dm$model == "historic_avg", ]
      df <- data.frame(
        month = m$month,
        actual = m$actual,
        model = m$predicted,
        baseline = b$predicted[match(m$month, b$month)],
        stringsAsFactors = FALSE
      )
      actual_vs_pred_chart(df)
    })

    output$quota_chart <- renderEcharts4r({
      q <- store$quota_outlook
      if (is.null(q)) return(NULL)
      cat <- input$cat
      one <- q[q$category == cat, ]
      one <- one[order(one$month), ]
      df <- data.frame(
        month = one$month,
        published = one$published,
        from_pred = one$outlook_from_forecast,
        from_actual = one$outlook_from_actual_exits,
        stringsAsFactors = FALSE
      )
      quota_compare_chart(df)
    })
  })
}
