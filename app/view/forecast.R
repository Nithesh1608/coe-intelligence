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
        tags$span(class = "nx-kicker", "How the link is built"),
        div(class = "nx-num nx-num--sm", "Exits → tender scale"),
        tags$span(class = "nx-hint", shiny::textOutput(ns("kpi_alpha"), inline = TRUE))
      ),
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "What we do not claim"),
        div(class = "nx-num nx-num--sm", "Not LTA’s internal rule"),
        tags$span(class = "nx-hint", "Policy injections can still move the print")
      ),
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "Why not raw 25%?"),
        div(class = "nx-num nx-num--sm", "Wrong scale for monthly"),
        tags$span(class = "nx-hint", "Textbook identity ≠ fortnightly release")
      )
    ),
    div(
      class = "formula-card",
      tags$p(class = "formula-label", "Exit-driven monthly outlook"),
      tags$code("monthly quota outlook ≈ α × trailing 12-month exits   (α fit on history through Aug 2024)")
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
      "The raw “25% of annual exits” rule is an annual replacement identity. ",
      "Monthly tender quotas sit on a different scale, so we calibrate α on pre-test history ",
      "and keep exits as the driver. That is the product-honest way to show both series together."
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
            tags$span("Baseline"),
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

    output$kpi_alpha <- renderText({
      a <- store$quota_alpha
      if (is.null(a)) return("—")
      row <- a[a$category == input$cat, ]
      if (!nrow(row)) return("—")
      sprintf("α = %.3f for Cat %s", row$alpha[[1]], input$cat)
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
