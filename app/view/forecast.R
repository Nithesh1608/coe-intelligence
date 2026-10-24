box::use(
  shiny[NS, moduleServer, div, tags, selectInput, renderUI, uiOutput],
  bslib[layout_columns, card, card_header, card_body],
  echarts4r[renderEcharts4r, echarts4rOutput],
)
box::use(
  app/logic/viewdata/charts[actual_vs_pred_chart, quota_compare_chart],
)

fmt_int <- function(x) {
  format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
}

#' Results: dereg track record + quota (published + outlook)
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
      "and a trailing-average baseline. Quota below shows what LTA actually released to the market ",
      "alongside our mechanical supply outlook from exits."
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
    tags$h2("What was released vs our supply outlook"),
    tags$p(
      class = "lede lede--wide",
      "White = certificates LTA put up for bidding that month (sum of exercises). ",
      "Green = our monthly supply outlook from predicted exits. ",
      "Grey dashed = the same outlook using actual exits — so you can see forecast error separately from policy."
    ),
    div(
      class = "formula-card",
      tags$p(
        class = "formula-label",
        "Mechanical rule (then shown as a monthly rate)"
      ),
      tags$code("A/B annual ≈ 0.90 × 0.25 × trailing 12-month exits   →   monthly ≈ annual ÷ 12")
    ),
    layout_columns(
      col_widths = c(12),
      card(
        class = "desk-card",
        full_screen = TRUE,
        card_header("Monthly quota — published tender vs outlook"),
        card_body(
          class = "chart-frame chart-frame--tall",
          echarts4rOutput(ns("quota_chart"), height = "400px")
        )
      )
    ),
    tags$p(
      class = "footnote",
      "Published quota is the official release. The outlook is the replacement identity from exits; ",
      "LTA can still inject, reallocate, or time supply differently — the gap is informative, not a bug."
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
      q <- store$quota_test
      cat <- input$cat
      primary <- store$meta$primary_model
      one <- q[q$category == cat & q$model == primary, ]
      one <- one[order(one$month), ]
      # Formula series are annual replacement; convert to monthly for scale with published
      df <- data.frame(
        month = one$month,
        published = one$quota_published,
        from_pred = one$quota_from_pred_exits / 12,
        from_actual = one$quota_from_actual_exits / 12,
        stringsAsFactors = FALSE
      )
      quota_compare_chart(df)
    })
  })
}
