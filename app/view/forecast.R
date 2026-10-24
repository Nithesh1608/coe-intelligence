box::use(
  shiny[NS, moduleServer, div, tags, selectInput, renderUI, uiOutput, reactive],
  bslib[layout_columns, card, card_header, card_body],
  echarts4r[renderEcharts4r, echarts4rOutput],
)
box::use(
  app/logic/viewdata/charts[actual_vs_pred_chart, quota_compare_chart],
)

fmt_int <- function(x) {
  format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
}

#' Simple showcase: dereg examples + quota from formula
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$p(class = "eyebrow", "Results · plain numbers"),
    tags$h2("Did the forecast match real exits?"),
    tags$p(
      class = "lede lede--wide",
      "Same months. Official deregistrations versus our forecast and the old trailing-average baseline. ",
      "No jargon on this page — Method explains the terms."
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
        card_header("Full test window — exits"),
        card_body(
          class = "chart-frame chart-frame--tall",
          echarts4rOutput(ns("dereg_chart"), height = "400px")
        )
      )
    ),
    tags$hr(class = "desk-rule"),
    tags$p(class = "eyebrow", "Level 2 · quota"),
    tags$h2("From exits to replacement quota"),
    tags$p(
      class = "lede lede--wide",
      "LTA’s mechanical idea: about a quarter of the last twelve months of exits comes back as ",
      "new COE supply. Categories A and B keep 90% of that base; 10% feeds Category E. ",
      "We plug our predicted exits into that formula and compare to plugging in the true exits."
    ),
    div(
      class = "formula-card",
      tags$code("A/B quota ≈ 0.90 × 0.25 × (sum of last 12 months of exits)")
    ),
    layout_columns(
      col_widths = c(12),
      card(
        class = "desk-card",
        full_screen = TRUE,
        card_header("Formula quota from our exits vs from actual exits"),
        card_body(
          class = "chart-frame chart-frame--tall",
          echarts4rOutput(ns("quota_chart"), height = "400px")
        )
      )
    ),
    tags$p(
      class = "footnote",
      "Published tender quotas can differ from this formula (policy injections, taxis, timing). ",
      "Here we judge whether bad exit forecasts spoil the quota identity — not whether LTA ",
      "published the exact same number."
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
        gap <- a$predicted[[1]] - a$actual[[1]]
        better <- abs(gap) < abs(b$predicted[[1]] - b$actual[[1]])
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
            tags$span("Our forecast"),
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
      df <- data.frame(
        month = one$month,
        from_actual = one$quota_from_actual_exits,
        from_pred = one$quota_from_pred_exits,
        stringsAsFactors = FALSE
      )
      quota_compare_chart(df)
    })
  })
}
