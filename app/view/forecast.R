box::use(
  shiny[NS, moduleServer, div, tags, selectInput, renderUI, uiOutput, renderText],
  bslib[layout_columns, card, card_header, card_body],
  echarts4r[renderEcharts4r, echarts4rOutput],
)
box::use(
  app/logic/viewdata/charts[two_model_chart, quota_compare_chart],
)

fmt_int <- function(x) {
  format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_pct <- function(x) {
  sprintf("%.0f%%", 100 * as.numeric(x))
}

#' Results page — plain language, two forecasts
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$p(class = "eyebrow", "Results"),
    tags$h2("Did we get close to the real numbers?"),
    tags$p(
      class = "lede lede--wide",
      "These months were hidden while we built the forecasts. ",
      "Below: real cars leaving Category A or Category B, next to both of our forecasts ",
      "and the simple twelve-month average."
    ),
    selectInput(
      ns("cat"),
      "Show results for",
      choices = c("Category A" = "A", "Category B" = "B"),
      selected = "A",
      width = "220px"
    ),
    uiOutput(ns("example_grid")),
    layout_columns(
      col_widths = c(12),
      card(
        class = "desk-card",
        full_screen = TRUE,
        card_header("Full picture — cars leaving each month"),
        card_body(
          class = "chart-frame chart-frame--tall",
          echarts4rOutput(ns("dereg_chart"), height = "420px")
        )
      )
    ),
    tags$hr(class = "desk-rule"),
    tags$p(class = "eyebrow", "Quota"),
    tags$h2("From cars leaving to certificates released"),
    tags$p(
      class = "lede lede--wide",
      "White line = how many Category A or Category B certificates LTA actually put up for bidding that month. ",
      "Green line = our outlook based on forecasted exits. ",
      "Grey dashed = the same outlook if we had known the real exits (to separate exit error from the quota map)."
    ),
    div(
      class = "nx-metrics",
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "Typical miss versus published"),
        div(class = "nx-num", shiny::textOutput(ns("kpi_mape"), inline = TRUE)),
        tags$span(class = "nx-hint", "On months we held out for testing")
      ),
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "Built from"),
        div(class = "nx-num nx-num--sm", "Exit forecasts"),
        tags$span(class = "nx-hint", "Then mapped to monthly certificate size")
      ),
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "We are not claiming"),
        div(class = "nx-num nx-num--sm", "LTA’s private formula"),
        tags$span(class = "nx-hint", "Policy can still change releases")
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
      tags$code("quota outlook = (cars that left recently) × (usual monthly certificate size from history)")
    ),
    layout_columns(
      col_widths = c(12),
      card(
        class = "desk-card",
        full_screen = TRUE,
        card_header("Monthly certificates — published versus our outlook"),
        card_body(
          class = "chart-frame chart-frame--tall",
          echarts4rOutput(ns("quota_chart"), height = "400px")
        )
      )
    ),
    tags$p(
      class = "footnote",
      "The exit forecasts are the main product. Quota is those exits expressed in certificate units ",
      "so you can read them next to what LTA published."
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    output$example_grid <- renderUI({
      dm <- store$two_monthly
      cat <- input$cat
      if (is.null(dm)) return(NULL)
      months <- as.Date(c(
        "2024-09-01", "2025-01-01", "2025-05-01", "2025-08-01",
        "2026-01-01", "2026-05-01"
      ))
      cards <- lapply(months, function(m) {
        core <- dm[dm$category == cat & dm$model == "core_forecast" & dm$month == m, ]
        mom <- dm[dm$category == cat & dm$model == "momentum_forecast" & dm$month == m, ]
        sim <- dm[dm$category == cat & dm$model == "simple_average" & dm$month == m, ]
        if (!nrow(core)) return(NULL)
        better <- abs(core$predicted[[1]] - core$actual[[1]]) <
          abs(sim$predicted[[1]] - sim$actual[[1]])
        div(
          class = paste("ex-tile", if (better) "ex-tile--win" else "ex-tile--miss"),
          tags$span(class = "ex-month", strftime(as.Date(m), "%b %Y")),
          tags$div(
            class = "ex-row",
            tags$span("Actually left"),
            tags$strong(fmt_int(core$actual[[1]]))
          ),
          tags$div(
            class = "ex-row ex-row--accent",
            tags$span("Core forecast"),
            tags$strong(fmt_int(core$predicted[[1]]))
          ),
          tags$div(
            class = "ex-row ex-row--green",
            tags$span("Momentum"),
            tags$strong(fmt_int(mom$predicted[[1]]))
          ),
          tags$div(
            class = "ex-row muted",
            tags$span("Simple average"),
            tags$strong(fmt_int(sim$predicted[[1]]))
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
      dm <- store$two_monthly
      cat <- input$cat
      core <- dm[dm$category == cat & dm$model == "core_forecast", ]
      mom <- dm[dm$category == cat & dm$model == "momentum_forecast", ]
      sim <- dm[dm$category == cat & dm$model == "simple_average", ]
      df <- data.frame(
        month = core$month,
        actual = core$actual,
        core = core$predicted,
        momentum = mom$predicted[match(core$month, mom$month)],
        simple = sim$predicted[match(core$month, sim$month)],
        stringsAsFactors = FALSE
      )
      two_model_chart(df)
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
