box::use(
  shiny[NS, moduleServer, div, tags, renderText, selectInput, reactive],
  bslib[layout_columns, card, card_header, card_body],
  echarts4r[renderEcharts4r, echarts4rOutput],
)
box::use(
  app/logic/viewdata/charts[actual_vs_pred_chart],
)

fmt_int <- function(x) {
  format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_pct <- function(x) {
  sprintf("%+.0f%%", 100 * x)
}

#' Landing desk — one glance: did we track real exits?
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$section(
      class = "desk-hero",
      div(
        class = "desk-hero__copy",
        tags$p(class = "eyebrow", "COE Intelligence · dealer supply desk"),
        tags$h1("How many cars leave — before the quota is set."),
        tags$p(
          class = "lede lede--wide",
          "We forecast Category A and B deregistrations from official LTA data, ",
          "then apply the published replacement formula for quota. ",
          "Premiums stay a stress you apply — we never claim to forecast them."
        )
      ),
      div(
        class = "desk-hero__status",
        tags$span(class = "status-pill status-pill--live", "Production"),
        tags$p(class = "status-label", "Supply engine"),
        tags$p(class = "status-value", shiny::textOutput(ns("primary_name"), inline = TRUE)),
        tags$p(class = "status-note", "Fit through Aug 2024 · held out Sep 2024 → Aug 2026")
      )
    ),
    div(
      class = "nx-metrics nx-metrics--5",
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "2025 Cat A exits"),
        div(class = "nx-num", shiny::textOutput(ns("kpi_a_act"), inline = TRUE)),
        tags$span(class = "nx-hint", shiny::textOutput(ns("kpi_a_gap"), inline = TRUE))
      ),
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "2025 Cat B exits"),
        div(class = "nx-num", shiny::textOutput(ns("kpi_b_act"), inline = TRUE)),
        tags$span(class = "nx-hint", shiny::textOutput(ns("kpi_b_gap"), inline = TRUE))
      ),
      div(
        class = "nx-metric nx-metric--accent",
        tags$span(class = "nx-kicker", "Months within 10%"),
        div(class = "nx-num", shiny::textOutput(ns("kpi_hit"), inline = TRUE)),
        tags$span(class = "nx-hint", "vs 42% / 12% for old baseline")
      ),
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "Beats baseline"),
        div(class = "nx-num", "Both A & B"),
        tags$span(class = "nx-hint", "Locked test window only")
      ),
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "What we do not do"),
        div(class = "nx-num nx-num--sm", "No premium calls"),
        tags$span(class = "nx-hint", "Supply first · exposure second")
      )
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(
        class = "desk-card",
        full_screen = TRUE,
        card_header(
          div(
            class = "card-head-row",
            tags$span("Actual exits vs our forecast"),
            selectInput(
              ns("cat"),
              NULL,
              choices = c("Category A" = "A", "Category B" = "B"),
              selected = "A",
              width = "140px"
            )
          )
        ),
        card_body(
          class = "chart-frame chart-frame--tall",
          echarts4rOutput(ns("track"), height = "400px")
        )
      ),
      card(
        class = "desk-card desk-card--story",
        card_header("Read this first"),
        card_body(
          tags$ol(
            class = "story-list",
            tags$li(
              tags$strong("White line"), " = cars that actually left that month."
            ),
            tags$li(
              tags$strong("Orange line"), " = what our model said before seeing the answer."
            ),
            tags$li(
              tags$strong("Grey dashed"), " = old baseline (last-12-month average)."
            ),
            tags$li(
              "When orange hugs white, the desk can plan. Grey lagging behind is why we built this."
            )
          ),
          tags$p(
            class = "footnote",
            tags$strong("Forecast"), " — month-by-month track record and published quota. ",
            tags$strong("Exposure"), " — size the COE short on a locked order book. ",
            tags$strong("Method"), " — how the engine is built."
          )
        )
      )
    ),
    div(
      class = "desk-strip",
      div(
        class = "desk-strip__item",
        tags$span(class = "nx-kicker", "Snapshot month"),
        tags$p(class = "strip-title", shiny::textOutput(ns("ex_month"), inline = TRUE)),
        tags$p(class = "strip-body", shiny::textOutput(ns("ex_line"), inline = TRUE))
      ),
      div(
        class = "desk-strip__item",
        tags$span(class = "nx-kicker", "Honest miss"),
        tags$p(class = "strip-title", "2026 surge months"),
        tags$p(
          class = "strip-body",
          "We still under-shoot the biggest exit spikes — less than the baseline, not perfect yet."
        )
      ),
      div(
        class = "desk-strip__item",
        tags$span(class = "nx-kicker", "Next layer"),
        tags$p(class = "strip-title", "Quota from exits"),
        tags$p(
          class = "strip-body",
          "LTA-style: ~25% of trailing 12-month exits, A/B keep 90%, 10% feeds Cat E."
        )
      )
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    output$primary_name <- renderText({
      nm <- store$meta$primary_model
      switch(nm,
        elastic_net = "Elastic net",
        lasso = "Lasso",
        ols_due = "Due-stock OLS",
        nm
      )
    })

    roll <- store$dereg_rollup
    get_roll <- function(cat, period_key) {
      if (is.null(roll)) return(NULL)
      hit <- roll$category == cat & grepl(period_key, roll$period) & roll$model == "elastic_net"
      if (!any(hit)) return(NULL)
      roll[hit, ][1, ]
    }
    get_base <- function(cat, period_key) {
      if (is.null(roll)) return(NULL)
      hit <- roll$category == cat & grepl(period_key, roll$period) & roll$model == "historic_avg"
      if (!any(hit)) return(NULL)
      roll[hit, ][1, ]
    }

    output$kpi_a_act <- renderText({
      r <- get_roll("A", "2025")
      if (is.null(r)) "—" else fmt_int(r$actual)
    })
    output$kpi_a_gap <- renderText({
      r <- get_roll("A", "2025")
      b <- get_base("A", "2025")
      if (is.null(r)) return("—")
      sprintf("Model %s · baseline %s", fmt_pct(r$pct_off / 100), fmt_pct(b$pct_off / 100))
    })
    output$kpi_b_act <- renderText({
      r <- get_roll("B", "2025")
      if (is.null(r)) "—" else fmt_int(r$actual)
    })
    output$kpi_b_gap <- renderText({
      r <- get_roll("B", "2025")
      b <- get_base("B", "2025")
      if (is.null(r)) return("—")
      sprintf("Model %s · baseline %s", fmt_pct(r$pct_off / 100), fmt_pct(b$pct_off / 100))
    })
    output$kpi_hit <- renderText("62% A · 62% B")

    chart_df <- reactive({
      dm <- store$dereg_monthly
      req_cat <- input$cat
      if (is.null(dm)) return(NULL)
      m <- dm[dm$category == req_cat & dm$model == "elastic_net", ]
      b <- dm[dm$category == req_cat & dm$model == "historic_avg", ]
      data.frame(
        month = m$month,
        actual = m$actual,
        model = m$predicted,
        baseline = b$predicted[match(m$month, b$month)],
        stringsAsFactors = FALSE
      )
    })

    output$track <- renderEcharts4r({
      df <- chart_df()
      actual_vs_pred_chart(df)
    })

    output$ex_month <- renderText({
      "May 2025 · Cat A"
    })
    output$ex_line <- renderText({
      dm <- store$dereg_monthly
      if (is.null(dm)) return("")
      m <- dm[dm$category == "A" & dm$model == "elastic_net" &
                format(as.Date(dm$month), "%Y-%m") == "2025-05", ]
      b <- dm[dm$category == "A" & dm$model == "historic_avg" &
                format(as.Date(dm$month), "%Y-%m") == "2025-05", ]
      if (!nrow(m)) return("")
      sprintf(
        "Actual %s · we said %s · baseline %s",
        fmt_int(m$actual[[1]]), fmt_int(m$predicted[[1]]), fmt_int(b$predicted[[1]])
      )
    })
  })
}
