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

#' Landing desk — startup product home
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$section(
      class = "desk-hero",
      div(
        class = "desk-hero__copy",
        tags$p(class = "eyebrow", "COE Intelligence"),
        tags$h1("Supply visibility for people who buy COEs every week."),
        tags$p(
          class = "lede lede--wide",
          "We forecast Category A and B exits, turn them into a monthly quota outlook, ",
          "and size the COE short on your locked order book. ",
          "We do not pretend to call the next premium."
        )
      ),
      div(
        class = "desk-hero__status",
        tags$span(class = "status-pill status-pill--live", "Live"),
        tags$p(class = "status-label", "Product"),
        tags$p(class = "status-value", "Supply desk"),
        tags$p(class = "status-note", "Official LTA series · Cat A & B · Singapore")
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
        tags$span(class = "nx-hint", "vs rule-of-thumb 42% / 12%")
      ),
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "Beats rule of thumb"),
        div(class = "nx-num", "A and B"),
        tags$span(class = "nx-hint", "Held-out Sep 2024 → Aug 2026")
      ),
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "Not in scope"),
        div(class = "nx-num nx-num--sm", "Premium tips"),
        tags$span(class = "nx-hint", "Stress them on Exposure instead")
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
        card_header("30-second read"),
        card_body(
          tags$ol(
            class = "story-list",
            tags$li(tags$strong("White"), " — cars that actually left."),
            tags$li(tags$strong("Orange"), " — our forecast before the month settled."),
            tags$li(tags$strong("Grey"), " — trailing twelve-month average (rule of thumb)."),
            tags$li("Orange tracking white is the product. Grey lagging is the gap we close.")
          ),
          tags$p(
            class = "footnote",
            tags$strong("Forecast"), " — track record and quota. ",
            tags$strong("Exposure"), " — dollar risk on a locked book. ",
            tags$strong("Engine"), " — how it is built."
          )
        )
      )
    ),
    div(
      class = "desk-strip",
      div(
        class = "desk-strip__item",
        tags$span(class = "nx-kicker", "Snapshot"),
        tags$p(class = "strip-title", shiny::textOutput(ns("ex_month"), inline = TRUE)),
        tags$p(class = "strip-body", shiny::textOutput(ns("ex_line"), inline = TRUE))
      ),
      div(
        class = "desk-strip__item",
        tags$span(class = "nx-kicker", "Known limit"),
        tags$p(class = "strip-title", "2026 surge months"),
        tags$p(
          class = "strip-body",
          "The sharpest spikes still run hotter than we call — closer than the rule of thumb, not perfect."
        )
      ),
      div(
        class = "desk-strip__item",
        tags$span(class = "nx-kicker", "Quota"),
        tags$p(class = "strip-title", "Exits → tender scale"),
        tags$p(
          class = "strip-body",
          "We map trailing exits to monthly release size so outlook sits next to what LTA published."
        )
      )
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
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
      sprintf("Us %s · rule of thumb %s", fmt_pct(r$pct_off / 100), fmt_pct(b$pct_off / 100))
    })
    output$kpi_b_act <- renderText({
      r <- get_roll("B", "2025")
      if (is.null(r)) "—" else fmt_int(r$actual)
    })
    output$kpi_b_gap <- renderText({
      r <- get_roll("B", "2025")
      b <- get_base("B", "2025")
      if (is.null(r)) return("—")
      sprintf("Us %s · rule of thumb %s", fmt_pct(r$pct_off / 100), fmt_pct(b$pct_off / 100))
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
      actual_vs_pred_chart(chart_df())
    })

    output$ex_month <- renderText("May 2025 · Cat A")
    output$ex_line <- renderText({
      dm <- store$dereg_monthly
      if (is.null(dm)) return("")
      m <- dm[dm$category == "A" & dm$model == "elastic_net" &
                format(as.Date(dm$month), "%Y-%m") == "2025-05", ]
      b <- dm[dm$category == "A" & dm$model == "historic_avg" &
                format(as.Date(dm$month), "%Y-%m") == "2025-05", ]
      if (!nrow(m)) return("")
      sprintf(
        "Actual %s · we said %s · rule of thumb %s",
        fmt_int(m$actual[[1]]), fmt_int(m$predicted[[1]]), fmt_int(b$predicted[[1]])
      )
    })
  })
}
