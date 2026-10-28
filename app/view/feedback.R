box::use(
  shiny[NS, moduleServer, div, tags, sliderInput],
  bslib[layout_sidebar, sidebar, card, card_header, card_body, layout_columns],
  echarts4r[renderEcharts4r, echarts4rOutput],
)
box::use(
  app/logic/viewdata/charts[style_chart],
  echarts4r[e_charts, e_line, e_datazoom, e_color],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Renewal loop",
      sliderInput(
        ns("pqp_mark"),
        "Highlight PQP above ($)",
        min = 20000,
        max = 120000,
        value = 80000,
        step = 5000
      ),
      tags$p(
        class = "footnote",
        "When PQP is high, more owners revalidate instead of scrap. That cuts ",
        "future deregistrations and therefore future quota. Move the marker to ",
        "see which months sat in that regime."
      )
    ),
    div(
      class = "page-stack",
      tags$h2("Feedback"),
      tags$p(
        class = "lede",
        "PQP versus revalidations versus deregistrations for Category A. ",
        "The loop is visible in the published series; it is not yet a fitted elasticity."
      ),
      layout_columns(
        col_widths = c(12),
        card(
          full_screen = TRUE,
          card_header("Category A — PQP, renewals, exits"),
          card_body(class = "chart-frame chart-frame--tall", echarts4rOutput(ns("loop"), height = "400px"))
        )
      )
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    output$loop <- renderEcharts4r({
      a <- store$panel
      a <- a[a$category == "A" & a$month >= as.Date("2015-01-01"), ]
      a$month_lab <- format(a$month, "%Y-%m")
      a$pqp_high <- ifelse(a$pqp >= input$pqp_mark, a$pqp, NA_real_)
      a |>
        e_charts(month_lab) |>
        e_line(pqp, name = "PQP", y_index = 1, symbol = "none") |>
        e_line(revalidations_total, name = "Revalidations", symbol = "none") |>
        e_line(deregistrations, name = "Deregistrations", symbol = "none") |>
        e_color(c("#fbbf24", "#34d399", "#7dd3fc")) |>
        style_chart() |>
        e_datazoom(type = "inside")
    })
  })
}
