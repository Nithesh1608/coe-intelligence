box::use(
  shiny[NS, moduleServer, div, tags, sliderInput],
  bslib[layout_sidebar, sidebar, card, card_header, card_body],
  echarts4r[renderEcharts4r, echarts4rOutput, e_charts, e_line, e_datazoom, e_color],
)
box::use(
  app/logic/viewdata/charts[style_chart],
)

#' Proposal view 2 — Feedback and scenarios (PQP → renewals → exits)
#' @export
ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Renewal feedback",
      sliderInput(
        ns("pqp_mark"),
        "Highlight Prevailing Quota Premium above ($)",
        min = 20000,
        max = 120000,
        value = 80000,
        step = 5000
      ),
      tags$p(
        class = "footnote",
        "When the Prevailing Quota Premium is high, more owners revalidate instead of scrap. ",
        "That cuts future deregistrations and therefore future quota."
      )
    ),
    div(
      class = "page-stack desk",
      tags$p(class = "eyebrow", "Feedback"),
      tags$h2("High premiums can suppress future supply"),
      tags$p(
        class = "lede",
        "Proposal loop: Prevailing Quota Premium → renewals → fewer exits → tighter quota later. ",
        "Category A series below."
      ),
      card(
        class = "desk-card",
        full_screen = TRUE,
        card_header("Category A — Prevailing Quota Premium, renewals, deregistrations"),
        card_body(
          class = "chart-frame chart-frame--tall",
          echarts4rOutput(ns("loop"), height = "400px")
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
      a <- a[order(a$month), ]
      a$month_lab <- format(a$month, "%Y-%m")
      # Keep columns the chart needs even if some NA
      if (!"pqp" %in% names(a)) a$pqp <- NA_real_
      if (!"revalidations_total" %in% names(a)) a$revalidations_total <- NA_real_
      a |>
        e_charts(month_lab) |>
        e_line(pqp, name = "Prevailing Quota Premium", y_index = 1, symbol = "none") |>
        e_line(revalidations_total, name = "Revalidations", symbol = "none") |>
        e_line(deregistrations, name = "Deregistrations", symbol = "none") |>
        e_color(c("#fbbf24", "#34d399", "#7dd3fc")) |>
        style_chart() |>
        e_datazoom(type = "inside")
    })
  })
}
