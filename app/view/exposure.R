box::use(
  shiny[NS, moduleServer, div, tags, numericInput, selectInput, sliderInput, renderText],
  bslib[layout_sidebar, sidebar, card, card_header, card_body, value_box, layout_columns],
  echarts4r[renderEcharts4r, echarts4rOutput],
  bsicons[bs_icon],
)
box::use(
  app/logic/viewdata/charts[bar_chart],
)

fmt_sgd <- function(x) {
  paste0("$", format(round(x), big.mark = ",", scientific = FALSE))
}

#' @export
ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Order book",
      selectInput(ns("category"), "Category", c("A", "B", "C", "D", "E"), "A"),
      numericInput(ns("n_cars"), "Vehicles per quarter", 50, min = 1, max = 5000),
      numericInput(ns("n_q"), "Quarters of buying", 4, min = 1, max = 8),
      sliderInput(ns("shift"), "Delay (quarters)", 0, 2, 0, step = 1),
      sliderInput(
        ns("stress"),
        "Premium stress vs last print ($)",
        min = -20000,
        max = 40000,
        value = 0,
        step = 1000
      ),
      tags$p(
        class = "footnote",
        "Cost = vehicles × (last premium + stress). Shifting a quarter only ",
        "changes timing; it does not forecast a new premium. Adverse case adds ",
        "the largest Cat A within-quarter swing on record in the window."
      )
    ),
    div(
      class = "page-stack",
      tags$h2("Exposure calculator"),
      tags$p(
        class = "lede",
        "A dealer locking fifty package prices is short COE. This converts that ",
        "book into dollars under a premium scenario — not a premium forecast."
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        value_box(
          title = "Expected COE outlay",
          value = shiny::textOutput(ns("kpi_central"), inline = TRUE),
          showcase = bs_icon("receipt")
        ),
        value_box(
          title = "Adverse (max intra-quarter swing)",
          value = shiny::textOutput(ns("kpi_adverse"), inline = TRUE),
          showcase = bs_icon("exclamation-triangle")
        ),
        value_box(
          title = "Vehicles in the book",
          value = shiny::textOutput(ns("kpi_n"), inline = TRUE),
          showcase = bs_icon("truck")
        )
      ),
      card(
        full_screen = TRUE,
        card_header("Quarterly COE outlay"),
        card_body(echarts4rOutput(ns("bars"), height = "380px"))
      )
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    last_premium <- function() {
      b <- store$bidding
      b <- b[b$category == input$category, ]
      utils::tail(b$premium, 1)
    }

    max_swing <- function() {
      b <- store$bidding
      b <- b[b$category == input$category, ]
      if (!nrow(b)) {
        return(8000)
      }
      b$q <- paste(format(b$month, "%Y"), (as.integer(format(b$month, "%m")) - 1L) %/% 3L)
      sw <- stats::aggregate(premium ~ q, b, function(z) diff(range(z)))
      max(sw$premium, na.rm = TRUE)
    }

    schedule <- function() {
      n <- input$n_q
      shift <- input$shift
      prem <- last_premium() + input$stress
      data.frame(
        quarter = paste0("Q", seq_len(n) + shift),
        vehicles = input$n_cars,
        premium = prem,
        cost = input$n_cars * prem,
        stringsAsFactors = FALSE
      )
    }

    output$kpi_central <- renderText({
      fmt_sgd(sum(schedule()$cost))
    })
    output$kpi_adverse <- renderText({
      sch <- schedule()
      fmt_sgd(sum(sch$cost) + input$n_cars * input$n_q * max_swing())
    })
    output$kpi_n <- renderText({
      format(input$n_cars * input$n_q, big.mark = ",")
    })

    output$bars <- renderEcharts4r({
      sch <- schedule()
      bar_chart(sch, "quarter", "cost")
    })
  })
}
