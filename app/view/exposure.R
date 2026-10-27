box::use(
  shiny[
    NS, moduleServer, div, tags, numericInput, selectInput, sliderInput,
    renderText, renderUI, uiOutput, reactive, actionButton, observeEvent,
    updateNumericInput, updateSelectInput, updateSliderInput
  ],
  bslib[card, card_header, card_body, layout_columns],
  echarts4r[renderEcharts4r, echarts4rOutput],
)
box::use(
  app/logic/viewdata/charts[exposure_bars_chart],
)

fmt_sgd <- function(x) {
  paste0("$", format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE))
}

fmt_int <- function(x) {
  format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
}

#' Your risk — demo scenario first, then controls
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$p(class = "eyebrow", "Your risk"),
    tags$h2("What does a locked order book cost if the premium moves?"),
    tags$p(
      class = "lede",
      "You already sold the cars at a fixed package price. The certificate still has to be bought. That gap is your risk."
    ),
    div(
      class = "scenario-banner",
      div(
        class = "scenario-banner__copy",
        tags$span(class = "nx-kicker", "Demo scenario"),
        tags$h3("A dealer sold 50 Category A cars this quarter"),
        tags$p(
          "Package price is locked. Bidding happens later. ",
          "If the Certificate of Entitlement premium rises by $5,000 before those bids clear, ",
          "the dealer pays the difference — the customer does not."
        ),
        actionButton(ns("load_demo"), "Load this scenario", class = "btn-demo")
      ),
      div(
        class = "scenario-banner__math",
        tags$span(class = "nx-kicker", "The loss if stress hits"),
        div(class = "risk-hero-num", shiny::textOutput(ns("kpi_extra"), inline = TRUE)),
        tags$p(class = "risk-hero-eq", shiny::textOutput(ns("kpi_extra_eq"), inline = TRUE))
      )
    ),
    div(
      class = "risk-board",
      div(
        class = "risk-board__col",
        tags$span(class = "nx-kicker", "At last settled premium"),
        div(class = "risk-board__val", shiny::textOutput(ns("kpi_base"), inline = TRUE)),
        tags$p(class = "risk-board__note", shiny::textOutput(ns("kpi_base_note"), inline = TRUE))
      ),
      div(class = "risk-board__arrow", "→"),
      div(
        class = "risk-board__col risk-board__col--hot",
        tags$span(class = "nx-kicker", "If premium rises by your stress"),
        div(class = "risk-board__val", shiny::textOutput(ns("kpi_stress"), inline = TRUE)),
        tags$p(class = "risk-board__note", "Same cars · higher certificate cost")
      ),
      div(class = "risk-board__arrow", "="),
      div(
        class = "risk-board__col risk-board__col--accent",
        tags$span(class = "nx-kicker", "Extra dollars on the dealer"),
        div(class = "risk-board__val", shiny::textOutput(ns("kpi_extra2"), inline = TRUE)),
        tags$p(class = "risk-board__note", "This is the unhedged short")
      )
    ),
    layout_columns(
      col_widths = c(4, 8),
      card(
        class = "desk-card",
        card_header("Adjust the book"),
        card_body(
          selectInput(
            ns("category"),
            "Category",
            choices = c(
              "Category A" = "A",
              "Category B" = "B",
              "Category C" = "C",
              "Category E" = "E"
            ),
            selected = "A"
          ),
          numericInput(ns("n_cars"), "Cars sold (still to bid)", 50, min = 1, max = 5000),
          numericInput(ns("n_q"), "Quarters of bidding", 1, min = 1, max = 8),
          sliderInput(
            ns("stress"),
            "Premium rise before you clear ($)",
            min = 0, max = 40000, value = 5000, step = 500
          ),
          tags$p(
            class = "footnote",
            shiny::textOutput(ns("kpi_print_line"), inline = TRUE)
          )
        )
      ),
      card(
        class = "desk-card",
        full_screen = TRUE,
        card_header("Certificate outlay by quarter"),
        card_body(
          class = "chart-frame",
          echarts4rOutput(ns("bars"), height = "320px")
        )
      )
    ),
    div(
      class = "scenario-why",
      tags$h3("Why this sits next to the supply forecast"),
      tags$p(
        "Home tells you whether supply looks tight or loose. ",
        "Your risk tells you what tightness costs ", tags$strong("after"),
        " you have already sold the cars. ",
        "We do not predict the next premium — you choose the stress; we turn it into dollars."
      )
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$load_demo, {
      updateSelectInput(session, "category", selected = "A")
      updateNumericInput(session, "n_cars", value = 50)
      updateNumericInput(session, "n_q", value = 1)
      updateSliderInput(session, "stress", value = 5000)
    })

    last_row <- reactive({
      b <- store$bidding
      b <- b[b$category == input$category & !is.na(b$premium), ]
      if (!nrow(b)) return(NULL)
      utils::tail(b[order(b$month), ], 1)
    })

    schedule <- reactive({
      row <- last_row()
      prem <- if (is.null(row)) 100000 else as.numeric(row$premium[[1]])
      data.frame(
        quarter = paste0("Q", seq_len(input$n_q)),
        vehicles = input$n_cars,
        at_last_print = input$n_cars * prem,
        with_stress = input$n_cars * (prem + input$stress),
        stringsAsFactors = FALSE
      )
    })

    n_total <- reactive(input$n_cars * input$n_q)
    extra <- reactive(n_total() * input$stress)

    output$kpi_extra <- renderText(fmt_sgd(extra()))
    output$kpi_extra2 <- renderText(fmt_sgd(extra()))
    output$kpi_extra_eq <- renderText({
      sprintf(
        "%s cars × %s rise = %s absorbed by the dealer",
        fmt_int(n_total()), fmt_sgd(input$stress), fmt_sgd(extra())
      )
    })

    output$kpi_base <- renderText(fmt_sgd(sum(schedule()$at_last_print)))
    output$kpi_base_note <- renderText({
      row <- last_row()
      prem <- if (is.null(row)) NA else row$premium[[1]]
      sprintf("%s cars × %s", fmt_int(n_total()), fmt_sgd(prem))
    })
    output$kpi_stress <- renderText(fmt_sgd(sum(schedule()$with_stress)))

    output$kpi_print_line <- renderText({
      row <- last_row()
      if (is.null(row)) return("No settled premium in the series for this category.")
      sprintf(
        "Last settled Category %s premium: %s (%s). Source: official bidding results.",
        input$category,
        fmt_sgd(row$premium[[1]]),
        format(as.Date(row$month[[1]]), "%b %Y")
      )
    })

    output$bars <- renderEcharts4r({
      exposure_bars_chart(schedule())
    })
  })
}
