box::use(
  shiny[
    NS, moduleServer, div, tags, numericInput, selectInput, sliderInput,
    renderText, renderUI, uiOutput, reactive
  ],
  bslib[layout_sidebar, sidebar, card, card_header, card_body, layout_columns],
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

#' Dealer exposure desk — size the COE short from a locked book
#' @export
ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Your book",
      selectInput(
        ns("category"),
        "Which category do you bid in?",
        choices = c(
          "Category A" = "A",
          "Category B" = "B",
          "Category C" = "C",
          "Category E" = "E"
        ),
        selected = "A"
      ),
      numericInput(
        ns("n_cars"),
        "Cars you have sold / will bid for",
        value = 50,
        min = 1,
        max = 5000
      ),
      numericInput(
        ns("n_q"),
        "Quarters those bids fall across",
        value = 4,
        min = 1,
        max = 8
      ),
      sliderInput(
        ns("stress"),
        "If the premium rises by ($)",
        min = 0,
        max = 40000,
        value = 5000,
        step = 500
      ),
      sliderInput(
        ns("shift"),
        "Push the whole book later (quarters)",
        min = 0,
        max = 2,
        value = 0,
        step = 1
      ),
      tags$p(
        class = "footnote",
        "Last print is filled from official bidding results. ",
        "We do not forecast the next premium — you set the stress."
      )
    ),
    div(
      class = "page-stack desk",
      tags$p(class = "eyebrow", "Exposure"),
      tags$h2("If you already sold the cars, what is your Certificate of Entitlement risk?"),
      tags$p(
        class = "lede lede--wide",
        "When you quote a price that includes the certificate, you are exposed until you win a bid. ",
        "This page turns that exposure into dollars — at the last settled price, ",
        "and if the price rises against you."
      ),
      div(
        class = "value-steps",
        div(
          class = "value-step",
          tags$span(class = "value-step__n", "1"),
          tags$div(
            tags$strong("Enter the book"),
            tags$p("How many cars, which category, over how many quarters.")
          )
        ),
        div(
          class = "value-step",
          tags$span(class = "value-step__n", "2"),
          tags$div(
            tags$strong("Set a stress"),
            tags$p("How much the premium might rise before you clear the bids.")
          )
        ),
        div(
          class = "value-step",
          tags$span(class = "value-step__n", "3"),
          tags$div(
            tags$strong("Read the risk"),
            tags$p("Base outlay, stressed outlay, and the extra dollars at risk.")
          )
        )
      ),
      div(
        class = "nx-metrics",
        div(
          class = "nx-metric",
          tags$span(class = "nx-kicker", "Last settled premium"),
          div(class = "nx-num", shiny::textOutput(ns("kpi_print"), inline = TRUE)),
          tags$span(class = "nx-hint", shiny::textOutput(ns("kpi_print_note"), inline = TRUE))
        ),
        div(
          class = "nx-metric",
          tags$span(class = "nx-kicker", "Outlay at last print"),
          div(class = "nx-num", shiny::textOutput(ns("kpi_base"), inline = TRUE)),
          tags$span(class = "nx-hint", shiny::textOutput(ns("kpi_base_note"), inline = TRUE))
        ),
        div(
          class = "nx-metric nx-metric--accent",
          tags$span(class = "nx-kicker", "Extra if premium rises"),
          div(class = "nx-num", shiny::textOutput(ns("kpi_extra"), inline = TRUE)),
          tags$span(class = "nx-hint", shiny::textOutput(ns("kpi_extra_note"), inline = TRUE))
        ),
        div(
          class = "nx-metric",
          tags$span(class = "nx-kicker", "Stressed total outlay"),
          div(class = "nx-num", shiny::textOutput(ns("kpi_stress"), inline = TRUE)),
          tags$span(class = "nx-hint", "Last print + your stress on every car")
        )
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(
          class = "desk-card",
          full_screen = TRUE,
          card_header("COE outlay by quarter"),
          card_body(
            class = "chart-frame",
            echarts4rOutput(ns("bars"), height = "340px")
          )
        ),
        card(
          class = "desk-card desk-card--story",
          card_header("How to read this"),
          card_body(
            uiOutput(ns("story")),
            tags$hr(class = "desk-rule"),
            tags$p(class = "nx-kicker", "Historical context"),
            tags$p(
              class = "strip-body",
              shiny::textOutput(ns("swing_note"), inline = TRUE)
            ),
            tags$p(
              class = "footnote",
              "Shifting quarters only changes when cash hits — it does not invent a new premium path."
            )
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    last_row <- reactive({
      b <- store$bidding
      b <- b[b$category == input$category & !is.na(b$premium), ]
      if (!nrow(b)) return(NULL)
      b[order(b$month), ]
      utils::tail(b, 1)
    })

    max_swing <- reactive({
      b <- store$bidding
      b <- b[b$category == input$category & !is.na(b$premium), ]
      if (nrow(b) < 4) return(NA_real_)
      b$q <- paste(
        format(b$month, "%Y"),
        (as.integer(format(b$month, "%m")) - 1L) %/% 3L
      )
      sw <- stats::aggregate(premium ~ q, b, function(z) diff(range(z)))
      max(sw$premium, na.rm = TRUE)
    })

    schedule <- reactive({
      row <- last_row()
      prem <- if (is.null(row)) 100000 else as.numeric(row$premium[[1]])
      n <- input$n_q
      shift <- input$shift
      stress <- input$stress
      data.frame(
        quarter = paste0("Q", seq_len(n) + shift),
        vehicles = input$n_cars,
        at_last_print = input$n_cars * prem,
        with_stress = input$n_cars * (prem + stress),
        stringsAsFactors = FALSE
      )
    })

    output$kpi_print <- renderText({
      row <- last_row()
      if (is.null(row)) "—" else fmt_sgd(row$premium[[1]])
    })
    output$kpi_print_note <- renderText({
      row <- last_row()
      if (is.null(row)) return("No print in series")
      paste("Cat", input$category, "·", format(as.Date(row$month[[1]]), "%b %Y"))
    })

    output$kpi_base <- renderText({
      fmt_sgd(sum(schedule()$at_last_print))
    })
    output$kpi_base_note <- renderText({
      paste(fmt_int(input$n_cars * input$n_q), "cars × last print")
    })

    output$kpi_extra <- renderText({
      fmt_sgd(input$n_cars * input$n_q * input$stress)
    })
    output$kpi_extra_note <- renderText({
      paste("Your stress of", fmt_sgd(input$stress), "per car")
    })

    output$kpi_stress <- renderText({
      fmt_sgd(sum(schedule()$with_stress))
    })

    output$swing_note <- renderText({
      sw <- max_swing()
      if (is.na(sw)) {
        return("Not enough history to size a within-quarter swing for this category.")
      }
      sprintf(
        "In this category, the largest premium move inside a single quarter in our history is %s. Use that as a reality check on the stress you set.",
        fmt_sgd(sw)
      )
    })

    output$story <- renderUI({
      n <- input$n_cars * input$n_q
      extra <- n * input$stress
      tags$div(
        class = "story-list-wrap",
        tags$p(
          sprintf(
            "You are carrying %s Category %s certificates. At the last print that book costs %s.",
            fmt_int(n), input$category, fmt_sgd(sum(schedule()$at_last_print))
          )
        ),
        tags$p(
          sprintf(
            "If the premium is %s higher when you bid, you absorb an extra %s — that is the unhedged short.",
            fmt_sgd(input$stress), fmt_sgd(extra)
          )
        ),
        tags$p(
          "Supply timing (Forecast tab) tells you whether the market is likely loose or tight. ",
          "This tab sizes what that tightness costs your P&L if you already sold the cars."
        )
      )
    })

    output$bars <- renderEcharts4r({
      exposure_bars_chart(schedule())
    })
  })
}
