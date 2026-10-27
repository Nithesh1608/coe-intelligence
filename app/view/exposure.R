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

#' Proposal view 4 — Exposure calculator
#' Order book → expected COE cost + adverse case; shift timing by quarters
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$p(class = "eyebrow", "Exposure calculator"),
    tags$h2("Convert a forward order book into Certificate of Entitlement cost"),
    tags$p(
      class = "lede",
      "Proposal case: a dealer holding fifty guaranteed Category A orders is short the certificate. ",
      "Enter the book. See expected cost, an adverse case, and what happens if you shift purchases by one or two quarters."
    ),
    div(
      class = "scenario-banner",
      div(
        class = "scenario-banner__copy",
        tags$span(class = "nx-kicker", "Worked example from the proposal"),
        tags$h3("50 guaranteed Category A orders"),
        tags$p(
          "Package price is locked. Bidding is still ahead. ",
          "If the premium moves between order and bid, the dealer absorbs it."
        ),
        actionButton(ns("load_demo"), "Load proposal example", class = "btn-demo")
      ),
      div(
        class = "scenario-banner__math",
        tags$span(class = "nx-kicker", "Adverse extra versus last print"),
        div(class = "risk-hero-num", shiny::textOutput(ns("kpi_adverse_extra"), inline = TRUE)),
        tags$p(class = "risk-hero-eq", shiny::textOutput(ns("kpi_adverse_eq"), inline = TRUE))
      )
    ),
    layout_columns(
      col_widths = c(4, 8),
      card(
        class = "desk-card",
        card_header("Forward book"),
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
          numericInput(ns("n_cars"), "Vehicles per quarter", 50, min = 1, max = 5000),
          numericInput(ns("n_q"), "Quarters of buying", 4, min = 1, max = 8),
          sliderInput(
            ns("shift"),
            "Shift purchases later (quarters)",
            min = 0, max = 2, value = 0, step = 1
          ),
          tags$p(class = "footnote", shiny::textOutput(ns("print_note"), inline = TRUE)),
          tags$p(
            class = "footnote",
            "Adverse case adds the largest within-quarter premium swing seen historically for this category — ",
            "the proposal’s measure of dealer exposure, not a premium forecast."
          )
        )
      ),
      div(
        div(
          class = "risk-board risk-board--3",
          div(
            class = "risk-board__col",
            tags$span(class = "nx-kicker", "Expected outlay (last print)"),
            div(class = "risk-board__val", shiny::textOutput(ns("kpi_central"), inline = TRUE)),
            tags$p(class = "risk-board__note", shiny::textOutput(ns("kpi_central_note"), inline = TRUE))
          ),
          div(
            class = "risk-board__col risk-board__col--hot",
            tags$span(class = "nx-kicker", "Adverse outlay"),
            div(class = "risk-board__val", shiny::textOutput(ns("kpi_adverse"), inline = TRUE)),
            tags$p(class = "risk-board__note", shiny::textOutput(ns("kpi_adverse_note"), inline = TRUE))
          ),
          div(
            class = "risk-board__col risk-board__col--accent",
            tags$span(class = "nx-kicker", "Timing"),
            div(class = "risk-board__val risk-board__val--sm", shiny::textOutput(ns("kpi_timing"), inline = TRUE)),
            tags$p(class = "risk-board__note", "Shift only moves when cash hits — same unit cost unless you change category")
          )
        ),
        card(
          class = "desk-card",
          full_screen = TRUE,
          card_header("Quarterly Certificate of Entitlement outlay"),
          card_body(
            class = "chart-frame",
            echarts4rOutput(ns("bars"), height = "300px")
          )
        )
      )
    ),
    uiOutput(ns("supply_timing")),
    div(
      class = "scenario-why",
      tags$h3("How this meets the proposal"),
      tags$p(
        "Supply answers how many certificates come to market. ",
        "Exposure answers what a locked book costs at the last print, under an adverse within-quarter swing, ",
        "and whether shifting the schedule by one or two quarters changes when that cost lands. ",
        "Together they answer: what is my cost exposure, and how do I time against it?"
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
      updateNumericInput(session, "n_q", value = 4)
      updateSliderInput(session, "shift", value = 0)
    })

    last_row <- reactive({
      b <- store$bidding
      b <- b[b$category == input$category & !is.na(b$premium), ]
      if (!nrow(b)) return(NULL)
      utils::tail(b[order(b$month), ], 1)
    })

    max_swing <- reactive({
      b <- store$bidding
      b <- b[b$category == input$category & !is.na(b$premium), ]
      if (nrow(b) < 4) return(8000)
      b$q <- paste(
        format(b$month, "%Y"),
        (as.integer(format(b$month, "%m")) - 1L) %/% 3L
      )
      sw <- stats::aggregate(premium ~ q, b, function(z) diff(range(z)))
      max(sw$premium, na.rm = TRUE)
    })

    prem <- reactive({
      row <- last_row()
      if (is.null(row)) 100000 else as.numeric(row$premium[[1]])
    })

    schedule <- reactive({
      data.frame(
        quarter = paste0("Q", seq_len(input$n_q) + input$shift),
        vehicles = input$n_cars,
        at_last_print = input$n_cars * prem(),
        with_stress = input$n_cars * (prem() + max_swing()),
        stringsAsFactors = FALSE
      )
    })

    n_total <- reactive(input$n_cars * input$n_q)

    output$kpi_central <- renderText(fmt_sgd(sum(schedule()$at_last_print)))
    output$kpi_central_note <- renderText({
      sprintf("%s vehicles × last print %s", fmt_int(n_total()), fmt_sgd(prem()))
    })
    output$kpi_adverse <- renderText(fmt_sgd(sum(schedule()$with_stress)))
    output$kpi_adverse_note <- renderText({
      sprintf("Last print + historical within-quarter swing %s", fmt_sgd(max_swing()))
    })
    output$kpi_adverse_extra <- renderText(fmt_sgd(n_total() * max_swing()))
    output$kpi_adverse_eq <- renderText({
      sprintf(
        "%s vehicles × %s max intra-quarter swing",
        fmt_int(n_total()), fmt_sgd(max_swing())
      )
    })
    output$kpi_timing <- renderText({
      if (input$shift == 0) "No shift" else paste0("+", input$shift, " quarter", if (input$shift > 1) "s" else "")
    })
    output$print_note <- renderText({
      row <- last_row()
      if (is.null(row)) return("No settled premium for this category.")
      sprintf(
        "Last settled Category %s premium: %s (%s).",
        input$category, fmt_sgd(row$premium[[1]]), format(as.Date(row$month[[1]]), "%b %Y")
      )
    })

    output$supply_timing <- renderUI({
      q <- store$two_quota
      if (is.null(q) || !input$category %in% c("A", "B")) {
        return(tags$p(
          class = "footnote",
          "Supply timing context is available for Category A and Category B on the Supply tab."
        ))
      }
      # Latest few months: forecast vs simple average as timing cue
      core <- q[q$model == "core_forecast" & q$category == input$category, ]
      base <- q[q$model == "simple_average" & q$category == input$category, ]
      core <- core[order(as.Date(core$month)), ]
      tail_n <- utils::tail(core, 3)
      if (!nrow(tail_n)) return(NULL)
      rows <- lapply(seq_len(nrow(tail_n)), function(i) {
        m <- as.Date(tail_n$month[[i]])
        b <- base$outlook[as.Date(base$month) == m]
        b <- if (length(b)) b[[1]] else NA_real_
        ratio <- tail_n$outlook[[i]] / pmax(b, 1)
        label <- if (is.na(ratio)) {
          "—"
        } else if (ratio < 0.95) {
          "Tighter than average"
        } else if (ratio > 1.05) {
          "Looser than average"
        } else {
          "Near average"
        }
        tags$div(
          class = "timing-chip",
          tags$strong(format(m, "%b %Y")),
          tags$span(label)
        )
      })
      div(
        class = "timing-row",
        tags$span(class = "nx-kicker", "Supply cue (Core forecast · recent months)"),
        div(class = "timing-chips", rows),
        tags$p(
          class = "footnote",
          "Use Supply to inspect the full path. Shift the book above to move cash into a different quarter window."
        )
      )
    })

    output$bars <- renderEcharts4r({
      exposure_bars_chart(schedule())
    })
  })
}
