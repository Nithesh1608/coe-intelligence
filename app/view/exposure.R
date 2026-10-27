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

#' Your risk — supply forecast drives tightness; dollars follow
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$p(class = "eyebrow", "Your risk"),
    tags$h2("Supply tells you when. This page tells you the dollars."),
    tags$p(
      class = "lede",
      "Anyone can multiply cars × a premium rise. The forecast adds the missing piece: ",
      "is the month you clear bids looking tight or loose — and therefore how hard to stress the book."
    ),
    div(
      class = "scenario-banner",
      div(
        class = "scenario-banner__copy",
        tags$span(class = "nx-kicker", "Demo scenario"),
        tags$h3("50 Category A cars sold. Certificates not bought yet."),
        tags$p(
          "Pick a clearing month. We read our supply forecast for that month. ",
          "If supply looks tighter than the simple average, we raise the stress you should plan for."
        ),
        actionButton(ns("load_demo"), "Load demo", class = "btn-demo")
      ),
      div(
        class = "scenario-banner__math",
        tags$span(class = "nx-kicker", "Extra dollars if stress hits"),
        div(class = "risk-hero-num", shiny::textOutput(ns("kpi_extra"), inline = TRUE)),
        tags$p(class = "risk-hero-eq", shiny::textOutput(ns("kpi_extra_eq"), inline = TRUE))
      )
    ),
    div(
      class = "desk-controls desk-controls--row",
      selectInput(
        ns("model"),
        "Supply forecast",
        choices = c(
          "Core supply forecast" = "core_forecast",
          "Momentum supply forecast" = "momentum_forecast"
        ),
        selected = "core_forecast",
        width = "240px"
      ),
      selectInput(
        ns("category"),
        "Category",
        choices = c("Category A" = "A", "Category B" = "B"),
        selected = "A",
        width = "160px"
      ),
      uiOutput(ns("month_ui")),
      numericInput(ns("n_cars"), "Cars still to bid", 50, min = 1, max = 5000, width = "140px")
    ),
    uiOutput(ns("tightness_card")),
    div(
      class = "risk-board",
      div(
        class = "risk-board__col",
        tags$span(class = "nx-kicker", "Certificate outlay at last print"),
        div(class = "risk-board__val", shiny::textOutput(ns("kpi_base"), inline = TRUE)),
        tags$p(class = "risk-board__note", shiny::textOutput(ns("kpi_base_note"), inline = TRUE))
      ),
      div(class = "risk-board__arrow", "→"),
      div(
        class = "risk-board__col risk-board__col--hot",
        tags$span(class = "nx-kicker", "Outlay if premium rises"),
        div(class = "risk-board__val", shiny::textOutput(ns("kpi_stress"), inline = TRUE)),
        tags$p(class = "risk-board__note", shiny::textOutput(ns("kpi_stress_note"), inline = TRUE))
      ),
      div(class = "risk-board__arrow", "="),
      div(
        class = "risk-board__col risk-board__col--accent",
        tags$span(class = "nx-kicker", "Dealer absorbs"),
        div(class = "risk-board__val", shiny::textOutput(ns("kpi_extra2"), inline = TRUE)),
        tags$p(class = "risk-board__note", "Unhedged short on the locked book")
      )
    ),
    layout_columns(
      col_widths = c(5, 7),
      card(
        class = "desk-card",
        card_header("Stress (can override)"),
        card_body(
          sliderInput(
            ns("stress"),
            "Premium rise before bids clear ($)",
            min = 0, max = 40000, value = 5000, step = 500
          ),
          tags$p(class = "footnote", shiny::textOutput(ns("suggest_note"), inline = TRUE)),
          tags$p(class = "footnote", shiny::textOutput(ns("print_note"), inline = TRUE))
        )
      ),
      card(
        class = "desk-card desk-card--story",
        card_header("What the forecast changed"),
        card_body(uiOutput(ns("value_story")))
      )
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    months_available <- reactive({
      q <- store$two_quota
      if (is.null(q)) return(as.Date(character()))
      sort(unique(as.Date(q$month[q$category == input$category & q$model == input$model])))
    })

    output$month_ui <- renderUI({
      ms <- months_available()
      if (!length(ms)) {
        return(tags$p(class = "footnote", "No supply months loaded."))
      }
      # Prefer a mid demo month if present
      default <- as.Date("2025-05-01")
      if (!default %in% ms) default <- ms[max(1, length(ms) %/% 2)]
      selectInput(
        session$ns("clear_month"),
        "Clear bids in",
        choices = setNames(as.character(ms), format(ms, "%b %Y")),
        selected = as.character(default),
        width = "160px"
      )
    })

    observeEvent(input$load_demo, {
      updateSelectInput(session, "model", selected = "core_forecast")
      updateSelectInput(session, "category", selected = "A")
      updateNumericInput(session, "n_cars", value = 50)
      ms <- months_available()
      pick <- as.Date("2025-05-01")
      if (!length(ms)) return()
      if (!pick %in% ms) pick <- ms[max(1, length(ms) %/% 2)]
      updateSelectInput(session, "clear_month", selected = as.character(pick))
    })

    supply_row <- reactive({
      q <- store$two_quota
      if (is.null(q) || is.null(input$clear_month)) return(NULL)
      m <- as.Date(input$clear_month)
      f <- q[q$model == input$model & q$category == input$category & as.Date(q$month) == m, ]
      b <- q[q$model == "simple_average" & q$category == input$category & as.Date(q$month) == m, ]
      if (!nrow(f) || !nrow(b)) return(NULL)
      ratio <- f$outlook[[1]] / pmax(b$outlook[[1]], 1)
      # Lower outlook than simple average => tighter supply signal
      signal <- if (ratio < 0.95) {
        "tighter"
      } else if (ratio > 1.05) {
        "looser"
      } else {
        "similar"
      }
      suggested <- switch(
        signal,
        tighter = 10000,
        similar = 5000,
        looser = 3000,
        5000
      )
      list(
        month = m,
        forecast = f$outlook[[1]],
        baseline = b$outlook[[1]],
        published = f$published[[1]],
        ratio = ratio,
        signal = signal,
        suggested = suggested,
        label = f$label[[1]]
      )
    })

    # When supply signal changes, move stress to suggestion (user can still override)
    observeEvent(supply_row(), {
      s <- supply_row()
      if (is.null(s)) return()
      updateSliderInput(session, "stress", value = s$suggested)
    }, ignoreInit = FALSE)

    last_row <- reactive({
      b <- store$bidding
      b <- b[b$category == input$category & !is.na(b$premium), ]
      if (!nrow(b)) return(NULL)
      utils::tail(b[order(b$month), ], 1)
    })

    prem <- reactive({
      row <- last_row()
      if (is.null(row)) 100000 else as.numeric(row$premium[[1]])
    })

    extra <- reactive(input$n_cars * input$stress)
    loose_stress <- 3000
    extra_if_loose <- reactive(input$n_cars * loose_stress)

    output$tightness_card <- renderUI({
      s <- supply_row()
      if (is.null(s)) return(NULL)
      tone <- switch(s$signal, tighter = "tight", looser = "loose", "mid")
      title <- switch(
        s$signal,
        tighter = "Supply looks tighter than the simple average",
        looser = "Supply looks looser than the simple average",
        "Supply looks similar to the simple average"
      )
      div(
        class = paste("tight-card", paste0("tight-card--", tone)),
        tags$span(class = "nx-kicker", format(s$month, "%B %Y"), " · ", s$label),
        tags$h3(title),
        tags$p(
          sprintf(
            "Forecast %s certificates · simple average %s · released %s",
            fmt_int(s$forecast), fmt_int(s$baseline),
            if (is.na(s$published)) "—" else fmt_int(s$published)
          )
        ),
        tags$p(
          class = "tight-suggest",
          sprintf("Suggested premium stress for this clearing month: %s per car", fmt_sgd(s$suggested))
        )
      )
    })

    output$kpi_extra <- renderText(fmt_sgd(extra()))
    output$kpi_extra2 <- renderText(fmt_sgd(extra()))
    output$kpi_extra_eq <- renderText({
      sprintf("%s cars × %s rise", fmt_int(input$n_cars), fmt_sgd(input$stress))
    })
    output$kpi_base <- renderText(fmt_sgd(input$n_cars * prem()))
    output$kpi_base_note <- renderText({
      sprintf("%s cars × last print %s", fmt_int(input$n_cars), fmt_sgd(prem()))
    })
    output$kpi_stress <- renderText(fmt_sgd(input$n_cars * (prem() + input$stress)))
    output$kpi_stress_note <- renderText({
      sprintf("Uses stress %s (from supply signal unless you override)", fmt_sgd(input$stress))
    })
    output$suggest_note <- renderText({
      s <- supply_row()
      if (is.null(s)) return("")
      sprintf(
        "Suggested from supply signal (%s): %s. Move the slider to override.",
        s$signal, fmt_sgd(s$suggested)
      )
    })
    output$print_note <- renderText({
      row <- last_row()
      if (is.null(row)) return("")
      sprintf(
        "Last settled Category %s premium: %s (%s).",
        input$category, fmt_sgd(row$premium[[1]]), format(as.Date(row$month[[1]]), "%b %Y")
      )
    })

    output$value_story <- renderUI({
      s <- supply_row()
      if (is.null(s)) {
        return(tags$p("Select a clearing month to link supply to stress."))
      }
      delta <- extra() - extra_if_loose()
      tags$div(
        class = "story-list-wrap",
        tags$p(
          tags$strong("Without the forecast:"),
          sprintf(
            " you only know %s cars × some rise. That is a calculator.",
            fmt_int(input$n_cars)
          )
        ),
        tags$p(
          tags$strong("With the forecast:"),
          sprintf(
            " %s looks %s than the simple average (outlook %s vs %s). We therefore plan a %s stress.",
            format(s$month, "%B %Y"),
            s$signal,
            fmt_int(s$forecast),
            fmt_int(s$baseline),
            fmt_sgd(s$suggested)
          )
        ),
        tags$p(
          sprintf(
            "Versus clearing in a loose month (stress %s), that is %s more capital at risk on the same book.",
            fmt_sgd(loose_stress),
            fmt_sgd(max(delta, 0))
          )
        ),
        tags$p(
          "Home answers timing. Your risk turns that timing into a P&L number on a locked package book."
        )
      )
    })
  })
}
