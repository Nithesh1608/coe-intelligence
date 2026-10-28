box::use(
  shiny[
    NS, moduleServer, div, tags, numericInput, selectInput, sliderInput,
    renderText, renderUI, uiOutput, reactive, actionButton, observeEvent,
    updateNumericInput, updateSelectInput, updateSliderInput, observe, isolate,
    req
  ],
  bslib[card, card_header, card_body, layout_columns],
)

fmt_sgd <- function(x) {
  paste0("$", format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE))
}

fmt_int <- function(x) {
  format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
}

stress_for_signal <- function(signal) {
  switch(as.character(signal), tighter = 10000, similar = 5000, looser = 3000, 5000)
}

row_to_score <- function(df_row) {
  list(
    month = as.Date(df_row$month[[1]]),
    forecast = as.numeric(df_row$forecast[[1]]),
    baseline = as.numeric(df_row$baseline[[1]]),
    published = df_row$published[[1]],
    ratio = as.numeric(df_row$ratio[[1]]),
    signal = as.character(df_row$signal[[1]]),
    suggested = as.numeric(df_row$suggested[[1]]),
    label = as.character(df_row$label[[1]])
  )
}

score_month <- function(q, model, category, month) {
  m <- as.Date(month)
  f <- q[q$model == model & q$category == category & as.Date(q$month) == m, ]
  b <- q[q$model == "simple_average" & q$category == category & as.Date(q$month) == m, ]
  if (!nrow(f) || !nrow(b)) return(NULL)
  forecast <- as.numeric(f$outlook[[1]])
  baseline <- as.numeric(b$outlook[[1]])
  ratio <- forecast / pmax(baseline, 1)
  signal <- if (ratio < 0.95) {
    "tighter"
  } else if (ratio > 1.05) {
    "looser"
  } else {
    "similar"
  }
  list(
    month = m,
    forecast = forecast,
    baseline = baseline,
    published = f$published[[1]],
    ratio = ratio,
    signal = signal,
    suggested = stress_for_signal(signal),
    label = f$label[[1]]
  )
}

signal_line <- function(signal) {
  switch(
    as.character(signal),
    tighter = "Supply tighter than the simple average",
    looser = "Supply looser than the simple average",
    "Supply similar to the simple average"
  )
}

#' Your risk — give your book, get timing in dollars
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$p(class = "eyebrow", "Your risk"),
    tags$h2("Give your locked book. Get the dollar cost of timing."),
    tags$p(
      class = "lede",
      "Enter the cars you still need to clear, and the month you plan to bid. ",
      "We compare that plan with the loosest month in the supply forecast — in dollars."
    ),

    div(
      class = "io-strip",
      div(
        class = "io-strip__give",
        tags$span(class = "nx-kicker", "You give"),
        tags$ul(
          class = "io-list",
          tags$li("Cars still waiting for a certificate"),
          tags$li("Category A or B"),
          tags$li("Which supply forecast to use"),
          tags$li(tags$strong("The month you plan to clear bids"))
        )
      ),
      div(
        class = "io-strip__get",
        tags$span(class = "nx-kicker", "You get"),
        tags$ul(
          class = "io-list",
          tags$li("Stress ($/car) for your plan vs a looser month"),
          tags$li(tags$strong("Extra dollars at risk"), " on your book"),
          tags$li("How much you avoid by shifting timing")
        )
      )
    ),

    div(
      class = "give-panel",
      tags$span(class = "nx-kicker", "1 · Your book"),
      tags$h3("What to enter"),
      div(
        class = "desk-controls desk-controls--row",
        numericInput(ns("n_cars"), "Cars still to bid", 50, min = 1, max = 5000, width = "140px"),
        selectInput(
          ns("category"),
          "Category",
          choices = c("Category A" = "A", "Category B" = "B"),
          selected = "A",
          width = "150px"
        ),
        selectInput(
          ns("model"),
          "Supply forecast",
          choices = c(
            "Core supply forecast" = "core_forecast",
            "Momentum supply forecast" = "momentum_forecast"
          ),
          selected = "core_forecast",
          width = "220px"
        ),
        selectInput(
          ns("clear_month"),
          "I plan to clear bids in",
          choices = c("Loading…" = ""),
          width = "180px"
        ),
        actionButton(ns("load_demo"), "Load 50-car example", class = "btn-demo")
      ),
      uiOutput(ns("planned_chip")),
      tags$p(
        class = "footnote",
        "Change the clearing month above — the comparison below updates to your plan vs the loosest month."
      )
    ),

    div(
      class = "get-hero",
      div(
        class = "get-hero__copy",
        tags$span(class = "nx-kicker", "2 · What you get"),
        tags$h3(shiny::textOutput(ns("hero_title"), inline = TRUE)),
        tags$p(class = "get-hero__sub", shiny::textOutput(ns("hero_sub"), inline = TRUE))
      ),
      div(
        class = "get-hero__math",
        tags$span(class = "nx-kicker", "Capital you can avoid by shifting timing"),
        div(class = "risk-hero-num", shiny::textOutput(ns("kpi_saved"), inline = TRUE)),
        tags$p(class = "risk-hero-eq", shiny::textOutput(ns("kpi_saved_eq"), inline = TRUE))
      )
    ),

    uiOutput(ns("timing_table")),

    layout_columns(
      col_widths = c(5, 7),
      card(
        class = "desk-card",
        card_header("Stress on your planned month"),
        card_body(
          sliderInput(
            ns("stress"),
            "Premium rise if you clear then ($ per car)",
            min = 0, max = 40000, value = 5000, step = 500
          ),
          tags$p(class = "footnote", shiny::textOutput(ns("suggest_note"), inline = TRUE)),
          tags$p(
            class = "footnote",
            "Default comes from the supply signal for your chosen month. Drag only to override."
          )
        )
      ),
      card(
        class = "desk-card desk-card--story",
        card_header("Dollars if you stick with your plan"),
        card_body(
          div(
            class = "risk-board risk-board--3",
            div(
              class = "risk-board__col",
              tags$span(class = "nx-kicker", "Outlay at last print"),
              div(class = "risk-board__val risk-board__val--sm", shiny::textOutput(ns("kpi_base"), inline = TRUE)),
              tags$p(class = "risk-board__note", shiny::textOutput(ns("kpi_base_note"), inline = TRUE))
            ),
            div(
              class = "risk-board__col risk-board__col--hot",
              tags$span(class = "nx-kicker", "Outlay if stress hits"),
              div(class = "risk-board__val risk-board__val--sm", shiny::textOutput(ns("kpi_stress"), inline = TRUE)),
              tags$p(class = "risk-board__note", shiny::textOutput(ns("kpi_stress_note"), inline = TRUE))
            ),
            div(
              class = "risk-board__col risk-board__col--accent",
              tags$span(class = "nx-kicker", "You absorb"),
              div(class = "risk-board__val risk-board__val--sm", shiny::textOutput(ns("kpi_extra"), inline = TRUE)),
              tags$p(class = "risk-board__note", "Extra dollars on the locked book")
            )
          ),
          tags$p(class = "footnote", shiny::textOutput(ns("print_note"), inline = TRUE))
        )
      )
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    scored_months <- reactive({
      q <- store$two_quota
      if (is.null(q)) return(NULL)
      ms <- sort(unique(as.Date(q$month[q$category == input$category & q$model == input$model])))
      if (!length(ms)) return(NULL)
      rows <- lapply(ms, function(m) score_month(q, input$model, input$category, m))
      rows <- Filter(Negate(is.null), rows)
      if (!length(rows)) return(NULL)
      do.call(rbind, lapply(rows, function(r) {
        data.frame(
          month = r$month,
          forecast = r$forecast,
          baseline = r$baseline,
          published = r$published,
          ratio = r$ratio,
          signal = r$signal,
          suggested = r$suggested,
          label = r$label,
          stringsAsFactors = FALSE
        )
      }))
    })

    months_available <- reactive({
      sc <- scored_months()
      if (is.null(sc)) return(as.Date(character()))
      sort(as.Date(sc$month))
    })

    # Keep a stable selectInput at the top — update choices without remounting
    observe({
      ms <- months_available()
      if (!length(ms)) return()
      choices <- as.character(ms)
      names(choices) <- format(ms, "%b %Y")
      current <- isolate(input$clear_month)
      if (!is.null(current) && nzchar(current) && current %in% choices) {
        selected <- current
      } else {
        # Prefer a mid horizon month for first load
        pick <- as.Date("2025-05-01")
        if (!pick %in% ms) pick <- ms[max(1L, length(ms) %/% 2L)]
        selected <- as.character(pick)
      }
      updateSelectInput(session, "clear_month", choices = choices, selected = selected)
    })

    observeEvent(input$load_demo, {
      updateSelectInput(session, "model", selected = "core_forecast")
      updateSelectInput(session, "category", selected = "A")
      updateNumericInput(session, "n_cars", value = 50)
      ms <- months_available()
      if (!length(ms)) return()
      pick <- as.Date("2025-05-01")
      if (!pick %in% ms) pick <- ms[max(1L, length(ms) %/% 2L)]
      updateSelectInput(session, "clear_month", selected = as.character(pick))
    })

    planned <- reactive({
      req(input$clear_month)
      req(nzchar(input$clear_month))
      q <- store$two_quota
      if (is.null(q)) return(NULL)
      score_month(q, input$model, input$category, input$clear_month)
    })

    alt_loose <- reactive({
      sc <- scored_months()
      if (is.null(sc) || !nrow(sc)) return(NULL)
      row_to_score(sc[which.max(sc$ratio), , drop = FALSE])
    })

    # Only push suggested stress when the chosen month / signal changes — not on every slider tick
    observeEvent(
      list(input$clear_month, input$model, input$category),
      {
        s <- planned()
        if (is.null(s)) return()
        updateSliderInput(session, "stress", value = s$suggested)
      },
      ignoreInit = FALSE
    )

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

    n_cars <- reactive({
      n <- as.numeric(input$n_cars)
      if (is.null(n) || is.na(n) || n < 1) 50 else n
    })

    saved <- reactive({
      s <- planned()
      a <- alt_loose()
      if (is.null(s) || is.null(a)) return(0)
      n_cars() * (s$suggested - a$suggested)
    })

    output$planned_chip <- renderUI({
      s <- planned()
      if (is.null(s)) return(NULL)
      tone <- switch(s$signal, tighter = "tight", looser = "loose", "mid")
      div(
        class = paste("planned-chip", paste0("planned-chip--", tone)),
        sprintf(
          "Your plan · %s · %s · suggested %s/car",
          format(s$month, "%b %Y"), signal_line(s$signal), fmt_sgd(s$suggested)
        )
      )
    })

    output$hero_title <- renderText({
      s <- planned()
      a <- alt_loose()
      if (is.null(s) || is.null(a)) return("Pick a clearing month above.")
      if (identical(s$month, a$month) || identical(s$suggested, a$suggested)) {
        return(sprintf("%s is already among the loosest months we see.", format(s$month, "%b %Y")))
      }
      sprintf(
        "Your plan %s vs looser %s",
        format(s$month, "%b %Y"),
        format(a$month, "%b %Y")
      )
    })

    output$hero_sub <- renderText({
      s <- planned()
      a <- alt_loose()
      if (is.null(s) || is.null(a)) return("")
      if (identical(s$month, a$month) || identical(s$suggested, a$suggested)) {
        return("Shifting timing does not reduce planned stress on this book.")
      }
      sprintf(
        "Your month: %s (%s/car). Looser month: %s (%s/car). Same %s cars.",
        signal_line(s$signal), fmt_sgd(s$suggested),
        signal_line(a$signal), fmt_sgd(a$suggested),
        fmt_int(n_cars())
      )
    })

    output$kpi_saved <- renderText(fmt_sgd(max(saved(), 0)))
    output$kpi_saved_eq <- renderText({
      s <- planned()
      a <- alt_loose()
      if (is.null(s) || is.null(a)) return("")
      sprintf(
        "%s cars × (%s − %s) stress",
        fmt_int(n_cars()),
        fmt_sgd(s$suggested),
        fmt_sgd(a$suggested)
      )
    })

    output$timing_table <- renderUI({
      s <- planned()
      a <- alt_loose()
      if (is.null(s) || is.null(a)) {
        return(tags$p(class = "footnote", "Choose a clearing month in the inputs above."))
      }
      n <- n_cars()
      plan_extra <- n * s$suggested
      alt_extra <- n * a$suggested
      delta <- plan_extra - alt_extra

      col_block <- function(kind, r, extra, tone) {
        div(
          class = paste("timing-col", paste0("timing-col--", tone)),
          tags$span(class = "nx-kicker", kind),
          tags$h3(format(as.Date(r$month), "%b %Y")),
          tags$p(class = "timing-signal", signal_line(r$signal)),
          tags$p(
            class = "timing-meta",
            sprintf("Forecast %s · average %s", fmt_int(r$forecast), fmt_int(r$baseline))
          ),
          div(
            class = "timing-metric",
            tags$span("Stress to plan"),
            tags$strong(fmt_sgd(r$suggested), " / car")
          ),
          div(
            class = "timing-metric timing-metric--money",
            tags$span("Extra $ if that stress hits"),
            tags$strong(fmt_sgd(extra))
          )
        )
      }

      plan_tone <- switch(s$signal, tighter = "tight", looser = "loose", "mid")
      div(
        class = "timing-compare",
        col_block("Your planned month", s, plan_extra, plan_tone),
        div(
          class = "timing-vs",
          tags$span("vs"),
          tags$p(
            class = "timing-delta",
            if (delta > 0) {
              sprintf("%s less at risk if you wait for the looser month", fmt_sgd(delta))
            } else {
              "Little or no timing saving vs the looser month"
            }
          )
        ),
        col_block("Looser month in forecast", a, alt_extra, "loose")
      )
    })

    output$kpi_base <- renderText(fmt_sgd(n_cars() * prem()))
    output$kpi_base_note <- renderText({
      sprintf("%s cars × last print %s", fmt_int(n_cars()), fmt_sgd(prem()))
    })
    output$kpi_stress <- renderText({
      req(!is.null(input$stress))
      fmt_sgd(n_cars() * (prem() + input$stress))
    })
    output$kpi_stress_note <- renderText({
      req(!is.null(input$stress))
      sprintf("Last print + %s stress", fmt_sgd(input$stress))
    })
    output$kpi_extra <- renderText({
      req(!is.null(input$stress))
      fmt_sgd(n_cars() * input$stress)
    })
    output$suggest_note <- renderText({
      s <- planned()
      if (is.null(s)) return("")
      sprintf("From supply signal (%s): %s. Slider overrides.", s$signal, fmt_sgd(s$suggested))
    })
    output$print_note <- renderText({
      row <- last_row()
      if (is.null(row)) return("")
      sprintf(
        "Last settled Category %s premium: %s (%s). Starting outlay only — not a forecast.",
        input$category, fmt_sgd(row$premium[[1]]), format(as.Date(row$month[[1]]), "%b %Y")
      )
    })
  })
}
