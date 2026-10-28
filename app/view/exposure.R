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

add_months <- function(d, n) {
  d <- as.Date(d)
  y <- as.integer(format(d, "%Y"))
  m <- as.integer(format(d, "%m")) + as.integer(n)
  while (m > 12L) {
    m <- m - 12L
    y <- y + 1L
  }
  while (m < 1L) {
    m <- m + 12L
    y <- y - 1L
  }
  as.Date(sprintf("%04d-%02d-01", y, m))
}

# Stress from relative supply inside the timing set (not vs a lagging global average).
# Highest forecast certificates = loosest = lowest stress.
stress_from_outlooks <- function(outlook, outlooks) {
  outlooks <- as.numeric(outlooks)
  outlook <- as.numeric(outlook)
  lo <- min(outlooks, na.rm = TRUE)
  hi <- max(outlooks, na.rm = TRUE)
  if (!is.finite(lo) || !is.finite(hi) || hi <= lo) return(5000)
  # 0 at loosest (hi), 1 at tightest (lo)
  t <- (hi - outlook) / (hi - lo)
  stress <- 3000 + t * 7000
  as.integer(round(stress / 500) * 500)
}

tone_from_rank <- function(outlook, outlooks) {
  outlooks <- as.numeric(outlooks)
  if (outlook >= max(outlooks, na.rm = TRUE) - 1e-9) return("loose")
  if (outlook <= min(outlooks, na.rm = TRUE) + 1e-9) return("tight")
  "mid"
}

label_from_tone <- function(tone) {
  switch(
    tone,
    tight = "Tightest supply in this timing set",
    loose = "Loosest supply in this timing set",
    "Middle of this timing set"
  )
}

lookup_outlook <- function(q, model, category, month) {
  m <- as.Date(month)
  f <- q[q$model == model & q$category == category & as.Date(q$month) == m, ]
  if (!nrow(f)) return(NULL)
  list(
    month = m,
    forecast = as.numeric(f$outlook[[1]]),
    published = f$published[[1]],
    label = as.character(f$label[[1]])
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
      "Pick when you plan to clear. We compare that month with shifting ",
      tags$strong("+1 quarter"), " and ", tags$strong("+2 quarters"),
      " — using forecast certificate supply, not a single “best month” across the whole chart."
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
          tags$li(tags$strong("The month you plan to clear"))
        )
      ),
      div(
        class = "io-strip__get",
        tags$span(class = "nx-kicker", "You get"),
        tags$ul(
          class = "io-list",
          tags$li("Your plan vs +1Q vs +2Q on the same book"),
          tags$li("Stress ($/car) from relative supply in that set"),
          tags$li(tags$strong("Dollars you avoid"), " by picking the loosest of the three")
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
      tags$p(
        class = "footnote",
        "Timing set = your month, +3 months, +6 months (when data exists). Stress is relative inside that set."
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
        tags$span(class = "nx-kicker", "Capital you can avoid vs your plan"),
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
            "Default = relative supply vs +1Q/+2Q. Drag only to override."
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
    months_available <- reactive({
      q <- store$two_quota
      if (is.null(q)) return(as.Date(character()))
      sort(unique(as.Date(q$month[q$category == input$category & q$model == input$model])))
    })

    observe({
      ms <- months_available()
      if (!length(ms)) return()
      # Need room for +2Q when possible — still allow late months
      choices <- as.character(ms)
      names(choices) <- format(ms, "%b %Y")
      current <- isolate(input$clear_month)
      if (!is.null(current) && nzchar(current) && current %in% choices) {
        selected <- current
      } else {
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

    # Timing set: plan, +1Q, +2Q (drop missing)
    timing_set <- reactive({
      req(input$clear_month)
      req(nzchar(input$clear_month))
      q <- store$two_quota
      if (is.null(q)) return(NULL)
      plan_m <- as.Date(input$clear_month)
      targets <- list(
        list(key = "plan", title = "Your plan", offset = 0L, month = plan_m),
        list(key = "q1", title = "Shift +1 quarter", offset = 3L, month = add_months(plan_m, 3L)),
        list(key = "q2", title = "Shift +2 quarters", offset = 6L, month = add_months(plan_m, 6L))
      )
      rows <- list()
      for (t in targets) {
        hit <- lookup_outlook(q, input$model, input$category, t$month)
        if (is.null(hit)) next
        rows[[length(rows) + 1L]] <- data.frame(
          key = t$key,
          title = t$title,
          offset = t$offset,
          month = hit$month,
          forecast = hit$forecast,
          published = hit$published,
          label = hit$label,
          stringsAsFactors = FALSE
        )
      }
      if (!length(rows)) return(NULL)
      sc <- do.call(rbind, rows)
      outlooks <- sc$forecast
      sc$suggested <- vapply(sc$forecast, stress_from_outlooks, integer(1), outlooks = outlooks)
      sc$tone <- vapply(sc$forecast, tone_from_rank, character(1), outlooks = outlooks)
      sc$rank_label <- vapply(sc$tone, label_from_tone, character(1))
      sc
    })

    planned_row <- reactive({
      sc <- timing_set()
      if (is.null(sc)) return(NULL)
      sc[sc$key == "plan", , drop = FALSE]
    })

    best_row <- reactive({
      sc <- timing_set()
      if (is.null(sc) || !nrow(sc)) return(NULL)
      sc[which.max(sc$forecast), , drop = FALSE]
    })

    observeEvent(
      list(input$clear_month, input$model, input$category),
      {
        pr <- planned_row()
        if (is.null(pr) || !nrow(pr)) return()
        updateSliderInput(session, "stress", value = as.numeric(pr$suggested[[1]]))
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
      pr <- planned_row()
      br <- best_row()
      if (is.null(pr) || is.null(br) || !nrow(pr) || !nrow(br)) return(0)
      n_cars() * (as.numeric(pr$suggested[[1]]) - as.numeric(br$suggested[[1]]))
    })

    output$hero_title <- renderText({
      pr <- planned_row()
      br <- best_row()
      if (is.null(pr) || is.null(br) || !nrow(pr) || !nrow(br)) {
        return("Pick a clearing month above.")
      }
      if (identical(as.Date(pr$month[[1]]), as.Date(br$month[[1]]))) {
        return(sprintf(
          "%s is already the loosest option in your +0/+1Q/+2Q set.",
          format(as.Date(pr$month[[1]]), "%b %Y")
        ))
      }
      sprintf(
        "Shift to %s instead of your plan %s",
        format(as.Date(br$month[[1]]), "%b %Y"),
        format(as.Date(pr$month[[1]]), "%b %Y")
      )
    })

    output$hero_sub <- renderText({
      sc <- timing_set()
      pr <- planned_row()
      br <- best_row()
      if (is.null(sc) || is.null(pr) || is.null(br)) return("")
      sprintf(
        "Compared inside {%s}. Higher forecast certificates → lower stress. Same %s cars.",
        paste(format(as.Date(sc$month), "%b %Y"), collapse = ", "),
        fmt_int(n_cars())
      )
    })

    output$kpi_saved <- renderText(fmt_sgd(max(saved(), 0)))
    output$kpi_saved_eq <- renderText({
      pr <- planned_row()
      br <- best_row()
      if (is.null(pr) || is.null(br) || !nrow(pr) || !nrow(br)) return("")
      sprintf(
        "%s cars × (%s − %s) stress",
        fmt_int(n_cars()),
        fmt_sgd(pr$suggested[[1]]),
        fmt_sgd(br$suggested[[1]])
      )
    })

    output$timing_table <- renderUI({
      sc <- timing_set()
      if (is.null(sc) || !nrow(sc)) {
        return(tags$p(
          class = "footnote",
          "Not enough forward months for a +1Q/+2Q set. Pick an earlier clearing month."
        ))
      }
      n <- n_cars()
      best_m <- as.Date(sc$month[which.max(sc$forecast)])

      cards <- lapply(seq_len(nrow(sc)), function(i) {
        r <- sc[i, ]
        extra <- n * as.numeric(r$suggested)
        is_best <- identical(as.Date(r$month), best_m)
        div(
          class = paste(
            "timing-col",
            paste0("timing-col--", r$tone),
            if (is_best) "timing-col--best" else NULL
          ),
          tags$span(class = "nx-kicker", r$title),
          tags$h3(format(as.Date(r$month), "%b %Y")),
          tags$p(class = "timing-signal", r$rank_label),
          tags$p(
            class = "timing-meta",
            sprintf("Forecast supply %s certificates", fmt_int(r$forecast))
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
      })

      div(class = "timing-compare timing-compare--3", cards)
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
      pr <- planned_row()
      if (is.null(pr) || !nrow(pr)) return("")
      sprintf(
        "From relative supply in your timing set (%s): %s. Slider overrides.",
        pr$rank_label[[1]], fmt_sgd(pr$suggested[[1]])
      )
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
