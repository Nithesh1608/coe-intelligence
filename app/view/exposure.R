box::use(
  shiny[
    NS, moduleServer, div, tags, numericInput, selectInput, sliderInput,
    renderText, renderUI, uiOutput, reactive, actionButton, observeEvent,
    updateNumericInput, updateSelectInput, updateSliderInput, req
  ],
  bslib[card, card_header, card_body],
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

# Map relative supply in the timing set to a premium-rise buffer ($/car).
# Anchored near historical month-to-month premium swings (Cat A ~$2.7k–$7.3k).
stress_from_outlooks <- function(outlook, outlooks) {
  outlooks <- as.numeric(outlooks)
  outlook <- as.numeric(outlook)
  lo <- min(outlooks, na.rm = TRUE)
  hi <- max(outlooks, na.rm = TRUE)
  if (!is.finite(lo) || !is.finite(hi) || hi <= lo) return(5000L)
  t <- (hi - outlook) / (hi - lo) # 0 = most supply, 1 = least
  as.integer(round((3000 + t * 7000) / 500) * 500)
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

quota_months <- function(q) {
  if (is.null(q) || !nrow(q)) return(as.Date(character()))
  sort(unique(as.Date(q$month[q$model %in% c("core_forecast", "momentum_forecast")])))
}

month_choices <- function(ms) {
  choices <- as.character(ms)
  names(choices) <- format(ms, "%b %Y")
  choices
}

demo_month <- function(ms) {
  pick <- as.Date("2025-05-01")
  if (!length(ms)) return(NA)
  if (!pick %in% ms) pick <- ms[max(1L, length(ms) %/% 2L)]
  pick
}

#' Simple exposure calculator: book × supply timing → dollars
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$p(class = "eyebrow", "Your risk"),
    tags$h2("Risk calculator"),
    tags$p(
      class = "lede",
      "Enter the locked book you still need to clear. We read our supply forecast for that month ",
      "and for +1 / +2 quarters, then show the extra dollars at risk — and what waiting can save."
    ),

    # ---- 1. Inputs ----
    div(
      class = "give-panel",
      tags$span(class = "nx-kicker", "1 · Your locked book"),
      tags$h3("What you enter"),
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
          ns("clear_month"),
          "I plan to clear in",
          choices = character(0),
          width = "180px"
        ),
        selectInput(
          ns("model"),
          "Supply forecast",
          choices = c(
            "Core" = "core_forecast",
            "Momentum" = "momentum_forecast"
          ),
          selected = "core_forecast",
          width = "160px"
        ),
        actionButton(ns("load_demo"), "Load example", class = "btn-demo")
      ),
      tags$p(class = "footnote", "Example: 50 Category A cars, clear May 2025, Core forecast.")
    ),

    # ---- 2. Supply for the timing set ----
    div(
      class = "calc-section",
      tags$span(class = "nx-kicker", "2 · What supply says"),
      tags$h3("Forecast certificates for your timing choices"),
      uiOutput(ns("supply_strip"))
    ),

    # ---- 3. Dollars ----
    div(
      class = "get-hero",
      div(
        class = "get-hero__copy",
        tags$span(class = "nx-kicker", "3 · Dollars at risk"),
        tags$h3(shiny::textOutput(ns("hero_title"), inline = TRUE)),
        tags$p(class = "get-hero__sub", shiny::textOutput(ns("hero_sub"), inline = TRUE))
      ),
      div(
        class = "get-hero__math",
        tags$span(class = "nx-kicker", "Extra $ on your plan if the buffer hits"),
        div(class = "risk-hero-num", shiny::textOutput(ns("kpi_plan_extra"), inline = TRUE)),
        tags$p(class = "risk-hero-eq", shiny::textOutput(ns("kpi_plan_eq"), inline = TRUE))
      )
    ),

    uiOutput(ns("compare_cards")),

    div(
      class = "save-banner",
      tags$span(class = "nx-kicker", "Timing value"),
      tags$h3(shiny::textOutput(ns("save_title"), inline = TRUE)),
      tags$p(shiny::textOutput(ns("save_sub"), inline = TRUE))
    ),

    # ---- Buffer control ----
    card(
      class = "desk-card",
      card_header("Premium-rise buffer (not a price forecast)"),
      card_body(
        tags$p(
          class = "stress-blurb",
          "Buffer = how much you assume the COE premium can rise per car before you clear. ",
          "We suggest it from relative supply in your timing set (more certificates → smaller buffer). ",
          "Extra dollars at risk = cars × buffer."
        ),
        sliderInput(
          ns("stress"),
          "Buffer for your plan ($ per car)",
          min = 0, max = 40000, value = 5000, step = 500
        ),
        tags$p(class = "footnote", shiny::textOutput(ns("suggest_note"), inline = TRUE)),
        tags$p(class = "footnote", shiny::textOutput(ns("print_note"), inline = TRUE))
      )
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    ms_all <- quota_months(store$two_quota)
    choices_all <- month_choices(ms_all)
    demo_m <- demo_month(ms_all)

    observeEvent(
      TRUE,
      {
        if (!length(ms_all)) return()
        updateSelectInput(
          session, "clear_month",
          choices = choices_all,
          selected = as.character(demo_m)
        )
      },
      once = TRUE
    )

    observeEvent(input$load_demo, {
      updateNumericInput(session, "n_cars", value = 50)
      updateSelectInput(session, "category", selected = "A")
      updateSelectInput(session, "model", selected = "core_forecast")
      if (length(ms_all) && !is.na(demo_m)) {
        updateSelectInput(session, "clear_month", selected = as.character(demo_m))
      }
    }, ignoreInit = TRUE)

    timing_set <- reactive({
      req(input$clear_month)
      req(nzchar(input$clear_month))
      q <- store$two_quota
      if (is.null(q)) return(NULL)
      plan_m <- as.Date(input$clear_month)
      if (is.na(plan_m)) return(NULL)

      targets <- list(
        list(key = "plan", title = "Your plan", month = plan_m),
        list(key = "q1", title = "+1 quarter", month = add_months(plan_m, 3L)),
        list(key = "q2", title = "+2 quarters", month = add_months(plan_m, 6L))
      )
      rows <- list()
      for (t in targets) {
        hit <- lookup_outlook(q, input$model, input$category, t$month)
        if (is.null(hit)) next
        rows[[length(rows) + 1L]] <- data.frame(
          key = t$key,
          title = t$title,
          month = hit$month,
          forecast = hit$forecast,
          published = hit$published,
          stringsAsFactors = FALSE
        )
      }
      if (!length(rows)) return(NULL)
      sc <- do.call(rbind, rows)
      sc$suggested <- vapply(
        sc$forecast, stress_from_outlooks, integer(1),
        outlooks = sc$forecast
      )
      sc
    })

    planned_row <- reactive({
      sc <- timing_set()
      if (is.null(sc)) return(NULL)
      hit <- sc[sc$key == "plan", , drop = FALSE]
      if (!nrow(hit)) return(NULL)
      hit
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

    plan_extra <- reactive({
      req(!is.null(input$stress))
      n_cars() * as.numeric(input$stress)
    })

    output$supply_strip <- renderUI({
      sc <- timing_set()
      if (is.null(sc) || !nrow(sc)) {
        return(tags$p(class = "footnote", "Pick a clearing month with enough forward forecast."))
      }
      best_i <- which.max(sc$forecast)
      chips <- lapply(seq_len(nrow(sc)), function(i) {
        r <- sc[i, ]
        div(
          class = paste("supply-chip", if (i == best_i) "supply-chip--best" else NULL),
          tags$span(class = "nx-kicker", r$title),
          tags$strong(format(as.Date(r$month), "%b %Y")),
          tags$p(sprintf("%s certificates forecast", fmt_int(r$forecast))),
          tags$p(class = "supply-chip__buf", sprintf("Suggested buffer %s/car", fmt_sgd(r$suggested)))
        )
      })
      div(class = "supply-strip", chips)
    })

    output$hero_title <- renderText({
      pr <- planned_row()
      if (is.null(pr) || !nrow(pr)) return("Enter your book above.")
      sprintf(
        "%s cars · clear %s · Category %s",
        fmt_int(n_cars()),
        format(as.Date(pr$month[[1]]), "%b %Y"),
        input$category
      )
    })

    output$hero_sub <- renderText({
      pr <- planned_row()
      if (is.null(pr) || !nrow(pr)) return("")
      sprintf(
        "Forecast supply that month: %s certificates. Last print %s used only as starting outlay.",
        fmt_int(pr$forecast[[1]]),
        fmt_sgd(prem())
      )
    })

    output$kpi_plan_extra <- renderText(fmt_sgd(plan_extra()))
    output$kpi_plan_eq <- renderText({
      sprintf("%s cars × %s buffer", fmt_int(n_cars()), fmt_sgd(input$stress))
    })

    output$compare_cards <- renderUI({
      sc <- timing_set()
      if (is.null(sc) || !nrow(sc)) return(NULL)
      n <- n_cars()
      best_m <- as.Date(sc$month[which.max(sc$forecast)])
      # For the plan card, use the live slider; for alternatives use suggested
      cards <- lapply(seq_len(nrow(sc)), function(i) {
        r <- sc[i, ]
        buf <- if (identical(r$key, "plan")) as.numeric(input$stress) else as.numeric(r$suggested)
        extra <- n * buf
        is_best <- identical(as.Date(r$month), best_m)
        div(
          class = paste("timing-col", if (is_best) "timing-col--best timing-col--loose" else if (identical(r$key, "plan")) "timing-col--tight" else "timing-col--mid"),
          tags$span(class = "nx-kicker", r$title),
          tags$h3(format(as.Date(r$month), "%b %Y")),
          tags$p(
            class = "timing-meta",
            sprintf("%s certificates", fmt_int(r$forecast))
          ),
          div(
            class = "timing-metric",
            tags$span("Buffer / car"),
            tags$strong(fmt_sgd(buf))
          ),
          div(
            class = "timing-metric timing-metric--money",
            tags$span("Extra $ at risk"),
            tags$strong(fmt_sgd(extra))
          )
        )
      })
      div(class = "timing-compare timing-compare--3", cards)
    })

    output$save_title <- renderText({
      pr <- planned_row()
      br <- best_row()
      if (is.null(pr) || is.null(br) || !nrow(pr) || !nrow(br)) return("")
      plan_buf <- as.numeric(input$stress)
      best_buf <- as.numeric(br$suggested[[1]])
      saved <- n_cars() * (plan_buf - best_buf)
      if (identical(as.Date(pr$month[[1]]), as.Date(br$month[[1]])) || saved <= 0) {
        return(sprintf(
          "%s is already the best of your plan / +1Q / +2Q set.",
          format(as.Date(pr$month[[1]]), "%b %Y")
        ))
      }
      sprintf(
        "Clear in %s instead → avoid %s",
        format(as.Date(br$month[[1]]), "%b %Y"),
        fmt_sgd(saved)
      )
    })

    output$save_sub <- renderText({
      pr <- planned_row()
      br <- best_row()
      if (is.null(pr) || is.null(br) || !nrow(pr) || !nrow(br)) return("")
      if (identical(as.Date(pr$month[[1]]), as.Date(br$month[[1]]))) {
        return("No timing saving inside this set on the current buffer.")
      }
      sprintf(
        "Same %s cars. Plan buffer %s/car vs %s/car in the higher-supply month.",
        fmt_int(n_cars()),
        fmt_sgd(input$stress),
        fmt_sgd(br$suggested[[1]])
      )
    })

    output$suggest_note <- renderText({
      pr <- planned_row()
      if (is.null(pr) || !nrow(pr)) return("")
      sprintf(
        "Suggested from supply in your timing set: %s/car. Slider overrides for your plan only.",
        fmt_sgd(pr$suggested[[1]])
      )
    })

    output$print_note <- renderText({
      row <- last_row()
      if (is.null(row)) return("")
      sprintf(
        "Last Category %s premium: %s (%s). Book at last print would be %s — not a forecast.",
        input$category,
        fmt_sgd(row$premium[[1]]),
        format(as.Date(row$month[[1]]), "%b %Y"),
        fmt_sgd(n_cars() * as.numeric(row$premium[[1]]))
      )
    })
  })
}
