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

# Relative supply in timing set → premium-rise buffer ($/car)
stress_from_outlooks <- function(outlook, outlooks) {
  outlooks <- as.numeric(outlooks)
  outlook <- as.numeric(outlook)
  lo <- min(outlooks, na.rm = TRUE)
  hi <- max(outlooks, na.rm = TRUE)
  if (!is.finite(lo) || !is.finite(hi) || hi <= lo) return(5000L)
  t <- (hi - outlook) / (hi - lo)
  as.integer(round((3000 + t * 7000) / 500) * 500)
}

lookup_outlook <- function(q, model, category, month) {
  m <- as.Date(month)
  f <- q[q$model == model & q$category == category & as.Date(q$month) == m, ]
  if (!nrow(f)) return(NULL)
  list(month = m, forecast = as.numeric(f$outlook[[1]]), published = f$published[[1]])
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

# Historical month-to-month premium *rises* only (adverse moves for a short book)
hist_premium_rises <- function(bidding, category) {
  b <- bidding[bidding$category == category & !is.na(bidding$premium), ]
  if (!nrow(b)) return(numeric())
  m <- stats::aggregate(premium ~ month, b, function(z) mean(z, na.rm = TRUE))
  m <- m[order(m$month), ]
  d <- diff(m$premium)
  d <- d[is.finite(d) & d > 0]
  d
}

last_premium <- function(bidding, category) {
  b <- bidding[bidding$category == category & !is.na(bidding$premium), ]
  if (!nrow(b)) return(100000)
  as.numeric(utils::tail(b[order(b$month), ], 1)$premium[[1]])
}

#' Dealer exposure calculator — Doris framing + our supply forecast
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$p(class = "eyebrow", "Your risk"),
    tags$h2("Dealer exposure calculator"),
    tags$p(
      class = "lede",
      "You locked a COE-inclusive price. Certificates are not bought yet. ",
      "Enter the book; we combine ", tags$strong("historical premium swings"),
      " with our ", tags$strong("supply forecast"),
      " to show dollars at risk — and whether waiting +1 or +2 quarters helps."
    ),

    div(
      class = "give-panel",
      tags$span(class = "nx-kicker", "1 · Locked book"),
      tags$h3("What you enter"),
      div(
        class = "desk-controls desk-controls--row",
        numericInput(ns("n_cars"), "Orders still to clear", 50, min = 1, max = 5000, width = "150px"),
        selectInput(
          ns("category"), "Category",
          choices = c("Category A" = "A", "Category B" = "B"),
          selected = "A", width = "150px"
        ),
        numericInput(ns("quoted"), "Quoted / locked premium ($)", 130000, min = 0, max = 400000, step = 500, width = "180px"),
        selectInput(ns("clear_month"), "Plan to clear in", choices = character(0), width = "170px"),
        selectInput(
          ns("model"), "Supply forecast",
          choices = c("Core" = "core_forecast", "Momentum" = "momentum_forecast"),
          selected = "core_forecast", width = "140px"
        ),
        actionButton(ns("load_demo"), "Load example", class = "btn-demo")
      ),
      tags$p(
        class = "footnote",
        "Example: 50 Cat A orders locked near last print, clear May 2025. ",
        "Upload-style multi-order books from Doris’s app need her simulation files — this page prices a pooled book."
      )
    ),

    div(
      class = "calc-section",
      tags$span(class = "nx-kicker", "2 · Supply for timing"),
      tags$h3("Forecast certificates — your plan vs delaying the book"),
      uiOutput(ns("supply_strip"))
    ),

    div(
      class = "calc-section",
      tags$span(class = "nx-kicker", "3 · Book outcomes"),
      tags$h3("If you clear on the plan month"),
      uiOutput(ns("lead_kpis"))
    ),

    div(
      class = "calc-section",
      tags$span(class = "nx-kicker", "4 · Timing shift"),
      tags$h3("Delay the whole book 0 / 1 / 2 quarters"),
      uiOutput(ns("shift_table")),
      div(
        class = "save-banner",
        tags$span(class = "nx-kicker", "Best timing in this set"),
        tags$h3(shiny::textOutput(ns("save_title"), inline = TRUE)),
        tags$p(shiny::textOutput(ns("save_sub"), inline = TRUE))
      )
    ),

    card(
      class = "desk-card",
      card_header("How the buffer is set"),
      card_body(
        tags$p(
          class = "stress-blurb",
          "We do ", tags$strong("not"), " forecast the next premium. ",
          "Buffer = premium rise per car you plan for before clearing. ",
          "Default blends (1) relative supply in your plan/+1Q/+2Q set with (2) the historical 90th-percentile month-to-month premium rise for that category. ",
          "Dealer loss vs quote = max(0, clear premium − quoted) × orders."
        ),
        sliderInput(
          ns("stress"),
          "Override buffer for your plan ($ per car)",
          min = 0, max = 40000, value = 5000, step = 500
        ),
        tags$p(class = "footnote", shiny::textOutput(ns("suggest_note"), inline = TRUE)),
        tags$p(class = "footnote", shiny::textOutput(ns("hist_note"), inline = TRUE))
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
        updateSelectInput(session, "clear_month", choices = choices_all, selected = as.character(demo_m))
        lp <- last_premium(store$bidding, "A")
        updateNumericInput(session, "quoted", value = round(lp / 500) * 500)
      },
      once = TRUE
    )

    # Keep quoted near last print when category changes (unless user already diverged a lot)
    observeEvent(input$category, {
      lp <- last_premium(store$bidding, input$category)
      updateNumericInput(session, "quoted", value = round(lp / 500) * 500)
    }, ignoreInit = TRUE)

    observeEvent(input$load_demo, {
      lp <- last_premium(store$bidding, "A")
      updateNumericInput(session, "n_cars", value = 50)
      updateSelectInput(session, "category", selected = "A")
      updateSelectInput(session, "model", selected = "core_forecast")
      updateNumericInput(session, "quoted", value = round(lp / 500) * 500)
      if (length(ms_all) && !is.na(demo_m)) {
        updateSelectInput(session, "clear_month", selected = as.character(demo_m))
      }
    }, ignoreInit = TRUE)

    hist_stats <- reactive({
      rises <- hist_premium_rises(store$bidding, input$category)
      if (!length(rises)) {
        return(list(p50 = 3000, p75 = 5000, p90 = 7500, n = 0L))
      }
      list(
        p50 = as.numeric(stats::median(rises)),
        p75 = as.numeric(stats::quantile(rises, 0.75, na.rm = TRUE)),
        p90 = as.numeric(stats::quantile(rises, 0.90, na.rm = TRUE)),
        n = length(rises)
      )
    })

    timing_set <- reactive({
      req(input$clear_month)
      req(nzchar(input$clear_month))
      q <- store$two_quota
      if (is.null(q)) return(NULL)
      plan_m <- as.Date(input$clear_month)
      if (is.na(plan_m)) return(NULL)
      targets <- list(
        list(key = "plan", title = "Clear on plan", shift = 0L, month = plan_m),
        list(key = "q1", title = "Delay +1 quarter", shift = 1L, month = add_months(plan_m, 3L)),
        list(key = "q2", title = "Delay +2 quarters", shift = 2L, month = add_months(plan_m, 6L))
      )
      hs <- hist_stats()
      rows <- list()
      for (t in targets) {
        hit <- lookup_outlook(q, input$model, input$category, t$month)
        if (is.null(hit)) next
        rows[[length(rows) + 1L]] <- data.frame(
          key = t$key,
          title = t$title,
          shift = t$shift,
          month = hit$month,
          forecast = hit$forecast,
          stringsAsFactors = FALSE
        )
      }
      if (!length(rows)) return(NULL)
      sc <- do.call(rbind, rows)
      supply_buf <- vapply(sc$forecast, stress_from_outlooks, integer(1), outlooks = sc$forecast)
      # Blend: supply-relative buffer pulled toward historical p90 when supply is tight
      sc$suggested <- as.integer(round(((0.55 * supply_buf) + (0.45 * pmax(hs$p90, 0))) / 500) * 500)
      sc$suggested <- pmax(2000L, pmin(20000L, sc$suggested))
      sc$supply_buf <- supply_buf
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
      # Best = lowest dealer adverse loss (lowest buffer), prefer more supply if tie
      sc[order(sc$suggested, -sc$forecast), ][1, , drop = FALSE]
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

    n_cars <- reactive({
      n <- as.numeric(input$n_cars)
      if (is.null(n) || is.na(n) || n < 1) 50 else n
    })

    quoted <- reactive({
      q <- as.numeric(input$quoted)
      if (is.null(q) || is.na(q) || q < 0) last_premium(store$bidding, input$category) else q
    })

    prem_now <- reactive(last_premium(store$bidding, input$category))

    # Clear premium under buffer = quoted path uses last print + buffer as adverse clear level
    # Dealer loss vs locked quote
    plan_clear <- reactive({
      req(!is.null(input$stress))
      prem_now() + as.numeric(input$stress)
    })

    dealer_loss <- reactive({
      n_cars() * pmax(0, plan_clear() - quoted())
    })

    output$supply_strip <- renderUI({
      sc <- timing_set()
      if (is.null(sc) || !nrow(sc)) {
        return(tags$p(class = "footnote", "Pick a clearing month with +1Q/+2Q forecast coverage."))
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

    output$lead_kpis <- renderUI({
      req(!is.null(input$stress))
      n <- n_cars()
      q <- quoted()
      clear <- plan_clear()
      loss <- dealer_loss()
      book_quote <- n * q
      book_clear <- n * clear
      hs <- hist_stats()

      div(
        class = "lead-grid",
        div(
          class = "lead-cell",
          tags$span(class = "nx-kicker", "Book at quote"),
          div(class = "lead-num", fmt_sgd(book_quote)),
          tags$p(class = "lead-lab", sprintf("%s × %s locked", fmt_int(n), fmt_sgd(q)))
        ),
        div(
          class = "lead-cell lead-cell--hot",
          tags$span(class = "nx-kicker", "Adverse clear cost"),
          div(class = "lead-num", fmt_sgd(book_clear)),
          tags$p(class = "lead-lab", sprintf("Last print %s + buffer %s", fmt_sgd(prem_now()), fmt_sgd(input$stress)))
        ),
        div(
          class = "lead-cell lead-cell--accent",
          tags$span(class = "nx-kicker", "Dealer absorbs"),
          div(class = "lead-num", fmt_sgd(loss)),
          tags$p(class = "lead-lab", "max(0, clear − quote) × orders")
        ),
        div(
          class = "lead-cell",
          tags$span(class = "nx-kicker", "Hist. P90 month rise"),
          div(class = "lead-num lead-num--sm", fmt_sgd(hs$p90)),
          tags$p(class = "lead-lab", sprintf("Cat %s · %s month moves", input$category, fmt_int(hs$n)))
        )
      )
    })

    output$shift_table <- renderUI({
      sc <- timing_set()
      if (is.null(sc) || !nrow(sc)) return(NULL)
      n <- n_cars()
      q <- quoted()
      p0 <- prem_now()
      best <- best_row()
      best_m <- if (!is.null(best) && nrow(best)) as.Date(best$month[[1]]) else as.Date(NA)

      cards <- lapply(seq_len(nrow(sc)), function(i) {
        r <- sc[i, ]
        buf <- if (identical(r$key, "plan")) as.numeric(input$stress) else as.numeric(r$suggested)
        clear <- p0 + buf
        loss <- n * pmax(0, clear - q)
        is_best <- !is.na(best_m) && identical(as.Date(r$month), best_m)
        div(
          class = paste(
            "timing-col",
            if (is_best) "timing-col--best timing-col--loose"
            else if (identical(r$key, "plan")) "timing-col--tight"
            else "timing-col--mid"
          ),
          tags$span(class = "nx-kicker", r$title),
          tags$h3(format(as.Date(r$month), "%b %Y")),
          tags$p(class = "timing-meta", sprintf("%s certificates · buffer %s", fmt_int(r$forecast), fmt_sgd(buf))),
          div(
            class = "timing-metric",
            tags$span("Adverse clear / car"),
            tags$strong(fmt_sgd(clear))
          ),
          div(
            class = "timing-metric timing-metric--money",
            tags$span("Dealer loss vs quote"),
            tags$strong(fmt_sgd(loss))
          )
        )
      })
      div(class = "timing-compare timing-compare--3", cards)
    })

    output$save_title <- renderText({
      pr <- planned_row()
      br <- best_row()
      if (is.null(pr) || is.null(br) || !nrow(pr) || !nrow(br)) return("")
      n <- n_cars()
      q <- quoted()
      p0 <- prem_now()
      plan_loss <- n * pmax(0, (p0 + as.numeric(input$stress)) - q)
      best_loss <- n * pmax(0, (p0 + as.numeric(br$suggested[[1]])) - q)
      saved <- plan_loss - best_loss
      if (identical(as.Date(pr$month[[1]]), as.Date(br$month[[1]])) || saved <= 0) {
        return(sprintf("%s already minimises dealer loss in this set.", format(as.Date(pr$month[[1]]), "%b %Y")))
      }
      sprintf("Delay to %s → reduce dealer loss by %s", format(as.Date(br$month[[1]]), "%b %Y"), fmt_sgd(saved))
    })

    output$save_sub <- renderText({
      pr <- planned_row()
      br <- best_row()
      if (is.null(pr) || is.null(br)) return("")
      sprintf(
        "Same %s orders at quote %s. Supply + historical swings set the buffer; delaying only helps if a later month gets a lower buffer.",
        fmt_int(n_cars()), fmt_sgd(quoted())
      )
    })

    output$suggest_note <- renderText({
      pr <- planned_row()
      if (is.null(pr) || !nrow(pr)) return("")
      sprintf(
        "Suggested plan buffer %s/car (55%% supply-relative %s + 45%% hist P90). Slider overrides the plan column only.",
        fmt_sgd(pr$suggested[[1]]), fmt_sgd(pr$supply_buf[[1]])
      )
    })

    output$hist_note <- renderText({
      hs <- hist_stats()
      sprintf(
        "Cat %s historical month rises — median %s · P75 %s · P90 %s (n=%s). Last print %s.",
        input$category, fmt_sgd(hs$p50), fmt_sgd(hs$p75), fmt_sgd(hs$p90),
        fmt_int(hs$n), fmt_sgd(prem_now())
      )
    })
  })
}
