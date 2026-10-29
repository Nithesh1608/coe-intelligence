box::use(
  shiny[
    NS, moduleServer, div, tags, numericInput, selectInput, sliderInput,
    checkboxInput, actionButton, renderText, renderUI, uiOutput, reactive,
    observeEvent, updateNumericInput, updateSelectInput, req, withProgress,
    incProgress
  ],
  bslib[card, card_header, card_body, navset_card_underline, nav_panel],
  echarts4r[
    renderEcharts4r, echarts4rOutput, e_charts, e_bar, e_tooltip,
    e_color, e_y_axis, e_x_axis, e_legend, e_mark_line
  ],
)
box::use(
  app/logic/doris/api[load_doris_funs, load_doris_data, make_pooled_book],
  app/logic/viewdata/store[project_root],
)

fmt_sgd <- function(x) {
  paste0("$", format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE))
}

fmt_int <- function(x) {
  format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_pct <- function(x) paste0(format(round(100 * as.numeric(x), 1), nsmall = 1), "%")

last_premium <- function(bidding, category) {
  b <- bidding[bidding$category == category & !is.na(bidding$premium), ]
  if (!nrow(b)) return(100000)
  as.numeric(utils::tail(b[order(b$month), ], 1)$premium[[1]])
}

last_pqp <- function(root, category, fallback) {
  p <- file.path(root, "data", "processed", "pqp.rds")
  if (!file.exists(p)) return(fallback)
  d <- readRDS(p)
  d$category <- as.character(d$category)
  if (is.numeric(d$month)) d$month <- as.Date(d$month, origin = "1970-01-01")
  else d$month <- as.Date(d$month)
  d <- d[d$category == category & !is.na(d$pqp), ]
  if (!nrow(d)) return(fallback)
  as.numeric(utils::tail(d[order(d$month), ], 1)$pqp[[1]])
}

#' Full dealer / fleet exposure (Doris engine + supply tilt)
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$p(class = "eyebrow", "Your risk"),
    tags$h2("Exposure calculator"),
    tags$p(
      class = "lede",
      "Prices a locked dealer book (or fleet schedule) with simulated premium paths. ",
      "Optionally tilt those paths with the supply forecast. We do not forecast the next premium."
    ),

    navset_card_underline(
      nav_panel(
        "Dealer order book",
        div(
          class = "give-panel",
          tags$span(class = "nx-kicker", "Inputs"),
          tags$h3("Locked book"),
          div(
            class = "desk-controls desk-controls--row",
            numericInput(ns("n_cars"), "Orders", 50, min = 1, max = 500, width = "110px"),
            selectInput(
              ns("category"), "Category",
              choices = c("Category A" = "A", "Category B" = "B", "A + B example book" = "AB"),
              selected = "A", width = "180px"
            ),
            numericInput(ns("quoted"), "Quoted premium ($)", 130000, min = 0, max = 400000, step = 500, width = "160px"),
            sliderInput(ns("psucc"), "Bid success chance", 0.3, 1, 0.7, 0.05, width = "180px"),
            sliderInput(ns("vol"), "Volatility ×", 0.5, 2, 1, 0.1, width = "160px"),
            checkboxInput(ns("tilt"), "Tilt paths by supply forecast", TRUE),
            numericInput(ns("nsim"), "Simulations", 1000, 200, 3000, 200, width = "120px"),
            actionButton(ns("load_demo"), "Load example", class = "btn-demo"),
            actionButton(ns("run"), "Price the book", class = "btn-demo")
          ),
          tags$p(
            class = "footnote",
            "Pooled guaranteed orders by default. “A + B example book” loads Doris’s 50-order template. ",
            "Supply tilt uses the calibrated quota path."
          )
        ),
        uiOutput(ns("dl_lead")),
        div(
          class = "calc-section",
          tags$span(class = "nx-kicker", "Outcomes"),
          tags$h3("Where the book can land"),
          div(
            class = "timing-compare",
            style = "grid-template-columns: 1.2fr 1fr;",
            card(
              class = "desk-card",
              card_header("Dealer P&L across simulations"),
              card_body(class = "chart-frame", echarts4rOutput(ns("dl_hist"), height = "320px"))
            ),
            card(
              class = "desk-card",
              card_header("Delay the whole book"),
              card_body(class = "chart-frame", echarts4rOutput(ns("dl_shift"), height = "320px"))
            )
          )
        ),
        tags$p(class = "footnote", shiny::textOutput(ns("dl_note"), inline = TRUE))
      ),
      nav_panel(
        "Fleet replacement",
        div(
          class = "give-panel",
          tags$span(class = "nx-kicker", "Inputs"),
          tags$h3("Replacement schedule"),
          div(
            class = "desk-controls desk-controls--row",
            sliderInput(ns("fl_shift"), "Shift schedule (quarters)", 0, 2, 0, 1, width = "220px"),
            checkboxInput(ns("fl_capex"), "Include vehicle capital (if columns present)", FALSE),
            actionButton(ns("fl_run"), "Compare options", class = "btn-demo")
          ),
          tags$p(class = "footnote", "Example fleet schedule + baseline premium simulations.")
        ),
        uiOutput(ns("fl_table")),
        tags$p(class = "footnote", shiny::textOutput(ns("fl_note"), inline = TRUE))
      )
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    funs <- load_doris_funs()
    doris <- load_doris_data()
    root <- project_root()

    observeEvent(TRUE, {
      lp <- last_premium(store$bidding, "A")
      updateNumericInput(session, "quoted", value = round(lp / 500) * 500)
    }, once = TRUE)

    observeEvent(input$category, {
      cat <- if (identical(input$category, "AB")) "A" else input$category
      lp <- last_premium(store$bidding, cat)
      updateNumericInput(session, "quoted", value = round(lp / 500) * 500)
    }, ignoreInit = TRUE)

    observeEvent(input$load_demo, {
      lp <- last_premium(store$bidding, "A")
      updateNumericInput(session, "n_cars", value = 50)
      updateSelectInput(session, "category", selected = "A")
      updateNumericInput(session, "quoted", value = round(lp / 500) * 500)
      updateNumericInput(session, "nsim", value = 1000)
    }, ignoreInit = TRUE)

    book <- reactive({
      if (identical(input$category, "AB") && !is.null(doris$example_book)) {
        b <- doris$example_book
        b$quoted_premium[b$category == "A"] <- as.numeric(input$quoted)
        b$quoted_premium[b$category == "B"] <- last_premium(store$bidding, "B")
        return(b)
      }
      cat <- if (identical(input$category, "AB")) "A" else input$category
      make_pooled_book(input$n_cars, cat, input$quoted)
    })

    quota_path_for <- function(cat) {
      scen <- doris$scenario_examples
      if (is.null(scen) || !("total_median" %in% names(scen))) return(NULL)
      s <- scen[grepl("^Baseline", as.character(scen$scenario)) & scen$category == cat, ]
      if (!nrow(s)) return(NULL)
      s <- s[order(s$h), ]
      as.numeric(s$total_median)
    }

    dlog_for <- function(cat) {
      m <- doris$premium_moves
      if (is.null(m) || is.null(m$round_to_round_detail)) return(numeric())
      d <- m$round_to_round_detail
      d <- d[d$category == cat & is.finite(d$chg_pct), ]
      log1p(as.numeric(d$chg_pct))
    }

    elasticity_for <- function(cat) {
      s <- doris$price_sensitivity
      if (is.null(s)) return(0)
      hit <- s$elasticity_quota[s$category == cat]
      if (!length(hit) || is.na(hit[[1]])) 0 else as.numeric(hit[[1]])
    }

    dl_res <- reactiveVal(NULL)

    observeEvent(input$run, {
      req(!is.null(doris$premium_moves), !is.null(doris$price_sensitivity))
      b <- book()
      cats <- unique(as.character(b$category))
      nsim <- max(200L, min(3000L, as.integer(input$nsim)))
      withProgress(message = "Simulating premium paths", value = 0, {
        sims <- list()
        for (i in seq_along(cats)) {
          cat <- cats[[i]]
          incProgress(1 / (length(cats) + 1), detail = paste("Cat", cat))
          p0 <- last_premium(store$bidding, cat)
          pqp0 <- last_pqp(root, cat, p0)
          sims[[cat]] <- funs$simulate_premiums(
            dlog_hist = dlog_for(cat),
            p0 = p0,
            pqp0 = pqp0,
            start_month = store$latest_bid_month,
            elasticity = elasticity_for(cat),
            n_sims = nsim,
            vol_mult = as.numeric(input$vol),
            supply_tilt = isTRUE(input$tilt),
            quota_path = quota_path_for(cat)
          )
        }
        incProgress(1 / (length(cats) + 1), detail = "Pricing book")
        base <- funs$dealer_exposure(b, sims, p_success = as.numeric(input$psucc))
        shifts <- funs$timing_shift(
          b, sims,
          shifts = c(0, 6, 12),
          p_success = as.numeric(input$psucc)
        )
        dl_res(list(base = base, shifts = shifts, nsim = nsim, n_orders = nrow(b)))
      })
    })

    output$dl_lead <- renderUI({
      r <- dl_res()
      if (is.null(r)) {
        return(tags$p(class = "footnote", "Click “Price the book” to run the exposure engine."))
      }
      s <- r$base$summary
      div(
        class = "lead-grid",
        div(
          class = "lead-cell",
          tags$span(class = "nx-kicker", "Expected COE cost"),
          div(class = "lead-num", fmt_sgd(s$expected_cost)),
          tags$p(class = "lead-lab", sprintf("%s orders · %s sims", fmt_int(r$n_orders), fmt_int(r$nsim)))
        ),
        div(
          class = "lead-cell lead-cell--hot",
          tags$span(class = "nx-kicker", "95% loss (VaR)"),
          div(class = "lead-num", fmt_sgd(s$var95_loss)),
          tags$p(class = "lead-lab", "Dealer loss exceeded one time in twenty")
        ),
        div(
          class = "lead-cell lead-cell--accent",
          tags$span(class = "nx-kicker", "Chance book loses"),
          div(class = "lead-num", fmt_pct(s$prob_loss)),
          tags$p(class = "lead-lab", "Share of sims with dealer P&L < 0")
        ),
        div(
          class = "lead-cell",
          tags$span(class = "nx-kicker", "Expected customer top-up"),
          div(class = "lead-num lead-num--sm", fmt_sgd(s$expected_customer_topup)),
          tags$p(class = "lead-lab", "Average across sims")
        )
      )
    })

    output$dl_hist <- renderEcharts4r({
      r <- dl_res()
      req(!is.null(r))
      pnl <- as.numeric(r$base$portfolio$dealer_pnl) / 1000
      h <- graphics::hist(pnl, breaks = 40, plot = FALSE)
      df <- data.frame(mid = h$mids, n = h$counts)
      df |>
        e_charts(mid) |>
        e_bar(n, name = "Simulations") |>
        e_mark_line(data = list(xAxis = 0), title = "0") |>
        e_tooltip() |>
        e_color("#ff5c1a") |>
        e_x_axis(name = "Dealer P&L vs quote ($k)") |>
        e_y_axis(name = "Count")
    })

    output$dl_shift <- renderEcharts4r({
      r <- dl_res()
      req(!is.null(r))
      d <- r$shifts
      d$label <- paste0("+", d$shift_quarters, "Q")
      d$exp_k <- as.numeric(d$expected_dealer_pnl) / 1000
      d$var_k <- -as.numeric(d$var95_loss) / 1000
      d |>
        e_charts(label) |>
        e_bar(exp_k, name = "Expected P&L ($k)") |>
        e_bar(var_k, name = "−VaR95 ($k)") |>
        e_tooltip() |>
        e_color(c("#c8f542", "#ff5c1a")) |>
        e_legend(bottom = 0)
    })

    output$dl_note <- renderText({
      r <- dl_res()
      if (is.null(r)) return("")
      sprintf(
        "Supply tilt %s. Paths from historical exercise moves; quota path tilts drift when enabled.",
        if (isTRUE(input$tilt)) "on" else "off"
      )
    })

    fl_res <- reactiveVal(NULL)
    observeEvent(input$fl_run, {
      req(!is.null(doris$example_fleet), !is.null(doris$premium_sims_baseline))
      withProgress(message = "Fleet options", {
        out <- funs$fleet_decision(
          doris$example_fleet,
          doris$premium_sims_baseline,
          shift_quarters = as.integer(input$fl_shift),
          include_capex = isTRUE(input$fl_capex)
        )
        fl_res(out)
      })
    })

    output$fl_table <- renderUI({
      d <- fl_res()
      if (is.null(d)) {
        return(tags$p(class = "footnote", "Click “Compare options” to price the example fleet schedule."))
      }
      # flatten mean columns if nested names from summarise
      nm <- names(d)
      bid <- if ("bid_new_py_mean" %in% nm) d$bid_new_py_mean else d[[grep("bid_new_py", nm)[1]]]
      r10 <- if ("renew_10y_py_mean" %in% nm) d$renew_10y_py_mean else d[[grep("renew_10y_py", nm)[1]]]
      r5 <- if ("renew_5y_py_mean" %in% nm) d$renew_5y_py_mean else d[[grep("renew_5y_py", nm)[1]]]
      rows <- lapply(seq_len(min(nrow(d), 12L)), function(i) {
        tags$tr(
          tags$td(d$quarter[i]),
          tags$td(d$category[i]),
          tags$td(d$n_vehicles[i]),
          tags$td(fmt_sgd(bid[i])),
          tags$td(fmt_sgd(r10[i])),
          tags$td(fmt_sgd(r5[i])),
          tags$td(fmt_pct(d$share_bid_best[i]))
        )
      })
      tags$table(
        class = "table table-sm",
        style = "color:#fafafa;",
        tags$thead(tags$tr(
          tags$th("Quarter"), tags$th("Cat"), tags$th("Vehicles"),
          tags$th("Bid new / yr"), tags$th("Renew 10y / yr"), tags$th("Renew 5y / yr"),
          tags$th("Share bid best")
        )),
        tags$tbody(rows)
      )
    })

    output$fl_note <- renderText({
      if (is.null(fl_res())) return("")
      sprintf("Schedule shifted by %s quarter(s). Expected cost per COE-year.", input$fl_shift)
    })
  })
}
