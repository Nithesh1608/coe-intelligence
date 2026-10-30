box::use(
  shiny[
    NS, moduleServer, div, tags, numericInput, selectInput, sliderInput,
    checkboxInput, actionButton, fileInput, downloadButton, downloadHandler,
    renderText, renderUI, uiOutput, reactive, reactiveVal, observe, observeEvent,
    invalidateLater, updateNumericInput, updateSelectInput, req, withProgress,
    incProgress, showNotification
  ],
  bslib[card, card_header, card_body, navset_card_underline, nav_panel],
  echarts4r[
    renderEcharts4r, echarts4rOutput, e_charts, e_bar, e_tooltip,
    e_color, e_y_axis, e_x_axis, e_legend
  ],
)
box::use(
  app/logic/exposure/api[
    load_exposure_funs, load_exposure_data, make_pooled_book,
    parse_order_book, parse_fleet_schedule
  ],
  app/logic/viewdata/store[project_root],
  app/logic/viewdata/proposal_viz[prep_exposure_waterfall, chart_exposure_waterfall],
)

`%||%` <- function(a, b) {
  if (is.null(a) || !length(a) || (length(a) == 1L && is.na(a))) b else a
}

fmt_sgd <- function(x) {
  paste0("$", format(round(as.numeric(x)[[1]]), big.mark = ",", scientific = FALSE, trim = TRUE))
}

fmt_int <- function(x) {
  format(round(as.numeric(x)[[1]]), big.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_pct <- function(x) {
  paste0(format(round(100 * as.numeric(x)[[1]], 1), nsmall = 1), "%")
}

last_premium <- function(bidding, category) {
  b <- bidding[bidding$category == category & !is.na(bidding$premium), ]
  if (!nrow(b)) return(100000)
  as.numeric(utils::tail(b[order(b$month), ], 1)$premium[[1]])
}

last_pqp <- function(root, category, fallback) {
  p <- file.path(root, "data", "processed", "pqp.rds")
  if (!file.exists(p)) return(fallback)
  d <- tryCatch(readRDS(p), error = function(e) NULL)
  if (is.null(d)) return(fallback)
  d$category <- as.character(d$category)
  if (is.numeric(d$month)) d$month <- as.Date(d$month, origin = "1970-01-01")
  else d$month <- as.Date(d$month)
  d <- d[d$category == category & !is.na(d$pqp), ]
  if (!nrow(d)) return(fallback)
  as.numeric(utils::tail(d[order(d$month), ], 1)$pqp[[1]])
}

#' Full dealer / fleet exposure calculator
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$p(class = "eyebrow", "Your risk"),
    tags$h2("Exposure calculator"),
    tags$p(
      class = "lede",
      "Turn a locked order book or fleet schedule into expected COE cost, a loss range, and timing options. ",
      "Premium paths are simulated; optionally tilted by the supply forecast. We do not forecast the next premium."
    ),

    navset_card_underline(
      id = ns("tabs"),
      nav_panel(
        "Dealer order book",
        div(
          class = "give-panel",
          tags$span(class = "nx-kicker", "Inputs"),
          tags$h3("Locked book"),
          div(
            class = "desk-controls desk-controls--row",
            numericInput(ns("n_cars"), "Orders", 30, min = 1, max = 200, width = "110px"),
            selectInput(
              ns("category"), "Category",
              choices = c("Category A" = "A", "Category B" = "B", "A + B example book" = "AB"),
              selected = "A", width = "180px"
            ),
            numericInput(ns("quoted"), "Quoted premium ($)", 130000, min = 0, max = 400000, step = 500, width = "160px"),
            sliderInput(ns("psucc"), "Bid success chance", 0.3, 1, 0.7, 0.05, width = "180px"),
            sliderInput(ns("vol"), "Volatility ×", 0.5, 2, 1, 0.1, width = "160px"),
            checkboxInput(ns("tilt"), "Tilt paths by supply forecast", TRUE),
            numericInput(ns("nsim"), "Simulations", 500, 100, 1500, 100, width = "120px"),
            actionButton(ns("load_demo"), "Load example", class = "btn-demo"),
            actionButton(ns("clear_upload"), "Clear upload", class = "btn-demo"),
            actionButton(ns("run"), "Price the book", class = "btn-demo")
          ),
          div(
            class = "desk-controls desk-controls--row",
            style = "margin-top: 0.75rem; align-items: end;",
            div(
              style = "min-width: 260px; flex: 1;",
              fileInput(
                ns("dl_file"),
                "Upload order book (CSV)",
                accept = c(".csv", "text/csv"),
                buttonLabel = "Browse…",
                placeholder = "No file selected"
              )
            ),
            downloadButton(ns("dl_tmpl"), "Download template", class = "btn btn-outline-secondary btn-sm")
          ),
          uiOutput(ns("book_status")),
          tags$p(
            class = "footnote",
            "Download the template, edit your orders, then upload. Columns: order_id, category (A/B), ",
            "quoted_premium, target_exercise, max_attempts, contract (guaranteed|capped|rebate), optional cap_topup. ",
            "Without an upload, pooled controls / example book apply. Tilt uses elastic-net predicted quota from Results."
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
        div(
          class = "calc-section viz-explain",
          tags$span(class = "nx-kicker", "Cost build-up"),
          tags$h3("Exposure waterfall — quote to clearing to timing"),
          tags$p(
            paste(
              "From the package total you quoted, to expected clearing under simulated paths,",
              "the p10–p90 range, and what happens if you delay the whole book by one or two quarters.",
              "Runs automatically on an example book; re-run after you change inputs."
            )
          ),
          tags$ul(
            class = "viz-explain__list",
            tags$li(tags$strong("Quoted book — "), "sum of locked package premiums."),
            tags$li(tags$strong("Clearing gap — "), "quoted total versus expected realised COE cost."),
            tags$li(tags$strong("Upside / downside — "), "distance from expected cost to p10 and p90."),
            tags$li(tags$strong("Delay +1Q / +2Q — "), "change in expected cost if every order shifts later.")
          ),
          card(
            class = "desk-card",
            card_header("Order-book cost waterfall ($ thousands)"),
            card_body(class = "chart-frame chart-frame--tall", echarts4rOutput(ns("dl_waterfall"), height = "400px"))
          ),
          tags$p(class = "footnote", shiny::textOutput(ns("dl_waterfall_note"), inline = TRUE))
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
            actionButton(ns("fl_clear_upload"), "Clear upload", class = "btn-demo"),
            actionButton(ns("fl_run"), "Compare options", class = "btn-demo")
          ),
          div(
            class = "desk-controls desk-controls--row",
            style = "margin-top: 0.75rem; align-items: end;",
            div(
              style = "min-width: 260px; flex: 1;",
              fileInput(
                ns("fl_file"),
                "Upload replacement schedule (CSV)",
                accept = c(".csv", "text/csv"),
                buttonLabel = "Browse…",
                placeholder = "No file selected"
              )
            ),
            downloadButton(ns("fl_tmpl"), "Download template", class = "btn btn-outline-secondary btn-sm")
          ),
          uiOutput(ns("fleet_status")),
          tags$p(
            class = "footnote",
            "Fleet template columns: quarter, category (A/B), n_vehicles, parf_rebate; ",
            "optional new_vehicle_cost, old_vehicle_residual for capex mode. Default is the example schedule."
          )
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
    root <- project_root()
    funs <- tryCatch(load_exposure_funs(root), error = function(e) {
      showNotification(paste("Exposure engine failed to load:", conditionMessage(e)), type = "error", duration = 12)
      NULL
    })
    exp_data <- tryCatch(load_exposure_data(root), error = function(e) {
      showNotification(paste("Exposure data failed to load:", conditionMessage(e)), type = "error", duration = 12)
      NULL
    })

    observeEvent(session$clientData$url_hostname, {
      if (is.null(funs) || is.null(exp_data)) return()
      lp <- last_premium(store$bidding, "A")
      updateNumericInput(session, "quoted", value = round(lp / 500) * 500)
    }, once = TRUE)

    observeEvent(input$category, {
      if (is.null(store$bidding)) return()
      cat <- if (identical(input$category, "AB")) "A" else input$category
      lp <- last_premium(store$bidding, cat)
      updateNumericInput(session, "quoted", value = round(lp / 500) * 500)
    }, ignoreInit = TRUE)

    uploaded_book <- reactiveVal(NULL)
    uploaded_fleet <- reactiveVal(NULL)

    observeEvent(input$load_demo, {
      uploaded_book(NULL)
      lp <- last_premium(store$bidding, "A")
      updateNumericInput(session, "n_cars", value = 50)
      updateSelectInput(session, "category", selected = "AB")
      updateNumericInput(session, "quoted", value = round(lp / 500) * 500)
      updateNumericInput(session, "nsim", value = 500)
      showNotification("Using A+B example book (upload cleared).", type = "message")
    }, ignoreInit = TRUE)

    observeEvent(input$dl_file, {
      f <- input$dl_file
      req(!is.null(f), nzchar(f$datapath %||% ""))
      tryCatch({
        b <- parse_order_book(f$datapath)
        uploaded_book(b)
        showNotification(
          sprintf("Uploaded order book: %d orders (%s).", nrow(b), paste(unique(b$category), collapse = "+")),
          type = "message"
        )
      }, error = function(e) {
        uploaded_book(NULL)
        showNotification(paste("Order book upload failed:", conditionMessage(e)), type = "error", duration = 12)
      })
    }, ignoreInit = TRUE)

    observeEvent(input$clear_upload, {
      uploaded_book(NULL)
      showNotification("Cleared uploaded order book — using on-screen / example controls.", type = "message")
    }, ignoreInit = TRUE)

    observeEvent(input$fl_file, {
      f <- input$fl_file
      req(!is.null(f), nzchar(f$datapath %||% ""))
      tryCatch({
        sch <- parse_fleet_schedule(f$datapath)
        uploaded_fleet(sch)
        showNotification(
          sprintf("Uploaded fleet schedule: %d rows.", nrow(sch)),
          type = "message"
        )
      }, error = function(e) {
        uploaded_fleet(NULL)
        showNotification(paste("Fleet upload failed:", conditionMessage(e)), type = "error", duration = 12)
      })
    }, ignoreInit = TRUE)

    observeEvent(input$fl_clear_upload, {
      uploaded_fleet(NULL)
      showNotification("Cleared uploaded fleet schedule — using example schedule.", type = "message")
    }, ignoreInit = TRUE)

    output$dl_tmpl <- downloadHandler(
      filename = function() "order_book_template.csv",
      content = function(file) {
        tmpl <- exp_data$example_book
        if (is.null(tmpl) || !nrow(tmpl)) {
          tmpl <- make_pooled_book(6L, "A", last_premium(store$bidding, "A"))
          tmpl$contract[c(3L, 5L)] <- c("capped", "rebate")
        }
        utils::write.csv(tmpl, file, row.names = FALSE)
      }
    )

    output$fl_tmpl <- downloadHandler(
      filename = function() "fleet_schedule_template.csv",
      content = function(file) {
        tmpl <- exp_data$example_fleet
        if (is.null(tmpl) || !nrow(tmpl)) {
          tmpl <- data.frame(
            quarter = rep(1:4, each = 2),
            category = rep(c("A", "B"), 4),
            n_vehicles = rep(c(50L, 25L), 4),
            parf_rebate = rep(c(8000, 12000), 4),
            stringsAsFactors = FALSE
          )
        }
        utils::write.csv(tmpl, file, row.names = FALSE)
      }
    )

    book <- reactive({
      req(!is.null(exp_data))
      up <- uploaded_book()
      if (!is.null(up) && nrow(up)) return(up)
      if (identical(input$category, "AB") && !is.null(exp_data$example_book)) {
        b <- exp_data$example_book
        b$quoted_premium[b$category == "A"] <- as.numeric(input$quoted)
        b$quoted_premium[b$category == "B"] <- last_premium(store$bidding, "B")
        return(b)
      }
      cat <- if (identical(input$category, "AB")) "A" else input$category
      make_pooled_book(input$n_cars, cat, input$quoted)
    })

    fleet_sched <- reactive({
      req(!is.null(exp_data))
      up <- uploaded_fleet()
      if (!is.null(up) && nrow(up)) return(up)
      exp_data$example_fleet
    })

    output$book_status <- renderUI({
      b <- tryCatch(book(), error = function(e) NULL)
      if (is.null(b) || !nrow(b)) return(NULL)
      src <- if (!is.null(uploaded_book())) "Uploaded CSV" else if (identical(input$category, "AB")) "A+B example book" else "Pooled controls"
      tags$p(
        class = "footnote",
        sprintf(
          "Active book: %s · %d orders · cats %s · contracts %s",
          src, nrow(b),
          paste(sort(unique(as.character(b$category))), collapse = "/"),
          paste(sort(unique(as.character(b$contract))), collapse = "/")
        )
      )
    })

    output$fleet_status <- renderUI({
      f <- tryCatch(fleet_sched(), error = function(e) NULL)
      if (is.null(f) || !nrow(f)) return(NULL)
      src <- if (!is.null(uploaded_fleet())) "Uploaded CSV" else "Example schedule"
      tags$p(
        class = "footnote",
        sprintf("Active schedule: %s · %d rows · %s vehicles", src, nrow(f), sum(as.integer(f$n_vehicles)))
      )
    })

    # Live predicted quota from the elastic-net desk (same series as Results).
    # simulate_premiums expects a quarterly path: monthly outlook is summed by quarter.
    quota_path_for <- function(cat) {
      path_from_monthly <- function(month, value) {
        ok <- is.finite(value) & !is.na(month)
        if (sum(ok) < 2L) return(NULL)
        month <- as.Date(month[ok])
        value <- as.numeric(value[ok])
        ord <- order(month)
        month <- month[ord]
        value <- value[ord]
        value <- pmax(value, 1e-6)
        qtr <- as.character(cut(month, breaks = "quarter"))
        agg <- tapply(value, qtr, sum, na.rm = TRUE)
        path <- as.numeric(agg)
        if (length(path) >= 2L && all(is.finite(path)) && all(path > 0)) path else NULL
      }

      q <- store$quota_outlook
      if (!is.null(q) && nrow(q) && all(c("month", "category", "outlook_from_forecast") %in% names(q))) {
        d <- q[as.character(q$category) == cat, , drop = FALSE]
        path <- path_from_monthly(d$month, d$outlook_from_forecast)
        if (!is.null(path)) return(path)
      }

      tq <- store$two_quota
      if (!is.null(tq) && nrow(tq) && all(c("month", "category", "model", "outlook") %in% names(tq))) {
        live <- store$meta$live_model %||% "core_forecast"
        d <- tq[as.character(tq$category) == cat & as.character(tq$model) == live, , drop = FALSE]
        path <- path_from_monthly(d$month, d$outlook)
        if (!is.null(path)) return(path)
      }

      # Last resort only if live outlook artefacts are missing.
      scen <- exp_data$scenario_examples
      if (is.null(scen) || !("total_median" %in% names(scen))) return(NULL)
      s <- scen[grepl("^Baseline", as.character(scen$scenario)) & scen$category == cat, ]
      if (!nrow(s)) return(NULL)
      s <- s[order(s$h), ]
      as.numeric(s$total_median)
    }

    dlog_for <- function(cat) {
      m <- exp_data$premium_moves
      if (is.null(m) || is.null(m$round_to_round_detail)) return(numeric())
      d <- m$round_to_round_detail
      d <- d[d$category == cat & is.finite(d$chg_pct), ]
      out <- log1p(as.numeric(d$chg_pct))
      out[is.finite(out)]
    }

    elasticity_for <- function(cat) {
      s <- exp_data$price_sensitivity
      if (is.null(s)) return(0)
      hit <- s$elasticity_quota[s$category == cat]
      if (!length(hit) || is.na(hit[[1]])) 0 else as.numeric(hit[[1]])
    }

    dl_res <- reactiveVal(NULL)

    price_book_once <- function(b, nsim, use_progress = TRUE) {
      cats <- unique(as.character(b$category))
      sims <- list()
      for (i in seq_along(cats)) {
        cat_code <- as.character(cats[[i]])
        if (isTRUE(use_progress)) {
          incProgress(1 / (length(cats) + 1), detail = paste("Cat", cat_code))
        }
        dlog <- dlog_for(cat_code)
        if (!length(dlog)) stop("No historical premium moves for category ", cat_code)
        p0 <- last_premium(store$bidding, cat_code)
        pqp0 <- last_pqp(root, cat_code, p0)
        q_path <- if (isTRUE(input$tilt)) quota_path_for(cat_code) else NULL
        if (isTRUE(input$tilt) && is.null(q_path)) {
          stop(
            "Tilt is on but no predicted quota path for Cat ", cat_code,
            ". Check quota_outlook_calibrated / core_forecast outlook."
          )
        }
        sims[[cat_code]] <- funs$simulate_premiums(
          dlog_hist = dlog,
          p0 = p0,
          pqp0 = pqp0,
          start_month = store$latest_bid_month,
          elasticity = elasticity_for(cat_code),
          n_sims = nsim,
          vol_mult = as.numeric(input$vol %||% 1),
          supply_tilt = isTRUE(input$tilt),
          quota_path = q_path
        )
      }
      if (isTRUE(use_progress)) {
        incProgress(1 / (length(cats) + 1), detail = "Pricing book")
      }
      base <- funs$dealer_exposure(b, sims, p_success = as.numeric(input$psucc %||% 0.7))
      shifts <- funs$timing_shift(
        b, sims,
        shifts = c(0, 6, 12),
        p_success = as.numeric(input$psucc %||% 0.7)
      )
      dl_res(list(base = base, shifts = shifts, nsim = nsim, n_orders = nrow(b)))
      invisible(TRUE)
    }

    run_dealer <- function(nsim_override = NULL, quiet = FALSE, use_progress = TRUE) {
      if (is.null(funs) || is.null(exp_data)) {
        if (!quiet) showNotification("Exposure engine is not available.", type = "error")
        return(invisible(FALSE))
      }
      if (is.null(exp_data$premium_moves) || is.null(exp_data$price_sensitivity)) {
        if (!quiet) showNotification("Missing premium-move or sensitivity tables.", type = "error")
        return(invisible(FALSE))
      }
      b <- tryCatch(book(), error = function(e) NULL)
      if (is.null(b) || !nrow(b)) {
        if (!quiet) showNotification("Order book is empty.", type = "error")
        return(invisible(FALSE))
      }
      nsim_raw <- if (!is.null(nsim_override)) nsim_override else as.integer(input$nsim)
      nsim <- max(100L, min(1500L, as.integer(nsim_raw %||% 500L)))
      tryCatch({
        if (isTRUE(use_progress)) {
          withProgress(
            message = "Simulating premium paths",
            value = 0,
            price_book_once(b, nsim, use_progress = TRUE)
          )
        } else {
          price_book_once(b, nsim, use_progress = FALSE)
        }
        invisible(TRUE)
      }, error = function(e) {
        dl_res(NULL)
        if (!quiet) {
          showNotification(paste("Could not price book:", conditionMessage(e)), type = "error", duration = 12)
        }
        invisible(FALSE)
      })
    }

    observeEvent(input$run, {
      run_dealer(use_progress = TRUE)
    })

    # Deferred light auto-price so Your risk is not empty; never blocks Home on Posit.
    auto_priced <- reactiveVal(FALSE)
    auto_tries <- reactiveVal(0L)
    observe({
      if (isTRUE(auto_priced()) || !is.null(dl_res())) return()
      tries <- as.integer(auto_tries())
      if (tries >= 5L) return()
      shiny::invalidateLater(900, session)
      auto_tries(tries + 1L)
      if (is.null(funs) || is.null(exp_data)) return()
      auto_priced(TRUE)
      run_dealer(nsim_override = 150L, quiet = TRUE, use_progress = FALSE)
    })

    output$dl_lead <- renderUI({
      r <- dl_res()
      if (is.null(r)) {
        return(tags$p(class = "footnote", "Pricing the book… or click “Price the book” to re-run."))
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
      h <- graphics::hist(pnl, breaks = 30, plot = FALSE)
      df <- data.frame(mid = as.numeric(h$mids), n = as.numeric(h$counts))
      df |>
        e_charts(mid) |>
        e_bar(n, name = "Simulations") |>
        e_tooltip() |>
        e_color("#ff5c1a") |>
        e_x_axis(name = "Dealer P&L vs quote ($k)") |>
        e_y_axis(name = "Count")
    })

    output$dl_shift <- renderEcharts4r({
      r <- dl_res()
      req(!is.null(r))
      d <- as.data.frame(r$shifts)
      d$label <- paste0("+", as.numeric(d$shift_quarters), "Q")
      d$exp_k <- as.numeric(d$expected_dealer_pnl) / 1000
      d$var_k <- -as.numeric(d$var95_loss) / 1000
      d[, c("label", "exp_k", "var_k")] |>
        e_charts(label) |>
        e_bar(exp_k, name = "Expected P&L ($k)") |>
        e_bar(var_k, name = "-VaR95 ($k)") |>
        e_tooltip() |>
        e_color(c("#c8f542", "#ff5c1a")) |>
        e_legend(bottom = 0)
    })

    output$dl_waterfall <- renderEcharts4r({
      r <- dl_res()
      req(!is.null(r))
      b <- book()
      quoted_total <- sum(as.numeric(b$quoted_premium), na.rm = TRUE)
      prep <- prep_exposure_waterfall(r$base$summary, r$shifts, quoted_total)
      chart_exposure_waterfall(prep)
    })

    output$dl_waterfall_note <- renderText({
      r <- dl_res()
      if (is.null(r)) return("Building waterfall from the active book…")
      s <- r$base$summary
      sprintf(
        "Expected COE cost %s · p10 %s · p90 %s · VaR95 loss %s. Timing uses the same paths as the delay chart.",
        fmt_sgd(s$expected_cost), fmt_sgd(s$cost_p10), fmt_sgd(s$cost_p90), fmt_sgd(s$var95_loss)
      )
    })

    # Keep exposure charts suspended until the tab is opened — avoids echarts
    # init on hidden panels blanking Home.

    output$dl_note <- renderText({
      r <- dl_res()
      if (is.null(r)) return("")
      if (isTRUE(input$tilt)) {
        cats <- unique(as.character(book()$category))
        n_q <- sum(vapply(cats, function(c) length(quota_path_for(c) %||% numeric()), integer(1)))
        sprintf(
          "Supply tilt ON — drift from elastic-net predicted quota (Results outlook), %s quarterly points across categories. Paths still resample historical premium moves.",
          n_q
        )
      } else {
        "Supply tilt OFF — premium paths are historical moves only; predicted quota is not applied."
      }
    })

    fl_res <- reactiveVal(NULL)
    observeEvent(input$fl_run, {
      if (is.null(funs) || is.null(exp_data)) {
        showNotification("Exposure engine is not available.", type = "error")
        return()
      }
      sch <- fleet_sched()
      if (is.null(sch) || !nrow(sch) || is.null(exp_data$premium_sims_baseline)) {
        showNotification("Fleet schedule or baseline sims missing.", type = "error")
        return()
      }
      tryCatch({
        withProgress(message = "Fleet options", value = 0.3, {
          out <- funs$fleet_decision(
            sch,
            exp_data$premium_sims_baseline,
            shift_quarters = as.integer(input$fl_shift),
            include_capex = isTRUE(input$fl_capex)
          )
          fl_res(out)
        })
      }, error = function(e) {
        fl_res(NULL)
        showNotification(paste("Fleet compare failed:", conditionMessage(e)), type = "error", duration = 12)
      })
    })

    output$fl_table <- renderUI({
      d <- fl_res()
      if (is.null(d)) {
        return(tags$p(class = "footnote", "Click “Compare options” to price the example fleet schedule."))
      }
      d <- as.data.frame(d)
      nm <- names(d)
      pick <- function(key) {
        if (key %in% nm) return(d[[key]])
        hit <- grep(key, nm, value = TRUE)
        if (!length(hit)) return(rep(NA_real_, nrow(d)))
        d[[hit[[1]]]]
      }
      bid <- pick("bid_new_py_mean")
      r10 <- pick("renew_10y_py_mean")
      r5 <- pick("renew_5y_py_mean")
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
