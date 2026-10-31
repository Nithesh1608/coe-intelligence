box::use(
  shiny[
    NS, moduleServer, div, tags, numericInput, selectInput, sliderInput,
    checkboxInput, actionButton, fileInput, downloadButton, downloadHandler,
    renderText, renderUI, uiOutput, reactive, reactiveVal, observeEvent,
    updateNumericInput, updateSelectInput, req, withProgress,
    incProgress, showNotification
  ],
  bslib[card, card_header, card_body, navset_card_underline, nav_panel],
  echarts4r[
    renderEcharts4r, echarts4rOutput, e_charts, e_bar, e_tooltip,
    e_color, e_grid, e_y_axis, e_x_axis, e_legend
  ],
)
box::use(
  app/logic/exposure/api[
    load_exposure_funs, load_exposure_data, make_pooled_book,
    parse_order_book, parse_fleet_schedule
  ],
  app/logic/viewdata/store[project_root],
  app/logic/viewdata/proposal_viz[prep_exposure_waterfall, chart_exposure_waterfall],
  app/logic/viewdata/scenario_viz[preset_choices, prep_scenario],
  app/logic/viewdata/pipeline[quarter_start],
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
    class = "page-stack desk risk-page",
    tags$section(
      class = "desk-hero desk-hero--pitch",
      div(
        class = "desk-hero__copy",
        tags$p(class = "eyebrow", "Your risk"),
        tags$h1("Price the COE risk on a locked book."),
        tags$p(
          class = "lede lede--wide",
          "Turn orders or a fleet schedule into expected cost, a bad-case loss, and the cost of waiting."
        )
      )
    ),
    div(
      class = "risk-value-strip",
      div(
        class = "risk-value-chip",
        tags$span(class = "risk-value-chip__k", "01"),
        tags$span(class = "risk-value-chip__t", "Expected cost"),
        tags$span(class = "risk-value-chip__d", "total COE $ vs quote")
      ),
      div(
        class = "risk-value-chip",
        tags$span(class = "risk-value-chip__k", "02"),
        tags$span(class = "risk-value-chip__t", "Bad-case loss"),
        tags$span(class = "risk-value-chip__d", "loss exceeded only 1 in 20 runs")
      ),
      div(
        class = "risk-value-chip",
        tags$span(class = "risk-value-chip__k", "03"),
        tags$span(class = "risk-value-chip__t", "Timing"),
        tags$span(class = "risk-value-chip__d", "cost if you wait 1–2 quarters")
      )
    ),

    navset_card_underline(
      id = ns("tabs"),
      nav_panel(
        "Dealer order book",
        tags$section(
          class = "risk-block",
          tags$span(class = "nx-kicker", "Step 1"),
          tags$h2(class = "risk-block__title", "Your book"),
          div(
            class = "risk-panel",
            div(
              class = "risk-panel__grid risk-panel__grid--3",
              numericInput(ns("n_cars"), "Orders", 30, min = 1, max = 200, width = "100%"),
              selectInput(
                ns("category"), "Category",
                choices = c("Category A" = "A", "Category B" = "B", "A + B example" = "AB"),
                selected = "A", width = "100%"
              ),
              numericInput(ns("quoted"), "Quoted premium ($)", 130000, min = 0, max = 400000, step = 500, width = "100%")
            ),
            div(
              class = "risk-panel__actions",
              div(
                class = "risk-upload",
                fileInput(
                  ns("dl_file"),
                  "Or upload CSV",
                  accept = c(".csv", "text/csv"),
                  buttonLabel = "Browse",
                  placeholder = "order book"
                )
              ),
              downloadButton(ns("dl_tmpl"), "Template", class = "btn btn-outline-secondary btn-sm"),
              actionButton(ns("load_demo"), "Example book", class = "btn-demo"),
              actionButton(ns("clear_upload"), "Clear upload", class = "btn-demo")
            ),
            uiOutput(ns("book_status"))
          )
        ),
        tags$section(
          class = "risk-block",
          tags$span(class = "nx-kicker", "Step 2"),
          tags$h2(class = "risk-block__title", "Stress knobs"),
          div(
            class = "risk-panel",
            div(
              class = "risk-panel__grid risk-panel__grid--4",
              sliderInput(
                ns("psucc"),
                "Win rate per bid attempt (%)",
                min = 30, max = 100, value = 70, step = 5,
                post = "%", width = "100%"
              ),
              sliderInput(
                ns("vol"),
                "Premium swing vs history",
                min = 0.5, max = 2, value = 1, step = 0.1,
                pre = "", post = "×", width = "100%"
              ),
              numericInput(ns("nsim"), "Monte Carlo runs", 500, 100, 1500, 100, width = "100%"),
              selectInput(
                ns("tilt_scn"), "Quota path for supply tilt",
                choices = preset_choices(), selected = "baseline", width = "100%"
              )
            ),
            div(
              class = "risk-panel__actions risk-panel__actions--end",
              checkboxInput(ns("tilt"), "Tilt premiums by supply outlook", TRUE),
              actionButton(ns("run"), "Price the book", class = "btn-demo btn-demo--primary")
            )
          )
        ),
        tags$section(
          class = "risk-block",
          tags$span(class = "nx-kicker", "Step 3"),
          tags$h2(class = "risk-block__title", "What you get"),
          uiOutput(ns("dl_lead")),
          div(
            class = "timing-compare risk-charts",
            style = "grid-template-columns: 1.15fr 1fr;",
            card(
              class = "desk-card",
              card_header("P&L distribution"),
              card_body(class = "chart-frame", echarts4rOutput(ns("dl_hist"), height = "320px"))
            ),
            card(
              class = "desk-card",
              card_header("If you delay +1Q / +2Q"),
              card_body(class = "chart-frame", echarts4rOutput(ns("dl_shift"), height = "320px"))
            )
          ),
          card(
            class = "desk-card",
            card_header("Cost build-up ($ thousands)"),
            card_body(class = "chart-frame chart-frame--tall", echarts4rOutput(ns("dl_waterfall"), height = "400px"))
          ),
          tags$p(class = "footnote", shiny::textOutput(ns("dl_waterfall_note"), inline = TRUE)),
          tags$p(class = "footnote", shiny::textOutput(ns("dl_note"), inline = TRUE))
        )
      ),
      nav_panel(
        "Fleet replacement",
        tags$section(
          class = "risk-block",
          tags$span(class = "nx-kicker", "Step 1"),
          tags$h2(class = "risk-block__title", "Replacement schedule"),
          div(
            class = "risk-panel",
            div(
              class = "risk-panel__grid risk-panel__grid--2",
              sliderInput(
                ns("fl_shift"),
                "Push every replacement later",
                min = 0, max = 2, value = 0, step = 1,
                post = " quarters", width = "100%"
              ),
              checkboxInput(ns("fl_capex"), "Include vehicle capital (if in CSV)", FALSE)
            ),
            div(
              class = "risk-panel__actions",
              div(
                class = "risk-upload",
                fileInput(
                  ns("fl_file"),
                  "Upload schedule CSV",
                  accept = c(".csv", "text/csv"),
                  buttonLabel = "Browse",
                  placeholder = "fleet schedule"
                )
              ),
              downloadButton(ns("fl_tmpl"), "Template", class = "btn btn-outline-secondary btn-sm"),
              actionButton(ns("fl_clear_upload"), "Clear upload", class = "btn-demo"),
              actionButton(ns("fl_run"), "Compare options", class = "btn-demo btn-demo--primary")
            ),
            uiOutput(ns("fleet_status"))
          )
        ),
        tags$section(
          class = "risk-block",
          tags$span(class = "nx-kicker", "Step 2"),
          tags$h2(class = "risk-block__title", "Bid-new vs renew"),
          uiOutput(ns("fl_table")),
          tags$p(class = "footnote", shiny::textOutput(ns("fl_note"), inline = TRUE))
        )
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

    # Forward quota path for the tilt: the quarter containing the latest tender
    # (published months plus forecast for the rest of it), then the next
    # forecast quarters under the selected scenario. simulate_premiums drifts
    # each quarter by the log change between consecutive quarterly totals.
    # Pass scenario_id explicitly — never read input$ in default args.
    quota_path_for <- function(cat, scenario_id = "baseline", n_quarters = 8L) {
      fq <- store$forward_quota
      if (is.null(fq) || !nrow(fq)) return(NULL)
      d <- fq[as.character(fq$category) == cat, ]
      d <- d[order(as.Date(d$month)), ]
      if (!nrow(d)) return(NULL)
      scn <- tryCatch(
        prep_scenario(store, category = cat, preset_id = scenario_id, horizon_quarters = n_quarters + 1L),
        error = function(e) NULL
      )
      # Scenario quarterly totals (full quarters only), keyed by quarter start.
      scn_q <- if (!is.null(scn)) stats::setNames(scn$quarters$quota_scenario, as.character(as.Date(scn$quarters$quarter))) else NULL
      cur_q <- quarter_start(store$latest_bid_month)
      # Current quarter: published months so far + forecast for the remainder.
      b <- store$bidding
      if ("partial" %in% names(b)) b <- b[!b$partial, ]
      pub_cur <- b[as.character(b$category) == cat & is.finite(b$quota) & quarter_start(b$month) == cur_q, ]
      fc_cur <- d[quarter_start(as.Date(d$month)) == cur_q & !(as.Date(d$month) %in% as.Date(pub_cur$month)), ]
      cur_total <- sum(pub_cur$quota, na.rm = TRUE) + sum(fc_cur$mid, na.rm = TRUE)
      if (!is.finite(cur_total) || cur_total <= 0) return(NULL)
      nxt <- numeric()
      q <- cur_q
      for (i in seq_len(n_quarters)) {
        q <- quarter_start(as.Date(q) + 93)
        key <- as.character(q)
        v <- if (!is.null(scn_q) && key %in% names(scn_q)) scn_q[[key]] else {
          dq <- d[quarter_start(as.Date(d$month)) == q, ]
          if (nrow(dq) == 3L) sum(dq$mid) else NA_real_
        }
        if (!is.finite(v)) break
        nxt <- c(nxt, v)
      }
      path <- c(cur_total, nxt)
      if (length(path) < 2L) NULL else as.numeric(path)
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

    price_book_once <- function(b, nsim, knobs, use_progress = TRUE) {
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
        q_path <- if (isTRUE(knobs$tilt)) quota_path_for(cat_code, scenario_id = knobs$tilt_scn) else NULL
        if (isTRUE(knobs$tilt) && is.null(q_path)) {
          stop(
            "Tilt is on but no forward quota path for Cat ", cat_code,
            ". Run scripts/fit_and_forecast.R to build forward_quota.csv."
          )
        }
        sims[[cat_code]] <- funs$simulate_premiums(
          dlog_hist = dlog,
          p0 = p0,
          pqp0 = pqp0,
          start_month = store$latest_bid_month,
          elasticity = elasticity_for(cat_code),
          n_sims = nsim,
          vol_mult = knobs$vol,
          supply_tilt = isTRUE(knobs$tilt),
          quota_path = q_path
        )
      }
      if (isTRUE(use_progress)) {
        incProgress(1 / (length(cats) + 1), detail = "Pricing book")
      }
      base <- funs$dealer_exposure(b, sims, p_success = knobs$p_win)
      shifts <- funs$timing_shift(
        b, sims,
        shifts = c(0, 6, 12),
        p_success = knobs$p_win
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
      p_win <- as.numeric(input$psucc %||% 70) / 100
      if (!is.finite(p_win) || p_win <= 0 || p_win > 1) p_win <- 0.7
      knobs <- list(
        tilt = isTRUE(input$tilt),
        tilt_scn = as.character(input$tilt_scn %||% "baseline"),
        vol = as.numeric(input$vol %||% 1),
        p_win = p_win
      )
      if (!is.finite(knobs$vol) || knobs$vol <= 0) knobs$vol <- 1
      nsim_raw <- if (!is.null(nsim_override)) nsim_override else as.integer(input$nsim)
      nsim <- max(100L, min(1500L, as.integer(nsim_raw %||% 500L)))
      tryCatch({
        if (isTRUE(use_progress)) {
          withProgress(
            message = "Simulating premium paths",
            value = 0,
            price_book_once(b, nsim, knobs, use_progress = TRUE)
          )
        } else {
          price_book_once(b, nsim, knobs, use_progress = FALSE)
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

    # One-shot light auto-price inside a real reactive consumer (onFlushed +
    # isolate() crashes the process — no currentContext there).
    auto_priced <- reactiveVal(FALSE)
    observeEvent(
      {
        list(ready = !is.null(funs) && !is.null(exp_data), book = tryCatch(book(), error = function(e) NULL))
      },
      {
        if (isTRUE(auto_priced()) || !is.null(dl_res())) return()
        b <- tryCatch(book(), error = function(e) NULL)
        if (is.null(b) || !nrow(b)) return()
        auto_priced(TRUE)
        run_dealer(nsim_override = 150L, quiet = TRUE, use_progress = FALSE)
      },
      ignoreInit = FALSE
    )

    output$dl_lead <- renderUI({
      r <- dl_res()
      if (is.null(r)) {
        return(div(
          class = "lead-grid",
          div(class = "lead-cell lead-cell--empty", tags$span(class = "nx-kicker", "Expected cost"), div(class = "lead-num", "-"), tags$p(class = "lead-lab", "Run Step 2")),
          div(class = "lead-cell lead-cell--empty", tags$span(class = "nx-kicker", "Bad-case loss"), div(class = "lead-num", "-"), tags$p(class = "lead-lab", "1-in-20 VaR")),
          div(class = "lead-cell lead-cell--empty", tags$span(class = "nx-kicker", "Chance of loss"), div(class = "lead-num", "-"), tags$p(class = "lead-lab", "% of sims")),
          div(class = "lead-cell lead-cell--empty", tags$span(class = "nx-kicker", "Customer top-up"), div(class = "lead-num lead-num--sm", "-"), tags$p(class = "lead-lab", "Expected"))
        ))
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
          class = "lead-cell",
          tags$span(class = "nx-kicker", "Bad-case loss"),
          div(class = "lead-num", fmt_sgd(s$var95_loss)),
          tags$p(class = "lead-lab", "Exceeded in only 1 of 20 runs")
        ),
        div(
          class = "lead-cell",
          tags$span(class = "nx-kicker", "Chance book loses"),
          div(class = "lead-num", fmt_pct(s$prob_loss)),
          tags$p(class = "lead-lab", "Share of runs with P&L < 0")
        ),
        div(
          class = "lead-cell",
          tags$span(class = "nx-kicker", "Customer top-up"),
          div(class = "lead-num lead-num--sm", fmt_sgd(s$expected_customer_topup)),
          tags$p(class = "lead-lab", "Expected across sims")
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
        e_legend(show = FALSE) |>
        e_grid(left = 48, right = 16, top = 28, bottom = 48, containLabel = FALSE) |>
        e_x_axis(
          name = "Dealer P&L vs quote ($k)",
          nameLocation = "middle",
          nameGap = 28,
          nameTextStyle = list(color = "#a3a3a3", fontSize = 11),
          splitLine = list(show = FALSE),
          axisLine = list(show = FALSE),
          axisTick = list(show = FALSE),
          axisLabel = list(color = "#a3a3a3")
        ) |>
        e_y_axis(
          name = "Count",
          nameLocation = "middle",
          nameGap = 36,
          nameRotate = 90,
          nameTextStyle = list(color = "#a3a3a3", fontSize = 11),
          splitLine = list(show = FALSE),
          axisLine = list(show = FALSE),
          axisTick = list(show = FALSE),
          axisLabel = list(color = "#a3a3a3")
        )
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
        e_legend(bottom = 0, textStyle = list(color = "#a3a3a3")) |>
        e_x_axis(
          splitLine = list(show = FALSE),
          axisLine = list(show = FALSE),
          axisTick = list(show = FALSE),
          axisLabel = list(color = "#a3a3a3")
        ) |>
        e_y_axis(
          splitLine = list(show = FALSE),
          axisLine = list(show = FALSE),
          axisTick = list(show = FALSE),
          axisLabel = list(color = "#a3a3a3")
        )
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
        scn_id <- as.character(input$tilt_scn %||% "baseline")
        cats <- unique(as.character(book()$category))
        n_q <- max(vapply(
          cats,
          function(c) length(quota_path_for(c, scenario_id = scn_id) %||% numeric()),
          integer(1)
        ))
        scn_label <- names(preset_choices())[match(scn_id, preset_choices())]
        sprintf(
          "Supply tilt on · scenario “%s” · %s quarters of outlook applied to cost paths.",
          scn_label %||% "baseline", max(0L, n_q - 1L)
        )
      } else {
        "Supply tilt off · cost paths use historical premium moves only."
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
