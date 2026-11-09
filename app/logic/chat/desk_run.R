# Constrained desk runner for COE bot.
# Uses existing scenario / exposure functions only — no free LLM tooling of the app.

box::use(
  app/logic/viewdata/scenario_viz[prep_scenario, chart_scenario_quota],
  app/logic/model/scenarios[scenario_presets],
  app/logic/exposure/run_pooled[run_pooled_exposure, last_premium],
  app/logic/viewdata/proposal_viz[prep_exposure_waterfall, chart_exposure_waterfall]
)

`%||%` <- function(a, b) {
  if (is.null(a) || !length(a) || (length(a) == 1L && is.na(a))) b else a
}

fmt_int <- function(x) {
  format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_sgd <- function(x) {
  paste0("$", format(round(as.numeric(x)[[1]]), big.mark = ",", scientific = FALSE, trim = TRUE))
}

fmt_pct <- function(x, d = 1) {
  paste0(format(round(100 * as.numeric(x)[[1]], d), nsmall = d), "%")
}

#' @export
desk_cancel_request <- function(q) {
  grepl("^\\s*(cancel|never\\s*mind|nevermind|stop|abort|forget it)\\s*$", q, ignore.case = TRUE)
}

unsupported_category_reply <- function() {
  "This desk covers **Cat A/B only** — not C/D/E or motorcycles. Reply **A** or **B**."
}

is_unsupported_category <- function(q) {
  ql <- tolower(trimws(q))
  grepl("^\\s*cat(?:egory)?\\s*[cde]\\s*$", ql) ||
    grepl("^\\s*[cde]\\s*$", ql) ||
    grepl("\\bcat(?:egory)?\\s*[cde]\\b", ql) ||
    grepl("\\bmotorcycle\\b|\\bcommercial\\b", ql)
}

#' Clarifying question mid slot-fill (keep pending; answer then re-ask).
is_slot_clarify <- function(q) {
  ql <- tolower(q)
  # Do not treat "Is December… published?" as preset help.
  if (grepl("published|forecast|outlook|mape|accur|held-?out|quota for|when .{0,20}bid", ql)) {
    return(FALSE)
  }
  grepl("\\b(means? what|what (does|is|do) (renew|inject|pqp|claw|baseline|preset|\\+?5)|explain (renew|inject|pqp|claw|baseline|preset)|tell me (about|what).{0,20}(renew|inject|pqp|claw|baseline|preset|\\+\\s*5)|how does (renew|inject|pqp|claw|baseline))\\b", ql) ||
    (grepl("\\?\\s*$", ql) && grepl("renew|inject|pqp|claw|baseline|preset|\\+\\s*5", ql) &&
       !grepl("published|forecast|quota for|december|2026", ql))
}

#' Short answers that continue slot-fill (not a topic change).
is_slot_value <- function(q) {
  ql <- tolower(trimws(q))
  if (grepl("^\\s*[ab]\\s*$", ql)) return(TRUE)
  if (grepl("^\\s*\\d{1,3}\\s*$", ql)) return(TRUE)
  if (grepl("^\\s*\\$?\\s*\\d{4,7}\\s*$", ql)) return(TRUE)
  if (!is.null(detect_category(q)) && nchar(ql) < 24) return(TRUE)
  if (!is.null(detect_preset(q)) && !grepl("\\?", ql) && nchar(ql) < 48) return(TRUE)
  if (!is.null(parse_orders(q)) && nchar(ql) < 48) return(TRUE)
  if (!is.null(parse_money(q)) && nchar(ql) < 48 && !grepl("published|forecast", ql)) return(TRUE)
  FALSE
}

#' User abandoned the slot wizard for a new question.
is_topic_switch <- function(q) {
  if (is_slot_value(q) || is_slot_clarify(q) || desk_cancel_request(q)) return(FALSE)
  ql <- tolower(q)
  grepl("\\?\\s*$", ql) ||
    grepl("^(is|are|was|were|what|when|where|why|how|can|could|should|do|does|did)\\b", ql) ||
    grepl("\\b(published|not released|forecast|outlook|mape|held-?out|track record|accur)\\b", ql)
}

preset_help_text <- function(q) {
  ql <- tolower(q)
  if (grepl("renew|\\+\\s*5", ql)) {
    return(paste(
      "**Renewal +5**: renewal share +5 percentage points (lever only).",
      "More renewals → fewer deregistrations → tighter forward quota.",
      "It does not move PQP or inject certificates."
    ))
  }
  if (grepl("inject\\s*750|750", ql)) {
    return("**Inject 750**: LTA adds 750 car COEs per quarter, split A/B by recent published shares.")
  }
  if (grepl("80\\s*k|80000", ql)) {
    return("**PQP 80k**: PQP glides down to $80k; fitted renewal response raises renewals and cuts deregistrations.")
  }
  if (grepl("160\\s*k|160000", ql)) {
    return("**PQP 160k**: PQP glides up to $160k; renewals fall and deregistrations rise.")
  }
  if (grepl("claw", ql)) {
    return("**Clawback**: LTA claws back brought-forward certificates over 8 quarters (negative injection).")
  }
  if (grepl("baseline", ql)) {
    return("**Baseline**: flat PQP, no injection — the reference forward quota path.")
  }
  paste(
    "Presets stress supply only (not premiums):",
    "baseline; inject 750; PQP 80k / 160k; clawback; renewal +5 (renewal share +5 pts)."
  )
}

#' Hard refusals the bot must not run as a desk tool.
#' @export
desk_refuse_reason <- function(q) {
  ql <- tolower(q)
  if (is_unsupported_category(q) &&
        grepl("forecast|quota|outlook|scenario|risk|price|exposure|lose|loss|bid", ql)) {
    return(unsupported_category_reply())
  }
  if (grepl("upload|csv|spreadsheet|excel|fleet schedule|order book file", ql) &&
        grepl("risk|exposure|price|book", ql)) {
    return(paste(
      "I can't ingest uploads in chat.",
      "For a custom CSV book, use the **Your risk** tab.",
      "Here I can price a **pooled** book if you give orders, category (A or B), and quoted premium."
    ))
  }
  if ((grepl("\\bpremium\\b", ql) || grepl("\\bclearing\\b", ql)) &&
        grepl("forecast|predict|next tender|what will|will .{0,40}be|next month|upcoming", ql)) {
    return(paste(
      "We do **not** forecast the next clearing premium.",
      "I can price risk on simulated paths, or show supply scenarios / quota outlook."
    ))
  }
  if (grepl("refit|retrain|clean(ing)? the data|scrape|etl|sql", ql)) {
    return("I don't run cleaning, scrapes, or refits from chat — only live desk artefacts and the Scenarios / Your risk engines.")
  }
  if (grepl("financial advise|financial advice|investment advice", ql) &&
        !grepl("exposure|coe|quota|order|book|var\\b|loss", ql)) {
    return(paste(
      "I don't give general financial advice.",
      "I can price **COE financial exposure** for a pooled Cat A/B book — say **financial exposure** (or give orders, category, and quoted $)."
    ))
  }
  NULL
}

detect_category <- function(q) {
  ql <- tolower(q)
  has_a <- grepl("\\bcat(?:egory)?\\s*a\\b|\\bcategory a\\b|\\bcats?\\s*a\\b", ql)
  has_b <- grepl("\\bcat(?:egory)?\\s*b\\b|\\bcategory b\\b|\\bcats?\\s*b\\b", ql)
  if (has_a && !has_b) return("A")
  if (has_b && !has_a) return("B")
  # Lone "A" / "B" replies during slot fill
  if (grepl("^\\s*[aA]\\s*$", q)) return("A")
  if (grepl("^\\s*[bB]\\s*$", q)) return("B")
  NULL
}

detect_preset <- function(q) {
  ql <- tolower(q)
  if (grepl("inject\\s*750|750\\s*(car\\s*)?(coe|cert)|injection of 750", ql)) return("inject_750")
  if (grepl("claw\\s*back|clawback|brought[- ]forward", ql)) return("clawback")
  if (grepl("80\\s*k|80000|\\$80,?000|pqp.*(fall|drop|down|cheap)", ql)) return("premium_80k")
  if (grepl("160\\s*k|160000|\\$160,?000|pqp.*(rise|up|dear|expensive)", ql)) return("premium_160k")
  if (grepl("renew(\\w*)?\\s*\\+?\\s*5|\\+5\\s*(pt|point|renew)|renewal.*\\+\\s*5", ql)) return("renew_plus5")
  if (grepl("\\bbaseline\\b|no injection|flat pqp", ql)) return("baseline")
  NULL
}

parse_orders <- function(q) {
  # "40 cars", "20 Cat A vehicles", "30 Cat A orders", "orders: 25", bare number during slot fill
  m <- regmatches(
    q,
    regexpr("(?i)\\b(\\d{1,3})\\b(?:\\s+cat(?:egory)?\\s*[ab])?\\s*(cars?|orders?|units?|vehicles?)\\b", q, perl = TRUE)
  )
  if (length(m) && nzchar(m)) {
    n <- suppressWarnings(as.integer(sub(".*?(\\d{1,3}).*", "\\1", m, perl = TRUE)))
    if (is.finite(n) && n >= 1L && n <= 200L) return(n)
  }
  # "bid for 20 Cat A" / "for 20 Cat A vehicles"
  m_for <- regmatches(
    q,
    regexpr("(?i)\\b(?:bid\\s+for|for)\\s+(\\d{1,3})\\s+cat(?:egory)?\\s*[ab]\\b", q, perl = TRUE)
  )
  if (length(m_for) && nzchar(m_for)) {
    n <- suppressWarnings(as.integer(sub(".*?(\\d{1,3}).*", "\\1", m_for, perl = TRUE)))
    if (is.finite(n) && n >= 1L && n <= 200L) return(n)
  }
  m2 <- regmatches(q, regexpr("(?i)\\b(orders?|cars?|vehicles?|book)\\s*(of|=|:)?\\s*(\\d{1,3})\\b", q, perl = TRUE))
  if (length(m2) && nzchar(m2)) {
    n <- suppressWarnings(as.integer(gsub("[^0-9]", "", m2)))
    if (is.finite(n) && n >= 1L && n <= 200L) return(n)
  }
  if (grepl("^\\s*\\d{1,3}\\s*$", q)) {
    n <- suppressWarnings(as.integer(trimws(q)))
    if (is.finite(n) && n >= 1L && n <= 200L) return(n)
  }
  NULL
}

parse_money <- function(q) {
  ql <- tolower(gsub(",", "", q))
  # $135k / 135k / 135000 / quoted 130000
  m <- regmatches(ql, regexpr("\\$?\\s*(\\d+(?:\\.\\d+)?)\\s*k\\b", ql, perl = TRUE))
  if (length(m) && nzchar(m)) {
    v <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", m)))
    if (is.finite(v)) return(round(v * 1000))
  }
  m2 <- regmatches(ql, regexpr("(?:quoted|quote|premium|at)\\s*\\$?\\s*(\\d{4,7})\\b", ql, perl = TRUE))
  if (length(m2) && nzchar(m2)) {
    v <- suppressWarnings(as.numeric(gsub("[^0-9]", "", m2)))
    if (is.finite(v) && v >= 1000) return(v)
  }
  if (grepl("^\\s*\\$?\\s*\\d{4,7}\\s*$", ql)) {
    v <- suppressWarnings(as.numeric(gsub("[^0-9]", "", ql)))
    if (is.finite(v) && v >= 1000) return(v)
  }
  NULL
}

renewal_shift_pts <- function(q) {
  m <- regmatches(q, regexpr("(?i)renew(?:al)?[^0-9\\-+]*([+-]?\\s*\\d+(?:\\.\\d+)?)\\s*(pt|point|%)?", q, perl = TRUE))
  if (!length(m) || !nzchar(m)) return(NULL)
  v <- suppressWarnings(as.numeric(gsub("[^0-9.+-]", "", m)))
  if (!is.finite(v)) return(NULL)
  # if user said +5 assume points; if |v|<1 assume share already
  if (abs(v) <= 1) v else v / 100
}

injection_extra <- function(q) {
  m <- regmatches(q, regexpr("(?i)inject(?:ion)?[^0-9]*(\\d{2,4})\\b", q, perl = TRUE))
  if (!length(m) || !nzchar(m)) return(NULL)
  v <- suppressWarnings(as.integer(gsub("[^0-9]", "", m)))
  if (is.finite(v) && v > 0 && v != 750L) return(v) # 750 maps to preset
  NULL
}

#' Classify desk intent from text (rules only).
#' @export
detect_desk_intent <- function(q, pending = NULL) {
  if (!is.null(pending) && !is.null(pending$kind)) {
    if (desk_cancel_request(q)) return("cancel")
    ql <- tolower(q)
    if (is_slot_clarify(q)) return("clarify")
    if (is_topic_switch(q)) return("chat")
    if (identical(pending$kind, "scenario") &&
          grepl("risk|exposure|price (the |my )?book|var\\b|cvar|\\blose\\b|\\bloose\\b|\\bloss\\b", ql) &&
          !grepl("inject|pqp|renew|claw", ql)) {
      return("exposure")
    }
    if (identical(pending$kind, "exposure") && grepl("scenario|inject|clawback|pqp (to|at)|renewal \\+", ql) &&
          !grepl("risk|exposure|price|lose|loose|loss", ql)) {
      return("scenario")
    }
    return(pending$kind)
  }

  refuse <- desk_refuse_reason(q)
  if (!is.null(refuse)) return("refuse")

  ql <- tolower(q)
  # Explanatory questions stay with the live model — never canned desk replies.
  if (grepl("^(what does|what is|how does|how do|explain|tell me about)\\b", ql) &&
        !grepl("\\brun\\b|inject|price \\d|orders? at", ql)) {
    return("chat")
  }
  # Money loss / VaR on a book → exposure (even if user says “scenarios”).
  if (grepl("\\b(lose|loose|loss|var95|cvar|underwater|how much .{0,40}(lose|loose|loss|cost))\\b", ql) ||
        grepl("\\b(calculate|compute|price)\\b.{0,40}\\b(lose|loose|loss|risk|exposure|var)\\b", ql)) {
    return("exposure")
  }
  # Pooled exposure in-chat (slot-fill). CSV uploads stay on the Your risk tab.
  has_book_units <- grepl("\\b(orders?|cars?|vehicles?|units?)\\b", ql) ||
    grepl("\\bbid for\\s+\\d{1,3}\\s+cat", ql)
  has_quote <- grepl("\\b(quote|quoted|premium)\\b", ql) ||
    grepl("(?:quoted|quote|at)\\s*\\$?\\s*\\d", ql) ||
    grepl("\\$?\\s*\\d+(?:\\.\\d+)?\\s*k\\b", ql)
  bid_timing_q <- grepl("\\bbid\\b", ql) &&
    grepl("this month|next month|now or|or wait|or the next|should i (bid|wait)", ql)
  if (grepl("\\b(price|price my|var95|bad-?case|pooled book)\\b", ql) ||
        (has_book_units && has_quote) ||
        (bid_timing_q && has_book_units && has_quote) ||
        grepl("\\b(financial\\s+)?exposure\\b", ql) ||
        grepl("\\b(get|show|give|need|want|run|price|calculate|compute)\\b.{0,48}\\b(exposure|var\\b|cvar|dealer (pnl|p&l)|book risk)\\b", ql) ||
        grepl("\\b(your risk)\\b", ql) && grepl("\\b(price|run|simulate|for \\d|orders?|here|in chat|in (the )?bot)\\b", ql)) {
    return("exposure")
  }
  if (grepl("scenario|inject|clawback|stress (renew|quota|supply)|pqp (to|at|falls|rises)|what if .{0,40}(renew|inject|pqp|quota)", ql) ||
        (!is.null(detect_preset(q)) && grepl("run|show|apply|compare|quota|dereg", ql))) {
    return("scenario")
  }
  "chat"
}

empty_scenario_slots <- function() {
  list(category = NULL, preset_id = NULL, renewal_shift = 0, injection_per_qtr = 0)
}

empty_exposure_slots <- function() {
  list(n_cars = NULL, category = NULL, quoted_premium = NULL)
}

#' Merge parsed fields from one user utterance into slots.
#' @export
extract_slots <- function(kind, q, slots = NULL) {
  if (identical(kind, "scenario")) {
    slots <- if (is.null(slots)) empty_scenario_slots() else slots
    cat <- detect_category(q)
    if (!is.null(cat)) slots$category <- cat
    pre <- detect_preset(q)
    if (!is.null(pre)) slots$preset_id <- pre
    rs <- renewal_shift_pts(q)
    if (!is.null(rs) && is.null(detect_preset(q))) slots$renewal_shift <- rs
    # if renew_plus5 preset, shift stays in preset
    inj <- injection_extra(q)
    if (!is.null(inj)) {
      slots$injection_per_qtr <- inj
      if (is.null(slots$preset_id)) slots$preset_id <- "baseline"
    }
    return(slots)
  }
  if (identical(kind, "exposure")) {
    slots <- if (is.null(slots)) empty_exposure_slots() else slots
    cat <- detect_category(q)
    if (!is.null(cat)) slots$category <- cat
    n <- parse_orders(q)
    if (!is.null(n)) slots$n_cars <- n
    money <- parse_money(q)
    if (!is.null(money)) slots$quoted_premium <- money
    return(slots)
  }
  slots
}

missing_slots <- function(kind, slots) {
  if (identical(kind, "scenario")) {
    miss <- character()
    if (is.null(slots$category)) miss <- c(miss, "category")
    if (is.null(slots$preset_id) && !(is.finite(slots$injection_per_qtr) && slots$injection_per_qtr != 0)) {
      # allow baseline if they said "scenario" vaguely — still need a preset choice
      miss <- c(miss, "preset_id")
    }
    return(miss)
  }
  if (identical(kind, "exposure")) {
    miss <- character()
    if (is.null(slots$n_cars)) miss <- c(miss, "n_cars")
    if (is.null(slots$category)) miss <- c(miss, "category")
    if (is.null(slots$quoted_premium)) miss <- c(miss, "quoted_premium")
    return(miss)
  }
  character()
}

#' Prompt asking only for the next compulsory field.
#' @export
slot_prompt <- function(kind, missing, store = NULL, slots = NULL) {
  need <- missing[[1]]
  if (identical(kind, "scenario")) {
    if (identical(need, "category")) {
      return("Which category should I run — **A** or **B**?")
    }
    if (identical(need, "preset_id")) {
      return(paste(
        "Which scenario preset?",
        "Reply with one of: **baseline**, **inject 750**, **PQP 80k**, **PQP 160k**, **clawback**, or **renewal +5**."
      ))
    }
  }
  if (identical(kind, "exposure")) {
    if (identical(need, "n_cars")) {
      return("How many **orders** (cars) in the pooled book? (1–200)")
    }
    if (identical(need, "category")) {
      return("Category **A** or **B**? (Pooled chat pricing is one category at a time.)")
    }
    if (identical(need, "quoted_premium")) {
      hint <- ""
      cat <- slots$category %||% "A"
      if (!is.null(store)) {
        lp <- tryCatch(last_premium(store$bidding, cat), error = function(e) NA_real_)
        if (is.finite(lp)) {
          hint <- sprintf(" Latest Cat %s clearing is about **%s** — you can use that or another quote.", cat, fmt_sgd(lp))
        }
      }
      return(paste0("What **quoted premium ($)** should I use?", hint))
    }
  }
  "I need one more detail to run that on the desk."
}

#' Compact engine facts for the live model — not a user-facing canned answer.
desk_scenario_facts <- function(prep) {
  q <- prep$quarters
  rows <- apply(utils::head(q, 8L), 1L, function(r) {
    sprintf(
      "%s baseline=%s scenario=%s delta=%s",
      r[["label"]],
      fmt_int(r[["quota_base"]]),
      fmt_int(r[["quota_scenario"]]),
      fmt_int(r[["delta"]])
    )
  })
  paste(
    c(
      "# DESK RUN FACTS (authoritative — quote these numbers; do not invent)",
      sprintf(
        "kind=scenario | category=%s | preset=%s | renewal_shift_pts=%s | injection_per_qtr=%s",
        prep$category,
        prep$preset$label,
        format(round(100 * as.numeric(prep$renewal_shift), 1), trim = TRUE),
        fmt_int(prep$injection_per_qtr)
      ),
      "note=Injection presets split A/B by recent published shares.",
      "quarterly_quota:",
      rows
    ),
    collapse = "\n"
  )
}

desk_exposure_facts <- function(res) {
  s <- res$base$summary
  sh <- as.data.frame(res$shifts)
  timing_lines <- character()
  if (nrow(sh)) {
    timing_lines <- apply(sh, 1L, function(r) {
      sprintf(
        "+%sQ Expected P&L=%s | Bad-case loss=%s",
        as.character(r[["shift_quarters"]]),
        fmt_sgd(r[["expected_dealer_pnl"]]),
        fmt_sgd(r[["var95_loss"]])
      )
    })
  }
  pl <- suppressWarnings(as.numeric(s$prob_loss[[1]]))
  chance_read <- if (is.finite(pl) && pl >= 0.65) {
    "Chance book loses reading=well above 50% — quote looks cheap vs simulated paths (P&L underwater often); not a tender win-rate."
  } else if (is.finite(pl) && pl <= 0.35) {
    "Chance book loses reading=well below 50% — margin built in vs simulated paths; still check Bad-case loss for cushion; not a tender win-rate."
  } else if (is.finite(pl)) {
    "Chance book loses reading=near 50% — quote near the model’s view / little P&L edge; carrying premium risk; not a tender win-rate."
  } else {
    "Chance book loses = share of sims with dealer P&L < 0 (frequency, not tender win-rate)."
  }
  # Labels mirror Your risk tab lead strip exactly.
  paste(
    c(
      "# DESK RUN FACTS (authoritative — same metrics as Your risk tab; do not invent)",
      "ui_parity=Your risk dealer book",
      sprintf(
        "book=%s orders | Cat %s | quoted=%s | win_rate=%s | vol=%sx | supply_tilt=%s | tilt_scenario=%s | sims=%s",
        fmt_int(res$n_orders),
        res$category,
        fmt_sgd(res$quoted_premium),
        fmt_pct(res$p_win, 0),
        format(res$vol, trim = TRUE),
        if (isTRUE(res$tilt)) "on" else "off",
        res$tilt_scn,
        fmt_int(res$nsim)
      ),
      sprintf("Expected COE cost=%s", fmt_sgd(s$expected_cost)),
      sprintf("Bad-case loss=%s (1-in-20 VaR; exceeded in only 1 of 20 runs)", fmt_sgd(s$var95_loss)),
      sprintf("Chance book loses=%s (share of runs with dealer P&L < 0 — frequency, not amount)", fmt_pct(s$prob_loss)),
      chance_read,
      sprintf("Customer top-up=%s (expected across sims; guaranteed books are usually $0)", fmt_sgd(s$expected_customer_topup)),
      "instruction=Use ONLY these Your risk labels. If you explain Chance book loses, use the reading line (P&L frequency). No tender win-rate. No bid/wait/should. Close: risk appetite.",
      "If you delay +1Q / +2Q (same paths as the delay chart):",
      timing_lines
    ),
    collapse = "\n"
  )
}

#' Execute a completed desk run. Returns list(ok, facts, viz) — facts feed the LLM.
#' @export
execute_desk <- function(kind, slots, store) {
  if (identical(kind, "scenario")) {
    preset_id <- slots$preset_id %||% "baseline"
    prep <- tryCatch(
      prep_scenario(
        store,
        category = slots$category,
        preset_id = preset_id,
        renewal_shift = as.numeric(slots$renewal_shift %||% 0),
        injection_per_qtr = as.numeric(slots$injection_per_qtr %||% 0),
        horizon_quarters = 12L
      ),
      error = function(e) NULL
    )
    if (is.null(prep) || is.null(prep$quarters) || !nrow(prep$quarters)) {
      return(list(ok = FALSE, facts = NULL, error = "Could not build that scenario from the live forward path.", viz = NULL))
    }
    return(list(
      ok = TRUE,
      facts = desk_scenario_facts(prep),
      viz = list(type = "scenario", prep = prep)
    ))
  }
  if (identical(kind, "exposure")) {
    res <- tryCatch(
      run_pooled_exposure(
        store,
        n_cars = slots$n_cars,
        category = slots$category,
        quoted_premium = slots$quoted_premium,
        nsim = 200L
      ),
      error = function(e) list(.err = conditionMessage(e))
    )
    if (!is.null(res$.err)) {
      return(list(ok = FALSE, facts = NULL, error = paste("Could not price the book:", res$.err), viz = NULL))
    }
    wf <- tryCatch(
      prep_exposure_waterfall(res$base$summary, res$shifts, res$quoted_total),
      error = function(e) NULL
    )
    return(list(
      ok = TRUE,
      facts = desk_exposure_facts(res),
      viz = list(type = "exposure", result = res, waterfall = wf)
    ))
  }
  list(ok = FALSE, facts = NULL, error = "Unknown desk action.", viz = NULL)
}

#' One shot: interpret message under optional pending slots.
#' @export
handle_desk_turn <- function(q, store, pending = NULL) {
  q <- trimws(as.character(q %||% ""))
  if (!nzchar(q)) {
    return(list(action = "ignore"))
  }

  if (desk_cancel_request(q) && !is.null(pending)) {
    return(list(
      action = "reply",
      clear_pending = TRUE,
      text = "Cancelled — nothing run. Ask a forecast question, a scenario, or a pooled risk price anytime.",
      viz = NULL
    ))
  }

  # Mid-slot: unsupported Cat C/D/E — keep pending and re-ask.
  if (!is.null(pending) && is_unsupported_category(q)) {
    miss <- missing_slots(pending$kind, pending$slots %||% list())
    if (!length(miss)) miss <- "category"
    return(list(
      action = "reply",
      pending = pending,
      text = paste(
        unsupported_category_reply(),
        slot_prompt(pending$kind, miss, store = store, slots = pending$slots),
        sep = "\n\n"
      ),
      viz = NULL
    ))
  }

  # Mid-slot clarifying question — answer briefly, keep slots, re-ask.
  if (!is.null(pending) && is_slot_clarify(q)) {
    miss <- missing_slots(pending$kind, pending$slots %||% list())
    if (!length(miss)) {
      miss <- if (identical(pending$kind, "scenario")) "preset_id" else "n_cars"
    }
    return(list(
      action = "reply",
      pending = pending,
      text = paste(
        preset_help_text(q),
        slot_prompt(pending$kind, miss, store = store, slots = pending$slots),
        sep = "\n\n"
      ),
      viz = NULL
    ))
  }

  # Mid-slot topic change (e.g. "Is Dec 2026 published?") — drop wizard, answer in chat.
  if (!is.null(pending) && is_topic_switch(q)) {
    return(list(action = "chat", clear_pending = TRUE))
  }

  refuse <- desk_refuse_reason(q)
  intent <- detect_desk_intent(q, pending)

  if (identical(intent, "refuse")) {
    return(list(
      action = "reply",
      clear_pending = TRUE,
      text = refuse %||% "I can't run that from chat.",
      viz = NULL
    ))
  }

  if (identical(intent, "chat") || identical(intent, "clarify")) {
    return(list(
      action = "chat",
      clear_pending = isTRUE(!is.null(pending) && is_topic_switch(q))
    ))
  }

  kind <- intent
  slots <- if (!is.null(pending) && identical(pending$kind, kind)) pending$slots else {
    if (identical(kind, "scenario")) empty_scenario_slots() else empty_exposure_slots()
  }
  slots <- extract_slots(kind, q, slots)
  # Default vague "run a scenario" toward baseline once category known? No — ask preset.
  miss <- missing_slots(kind, slots)
  if (length(miss)) {
    return(list(
      action = "reply",
      pending = list(kind = kind, slots = slots),
      text = slot_prompt(kind, miss, store = store, slots = slots),
      viz = NULL
    ))
  }

  out <- execute_desk(kind, slots, store)
  if (!isTRUE(out$ok)) {
    return(list(
      action = "reply",
      clear_pending = TRUE,
      text = out$error %||% "Desk run failed.",
      viz = NULL,
      ok = FALSE
    ))
  }
  list(
    action = "generate",
    clear_pending = TRUE,
    facts = out$facts,
    viz = out$viz,
    ok = TRUE
  )
}

#' Chart object for the latest desk viz (or NULL).
#' @export
desk_viz_chart <- function(viz) {
  if (is.null(viz) || is.null(viz$type)) return(NULL)
  box::use(
    echarts4r[e_charts, e_bar, e_line, e_tooltip, e_color, e_legend, e_grid, e_x_axis, e_y_axis]
  )
  if (identical(viz$type, "scenario")) {
    # Prefer the shared Scenarios chart; fall back to a minimal line if datazoom fails.
    ch <- tryCatch(chart_scenario_quota(viz$prep), error = function(e) NULL)
    if (!is.null(ch)) return(ch)
    d <- as.data.frame(viz$prep$quarters, stringsAsFactors = FALSE)
    if (!nrow(d)) return(NULL)
    d$label <- as.character(d$label)
    d$quota_base <- suppressWarnings(as.numeric(d$quota_base))
    d$quota_scenario <- suppressWarnings(as.numeric(d$quota_scenario))
    return(
      d |>
        e_charts(label) |>
        e_line(quota_base, name = "Baseline quota", symbol = "none") |>
        e_line(quota_scenario, name = "Scenario quota", symbol = "circle", symbolSize = 5) |>
        e_color(c("#d4d4d4", "#ff5c1a")) |>
        e_tooltip(trigger = "axis") |>
        e_legend(right = 8, top = 4) |>
        e_grid(left = 12, right = 16, top = 48, bottom = 16, containLabel = TRUE)
    )
  }
  if (identical(viz$type, "exposure") && !is.null(viz$waterfall)) {
    ch <- tryCatch(chart_exposure_waterfall(viz$waterfall), error = function(e) NULL)
    if (!is.null(ch)) return(ch)
  }
  if (identical(viz$type, "exposure")) {
    r <- viz$result
    pnl <- as.numeric(r$base$portfolio$dealer_pnl) / 1000
    h <- graphics::hist(pnl, breaks = 24, plot = FALSE)
    df <- data.frame(mid = as.numeric(h$mids), n = as.numeric(h$counts))
    return(
      df |>
        e_charts(mid) |>
        e_bar(n, name = "Simulations") |>
        e_tooltip() |>
        e_color("#ff5c1a") |>
        e_legend(show = FALSE) |>
        e_grid(left = 48, right = 16, top = 28, bottom = 48) |>
        e_x_axis(name = "Dealer P&L vs quote ($k)", nameLocation = "middle", nameGap = 28) |>
        e_y_axis(name = "Count", nameLocation = "middle", nameGap = 36)
    )
  }
  NULL
}

#' @export
desk_suggestion_chips <- function() {
  c(
    "What is the Cat A quota forecast?",
    "Run inject 750 on Cat A",
    "Price 30 Cat A orders at $130k",
    "How accurate is the model?",
    "PQP to 80k for Cat B",
    "What does Your risk do?"
  )
}
