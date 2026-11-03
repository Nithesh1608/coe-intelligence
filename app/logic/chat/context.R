# Build grounded context for COE bot from a tight brief + live artefacts.
# Smart packing: always ship core desk facts; attach denser blocks only when
# the user question (or chips) points at that part of the app.

`%||%` <- function(a, b) {
  if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
}

clip <- function(x, n = 48000L) {
  x <- paste(x, collapse = "\n")
  if (nchar(x) <= n) return(x)
  paste0(substr(x, 1L, n), "\n\n[…truncated…]")
}

fmt_int <- function(x) {
  format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
}

friendly_model_name <- function(model, label = NULL) {
  m <- as.character(model %||% "")
  lab <- as.character(label %||% "")
  if (identical(m, "core_forecast") || grepl("elastic", lab, ignore.case = TRUE)) {
    return("Elastic net")
  }
  if (identical(m, "simple_average") || grepl("average|baseline", lab, ignore.case = TRUE)) {
    return("12-month average (baseline)")
  }
  if (nzchar(lab)) lab else m
}

fmt_pct_score <- function(x, d = 1) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || !is.finite(x[[1]])) return("-")
  paste0(format(round(100 * x[[1]], d), nsmall = d, trim = TRUE), "%")
}

fmt_scores <- function(df, title) {
  if (is.null(df) || !nrow(df)) return("")
  lines <- apply(df, 1L, function(r) {
    nm <- friendly_model_name(r[["model"]], r[["label"]])
    cat <- as.character(r[["category"]] %||% "")
    bits <- c(sprintf("model=%s", nm), sprintf("category=%s", cat))
    if ("MAE" %in% names(r) && !is.na(r[["MAE"]])) {
      bits <- c(bits, sprintf("MAE=%s", format(round(as.numeric(r[["MAE"]]), 1), trim = TRUE)))
    }
    if ("MAPE" %in% names(r) && !is.na(r[["MAPE"]])) {
      bits <- c(bits, sprintf("MAPE=%s", fmt_pct_score(r[["MAPE"]])))
    }
    if ("within_10pct" %in% names(r) && !is.na(r[["within_10pct"]])) {
      bits <- c(bits, sprintf("within_10pct=%s", fmt_pct_score(r[["within_10pct"]], 0)))
    }
    if ("within_15pct" %in% names(r) && !is.na(r[["within_15pct"]])) {
      bits <- c(bits, sprintf("within_15pct=%s", fmt_pct_score(r[["within_15pct"]], 0)))
    }
    paste(bits, collapse = " | ")
  })
  paste(c(paste0("## ", title), lines), collapse = "\n")
}

#' @export
system_prompt <- function() {
  paste(
    "You are COE bot for COE Intelligence — a Singapore COE supply forecast and exposure desk.",
    "Answer ONLY from PROJECT CONTEXT below. Be precise, useful, and compact.",
    "Parity rule: anything the user can do in the app tabs (Home, Scenarios, Market, Track record, Your risk, About),",
    "you should be able to explain, quote numbers for, or walk through — using the live artefacts in context.",
    "Do NOT invent numbers. Do NOT dive into data-cleaning pipelines, scrape scripts, or ETL unless asked.",
    "No fluff, no marketing, no AWS/NVIDIA/API/key talk, no inventing metrics or dates.",
    "Core product forecasts deregistrations / quota supply — not clearing premiums.",
    "NAMING: Call the live model “elastic net” (alpha=0.5, lambda chosen by the 1-SE rule).",
    "Never say core_forecast, simple_average, or other internal artefact ids to the user.",
    "The baseline comparator is the “12-month average”.",
    "MAPE and hit-rates in context are already percentages — quote them as 11.9%, not 0.119.",
    "Do not use LaTeX or $...$ math. Write plain text: alpha=0.5, lambda 1-SE rule.",
    "When asked for a past held-out month (e.g. Jan 2025), quote published and outlook from the held-out monthly table.",
    "When asked for the live forward outlook, quote Forward monthly mid / lo80 / hi80 (or quarterly sums) exactly.",
    "For Scenarios / pooled Your risk runs: a separate desk engine may already have answered with live tables and a chart.",
    "You only handle ordinary Q&A from PROJECT CONTEXT — forecasts, MAPE, how tabs work, Market explanations.",
    "Never invent scenario deltas, VaR, or book P&L. Never claim you uploaded a CSV or refit a model.",
    "For Market: summarise what the charts show without fabricating series.",
    "Prefer short markdown tables. FORMAT: GitHub-flavoured markdown with ## headings for multi-part answers.",
    "Short paragraphs, bullets, tables. Aim under ~320 words unless the user asks for depth.",
    "Always finish the full answer. Keep numbers exact. If not in context, say so briefly and name the tab that has it.",
    "Never add closing offers like “let me know if you need anything” or “feel free to ask”.",
    sep = " "
  )
}

#' Tight static product facts for the system prompt.
compact_brief <- function() {
  paste(
    c(
      "## Product",
      "DBA3702 COE supply desk (Rhino/Shiny). Forecasts Cat A/B deregistrations and implied quota ~36 months with bootstrap bands. Does not forecast premiums. Your risk prices a locked dealer book / fleet schedule on simulated premium paths (optional supply tilt from forward quota).",
      "Tabs: Home, Scenarios, Market, Track record, Your risk, COE bot, About.",
      "",
      "## App capability map (parity)",
      "- Home: desk overview and navigation into forecast / risk.",
      "- Scenarios: stress renewals (+pts), PQP glide, LTA injection or clawback; compare baseline vs scenario quota and deregistrations for Cat A/B/A+B over ~12 quarters. Live levers — no model refit.",
      "- Market: interactive echarts gallery — premium history, within-quarter swings, Cat A–B spread & volume shares, cohort flow / renewals, owner age stock, bids received vs success, supply↔price, top-up / risk views. Context for packaging, not a cleaning tutorial.",
      "- Track record: held-out exits and quota vs published releases; MAPE / hit-rates for elastic net vs 12-month average.",
      "- Your risk: block-bootstrap premium paths, optional supply tilt from a Scenarios preset, VaR/CVaR and +1Q/+2Q timing; user uploads a book or uses templates.",
      "- About: product framing and limits (supply desk, not premium oracle).",
      "",
      "## Model",
      "Live supply model: elastic net (mixing alpha=0.5, lambda by 1-SE rule) on SingStat VQS deregistrations.",
      "Internal artefact id core_forecast = this elastic net — never show that id to users.",
      "Features before month t: due10/due15/due_near, recent12, y_lag2, y_lag12, reval5 lags, lpqp, post_2022, seasonality.",
      "Quota mapping: quota = alpha_map × trailing-12m deregistrations (alpha_map fitted on train).",
      "Held-out quota MAPE: about 11.9% Cat A / 11.6% Cat B vs 12-month-average baseline.",
      "Train 2018-01…2024-08; held-out from 2024-09 (24 months).",
      "",
      "## Scenarios / risk (summary)",
      "Scenarios stress renewals / quota injections / premium→renewal. Your risk: block-bootstrap premium paths, VaR/CVaR, timing +1Q/+2Q."
    ),
    collapse = "\n"
  )
}

scenario_presets_block <- function(store = NULL) {
  box::use(app/logic/model/scenarios[scenario_presets])
  claw <- NA_real_
  if (!is.null(store)) {
    claw <- suppressWarnings(as.numeric(store$meta$spec$brought_forward_since_2023 %||% NA))
  }
  presets <- tryCatch(scenario_presets(claw), error = function(e) NULL)
  if (is.null(presets) || !length(presets)) return("")
  lines <- vapply(presets, function(p) {
    inj <- p$injection_per_qtr
    inj_s <- if (is.finite(inj) && inj != 0) {
      sprintf("injection_per_qtr=%s", format(round(inj), trim = TRUE))
    } else {
      "injection_per_qtr=0"
    }
    pqp_s <- if (is.finite(p$pqp_target)) {
      sprintf("pqp_target=%s", format(round(p$pqp_target), trim = TRUE))
    } else {
      "pqp_target=flat(last)"
    }
    ren_s <- sprintf("renewal_shift=%s pts", format(round(100 * as.numeric(p$renewal_shift %||% 0), 1), trim = TRUE))
    sprintf("- %s [%s]: %s; %s; %s. %s", p$label, p$id, pqp_s, inj_s, ren_s, p$note %||% "")
  }, character(1))
  paste(c("## Scenario presets (same as Scenarios tab)", lines), collapse = "\n")
}

alphas_block <- function(store) {
  a <- store$alphas
  if (is.null(a) || !nrow(a)) return("")
  lines <- apply(a, 1L, function(r) {
    sprintf(
      "Cat %s alpha_map=%s",
      as.character(r[["category"]]),
      format(round(as.numeric(r[["alpha"]]), 4), nsmall = 4, trim = TRUE)
    )
  })
  paste(c("## Quota calibration alphas", lines), collapse = "\n")
}

recent_market_block <- function(store, n_months = 8L) {
  b <- store$bidding
  if (is.null(b) || !nrow(b)) return("")
  b <- b[as.character(b$category) %in% c("A", "B") & is.finite(b$premium), , drop = FALSE]
  if (!nrow(b)) return("")
  b$month <- as.Date(b$month)
  cut <- max(b$month, na.rm = TRUE) - (as.integer(n_months) * 31)
  b <- b[b$month >= cut, , drop = FALSE]
  b <- b[order(b$month, b$category), , drop = FALSE]
  lines <- apply(b, 1L, function(r) {
    q <- if ("quota" %in% names(r) && is.finite(suppressWarnings(as.numeric(r[["quota"]])))) {
      sprintf(" quota=%s", fmt_int(r[["quota"]]))
    } else {
      ""
    }
    sprintf(
      "%s Cat %s premium=%s%s",
      format(as.Date(r[["month"]]), "%Y-%m"),
      as.character(r[["category"]]),
      fmt_int(r[["premium"]]),
      q
    )
  })
  paste(
    c(
      "## Recent tender snapshot (Market / Home numbers)",
      "Clearing premiums and published quota from the bidding store — not a premium forecast.",
      lines
    ),
    collapse = "\n"
  )
}

market_topics_block <- function() {
  paste(
    c(
      "## Market tab topics (what the charts cover)",
      "- Premium history across Cat A/B",
      "- Largest within-quarter premium swing (package risk between tenders)",
      "- Cat A–B spread and volume / bid shares",
      "- Cohort flow and renewals / due stock",
      "- Owner age structure",
      "- Bids received vs success",
      "- Supply versus price overlays",
      "- Top-up / residual risk style views",
      "Use recent tender snapshot + forward quota when quoting levels; describe chart roles when the user asks what Market shows."
    ),
    collapse = "\n"
  )
}

intent_flags <- function(question = NULL) {
  q <- tolower(trimws(as.character(question %||% "")))
  if (!nzchar(q)) {
    return(list(
      scenarios = TRUE, market = TRUE, risk = TRUE, track = TRUE, all = TRUE
    ))
  }
  list(
    scenarios = grepl("scenario|inject|clawback|renewal|pqp|lever|policy|stress", q),
    market = grepl("market|premium|tender|bid|spread|cohort|swing|gallery|chart|history", q),
    risk = grepl("risk|exposure|var|cvar|book|fleet|dealer|timing", q),
    track = grepl("track|accur|mape|held-?out|backtest|miss|hit rate|performance", q),
    all = grepl("everything|overview|what can|help| parity|whole app|all tabs", q)
  )
}

#' @export
build_project_context <- function(store = NULL, root = NULL, question = NULL) {
  box::use(
    app/logic/chat/desk_answers[forecast_context_block, heldout_quota_context_block]
  )

  flags <- intent_flags(question)
  # Always include core score + forward path — that is the desk's spine.
  score_bits <- character()
  if (!is.null(store)) {
    score_bits <- c(
      fmt_scores(store$two_quota_scores, "Live quota scores (held-out)"),
      fmt_scores(store$two_hits, "Deregistration hit rates (held-out)"),
      if (!is.null(store$meta$train_end)) {
        sprintf(
          paste0(
            "## Fit window\n",
            "train_end=%s | test_start=%s | live_model=Elastic net | ",
            "lambda_rule=1-SE | latest_bid_month=%s | latest_dereg_month=%s"
          ),
          as.character(store$meta$train_end),
          as.character(store$meta$test_start),
          as.character(store$latest_bid_month %||% NA),
          as.character(store$latest_dereg_month %||% NA)
        )
      },
      alphas_block(store),
      heldout_quota_context_block(store),
      forecast_context_block(store)
    )
  }

  extras <- character()
  want_scen <- isTRUE(flags$all) || isTRUE(flags$scenarios) || is.null(question)
  want_mkt <- isTRUE(flags$all) || isTRUE(flags$market) || is.null(question)
  want_risk <- isTRUE(flags$all) || isTRUE(flags$risk) || is.null(question)

  if (want_scen) extras <- c(extras, scenario_presets_block(store))
  if (want_mkt) extras <- c(extras, market_topics_block(), recent_market_block(store))
  if (want_risk) {
    extras <- c(
      extras,
      paste(
        c(
          "## Your risk (exposure) — same as the tab",
          "Paths: block-bootstrap of historical A/B tender-to-tender premium moves.",
          "Optional supply tilt from Scenarios preset forward quota.",
          "Outputs: expected loss, percentiles, VaR / CVaR, timing shifts (+1Q / +2Q).",
          "Personalised book P&L needs an uploaded schedule in the Your risk tab — bot explains mechanics and cites shared supply numbers only."
        ),
        collapse = "\n"
      )
    )
  }

  # First turn / null question: keep extras but clip harder if huge.
  clip_n <- if (is.null(question) || !nzchar(trimws(question))) 36000L else 48000L

  clip(paste(
    c(
      "# PROJECT CONTEXT",
      compact_brief(),
      paste(score_bits[nzchar(score_bits)], collapse = "\n\n"),
      paste(extras[nzchar(extras)], collapse = "\n\n")
    ),
    collapse = "\n\n"
  ), n = clip_n)
}
