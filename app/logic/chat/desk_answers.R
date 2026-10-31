# Instant desk answers from live artefacts — no network. Used by COE bot first.

`%||%` <- function(a, b) {
  if (is.null(a) || !length(a) || (length(a) == 1L && is.na(a))) b else a
}

fmt_int <- function(x) {
  format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_pct <- function(x, d = 1) {
  if (!is.finite(x)) return("-")
  paste0(format(round(100 * as.numeric(x), d), nsmall = d), "%")
}

quarter_label <- function(q) {
  q <- as.Date(q)
  paste0("Q", ((as.integer(format(q, "%m")) - 1L) %/% 3L) + 1L, " ", format(q, "%Y"))
}

score_mape <- function(store, model, cat) {
  sc <- store$two_quota_scores
  if (is.null(sc) || !nrow(sc)) return(NA_real_)
  v <- sc$MAPE[as.character(sc$model) == model & as.character(sc$category) == cat]
  if (length(v)) as.numeric(v[[1]]) else NA_real_
}

forward_quarter_table <- function(store, cat = "A", n = 8L) {
  fq <- store$forward_quota
  if (is.null(fq) || !nrow(fq)) return(NULL)
  d <- fq[as.character(fq$category) == cat, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  d$quarter <- as.Date(d$quarter)
  agg <- stats::aggregate(
    cbind(mid, lo80, hi80) ~ quarter,
    data = d,
    FUN = function(z) sum(as.numeric(z), na.rm = TRUE)
  )
  cnt <- stats::aggregate(mid ~ quarter, data = d, FUN = length)
  names(cnt)[2] <- "n_months"
  agg <- merge(agg, cnt, by = "quarter", all.x = TRUE)
  agg <- agg[order(agg$quarter), ]
  # Prefer full quarters for the customer-facing table; keep a trailing partial if needed.
  full <- agg[agg$n_months >= 3L, , drop = FALSE]
  if (nrow(full) >= 1L) utils::head(full, n) else utils::head(agg, n)
}

answer_forecast <- function(store, cat = NULL) {
  cats <- if (is.null(cat)) c("A", "B") else cat
  bits <- c(
    "## Forward quota outlook",
    "Central path from the live elastic net, with an 80% band from the recursive bootstrap.",
    sprintf(
      "Last observed month: **%s**. Train ended **%s**; held-out from **%s**.",
      format(as.Date(store$meta$spec$last_observed_month %||% store$latest_dereg_month), "%b %Y"),
      format(store$meta$train_end, "%b %Y"),
      format(store$meta$test_start, "%b %Y")
    )
  )
  for (c in cats) {
    tab <- forward_quarter_table(store, c, 8L)
    if (is.null(tab) || !nrow(tab)) {
      bits <- c(bits, sprintf("### Category %s\nNo forward quota table loaded.", c))
      next
    }
    rows <- apply(tab, 1L, function(r) {
      sprintf(
        "| %s | %s | %s | %s |",
        quarter_label(r[["quarter"]]),
        fmt_int(r[["mid"]]),
        fmt_int(r[["lo80"]]),
        fmt_int(r[["hi80"]])
      )
    })
    bits <- c(
      bits,
      sprintf("### Category %s (certificates / quarter)", c),
      "| Quarter | Mid | Low 80% | High 80% |",
      "| --- | ---: | ---: | ---: |",
      rows,
      ""
    )
  }
  bits <- c(
    bits,
    "We forecast **supply (quota)**, not premiums. Use **Scenarios** to stress renewals / policy, and **Your risk** to price a locked book."
  )
  paste(bits, collapse = "\n")
}

answer_accuracy <- function(store) {
  paste(
    c(
      "## Model accuracy (held-out)",
      "One live model: **elastic net**. Trailing 12-month average is only a baseline.",
      "",
      sprintf(
        "- **Quota MAPE:** %s (Cat A), %s (Cat B)",
        fmt_pct(score_mape(store, "core_forecast", "A")),
        fmt_pct(score_mape(store, "core_forecast", "B"))
      ),
      sprintf(
        "- **Baseline MAPE:** %s (A), %s (B)",
        fmt_pct(score_mape(store, "simple_average", "A")),
        fmt_pct(score_mape(store, "simple_average", "B"))
      ),
      sprintf(
        "- **Fit window:** train %s → %s; test from %s (24 months).",
        format(as.Date(store$meta$spec$train_start %||% "2018-01-01"), "%b %Y"),
        format(store$meta$train_end, "%b %Y"),
        format(store$meta$test_start, "%b %Y")
      ),
      "",
      "Full period misses and the recursive backtest are on the **Results** tab."
    ),
    collapse = "\n"
  )
}

answer_model <- function(store) {
  paste(
    c(
      "## How the supply model works",
      "1. Predict monthly **Cat A/B deregistrations** with an elastic net (α = 0.5, λ.1se).",
      "2. Map to **quota** with a calibrated scale α × trailing-12m deregistrations.",
      "3. Roll the production fit ~36–40 months ahead; bands come from residual / refit / PQP bootstrap.",
      "",
      "**Features (before month t):** due10 / due15 / due_near, recent12, y_lag2, y_lag12, reval5 lags, lpqp, post_2022, seasonality.",
      "",
      "We do **not** forecast clearing premiums. **Your risk** stresses premiums with history and an optional supply tilt."
    ),
    collapse = "\n"
  )
}

answer_exposure <- function() {
  paste(
    c(
      "## Your risk (exposure)",
      "Prices a **locked dealer book** or **fleet schedule** on simulated premium paths.",
      "",
      "- Paths: block-bootstrap of historical A/B tender-to-tender premium moves.",
      "- Optional **supply tilt** from the forward quota path under a Scenarios preset.",
      "- Outputs: expected loss, percentiles, VaR / CVaR, timing shifts.",
      "",
      "Open the **Your risk** tab to upload a book or use the templates."
    ),
    collapse = "\n"
  )
}

answer_help <- function() {
  paste(
    c(
      "## What I can answer fast",
      "- Forward **quota forecasts** for Cat A / B (with bands)",
      "- **Accuracy** (held-out MAPE vs baseline)",
      "- How the **elastic net** and quota mapping work",
      "- What **Your risk** does",
      "",
      "Try: *What is the Cat A quota forecast?* or *How accurate is the model?*"
    ),
    collapse = "\n"
  )
}

detect_cat <- function(q) {
  ql <- tolower(q)
  has_a <- grepl("\\bcat(?:egory)?\\s*a\\b|\\bcategory a\\b", ql)
  has_b <- grepl("\\bcat(?:egory)?\\s*b\\b|\\bcategory b\\b", ql)
  if (has_a && !has_b) return("A")
  if (has_b && !has_a) return("B")
  NULL
}

#' Try to answer from local artefacts. Returns NULL if the question needs NIM.
#' @export
try_desk_answer <- function(question, store) {
  q <- tolower(trimws(as.character(question %||% "")))
  if (!nzchar(q)) return(NULL)

  if (grepl("help|what can you|what do you", q)) return(answer_help())

  if (grepl("accur|mape|score|hit rate|how good|performance|error|miss", q)) {
    return(answer_accuracy(store))
  }

  if (grepl("your risk|exposure|order book|var|cvar|dealer book|fleet schedule", q)) {
    return(answer_exposure())
  }

  if (grepl("how (does|do) (the )?model|elastic net|feature|train|lambda|what do you forecast|how .* forecast", q) &&
        !grepl("outlook|next|forward|quarter|what is the|show me", q)) {
    return(answer_model(store))
  }

  if (grepl("forecast|outlook|forward|quota path|supply path|next (year|quarter)|projected quota|what.*(quota|supply)|show.*(quota|forecast)", q)) {
    return(answer_forecast(store, detect_cat(q)))
  }

  if (grepl("premium", q) && grepl("forecast|predict|next", q)) {
    return(paste(
      "## Premiums",
      "This desk does **not** forecast the next clearing premium.",
      "We forecast **supply**. Use **Your risk** to stress premiums on simulated paths, optionally tilted by forecast quota.",
      sep = "\n"
    ))
  }

  NULL
}

#' Held-out monthly quota: published vs elastic-net outlook (core_forecast).
#' Covers questions like "what was the forecast for Jan 2025?".
#' @export
heldout_quota_context_block <- function(store) {
  d <- store$two_quota
  if (is.null(d) || !nrow(d)) return("")
  need <- c("month", "category", "model", "published", "outlook")
  if (!all(need %in% names(d))) return("")
  d <- d[as.character(d$model) == "core_forecast" & as.character(d$category) %in% c("A", "B"), , drop = FALSE]
  if (!nrow(d)) return("")
  d$month <- as.Date(d$month)
  d <- d[order(d$month, d$category), , drop = FALSE]
  lines <- apply(d, 1L, function(r) {
    sprintf(
      "%s Cat %s published=%s outlook=%s",
      format(as.Date(r[["month"]]), "%Y-%m"),
      as.character(r[["category"]]),
      fmt_int(r[["published"]]),
      fmt_int(r[["outlook"]])
    )
  })
  paste(
    c(
      "## Held-out monthly quota (Sep 2024 onward)",
      "published = released certificates that month; outlook = live elastic-net quota path (not a new refit).",
      lines
    ),
    collapse = "\n"
  )
}

#' Forward path: monthly mid/lo80/hi80, plus a quarterly rollup for quick scans.
#' @export
forecast_context_block <- function(store) {
  bits <- character()
  fq <- store$forward_quota
  if (!is.null(fq) && nrow(fq) && all(c("month", "category", "mid") %in% names(fq))) {
    d <- fq[as.character(fq$category) %in% c("A", "B"), , drop = FALSE]
    d$month <- as.Date(d$month)
    d <- d[order(d$month, d$category), , drop = FALSE]
    has_band <- all(c("lo80", "hi80") %in% names(d))
    lines <- apply(d, 1L, function(r) {
      if (has_band) {
        sprintf(
          "%s Cat %s mid=%s lo80=%s hi80=%s",
          format(as.Date(r[["month"]]), "%Y-%m"),
          as.character(r[["category"]]),
          fmt_int(r[["mid"]]), fmt_int(r[["lo80"]]), fmt_int(r[["hi80"]])
        )
      } else {
        sprintf(
          "%s Cat %s mid=%s",
          format(as.Date(r[["month"]]), "%Y-%m"),
          as.character(r[["category"]]),
          fmt_int(r[["mid"]])
        )
      }
    })
    bits <- c(
      bits,
      "## Forward monthly quota (production roll-forward)",
      "Use these for months after the held-out window. mid = central; lo80/hi80 = 80% band.",
      lines
    )
  }
  for (cat in c("A", "B")) {
    tab <- forward_quarter_table(store, cat, 12L)
    if (is.null(tab) || !nrow(tab)) next
    lines <- apply(tab, 1L, function(r) {
      sprintf(
        "%s mid=%s lo80=%s hi80=%s",
        quarter_label(r[["quarter"]]),
        fmt_int(r[["mid"]]), fmt_int(r[["lo80"]]), fmt_int(r[["hi80"]])
      )
    })
    bits <- c(bits, paste0("## Forward quota Cat ", cat, " (quarterly sum)"), lines)
  }
  paste(bits, collapse = "\n")
}

#' @export
suggestion_chips <- function() {
  c(
    "What is the Cat A quota forecast?",
    "What is the Cat B quota forecast?",
    "How accurate is the model?",
    "How does the supply model work?",
    "What does Your risk do?"
  )
}
