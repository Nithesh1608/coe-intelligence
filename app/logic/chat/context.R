# Build grounded context for COE bot from a tight brief + live artefacts.

`%||%` <- function(a, b) {
  if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
}

clip <- function(x, n = 14000L) {
  x <- paste(x, collapse = "\n")
  if (nchar(x) <= n) return(x)
  paste0(substr(x, 1L, n), "\n\n[…truncated…]")
}

fmt_scores <- function(df, title) {
  if (is.null(df) || !nrow(df)) return("")
  keep <- intersect(
    c("model", "label", "category", "MAE", "MAPE", "within_10pct", "within_15pct"),
    names(df)
  )
  if (!length(keep)) return("")
  lines <- apply(df[, keep, drop = FALSE], 1L, function(r) {
    paste(paste(keep, r, sep = "="), collapse = " | ")
  })
  paste(c(paste0("## ", title), lines), collapse = "\n")
}

#' @export
system_prompt <- function() {
  paste(
    "You are COE bot for COE Intelligence — a Singapore COE supply forecast and exposure desk.",
    "Answer ONLY from PROJECT CONTEXT below. Be precise, useful, and compact.",
    "No fluff, no marketing, no NVIDIA/API/key talk, no inventing metrics or dates.",
    "Core product forecasts deregistrations / quota supply — not clearing premiums.",
    "When asked for a past held-out month (e.g. Jan 2025), quote published and outlook from the held-out monthly table.",
    "When asked for the live forward outlook, quote Forward monthly mid / lo80 / hi80 (or quarterly sums) exactly.",
    "Prefer short markdown tables. FORMAT: GitHub-flavoured markdown with ## headings for multi-part answers.",
    "Short paragraphs, bullets, tables. Aim under ~280 words unless the user asks for depth.",
    "Always finish the full answer. Keep numbers exact. If not in context, say so briefly.",
    sep = " "
  )
}

#' Tight static product facts (replaces long project_brief.md in the prompt).
compact_brief <- function() {
  paste(
    c(
      "## Product",
      "DBA3702 COE supply desk (Rhino/Shiny). Forecasts Cat A/B deregistrations and implied quota ~36 months with bootstrap bands. Does not forecast premiums. Your risk prices a locked dealer book / fleet schedule on simulated premium paths (optional supply tilt from forward quota).",
      "Tabs: Home, Scenarios, Market, Results, Your risk, COE bot, About.",
      "",
      "## Model",
      "Elastic net α=0.5, λ.1se on SingStat VQS deregistrations. Features before t: due10/due15/due_near, recent12, y_lag2, y_lag12, reval5 lags, lpqp, post_2022, seasonality.",
      "Quota = α × trailing-12m deregistrations (α from train). Live held-out quota MAPE ≈ 11.9% A / 11.6% B vs trailing-12 baseline.",
      "Train 2018-01…2024-08; held-out from 2024-09 (24m).",
      "",
      "## Scenarios / risk",
      "Scenarios stress renewals / quota injections / premium→renewal. Your risk: block-bootstrap premium paths, VaR/CVaR, timing +1Q/+2Q."
    ),
    collapse = "\n"
  )
}

#' @export
build_project_context <- function(store = NULL, root = NULL) {
  box::use(
    app/logic/chat/desk_answers[forecast_context_block, heldout_quota_context_block]
  )

  score_bits <- character()
  if (!is.null(store)) {
    score_bits <- c(
      fmt_scores(store$two_quota_scores, "Live quota scores (held-out)"),
      fmt_scores(store$two_hits, "Deregistration hit rates (held-out)"),
      if (!is.null(store$meta$train_end)) {
        sprintf(
          "## Fit window\ntrain_end=%s | test_start=%s | live_model=%s | lambda=%s | latest_bid_month=%s",
          as.character(store$meta$train_end),
          as.character(store$meta$test_start),
          store$meta$live_model %||% "core_forecast",
          store$meta$spec$lambda_rule %||% "lambda.1se",
          as.character(store$latest_bid_month %||% NA)
        )
      },
      heldout_quota_context_block(store),
      forecast_context_block(store)
    )
  }

  clip(paste(
    c(
      "# PROJECT CONTEXT",
      compact_brief(),
      paste(score_bits[nzchar(score_bits)], collapse = "\n\n")
    ),
    collapse = "\n\n"
  ))
}
