# Build grounded context for COE bot from a tight brief + live artefacts.

`%||%` <- function(a, b) {
  if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
}

clip <- function(x, n = 14000L) {
  x <- paste(x, collapse = "\n")
  if (nchar(x) <= n) return(x)
  paste0(substr(x, 1L, n), "\n\n[…truncated…]")
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
  # Scores are stored as fractions (0.119 → 11.9%).
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
    "No fluff, no marketing, no NVIDIA/API/key talk, no inventing metrics or dates.",
    "Core product forecasts deregistrations / quota supply — not clearing premiums.",
    "NAMING: Call the live model “elastic net” (α=0.5, λ chosen by the 1-SE rule).",
    "Never say core_forecast, simple_average, or other internal artefact ids to the user.",
    "The baseline comparator is the “12-month average”.",
    "MAPE and hit-rates in context are already percentages — quote them as 11.9%, not 0.119.",
    "Do not use LaTeX or $...$ math. Write plain text: alpha=0.5, lambda 1-SE rule.",
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
      "Live supply model: elastic net (mixing alpha=0.5, lambda by 1-SE rule) on SingStat VQS deregistrations.",
      "Internal artefact id core_forecast = this elastic net — never show that id to users.",
      "Features before month t: due10/due15/due_near, recent12, y_lag2, y_lag12, reval5 lags, lpqp, post_2022, seasonality.",
      "Quota mapping: quota = alpha_map × trailing-12m deregistrations (alpha_map fitted on train).",
      "Held-out quota MAPE: about 11.9% Cat A / 11.6% Cat B vs 12-month-average baseline.",
      "Train 2018-01…2024-08; held-out from 2024-09 (24 months).",
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
          paste0(
            "## Fit window\n",
            "train_end=%s | test_start=%s | live_model=Elastic net | ",
            "lambda_rule=1-SE | latest_bid_month=%s"
          ),
          as.character(store$meta$train_end),
          as.character(store$meta$test_start),
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
