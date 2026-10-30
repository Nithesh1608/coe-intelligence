# Instant grounded answers from the project brief + live score artefacts.
# No remote LLM call — reliable on Posit and local.

`%||%` <- function(a, b) {
  if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
}

fmt_num <- function(x, digits = 2) {
  if (is.null(x) || !length(x) || is.na(x[[1]])) return("—")
  format(round(as.numeric(x[[1]]), digits), nsmall = digits, big.mark = ",")
}

fmt_pct <- function(x, digits = 1) {
  if (is.null(x) || !length(x) || is.na(x[[1]])) return("—")
  paste0(format(round(100 * as.numeric(x[[1]]), digits), nsmall = digits), "%")
}

pick_row <- function(df, model, category) {
  if (is.null(df) || !nrow(df)) return(NULL)
  hit <- df$model == model & df$category == category
  if (!any(hit)) return(NULL)
  df[which(hit)[[1]], , drop = FALSE]
}

quota_score_block <- function(store) {
  df <- store$two_quota_scores
  if (is.null(df) || !nrow(df)) return("_Quota scores not loaded._")
  lines <- c(
    "| model | cat | MAE | MAPE |",
    "| --- | --- | --- | --- |"
  )
  for (i in seq_len(nrow(df))) {
    r <- df[i, , drop = FALSE]
    mape <- if ("MAPE" %in% names(r)) fmt_num(r$MAPE, 3) else "—"
    lines <- c(lines, sprintf(
      "| %s | %s | %s | %s |",
      as.character(r$model[[1]]),
      as.character(r$category[[1]]),
      fmt_num(r$MAE, 2),
      mape
    ))
  }
  paste(lines, collapse = "\n")
}

hit_rate_block <- function(store) {
  df <- store$two_hits
  if (is.null(df) || !nrow(df)) return("_Hit rates not loaded._")
  core <- df[df$model == "core_forecast", , drop = FALSE]
  if (!nrow(core)) core <- df
  lines <- character()
  for (i in seq_len(nrow(core))) {
    r <- core[i, , drop = FALSE]
    w10 <- if ("within_10pct" %in% names(r)) fmt_pct(r$within_10pct, 1) else "—"
    w15 <- if ("within_15pct" %in% names(r)) fmt_pct(r$within_15pct, 1) else "—"
    lines <- c(lines, sprintf(
      "- **Cat %s** (`core_forecast`): within 10%% = %s, within 15%% = %s",
      as.character(r$category[[1]]), w10, w15
    ))
  }
  paste(lines, collapse = "\n")
}

fit_window_line <- function(store) {
  meta <- store$meta
  sprintf(
    "Train ≤ %s, locked test ≥ %s (~24 months). Live model id: `%s` (Elastic net). Baseline: `simple_average` (trailing 12-month mean).",
    as.character(meta$train_end %||% "2024-08-01"),
    as.character(meta$test_start %||% "2024-09-01"),
    meta$live_model %||% "core_forecast"
  )
}

answer_supply <- function(store) {
  paste(
    c(
      "## How supply is forecast",
      "",
      "The modelling unit is **category-month deregistrations** from the **SingStat VQS** series. That is `y`. **LTA M05** stays for audits / formula checks only — it is never mixed into the target.",
      "",
      "- **Model:** `glmnet::cv.glmnet`, elastic net with mixing α = 0.5, MAE loss, λ at `lambda.1se`. Predictions are floored at 0.2 × `recent12`, then rounded.",
      paste0("- **Split:** ", fit_window_line(store)),
      "- **Quota link:** category factor α = train median of published monthly quota ÷ trailing-12m deregistrations (≈ 0.082 for A, ≈ 0.078 for B). Outlook = α × D₁₂, where D₁₂ uses actual deregistrations before the test window and model predictions inside it.",
      "- The textbook LTA rule (~25% of trailing 12m, with 10% of the A/B base to E) is for EDA / explanation only — it is **not** the published desk number.",
      "",
      "## Held-out quota scores",
      "",
      quota_score_block(store),
      "",
      "## Deregistration hit rates",
      "",
      hit_rate_block(store)
    ),
    collapse = "\n"
  )
}

answer_features <- function(store) {
  paste(
    c(
      "## What the features mean",
      "",
      "All predictors are dated **strictly before** month t (age / PQP from t−1). Unit: category-month SingStat deregistrations.",
      "",
      "### Core matrix",
      "",
      "- `due10`, `due15`, `due_near` — due-stock / 12 after A/B share from the Cars age stock",
      "- `recent12`, `y_lag2`, `y_lag12` — recent deregistration flow",
      "- `reval5_lag12` / `24` / `60` — lagged 5-year revalidation counts",
      "- `lpqp` = log(PQP_{t−1} / 50000)",
      "- `post_2022` — A/B EV redefinition regime",
      "- `moy_sin` / `moy_cos` — seasonal cycle",
      "",
      "Train screens (correlation + lasso λ.1se) kept due-stock and lags; reval lags and `post_2022` were stronger for Cat B. The **live** model still passes the **full** core matrix into elastic net and lets shrinkage decide — not a hard-zeroed subset.",
      "",
      paste0("### Fit window\n", fit_window_line(store))
    ),
    collapse = "\n"
  )
}

answer_scores <- function(store) {
  paste(
    c(
      "## Held-out performance",
      "",
      fit_window_line(store),
      "",
      "### Quota scores (MAE / MAPE)",
      "",
      quota_score_block(store),
      "",
      "### Deregistration hit rates (`core_forecast`)",
      "",
      hit_rate_block(store),
      "",
      "`core_forecast` beats `simple_average` on both categories for quota MAE/MAPE on the locked test window."
    ),
    collapse = "\n"
  )
}

answer_quota <- function(store) {
  paste(
    c(
      "## Quota mapping",
      "",
      "After forecasting deregistrations D, the desk maps to quota with a **calibrated** category factor:",
      "",
      "- α = train median of (published monthly quota ÷ trailing-12m deregistrations)",
      "- ≈ **0.082** for Cat A, ≈ **0.078** for Cat B",
      "- Outlook = α × D₁₂ (actual D before test; model D inside test)",
      "",
      "The familiar LTA textbook sketch (~25% of trailing 12 months, siphoning 10% of A/B base to E) is shown in EDA for intuition only. It is **not** what Home / Results publish as the desk outlook.",
      "",
      "### Quota scorecard",
      "",
      quota_score_block(store)
    ),
    collapse = "\n"
  )
}

answer_exposure <- function(store) {
  paste(
    c(
      "## Your risk (exposure)",
      "",
      "Your risk is an **exposure calculator**, not a premium forecast product.",
      "",
      "- Premium paths: block-bootstrap of historical tender-to-tender log shocks; PQP from trailing premiums; horizon in exercises (6 per quarter).",
      "- Optional **supply tilt**: drift ≈ elasticity × Δlog(Q) / 6 per exercise; estimated quota→premium elasticity ≈ **−0.22** for A/B. Tighter supply → upward premium drift.",
      "- **Dealer book:** guaranteed / capped / rebate contracts; i.i.d. bid success; VaR / CVaR / P(loss); timing shift 0–2 quarters.",
      "- **Fleet:** quarterly A/B schedule; bid-new vs 5y / 10y renew under simulated QP / PQP.",
      "",
      "Predicted quota from the elastic net can tilt the premium paths; the core shipped product remains supply + exposure under those paths."
    ),
    collapse = "\n"
  )
}

answer_cleaning <- function(store) {
  paste(
    c(
      "## Data cleaning",
      "",
      "Sources: LTA DataMall (bidding, PQP, LTA deregistrations by quota category, population by age, COE revalidations, car-cost) and SingStat / data.gov.sg (long-run premiums, registrations, deregistrations, VQS population).",
      "",
      "- Bidding: normalised categories; complete month × round × category grid; COVID Apr–Jun 2020 suspensions stay NA (not zero). Bid pressure = bids_received / quota.",
      "- Regimes: Cat A power cap Feb 2014; A/B EV redefinition May 2022 (`post_2022`).",
      "- Two dereg series: **SingStat VQS** = modelling target; **LTA M05** for audits only.",
      "- Age stock: Cars pooled A+B in source — A/B share inferred from recent dereg mix for due-stock.",
      "- Revalidations: 5y vs 10y tenure; live model uses lagged reval counts, not a separate renewal simulator.",
      "",
      "Audits live under `data/audit/`; cleaning notebook: `data/data_cleaning.Rmd`."
    ),
    collapse = "\n"
  )
}

answer_overview <- function(store) {
  paste(
    c(
      "## COE Intelligence",
      "",
      "DBA3702 dealer / fleet **COE supply** desk. It forecasts Category A and B **deregistrations** and the **quota** they imply. It does **not** forecast premiums as the core product. **Your risk** is exposure for a locked dealer book or fleet schedule.",
      "",
      fit_window_line(store),
      "",
      "Ask about: supply forecast, features, quota mapping, held-out scores, cleaning, or Your risk.",
      "",
      "### Snapshot — quota scores",
      "",
      quota_score_block(store)
    ),
    collapse = "\n"
  )
}

#' Classify a free-text question into a topic key.
#' @export
classify_topic <- function(q) {
  q <- tolower(as.character(q %||% ""))
  if (!nzchar(q)) return("overview")

  if (grepl("feature|predictor|due10|due15|due_near|recent12|lpqp|reval|what do the|what does", q)) {
    return("features")
  }
  if (grepl("score|mae|mape|hit rate|accuracy|performance|how good|held-?out|test set", q)) {
    return("scores")
  }
  if (grepl("your risk|exposure|var|cvar|dealer book|fleet|elasticity|premium path", q)) {
    return("exposure")
  }
  if (grepl("clean|singstat|data mall|vqs|m05|bidding|source", q)) {
    return("cleaning")
  }
  if (grepl("quota|alpha|α|outlook|25%|calibrat", q) && !grepl("forecast|elastic|glmnet|model", q)) {
    return("quota")
  }
  if (grepl("supply|forecast|elastic|glmnet|dereg|model|lambda|split|train|how.*(predict|work)", q)) {
    return("supply")
  }
  if (grepl("quota|alpha|α|outlook", q)) return("quota")
  "overview"
}

topic_answer <- function(topic, store) {
  switch(
    topic,
    supply = answer_supply(store),
    features = answer_features(store),
    scores = answer_scores(store),
    quota = answer_quota(store),
    exposure = answer_exposure(store),
    cleaning = answer_cleaning(store),
    answer_overview(store)
  )
}

detect_topics <- function(q) {
  q <- tolower(as.character(q %||% ""))
  topics <- character()
  if (grepl("feature|predictor|due10|due_near|what the features|what do the features", q)) {
    topics <- c(topics, "features")
  }
  if (grepl("score|mae|mape|hit rate|performance|held-?out", q)) {
    topics <- c(topics, "scores")
  }
  if (grepl("your risk|exposure|dealer book|fleet schedule|elasticity", q)) {
    topics <- c(topics, "exposure")
  }
  if (grepl("clean|singstat|data mall|vqs|m05", q)) {
    topics <- c(topics, "cleaning")
  }
  if (grepl("quota|calibrat|outlook|alpha|α", q)) {
    topics <- c(topics, "quota")
  }
  if (grepl("supply|forecast|elastic|glmnet|deregistration|how.*(model|predict)", q)) {
    topics <- c(topics, "supply")
  }
  # Numbered multi-part asks ("1. … 2. …") — cover the usual desk trio.
  if (grepl("\\b1\\.|first|three things|in order", q) && grepl("\\b2\\.|second|feature", q)) {
    topics <- unique(c("supply", "features", topics))
  }
  if (!length(topics)) topics <- classify_topic(q)
  unique(topics)
}

#' Build an instant markdown answer grounded on store scores + fixed desk facts.
#' @export
answer_question <- function(q, store) {
  topics <- detect_topics(q)
  parts <- vapply(topics, function(t) topic_answer(t, store), character(1))
  # Avoid repeating the same quota table when supply + scores both asked.
  text <- paste(parts, collapse = "\n\n---\n\n")
  list(ok = TRUE, text = text, topic = paste(topics, collapse = "+"), source = "local")
}
