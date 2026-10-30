# Build grounded context for COE bot from brief + live score artefacts.

`%||%` <- function(a, b) {
  if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
}

clip <- function(x, n = 9000L) {
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
    "You are COE bot, the in-app assistant for COE Intelligence — a Singapore COE supply",
    "forecast and exposure desk for dealers and fleets (DBA3702 project).",
    "Answer only from the PROJECT CONTEXT below and the user's question.",
    "Be precise and complete. No fluff, no marketing, no NVIDIA/API setup talk.",
    "If something is not in context, say you do not have it — do not invent metrics, dates, or APIs.",
    "Prefer deregistration / quota / elastic net / exposure terminology used by the product.",
    "Never ask for or reveal API keys. Never claim you forecast premiums as the core product.",
    "FORMAT: Use GitHub-flavoured markdown. For multi-part questions, answer every part in order",
    "with ## headings. Use short paragraphs, bullet lists, and markdown tables for score grids.",
    "Free-form answers are fine — paraphrase and structure for clarity; do not invent facts.",
    "Keep answers compact (aim under ~350 words unless the user asks for depth).",
    "Always finish the full answer — never stop mid-sentence or mid-word. Keep numbers exact.",
    sep = " "
  )
}

#' @export
build_project_context <- function(store = NULL, root = NULL) {
  box::use(app/logic/viewdata/store[project_root])
  if (is.null(root)) root <- project_root()

  brief_path <- file.path(root, "app", "logic", "chat", "project_brief.md")
  brief <- if (file.exists(brief_path)) {
    paste(readLines(brief_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  } else {
    "Brief missing."
  }

  score_bits <- character()
  if (!is.null(store)) {
    score_bits <- c(
      fmt_scores(store$two_quota_scores, "Live quota scores (held-out)"),
      fmt_scores(store$two_hits, "Deregistration hit rates (held-out)"),
      if (!is.null(store$meta$train_end)) {
        sprintf(
          "## Fit window\ntrain_end=%s | test_start=%s | live_model=%s",
          as.character(store$meta$train_end),
          as.character(store$meta$test_start),
          store$meta$live_model %||% "core_forecast"
        )
      }
    )
  }

  # Brief + live scores only — large README/proposal payloads slowed every turn.
  clip(paste(
    c(
      "# PROJECT CONTEXT",
      brief,
      paste(score_bits[nzchar(score_bits)], collapse = "\n\n")
    ),
    collapse = "\n\n"
  ))
}
