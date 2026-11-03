# Context builders + suggestion chips for COE bot (live artefacts, no network).
# Column names are deliberately verbose so the model cannot confuse published vs forecast.

`%||%` <- function(a, b) {
  if (is.null(a) || !length(a) || (length(a) == 1L && is.na(a))) b else a
}

fmt_int <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || !is.finite(x[[1]])) return("NA")
  format(round(x[[1]]), big.mark = ",", scientific = FALSE, trim = TRUE)
}

quarter_label <- function(q) {
  q <- as.Date(q)
  paste0("Q", ((as.integer(format(q, "%m")) - 1L) %/% 3L) + 1L, " ", format(q, "%Y"))
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
  full <- agg[agg$n_months >= 3L, , drop = FALSE]
  if (nrow(full) >= 1L) utils::head(full, n) else utils::head(agg, n)
}

#' Hard calendar cutoffs so the model cannot invent “published” for future months.
#' @export
truth_cutoff_block <- function(store) {
  if (is.null(store)) return("")
  last_bid <- as.Date(store$latest_bid_month %||% NA)
  last_complete <- as.Date(store$latest_complete_bid_month %||% last_bid)
  last_dereg <- as.Date(store$latest_dereg_month %||% NA)

  held_max <- NA
  d <- store$two_quota
  if (!is.null(d) && nrow(d) && "month" %in% names(d)) {
    held_max <- max(as.Date(d$month), na.rm = TRUE)
  }

  fwd_min <- NA
  fwd_pub_months <- character()
  fq <- store$forward_quota
  if (!is.null(fq) && nrow(fq) && "month" %in% names(fq)) {
    fwd_min <- min(as.Date(fq$month), na.rm = TRUE)
    if ("published" %in% names(fq)) {
      ok <- is.finite(suppressWarnings(as.numeric(fq$published)))
      if (any(ok)) {
        fwd_pub_months <- sort(unique(format(as.Date(fq$month[ok]), "%Y-%m")))
      }
    }
  }

  paste(
    c(
      "## TRUTH CUTOFFS (mandatory — break these and the answer is wrong)",
      sprintf("latest_bid_month=%s", as.character(last_bid)),
      sprintf("latest_complete_bid_month=%s", as.character(last_complete)),
      sprintf("latest_dereg_month=%s", as.character(last_dereg)),
      sprintf("heldout_table_last_month=%s", as.character(held_max)),
      sprintf("forward_table_first_month=%s", as.character(fwd_min)),
      sprintf(
        "forward_months_with_REAL_published=%s",
        if (length(fwd_pub_months)) paste(fwd_pub_months, collapse = ",") else "NONE"
      ),
      "RULE: The word published means LTA-released certificates already in the data.",
      "RULE: If published_status=NOT_RELEASED (or published=NA), you MUST say not released — never invent a published number.",
      "RULE: forward_mid / forward_lo80 / forward_hi80 are forecasts, never call them published.",
      "RULE: heldout_published / heldout_outlook are only for months listed in the held-out table.",
      "RULE: For months after heldout_table_last_month, use the FORWARD table only."
    ),
    collapse = "\n"
  )
}

#' Held-out monthly quota: published vs elastic-net outlook (core_forecast).
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
  last_m <- format(max(d$month), "%Y-%m")
  first_m <- format(min(d$month), "%Y-%m")
  lines <- apply(d, 1L, function(r) {
    sprintf(
      "%s Cat %s heldout_published=%s heldout_outlook=%s",
      format(as.Date(r[["month"]]), "%Y-%m"),
      as.character(r[["category"]]),
      fmt_int(r[["published"]]),
      fmt_int(r[["outlook"]])
    )
  })
  paste(
    c(
      sprintf("## Held-out monthly quota ONLY (%s … %s)", first_m, last_m),
      "Fields: heldout_published = released certificates; heldout_outlook = elastic-net path on the held-out window.",
      "Do not use this table for months after the last month above.",
      lines
    ),
    collapse = "\n"
  )
}

#' Forward path with explicit published_status so NA cannot be misread as a number.
#' @export
forecast_context_block <- function(store) {
  bits <- character()
  fq <- store$forward_quota
  if (!is.null(fq) && nrow(fq) && all(c("month", "category", "mid") %in% names(fq))) {
    d <- fq[as.character(fq$category) %in% c("A", "B"), , drop = FALSE]
    d$month <- as.Date(d$month)
    d <- d[order(d$month, d$category), , drop = FALSE]
    has_band <- all(c("lo80", "hi80") %in% names(d))
    has_pub <- "published" %in% names(d)
    lines <- apply(d, 1L, function(r) {
      pub_num <- if (has_pub) suppressWarnings(as.numeric(r[["published"]])) else NA_real_
      pub_status <- if (is.finite(pub_num)) {
        sprintf("published_status=RELEASED published=%s", fmt_int(pub_num))
      } else {
        "published_status=NOT_RELEASED published=NA"
      }
      if (has_band) {
        sprintf(
          "%s Cat %s %s forward_mid=%s forward_lo80=%s forward_hi80=%s",
          format(as.Date(r[["month"]]), "%Y-%m"),
          as.character(r[["category"]]),
          pub_status,
          fmt_int(r[["mid"]]), fmt_int(r[["lo80"]]), fmt_int(r[["hi80"]])
        )
      } else {
        sprintf(
          "%s Cat %s %s forward_mid=%s",
          format(as.Date(r[["month"]]), "%Y-%m"),
          as.character(r[["category"]]),
          pub_status,
          fmt_int(r[["mid"]])
        )
      }
    })
    bits <- c(
      bits,
      "## Forward monthly quota (forecast path — not held-out)",
      "Fields: published_status / published (only if RELEASED); forward_mid / forward_lo80 / forward_hi80 = forecast band.",
      "NEVER label forward_mid as published. NEVER fill NOT_RELEASED with a made-up published figure.",
      lines
    )
  }
  for (cat in c("A", "B")) {
    tab <- forward_quarter_table(store, cat, 12L)
    if (is.null(tab) || !nrow(tab)) next
    lines <- apply(tab, 1L, function(r) {
      sprintf(
        "%s forward_mid=%s forward_lo80=%s forward_hi80=%s",
        quarter_label(r[["quarter"]]),
        fmt_int(r[["mid"]]), fmt_int(r[["lo80"]]), fmt_int(r[["hi80"]])
      )
    })
    bits <- c(
      bits,
      paste0("## Forward quota Cat ", cat, " (quarterly sum of forward_mid / bands)"),
      "These quarterly rows are forecasts only — no published column.",
      lines
    )
  }
  paste(bits, collapse = "\n")
}

#' @export
suggestion_chips <- function() {
  box::use(app/logic/chat/desk_run[desk_suggestion_chips])
  desk_suggestion_chips()
}
