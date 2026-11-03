# Context builders + suggestion chips for COE bot (live artefacts, no network).

`%||%` <- function(a, b) {
  if (is.null(a) || !length(a) || (length(a) == 1L && is.na(a))) b else a
}

fmt_int <- function(x) {
  format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
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
  box::use(app/logic/chat/desk_run[desk_suggestion_chips])
  desk_suggestion_chips()
}
