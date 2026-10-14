# Seasonal-naive illustration and the published replacement quota identity.
# This is not a fitted GAM or ARIMA. Bands widen with horizon from historical
# year-on-year residual scale.

#' @export
quarter_start <- function(month) {
  m <- as.Date(month)
  q <- ((as.integer(format(m, "%m")) - 1L) %/% 3L) * 3L + 1L
  as.Date(sprintf("%04d-%02d-01", as.integer(format(m, "%Y")), q))
}

#' @export
add_months <- function(month, n) {
  m <- as.Date(month)
  y <- as.integer(format(m, "%Y"))
  mo <- as.integer(format(m, "%m")) + as.integer(n)
  y <- y + (mo - 1L) %/% 12L
  mo <- (mo - 1L) %% 12L + 1L
  as.Date(sprintf("%04d-%02d-01", y, mo))
}

trailing_sum <- function(values, months, end_month, n = 12L) {
  keep <- months <= end_month & months > add_months(end_month, -n)
  sum(values[keep], na.rm = TRUE)
}

#' @export
replacement_quota <- function(dereg_12, pop_c_year_end) {
  base_a <- 0.25 * dereg_12[["A"]]
  base_b <- 0.25 * dereg_12[["B"]]
  base_c <- 0.25 * dereg_12[["C"]]
  base_d <- 0.25 * dereg_12[["D"]]
  c(
    A = 0.9 * base_a,
    B = 0.9 * base_b,
    C = 0.9 * base_c + 0.0025 * pop_c_year_end / 4,
    D = base_d,
    E = 0.1 * (base_a + base_b + base_c)
  )
}

yoy_scale <- function(series) {
  if (length(series) < 24L) {
    return(stats::sd(series, na.rm = TRUE))
  }
  lagged <- series[seq_len(length(series) - 12L)]
  now <- series[seq.int(13L, length(series))]
  stats::sd(now - lagged, na.rm = TRUE)
}

#' @export
naive_path <- function(store, category, horizon_months = 36L, renewal_shift = 0) {
  panel <- store$panel
  part <- panel[panel$category == category, , drop = FALSE]
  part <- part[order(part$month), , drop = FALSE]
  obs <- part[!is.na(part$deregistrations), , drop = FALSE]
  origin <- max(obs$month)
  hist <- obs$deregistrations
  names(hist) <- as.character(obs$month)
  scale <- yoy_scale(hist)
  factor <- max(0, 1 - renewal_shift)
  forward <- lapply(seq_len(horizon_months), function(h) {
    m <- add_months(origin, h)
    look <- as.character(add_months(m, -12L))
    if (!look %in% names(hist)) {
      look <- as.character(add_months(m, -24L))
    }
    mid <- if (look %in% names(hist)) hist[[look]] * factor else NA_real_
    width <- scale * sqrt(h / 12) * 1.96
    data.frame(
      month = m,
      category = category,
      mid = mid,
      lo = pmax(0, mid - width),
      hi = mid + width,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, forward)
}

#' @export
quota_path <- function(store, naive_list, pop_c) {
  cats <- c("A", "B", "C", "D")
  months <- sort(unique(naive_list[[1]]$month))
  rows <- lapply(months, function(m) {
    dereg_12 <- vapply(cats, function(cat) {
      path <- naive_list[[cat]]
      hist <- store$panel
      hist <- hist[hist$category == cat & !is.na(hist$deregistrations), ]
      past <- hist[hist$month <= add_months(m, -1L), c("month", "deregistrations")]
      fut <- path[path$month <= m, c("month", "mid")]
      names(fut) <- c("month", "deregistrations")
      both <- rbind(past, fut)
      trailing_sum(both$deregistrations, both$month, m, 12L)
    }, numeric(1))
    names(dereg_12) <- cats
    q <- replacement_quota(dereg_12, pop_c)
    data.frame(
      month = m,
      quarter = quarter_start(m),
      category = names(q),
      quota = as.numeric(q),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' @export
year_end_pop_c <- function(store) {
  panel <- store$panel
  part <- panel[panel$category == "C" & !is.na(panel$population), ]
  dec <- part[format(part$month, "%m") == "12", ]
  if (!nrow(dec)) {
    return(utils::tail(part$population, 1))
  }
  utils::tail(dec$population, 1)
}
