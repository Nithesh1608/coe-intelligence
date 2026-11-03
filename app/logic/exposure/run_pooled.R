# Non-Shiny pooled dealer-book pricing — same engines as the Your risk tab.

box::use(
  app/logic/exposure/api[
    load_exposure_funs, load_exposure_data, make_pooled_book
  ],
  app/logic/viewdata/scenario_viz[prep_scenario],
  app/logic/viewdata/pipeline[quarter_start],
  app/logic/viewdata/store[project_root]
)

`%||%` <- function(a, b) {
  if (is.null(a) || !length(a) || (length(a) == 1L && is.na(a))) b else a
}

#' @export
last_premium <- function(bidding, category) {
  b <- bidding[as.character(bidding$category) == category & !is.na(bidding$premium), ]
  if (!nrow(b)) return(100000)
  as.numeric(utils::tail(b[order(b$month), ], 1)$premium[[1]])
}

last_pqp <- function(root, category, fallback) {
  p <- file.path(root, "data", "processed", "pqp.rds")
  if (!file.exists(p)) return(fallback)
  d <- tryCatch(readRDS(p), error = function(e) NULL)
  if (is.null(d)) return(fallback)
  d$category <- as.character(d$category)
  if (is.numeric(d$month)) d$month <- as.Date(d$month, origin = "1970-01-01")
  else d$month <- as.Date(d$month)
  d <- d[d$category == category & is.finite(d$pqp), ]
  if (!nrow(d)) return(fallback)
  as.numeric(utils::tail(d[order(d$month), ], 1)$pqp[[1]])
}

quota_path_for <- function(store, cat, scenario_id = "baseline", n_quarters = 8L) {
  fq <- store$forward_quota
  if (is.null(fq) || !nrow(fq)) return(NULL)
  d <- fq[as.character(fq$category) == cat, ]
  d <- d[order(as.Date(d$month)), ]
  if (!nrow(d)) return(NULL)
  scn <- tryCatch(
    prep_scenario(store, category = cat, preset_id = scenario_id, horizon_quarters = n_quarters + 1L),
    error = function(e) NULL
  )
  scn_q <- if (!is.null(scn)) {
    stats::setNames(scn$quarters$quota_scenario, as.character(as.Date(scn$quarters$quarter)))
  } else {
    NULL
  }
  cur_q <- quarter_start(store$latest_bid_month)
  b <- store$bidding
  if ("partial" %in% names(b)) b <- b[!b$partial, ]
  pub_cur <- b[as.character(b$category) == cat & is.finite(b$quota) & quarter_start(b$month) == cur_q, ]
  fc_cur <- d[quarter_start(as.Date(d$month)) == cur_q & !(as.Date(d$month) %in% as.Date(pub_cur$month)), ]
  cur_total <- sum(pub_cur$quota, na.rm = TRUE) + sum(fc_cur$mid, na.rm = TRUE)
  if (!is.finite(cur_total) || cur_total <= 0) return(NULL)
  nxt <- numeric()
  q <- cur_q
  for (i in seq_len(n_quarters)) {
    q <- quarter_start(as.Date(q) + 93)
    key <- as.character(q)
    v <- if (!is.null(scn_q) && key %in% names(scn_q)) scn_q[[key]] else {
      dq <- d[quarter_start(as.Date(d$month)) == q, ]
      if (nrow(dq) == 3L) sum(dq$mid) else NA_real_
    }
    if (!is.finite(v)) break
    nxt <- c(nxt, v)
  }
  path <- c(cur_total, nxt)
  if (length(path) < 2L) NULL else as.numeric(path)
}

dlog_for <- function(exp_data, cat) {
  m <- exp_data$premium_moves
  if (is.null(m) || is.null(m$round_to_round_detail)) return(numeric())
  d <- m$round_to_round_detail
  d <- d[d$category == cat & is.finite(d$chg_pct), ]
  out <- log1p(as.numeric(d$chg_pct))
  out[is.finite(out)]
}

elasticity_for <- function(exp_data, cat) {
  s <- exp_data$price_sensitivity
  if (is.null(s)) return(0)
  hit <- s$elasticity_quota[s$category == cat]
  if (!length(hit) || is.na(hit[[1]])) 0 else as.numeric(hit[[1]])
}

#' Price a pooled guaranteed book with the same engines as Your risk.
#' Defaults match the tab: win 70%, vol 1x, tilt on, baseline scenario, 200 sims.
#' @export
run_pooled_exposure <- function(store,
                                n_cars,
                                category = "A",
                                quoted_premium,
                                p_win = 0.70,
                                vol = 1,
                                tilt = TRUE,
                                tilt_scn = "baseline",
                                nsim = 200L,
                                root = NULL) {
  root <- root %||% store$root %||% project_root()
  cat <- toupper(as.character(category)[[1]])
  if (!cat %in% c("A", "B")) {
    stop("Pooled bot pricing supports Cat A or B only (not AB / fleet CSV).", call. = FALSE)
  }
  n_cars <- as.integer(n_cars)
  quoted_premium <- as.numeric(quoted_premium)
  if (!is.finite(n_cars) || n_cars < 1L || n_cars > 200L) {
    stop("Orders must be between 1 and 200.", call. = FALSE)
  }
  if (!is.finite(quoted_premium) || quoted_premium < 0) {
    stop("Quoted premium must be a non-negative number.", call. = FALSE)
  }

  funs <- load_exposure_funs(root)
  exp_data <- load_exposure_data(root)
  if (is.null(exp_data$premium_moves) || is.null(exp_data$price_sensitivity)) {
    stop("Missing premium-move or sensitivity tables.", call. = FALSE)
  }

  b <- make_pooled_book(n_cars, cat, quoted_premium)
  nsim <- max(100L, min(800L, as.integer(nsim)))
  p_win <- as.numeric(p_win)
  if (!is.finite(p_win) || p_win <= 0 || p_win > 1) p_win <- 0.7
  vol <- as.numeric(vol)
  if (!is.finite(vol) || vol <= 0) vol <- 1

  dlog <- dlog_for(exp_data, cat)
  if (!length(dlog)) stop("No historical premium moves for category ", cat, call. = FALSE)
  p0 <- last_premium(store$bidding, cat)
  pqp0 <- last_pqp(root, cat, p0)
  q_path <- if (isTRUE(tilt)) quota_path_for(store, cat, scenario_id = tilt_scn) else NULL
  if (isTRUE(tilt) && is.null(q_path)) {
    stop("Tilt is on but no forward quota path for Cat ", cat, ".", call. = FALSE)
  }

  sims <- list()
  sims[[cat]] <- funs$simulate_premiums(
    dlog_hist = dlog,
    p0 = p0,
    pqp0 = pqp0,
    start_month = store$latest_bid_month,
    elasticity = elasticity_for(exp_data, cat),
    n_sims = nsim,
    vol_mult = vol,
    supply_tilt = isTRUE(tilt),
    quota_path = q_path
  )
  base <- funs$dealer_exposure(b, sims, p_success = p_win)
  shifts <- funs$timing_shift(b, sims, shifts = c(0, 6, 12), p_success = p_win)

  list(
    book = b,
    base = base,
    shifts = shifts,
    nsim = nsim,
    n_orders = nrow(b),
    category = cat,
    quoted_premium = quoted_premium,
    p_win = p_win,
    vol = vol,
    tilt = isTRUE(tilt),
    tilt_scn = as.character(tilt_scn),
    quoted_total = sum(as.numeric(b$quoted_premium), na.rm = TRUE)
  )
}
