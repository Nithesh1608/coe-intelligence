# Multi-case verification for the Your risk / exposure calculator.
# Run: Rscript scripts/verify_exposure.R

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(root, "rhino.yml"))) stop("Run from the repo root: Rscript scripts/verify_exposure.R")
options(box.path = root)
set.seed(3702)

pass <- 0L
fail <- 0L
notes <- character()

check <- function(label, cond, detail = "") {
  ok <- isTRUE(cond)
  if (ok) {
    pass <<- pass + 1L
    cat("  PASS  ", label, "\n", sep = "")
  } else {
    fail <<- fail + 1L
    msg <- paste0(label, if (nzchar(detail)) paste0(" — ", detail) else "")
    notes <<- c(notes, msg)
    cat("  FAIL  ", msg, "\n", sep = "")
  }
  invisible(ok)
}

finite_df <- function(df, cols) {
  cols <- intersect(cols, names(df))
  if (!length(cols)) return(FALSE)
  all(vapply(cols, function(c) all(is.finite(as.numeric(df[[c]]))), logical(1)))
}

cat("=== Load engine + data ===\n")
box::use(
  app/logic/exposure/api[load_exposure_funs, load_exposure_data, make_pooled_book],
  app/logic/viewdata/store[load_store, project_root]
)

funs <- load_exposure_funs()
exp_data <- load_exposure_data()
store <- load_store()
root <- project_root()

check("funs loaded", exists("simulate_premiums", envir = funs) &&
  exists("dealer_exposure", envir = funs) &&
  exists("fleet_decision", envir = funs) &&
  exists("timing_shift", envir = funs))
check("premium_moves present", !is.null(exp_data$premium_moves))
check("price_sensitivity present", !is.null(exp_data$price_sensitivity))
check("example_book present", !is.null(exp_data$example_book) && nrow(exp_data$example_book) > 0)
check("example_fleet present", !is.null(exp_data$example_fleet) && nrow(exp_data$example_fleet) > 0)
check("premium_sims_baseline present", !is.null(exp_data$premium_sims_baseline))
check("price_sensitivity is A/B only", all(exp_data$price_sensitivity$category %in% c("A", "B")))
check("forward quota available", !is.null(store$forward_quota) && nrow(store$forward_quota) > 0)
check("bidding / latest month", !is.null(store$bidding) && !is.null(store$latest_bid_month))

dlog_for <- function(cat) {
  m <- exp_data$premium_moves
  d <- m$round_to_round_detail
  d <- d[d$category == cat & is.finite(d$chg_pct), ]
  out <- log1p(as.numeric(d$chg_pct))
  out[is.finite(out)]
}

elasticity_for <- function(cat) {
  s <- exp_data$price_sensitivity
  hit <- s$elasticity_quota[s$category == cat]
  if (!length(hit) || is.na(hit[[1]])) 0 else as.numeric(hit[[1]])
}

last_premium <- function(category) {
  b <- store$bidding
  b <- b[b$category == category & !is.na(b$premium), ]
  if (!nrow(b)) return(100000)
  as.numeric(utils::tail(b[order(b$month), ], 1)$premium[[1]])
}

last_pqp <- function(category, fallback) {
  p <- file.path(root, "data", "processed", "pqp.rds")
  if (!file.exists(p)) return(fallback)
  d <- readRDS(p)
  d$category <- as.character(d$category)
  if (is.numeric(d$month)) d$month <- as.Date(d$month, origin = "1970-01-01")
  else d$month <- as.Date(d$month)
  d <- d[d$category == category & !is.na(d$pqp), ]
  if (!nrow(d)) return(fallback)
  as.numeric(utils::tail(d[order(d$month), ], 1)$pqp[[1]])
}

# Quarterly forward quota (full quarters) from the production forward path.
quota_path_for <- function(cat) {
  fq <- store$forward_quota
  if (is.null(fq) || !nrow(fq)) return(NULL)
  d <- fq[as.character(fq$category) == cat, ]
  d$quarter <- as.Date(d$quarter)
  agg <- stats::aggregate(mid ~ quarter, d, sum)
  n_m <- stats::aggregate(month ~ quarter, d, length)
  agg <- agg[n_m$month[match(agg$quarter, n_m$quarter)] == 3L, ]
  if (nrow(agg) < 2L) return(NULL)
  as.numeric(agg$mid[order(agg$quarter)])
}

build_sims <- function(cats, n_sims = 200L, vol_mult = 1, tilt = TRUE) {
  sims <- list()
  for (cat in cats) {
    dlog <- dlog_for(cat)
    if (!length(dlog)) stop("No dlog for ", cat)
    p0 <- last_premium(cat)
    sims[[cat]] <- funs$simulate_premiums(
      dlog_hist = dlog,
      p0 = p0,
      pqp0 = last_pqp(cat, p0),
      start_month = store$latest_bid_month,
      elasticity = elasticity_for(cat),
      n_sims = n_sims,
      n_ex = 24L,
      vol_mult = vol_mult,
      supply_tilt = tilt,
      quota_path = if (tilt) quota_path_for(cat) else NULL
    )
  }
  sims
}

validate_dealer <- function(label, book, sims, p_success = 0.7) {
  cat("\n=== Dealer: ", label, " ===\n", sep = "")
  out <- tryCatch(
    funs$dealer_exposure(book, sims, p_success = p_success),
    error = function(e) e
  )
  check(paste0(label, " runs"), !inherits(out, "error"),
        if (inherits(out, "error")) conditionMessage(out) else "")
  if (inherits(out, "error")) return(invisible(NULL))

  n_sims <- nrow(sims[[1]]$qp)
  check(paste0(label, " per_order rows"), nrow(out$per_order) == nrow(book) * n_sims)
  check(paste0(label, " portfolio sims"), nrow(out$portfolio) == n_sims)
  check(paste0(label, " summary one row"), nrow(out$summary) == 1L)
  check(
    paste0(label, " summary finite"),
    finite_df(out$summary, c(
      "expected_cost", "expected_dealer_pnl", "var95_loss", "cvar95_loss",
      "prob_loss", "expected_customer_topup", "expected_unfilled"
    ))
  )
  check(paste0(label, " prob_loss in [0,1]"), {
    p <- as.numeric(out$summary$prob_loss[[1]])
    is.finite(p) && p >= 0 && p <= 1
  })
  check(paste0(label, " VaR >= 0"), as.numeric(out$summary$var95_loss[[1]]) >= -1e-6)
  check(paste0(label, " CVaR >= VaR"), {
    as.numeric(out$summary$cvar95_loss[[1]]) + 1e-6 >= as.numeric(out$summary$var95_loss[[1]])
  })
  invisible(out)
}

# --- Cases ---
cat("\n=== Premium path shapes ===\n")
sims_a <- build_sims("A", n_sims = 150L, tilt = TRUE)
check("sim A dims", identical(dim(sims_a$A$qp), c(150L, 24L)) && identical(dim(sims_a$A$pqp), c(150L, 24L)))
check("sim A premiums positive", all(sims_a$A$qp > 0) && all(sims_a$A$pqp > 0))
check("sim A dates length", length(sims_a$A$dates) == 24L)

sims_a_flat <- build_sims("A", n_sims = 150L, tilt = FALSE)
check(
  "tilt changes mean path",
  abs(mean(sims_a$A$qp) - mean(sims_a_flat$A$qp)) > 1 ||
    identical(quota_path_for("A"), NULL) # if no quota path, tilt is no-op
)

# Case 1: small pooled Cat A guaranteed
book1 <- make_pooled_book(10, "A", last_premium("A"))
out1 <- validate_dealer("pooled A n=10", book1, sims_a, p_success = 0.7)

# Case 2: Cat B
sims_b <- build_sims("B", n_sims = 150L, tilt = TRUE)
book2 <- make_pooled_book(8, "B", last_premium("B"))
validate_dealer("pooled B n=8", book2, sims_b, p_success = 0.55)

# Case 3: example AB book (mixed contracts)
book3 <- exp_data$example_book
sims_ab <- build_sims(unique(as.character(book3$category)), n_sims = 120L, tilt = TRUE)
out3 <- validate_dealer("example AB book", book3, sims_ab, p_success = 0.7)

# Case 4: high success → fewer unfilled
out_hi <- funs$dealer_exposure(book1, sims_a, p_success = 0.95)
out_lo <- funs$dealer_exposure(book1, sims_a, p_success = 0.35)
check(
  "higher bid success → fewer unfilled",
  as.numeric(out_hi$summary$expected_unfilled) <= as.numeric(out_lo$summary$expected_unfilled) + 1e-9
)

# Case 5: guaranteed vs rebate on same book
book_g <- book1
book_g$contract <- "guaranteed"
book_r <- book1
book_r$contract <- "rebate"
out_g <- funs$dealer_exposure(book_g, sims_a, p_success = 0.7)
out_r <- funs$dealer_exposure(book_r, sims_a, p_success = 0.7)
check(
  "rebate dealer loss ~ 0",
  abs(as.numeric(out_r$summary$expected_dealer_pnl)) < 1e-6 ||
    all(abs(out_r$per_order$dealer_loss) < 1e-9)
)
check(
  "guaranteed can lose or gain",
  is.finite(as.numeric(out_g$summary$prob_loss))
)

# Case 6: capped contracts stay within cap on upside
book_c <- book1
book_c$contract <- "capped"
book_c$cap_topup <- 3000
out_c <- funs$dealer_exposure(book_c, sims_a, p_success = 0.7)
upside <- out_c$per_order$dealer_loss[out_c$per_order$dealer_loss > 0]
check(
  "capped upside <= cap_topup",
  !length(upside) || max(upside) <= 3000 + 1e-6
)

# Case 7: timing shift
cat("\n=== Timing shift ===\n")
shifts <- funs$timing_shift(book1, sims_a, shifts = c(0, 6, 12), p_success = 0.7)
check("timing_shift 3 rows", nrow(shifts) == 3L)
check(
  "timing_shift finite",
  finite_df(shifts, c("expected_dealer_pnl", "var95_loss", "prob_loss"))
)

# Case 8: vol_mult changes dispersion
sims_calm <- build_sims("A", n_sims = 150L, vol_mult = 0.5, tilt = FALSE)
sims_wild <- build_sims("A", n_sims = 150L, vol_mult = 2.0, tilt = FALSE)
check(
  "higher vol → wider premium range",
  sd(as.numeric(sims_wild$A$qp)) > sd(as.numeric(sims_calm$A$qp))
)

# Case 9: single order edge
book9 <- make_pooled_book(1, "A", last_premium("A"))
validate_dealer("single order", book9, sims_a)

# Case 10: fleet schedule
cat("\n=== Fleet ===\n")
base_sims <- exp_data$premium_sims_baseline
check("baseline sims have A and B", all(c("A", "B") %in% names(base_sims)))

for (sh in c(0L, 1L, 2L)) {
  fl <- tryCatch(
    funs$fleet_decision(exp_data$example_fleet, base_sims, shift_quarters = sh, include_capex = FALSE),
    error = function(e) e
  )
  lab <- paste0("fleet shift=", sh)
  check(paste0(lab, " runs"), !inherits(fl, "error"),
        if (inherits(fl, "error")) conditionMessage(fl) else "")
  if (!inherits(fl, "error")) {
    check(paste0(lab, " rows"), nrow(fl) == nrow(exp_data$example_fleet))
    check(
      paste0(lab, " shares sum ~1"),
      all(abs(fl$share_bid_best + fl$share_10y_best + fl$share_5y_best - 1) < 1e-6)
    )
    check(
      paste0(lab, " costs finite"),
      finite_df(as.data.frame(fl), grep("_mean$", names(fl), value = TRUE))
    )
  }
}

fl_capex <- tryCatch(
  funs$fleet_decision(exp_data$example_fleet, base_sims, shift_quarters = 0, include_capex = TRUE),
  error = function(e) e
)
check("fleet include_capex runs", !inherits(fl_capex, "error"),
      if (inherits(fl_capex, "error")) conditionMessage(fl_capex) else "")

# Case 11: quoted far above market → dealer rarely loses on guaranteed
book_hi <- make_pooled_book(12, "A", last_premium("A") * 1.5)
out_hiq <- funs$dealer_exposure(book_hi, sims_a, p_success = 0.8)
book_lo <- make_pooled_book(12, "A", last_premium("A") * 0.6)
out_loq <- funs$dealer_exposure(book_lo, sims_a, p_success = 0.8)
check(
  "higher quote → lower P(loss)",
  as.numeric(out_hiq$summary$prob_loss) <= as.numeric(out_loq$summary$prob_loss) + 0.05
)

cat("\n=== Summary ===\n")
cat("Passed: ", pass, "\nFailed: ", fail, "\n", sep = "")
if (fail) {
  cat("\nFailures:\n")
  cat(paste0(" - ", notes, collapse = "\n"), "\n")
  quit(status = 1)
}
cat("All exposure cases OK.\n")
