box::use(
  testthat[expect_equal, expect_true, expect_error, expect_lte, test_that],
)
box::use(
  app/logic/exposure/api[load_exposure_funs, make_pooled_book, parse_order_book, parse_fleet_schedule],
  app/logic/viewdata/store[project_root],
)

funs <- load_exposure_funs(project_root())

flat_sims <- function(p0, n_sims = 50L, n_ex = 12L) {
  list(A = list(
    qp = matrix(p0, n_sims, n_ex),
    pqp = matrix(p0, n_sims, n_ex),
    dates = seq(as.Date("2026-11-04"), by = "14 days", length.out = n_ex)
  ))
}

test_that("order book parser enforces the schema and drops non A/B rows", {
  b <- data.frame(
    order_id = c("1", "2", "3"), category = c("A", "b", "E"), quoted_premium = 120000,
    target_exercise = 1L, max_attempts = 3L, contract = c("guaranteed", "Capped", "guaranteed")
  )
  out <- parse_order_book(b)
  expect_equal(nrow(out), 2L)
  expect_equal(out$category, c("A", "B"))
  expect_equal(out$contract, c("guaranteed", "capped"))
  expect_equal(out$cap_topup, c(5000, 5000))
  expect_error(parse_order_book(b[, -3]), "needs columns")
  bad <- b
  bad$contract <- "swap"
  expect_error(parse_order_book(bad), "Unknown contract")
})

test_that("fleet parser fills optional capex columns", {
  f <- parse_fleet_schedule(data.frame(quarter = 1:2, category = "A", n_vehicles = 10, parf_rebate = 8000))
  expect_equal(f$new_vehicle_cost, c(0, 0))
  expect_error(parse_fleet_schedule(data.frame(quarter = 1, category = "E", n_vehicles = 1, parf_rebate = 0)), "No Cat A/B")
})

test_that("with certain wins and flat premiums the dealer loss is the quote gap", {
  book <- make_pooled_book(4, "A", quoted_premium = 100000)
  sims <- flat_sims(110000)
  out <- funs$dealer_exposure(book, sims, p_success = 1)
  expect_equal(unique(out$per_order$dealer_loss), 10000)
  expect_equal(out$summary$expected_cost, 4 * 110000)
  expect_equal(out$summary$expected_dealer_pnl, -40000)
  expect_equal(out$summary$prob_loss, 1)
  expect_equal(unname(out$summary$var95_loss), 40000)
})

test_that("contract types cap or remove the dealer's loss", {
  book <- make_pooled_book(3, "A", quoted_premium = 100000)
  book$contract <- c("guaranteed", "capped", "rebate")
  book$cap_topup <- 2000
  out <- funs$dealer_exposure(book, flat_sims(110000), p_success = 1)
  loss <- tapply(out$per_order$dealer_loss, out$per_order$contract, unique)
  expect_equal(unname(loss[["guaranteed"]]), 10000)
  expect_equal(unname(loss[["capped"]]), 2000)
  expect_equal(unname(loss[["rebate"]]), 0)
})

test_that("timing shift reports one summary per shift in quarters", {
  book <- make_pooled_book(2, "A", quoted_premium = 100000)
  out <- funs$timing_shift(book, flat_sims(100000, n_ex = 24L), shifts = c(0, 6, 12), p_success = 1)
  expect_equal(out$shift_quarters, c(0, 1, 2))
  expect_true(all(is.finite(out$expected_cost)))
})

test_that("simulate_premiums honours the supply tilt direction", {
  set.seed(1)
  dlog <- rnorm(300, 0, 0.02)
  base <- funs$simulate_premiums(dlog, 100000, 100000, as.Date("2026-10-01"), elasticity = -0.2,
                                 n_ex = 12, n_sims = 400, supply_tilt = FALSE)
  tight <- funs$simulate_premiums(dlog, 100000, 100000, as.Date("2026-10-01"), elasticity = -0.2,
                                  n_ex = 12, n_sims = 400, supply_tilt = TRUE, quota_path = c(1000, 800, 800))
  expect_equal(dim(base$qp), c(400L, 12L))
  # Quota falls 20% in quarter two: with negative elasticity premiums drift up.
  expect_true(mean(tight$qp[, 12]) > mean(base$qp[, 12]))
})
