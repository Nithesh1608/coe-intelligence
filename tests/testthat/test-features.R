box::use(
  testthat[expect_equal, expect_true, expect_length, expect_named, test_that],
)
box::use(
  app/logic/model/features[
    CATS, COHORT_COLS, BASE_COLS, age_stock_matrix, cohort_survival, cohort_regs,
    project_age_stock, due_features, feature_row, dereg_share, reg_total_series
  ],
)

# A synthetic fleet: every month 1,000 A+B cars are registered, and the
# observed stock is 12,000 cars in every band 0..19 (full survival) and
# 5,000 in the 20+ band.
make_stock <- function(months = 15L) {
  mo <- seq(as.Date("2025-01-01"), by = "month", length.out = months)
  rows <- list()
  for (m in as.list(mo)) {
    for (k in 0:20) {
      rows[[length(rows) + 1L]] <- data.frame(
        month = m, vehicle_type = "Cars", coe_group = "A+B", age_lower = k,
        population = if (k == 20L) 5000 else 12000, stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

make_flows <- function() {
  mo <- seq(as.Date("1995-01-01"), by = "month", to = as.Date("2026-03-01"))
  rbind(
    data.frame(month = mo, category = "A", registrations = 600, deregistrations = 300, stringsAsFactors = FALSE),
    data.frame(month = mo, category = "B", registrations = 400, deregistrations = 200, stringsAsFactors = FALSE)
  )
}

test_that("age stock matrix is months x 21 bands", {
  st <- age_stock_matrix(make_stock())
  expect_equal(dim(st$mat), c(15L, 21L))
  expect_equal(unname(st$mat[1, 10]), 12000)
})

test_that("cohort survival recovers a flat fleet exactly", {
  st <- age_stock_matrix(make_stock())
  reg <- reg_total_series(make_flows())
  s <- cohort_survival(st, reg)
  expect_length(s, 20L)
  expect_true(all(abs(s - 1) < 1e-9))
  expect_equal(cohort_regs(reg, as.Date("2026-03-01"), 9L), 12000)
})

test_that("projection keeps a flat fleet flat and holds the 20+ band", {
  st <- age_stock_matrix(make_stock())
  reg <- reg_total_series(make_flows())
  proj <- project_age_stock(st, 6L, reg)
  expect_equal(dim(proj), c(6L, 21L))
  expect_true(all(abs(proj[, 1:20] - 12000) < 1e-6))
  expect_true(all(proj[, 21] == 5000))
})

test_that("due features scale the year-10 band by share over twelve months", {
  row <- rep(12000, 21)
  f <- due_features(row, share = 0.5)
  expect_equal(unname(f[["due10"]]), 12000 * 0.5 / 12)
  expect_equal(unname(f[["cohort"]]), unname(f[["due10"]] + f[["due15"]]))
})

test_that("deregistration share splits A and B", {
  expect_equal(dereg_share(make_flows(), "A", as.Date("2026-03-01")), 0.6)
})

test_that("feature_row produces every predictor the model needs", {
  flows <- make_flows()
  st <- age_stock_matrix(make_stock())
  stock_at <- function(m) {
    key <- as.character(as.Date(m))
    if (key %in% rownames(st$mat)) as.numeric(st$mat[key, ]) else rep(12000, 21)
  }
  y <- stats::setNames(rep(300, 200), as.character(seq(as.Date("2009-08-01"), by = "month", length.out = 200)))
  rv <- stats::setNames(rep(50, 200), names(y))
  reg <- stats::setNames(rep(600, 400), as.character(seq(as.Date("1993-01-01"), by = "month", length.out = 400)))
  f <- feature_row("A", as.Date("2026-03-01"), y, stock_at, 0.6, rv, 60000, reg)
  expect_true(all(BASE_COLS %in% names(f)))
  expect_true(all(COHORT_COLS %in% names(f)))
  expect_equal(unname(f[["reg_lag120"]]), 600)
  expect_equal(unname(f[["recent12"]]), 300)
  expect_equal(unname(f[["lpqp"]]), log(60000 / 50000))
  expect_equal(unname(f[["post_2022"]]), 1)
})
