box::use(
  testthat[expect_equal, expect_true, test_that],
)
box::use(
  app/logic/viewdata/pipeline[replacement_quota, add_months, quarter_start],
)

test_that("replacement quota sends 10 percent of A B C to E", {
  dereg <- c(A = 4000, B = 4000, C = 2000, D = 800)
  q <- replacement_quota(dereg, pop_c_year_end = 160000)
  expect_equal(unname(q[["A"]]), 0.9 * 0.25 * 4000)
  expect_equal(unname(q[["D"]]), 0.25 * 800)
  expect_equal(unname(q[["E"]]), 0.1 * (1000 + 1000 + 500))
  expect_true(q[["C"]] > 0.9 * 500)
})

test_that("month arithmetic lands on the first", {
  expect_equal(add_months(as.Date("2026-08-01"), 1L), as.Date("2026-09-01"))
  expect_equal(quarter_start(as.Date("2026-08-01")), as.Date("2026-07-01"))
})
