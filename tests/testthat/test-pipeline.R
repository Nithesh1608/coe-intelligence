box::use(
  testthat[expect_equal, expect_true, test_that],
)
box::use(
  app/logic/viewdata/pipeline[add_months, quarter_start, month_seq, quarter_label],
)

test_that("month arithmetic lands on the first", {
  expect_equal(add_months(as.Date("2026-08-01"), 1L), as.Date("2026-09-01"))
  expect_equal(add_months(as.Date("2026-12-01"), 2L), as.Date("2027-02-01"))
  expect_equal(add_months(as.Date("2026-01-01"), -1L), as.Date("2025-12-01"))
  expect_equal(quarter_start(as.Date("2026-08-01")), as.Date("2026-07-01"))
})

test_that("month_seq is inclusive and quarter labels read naturally", {
  s <- month_seq("2026-11-01", "2027-02-01")
  expect_equal(length(s), 4L)
  expect_equal(s[4], as.Date("2027-02-01"))
  expect_equal(quarter_label(as.Date("2027-04-01")), "Q2 2027")
})
