box::use(
  testthat[expect_equal, expect_identical, test_that],
)
box::use(
  app/logic/data/parse[blank_to_na, classify_period, singstat_month],
)

test_that("SingStat month labels are first-of-month dates", {
  expect_equal(singstat_month("1990May"), as.Date("1990-05-01"))
  expect_equal(singstat_month("2026Sep"), as.Date("2026-09-01"))
})

test_that("official missing tokens become NA and zeros do not", {
  parsed <- blank_to_na(c("na", "-", "0", "12", ""))
  expect_identical(parsed, c(NA_character_, NA_character_, "0", "12", NA_character_))
})

test_that("LTA period cells keep years separate from months", {
  expect_identical(classify_period("2024")$kind, "year")
  expect_identical(classify_period("2026 (Jan-Aug)")$kind, "partial_year")
  expect_equal(classify_period("2025/01")$month, as.Date("2025-01-01"))
  serial <- classify_period(45658)
  expect_identical(serial$kind, "month")
  expect_equal(serial$month, as.Date("2025-01-01"))
})
