box::use(
  testthat[expect_equal, expect_false, expect_true, test_path, test_that],
)
box::use(
  app/logic/data/build_processed[build_all, publication_lags],
  app/logic/data/standardise[shift_month],
)

test_that("month shifts stay on the first of the month", {
  origin <- as.Date("2020-01-01")
  expect_equal(shift_month(origin, 1L), as.Date("2020-02-01"))
  expect_equal(shift_month(origin, -1L), as.Date("2019-12-01"))
  expect_equal(shift_month(origin, 0L), origin)
})

test_that("deregistration features wait for publication", {
  lags <- publication_lags()
  expect_true(lags[["deregistrations"]] >= 2L)
  expect_true(lags[["revalidations"]] >= 2L)
  expect_true(lags[["bidding"]] >= 1L)
})

test_that("processed tables pass structural checks", {
  root <- normalizePath(file.path(test_path(), "..", ".."))
  built <- build_all(
    raw_dir = file.path(root, "data", "raw"),
    out_dir = tempfile("coe-processed-")
  )
  expect_true(all(built$checks$status == "pass"))
  expect_false("dereg_lag_1" %in% names(built$model_frame))
  category_e <- built$panel$category == "E"
  expect_true(all(is.na(built$panel$registrations[category_e])))
  expect_true(all(is.na(built$panel$deregistrations[category_e])))
  expect_equal(nrow(built$panel), 2190L)
  expect_equal(length(unique(built$age$vehicle_type)), 4L)
})
