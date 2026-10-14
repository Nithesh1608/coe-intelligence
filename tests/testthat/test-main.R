box::use(
  testthat[expect_true, test_that],
)
box::use(
  app/logic/viewdata/store[load_store],
)

test_that("processed store loads for the app", {
  store <- load_store()
  expect_true(nrow(store$panel) >= 2190L)
  expect_true(nrow(store$bidding) > 0)
})
