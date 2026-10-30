box::use(
  testthat[expect_equal, expect_true, expect_gt, expect_lt, test_that, skip_if_not],
)
box::use(
  app/logic/model/forecast[
    trailing12, quota_from_dereg, band_summary, calibration_table, published_monthly_quota
  ],
  app/logic/model/features[add_months],
  app/logic/viewdata/store[project_root],
)

test_that("trailing12 needs a complete window", {
  months <- as.character(seq(as.Date("2025-01-01"), by = "month", length.out = 12))
  y <- stats::setNames(rep(100, 12), months)
  expect_equal(trailing12(y, as.Date("2025-12-01")), 1200)
  expect_true(is.na(trailing12(y, as.Date("2026-01-01"))))
})

test_that("quota_from_dereg splices history and forecast", {
  months <- as.character(seq(as.Date("2025-01-01"), by = "month", length.out = 12))
  y <- stats::setNames(rep(100, 12), months)
  fwd_months <- add_months(as.Date("2025-12-01"), 1:3)
  q <- quota_from_dereg(y, fwd_months, c(200, 200, 200), alpha = 0.1)
  # Jan 2026 window: 11 x 100 history + 200 forecast
  expect_equal(q[1, 1], 0.1 * (1100 + 200))
  expect_equal(q[1, 3], 0.1 * (900 + 600))
})

test_that("band_summary brackets the point path", {
  point <- c(100, 110, 120)
  sims <- matrix(c(90, 100, 110, 130, 95, 140), nrow = 2, byrow = TRUE)
  b <- band_summary(point, sims)
  expect_true(all(b$lo80 <= b$mid & b$mid <= b$hi80))
  expect_true(all(b$lo80 <= b$lo50 & b$hi50 <= b$hi80))
})

test_that("calibration_table reports coverage per horizon bucket", {
  bt <- data.frame(
    category = "A", h = 1:12,
    dereg_actual = 100, dereg_mid = c(rep(100, 6), rep(120, 6)),
    dereg_lo80 = c(rep(90, 6), rep(110, 6)), dereg_hi80 = c(rep(110, 6), rep(130, 6)),
    quota_published = 50, quota_mid = 50, quota_lo80 = 45, quota_hi80 = 55
  )
  cal <- calibration_table(bt)
  expect_equal(nrow(cal), 2L)
  expect_equal(cal$dereg_cover80[cal$horizon == "1-6 months"], 1)
  expect_equal(cal$dereg_cover80[cal$horizon == "7-12 months"], 0)
  expect_equal(cal$dereg_bias_pct[cal$horizon == "7-12 months"], 20)
})

test_that("shipped forward artefacts are internally consistent", {
  root <- project_root()
  fq_path <- file.path(root, "data", "processed", "model_experiment", "forward_quota.csv")
  skip_if_not(file.exists(fq_path), "forward_quota.csv not built")
  fq <- utils::read.csv(fq_path, stringsAsFactors = FALSE)
  fd <- utils::read.csv(file.path(root, "data", "processed", "model_experiment", "forward_dereg.csv"), stringsAsFactors = FALSE)
  for (cat in c("A", "B")) {
    d <- fq[fq$category == cat, ]
    d <- d[order(d$month), ]
    expect_true(nrow(d) >= 36L)
    expect_true(all(d$lo80 <= d$mid + 1e-6 & d$mid <= d$hi80 + 1e-6))
    expect_true(all(d$mid > 0))
    # Twelve full quarters are available for the scenario view.
    full <- table(d$quarter)
    expect_true(sum(full == 3L) >= 12L)
    # Recursive error compounds: the relative deregistration band widens with horizon.
    e <- fd[fd$category == cat, ]
    e <- e[order(e$month), ]
    rel <- (e$hi80 - e$lo80) / e$mid
    expect_gt(mean(rel[25:36]), mean(rel[1:12]))
  }
  alpha <- utils::read.csv(file.path(root, "data", "processed", "model_experiment", "quota_calibration_alpha.csv"))
  expect_true(all(alpha$alpha > 0.05 & alpha$alpha < 0.12))
})
