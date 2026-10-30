box::use(
  testthat[expect_equal, expect_true, expect_lt, expect_gt, test_that],
)
box::use(
  app/logic/model/scenarios[
    renewal_response, scenario_presets, pqp_glide, apply_scenario, rollup_quarters, injection_split
  ],
  app/logic/model/features[add_months],
)

make_forward <- function() {
  # Oct 2026 to Sep 2027: four full quarters.
  months <- add_months(as.Date("2026-09-01"), 1:12)
  data.frame(
    month = months, mid = 1000, lo50 = 950, hi50 = 1050, lo80 = 900, hi80 = 1100,
    due_flow = 2000, pqp_base = 100000, stringsAsFactors = FALSE
  )
}

make_hist <- function() {
  months <- seq(as.Date("2025-01-01"), by = "month", length.out = 21)
  stats::setNames(rep(1000, 21), as.character(months))
}

response <- data.frame(category = "A", slope_log_pqp = -0.15, intercept = 2, stringsAsFactors = FALSE)

test_that("presets include baseline, injection and both premium shocks", {
  ids <- vapply(scenario_presets(), function(p) p$id, character(1))
  expect_true(all(c("baseline", "inject_750", "premium_80k", "premium_160k") %in% ids))
})

test_that("pqp glide ends at the target", {
  g <- pqp_glide(rep(100000, 10), 80000)
  expect_equal(g[10], 80000)
  expect_equal(pqp_glide(rep(100000, 3), NA_real_), rep(100000, 3))
})

test_that("baseline scenario reproduces the baseline path", {
  out <- apply_scenario(make_forward(), make_hist(), alpha = 0.08, response)
  expect_equal(out$dereg_scenario, out$dereg_base)
  expect_equal(out$quota_scenario, out$quota_base)
  expect_equal(out$quota_base[1], 0.08 * 12 * 1000)
})

test_that("a renewal shift cuts exits by shift x due cohort and quota follows", {
  out <- apply_scenario(make_forward(), make_hist(), alpha = 0.08, response, renewal_shift = 0.05)
  expect_equal(out$dereg_scenario, out$dereg_base - 0.05 * 2000)
  # Quota in month h reflects h months of lower exits in its trailing window.
  expect_equal(out$quota_scenario[3] - out$quota_base[3], -0.08 * 3 * 100)
})

test_that("falling premiums raise renewals and tighten supply under the fitted sign", {
  out <- apply_scenario(make_forward(), make_hist(), alpha = 0.08, response, pqp_target = 80000)
  expect_true(all(out$renewal_share_delta[-1] > 0))
  expect_lt(sum(out$quota_scenario), sum(out$quota_base))
})

test_that("injection adds certificates without touching exits", {
  out <- apply_scenario(make_forward(), make_hist(), alpha = 0.08, response, injection_per_qtr = 300)
  expect_equal(out$dereg_scenario, out$dereg_base)
  expect_equal(out$quota_scenario - out$quota_base, rep(100, 12))
  q <- rollup_quarters(out)
  expect_equal(nrow(q), 4L)
  expect_equal(q$quota_scenario - q$quota_base, rep(300, 4))
})

test_that("injection split follows recent published shares", {
  pub <- data.frame(
    month = rep(seq(as.Date("2026-01-01"), by = "month", length.out = 6), each = 2),
    category = rep(c("A", "B"), 6), published = rep(c(600, 400), 6), stringsAsFactors = FALSE
  )
  sp <- injection_split(pub, 1000)
  expect_equal(unname(sp[["A"]]), 600)
  expect_equal(unname(sp[["B"]]), 400)
})

test_that("renewal response fits one slope per category", {
  set.seed(1)
  months <- seq(as.Date("2015-01-01"), by = "month", length.out = 60)
  rr <- rbind(
    data.frame(month = months, category = "A", pqp = exp(seq(10, 11.5, length.out = 60)),
               renewal_rate = 0.4 - 0.1 * seq(0, 1.5, length.out = 60) + rnorm(60, 0, 0.005)),
    data.frame(month = months, category = "B", pqp = exp(seq(10, 11.5, length.out = 60)),
               renewal_rate = 0.3 - 0.05 * seq(0, 1.5, length.out = 60) + rnorm(60, 0, 0.005))
  )
  r <- renewal_response(rr)
  expect_equal(r$category, c("A", "B"))
  expect_lt(r$slope_log_pqp[1], -0.08)
  expect_gt(r$slope_log_pqp[2], -0.08)
})
