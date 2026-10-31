box::use(
  shiny[NS, moduleServer, div, tags, renderUI, uiOutput, renderText, textOutput],
)

`%||%` <- function(a, b) {
  if (is.null(a) || !length(a) || (length(a) == 1L && is.na(a))) b else a
}

section <- function(title, ...) {
  tags$section(
    class = "about-section",
    tags$h2(title),
    ...
  )
}

stat <- function(label, value) {
  div(
    class = "about-stat",
    tags$span(class = "eyebrow", label),
    tags$p(value)
  )
}

pct <- function(x, d = 0) if (is.finite(x)) paste0(format(round(100 * x, d), nsmall = d), "%") else "-"
num <- function(x) if (is.finite(x)) format(round(x), big.mark = ",", trim = TRUE) else "-"

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk about-page",
    tags$section(
      class = "about-hero",
      tags$p(class = "eyebrow", "About"),
      tags$h1("COE Intelligence"),
      tags$p(
        class = "lede lede--wide",
        "A supply desk for dealers, parallel importers, and fleets that buy Category A and B certificates ",
        "every month. The main pages show what the outlook means for your book. This page is the method: ",
        "data, model, scenarios, exposure engine, and the limits we design around."
      )
    ),

    div(
      class = "about-stats",
      stat("Horizon", "12 quarters"),
      stat("Categories", "A and B"),
      stat("Live model", "Elastic net"),
      stat("Band", "Bootstrap paths")
    ),

    section(
      "The problem we solve",
      tags$p(
        "Most COE tools serve the individual buyer. Trade buyers face a different risk. ",
        "A dealer who quotes a COE-inclusive package is short certificates until the tender clears: ",
        "if the premium rises between order and win, the dealer absorbs the difference. ",
        "A fleet replacing hundreds of vehicles cares whether that cohort lands in a supply trough or a peak; ",
        "shifting replacement by a quarter or two can move seven-figure totals."
      ),
      tags$p(
        "Both need a view of ",
        tags$strong("supply"),
        " far enough ahead to plan against, expressed as a range rather than a single number."
      )
    ),

    section(
      "Who it is for",
      tags$ul(
        class = "about-list",
        tags$li(
          tags$strong("Dealers and parallel importers. "),
          "Guaranteed, capped, or rebate package deals on a forward order book. ",
          "Your risk turns that book into expected loss, VaR / CVaR, and timing shifts."
        ),
        tags$li(
          tags$strong("Fleet, leasing, and ride-hail operators. "),
          "Quarterly A/B replacement schedules priced against simulated QP / PQP, ",
          "with bid-new versus 5-year / 10-year renew choices."
        ),
        tags$li(
          tags$strong("Out of scope. "),
          "A household deciding whether to bid this fortnight. Consumer Buy / Wait signals are a different product."
        )
      )
    ),

    section(
      "What each page does",
      tags$ul(
        class = "about-list",
        tags$li(
          tags$strong("Home. "),
          "Released certificates against the outlook, then ~36 months forward with 50% and 80% bands and a quarterly table."
        ),
        tags$li(
          tags$strong("Scenarios. "),
          "Renewal share as a fitted function of PQP, a direct renewal lever, premium glide paths, ",
          "LTA injections or clawbacks, and a merged A+B view."
        ),
        tags$li(
          tags$strong("Market. "),
          "Cohort flow, within-quarter premium swings, the A/B spread and volume shares, plus supporting EDA."
        ),
        tags$li(
          tags$strong("Results. "),
          "One-step held-out scores, period totals, and a 24-month walk-forward backtest with 80% band coverage."
        ),
        tags$li(
          tags$strong("Your risk. "),
          "Dealer book / fleet schedule settled against block-bootstrap premium paths, ",
          "optionally tilted by the forward quota path under a chosen scenario."
        ),
        tags$li(
          tags$strong("COE bot. "),
          "Q&A on the live outlook and track record, grounded on desk artefacts (optional NVIDIA NIM phrasing)."
        )
      )
    ),

    section(
      "Why supply is forecastable — and what is not",
      tags$p(
        "Every COE runs about ten years and most cars leave just before the PARF deadline. Quota is a share of ",
        "recent deregistrations; deregistrations are driven by the registration cohort reaching year ten. Those ",
        "cohorts are in the public record, so the next two to three years of due stock are knowable today. ",
        "The 2016-17 registration boom is reaching year ten now, which is why exits surged in 2025-26."
      ),
      tags$p(
        "Two things are not mechanical. The exit rate of a due cohort moves with PQP, and in the data dearer PQP ",
        "means fewer renewals and more exits, not more renewals. And LTA sets quota with discretion over the ",
        "formula: it released more than the rule implied in 2023-25. Both are modelled explicitly and both are in the band."
      ),
      tags$p(
        tags$strong("We forecast supply, not premiums. "),
        "Premiums also depend on demand, credit, and policy. Your risk stresses premium paths with history ",
        "and an optional supply tilt; it does not claim the next clearing price."
      )
    ),

    section(
      "Data sources and cleaning",
      tags$p(
        "Raw extracts come from LTA DataMall (bidding results, PQP, deregistrations by quota category, population by age, ",
        "COE revalidations, the Car Cost Update workbook) and SingStat / data.gov.sg (long-run premiums, registrations, ",
        "deregistrations and population under the Vehicle Quota System). ",
        tags$code("scripts/acquire_data.R"),
        " pulls the two open-API tables with httr2 and documents the browser downloads."
      ),
      tags$ul(
        class = "about-list",
        tags$li(
          tags$strong("Bidding panel. "),
          "Rows are completed over month x round x category so suspended exercises stay as missing values; ",
          "the Apr-Jun 2020 suspension is flagged that way. A month with only one exercise in is marked partial ",
          "and never compared with a forecast."
        ),
        tags$li(
          tags$strong("Regime markers. "),
          "Cat A's power cap (Feb 2014) and the A/B EV redefinition (May 2022) are carried as regime flags; ",
          "the model has a post-2022 indicator."
        ),
        tags$li(
          tags$strong("Two deregistration series. "),
          "SingStat VQS flows are the modelling target. LTA M05 is reconciled against them in an audit table ",
          "and agrees to the unit on the overlap."
        ),
        tags$li(
          tags$strong("Age stock. "),
          "LTA age bands are mapped to numeric ages; cars are pooled A+B at source, so A and B due stock use ",
          "the category's share of recent deregistrations."
        ),
        tags$li(
          tags$strong("Revalidations. "),
          "5-year and 10-year renewals are separated; renewal share is joined to PQP for the response fit."
        )
      )
    ),

    section(
      "Features",
      tags$p(
        "The unit is one category-month of SingStat deregistrations. Every predictor is dated strictly before ",
        "month t. The live desk uses due-stock, short series memory, revalidation lags and PQP / regime / seasonality — ",
        "the recipe that beat a cohort-only / lambda.min experiment on held-out quota MAPE."
      ),
      tags$ul(
        class = "about-list",
        tags$li(
          tags$code("due10"), ", ", tags$code("due15"), ", ", tags$code("due_near"),
          " - cars aged 9-<10, 14-<15 and the neighbouring bands at t-1, scaled by category share and spread over 12 months."
        ),
        tags$li(
          tags$code("recent12"), ", ", tags$code("y_lag2"), ", ", tags$code("y_lag12"),
          " - short memory of the deregistration series itself."
        ),
        tags$li(
          tags$code("reval5_lag12/24/60"),
          " - five-year revalidations at those lags; a 5-year renewal is a locked exit five years later."
        ),
        tags$li(
          tags$code("lpqp"), ", ", tags$code("post_2022"), ", month-of-year sine and cosine."
        )
      ),
      tags$p(
        "Forward rows are built with the same function. Age stock after the last observed month is still projected from ",
        "registration cohorts for bands and scenarios, even though those extra cohort columns are not in the live fit."
      )
    ),

    section(
      "Modelling",
      uiOutput(ns("model_block"))
    ),

    section(
      "Scenarios and the renewal feedback",
      uiOutput(ns("scenario_block"))
    ),

    section(
      "Exposure calculator (Your risk)",
      tags$p(
        "Your risk does not invent a premium forecast. It resamples historical tender-to-tender premium shocks ",
        "(block bootstrap on log changes), optionally tilts those paths by the forward quota path, then settles ",
        "a dealer order book or fleet schedule against the simulated premiums."
      ),
      tags$ul(
        class = "about-list",
        tags$li(
          tags$strong("Premium paths. "),
          "Shocks come from published A/B round-to-round moves since 2010, demeaned and accumulated from the ",
          "latest premium. PQP on the path is a trailing mean of recent premiums. Six exercises per quarter."
        ),
        tags$li(
          tags$strong("Supply tilt. "),
          "Each path gets a drift of elasticity x change in log quota per exercise, with quota-to-premium ",
          "elasticities of about -0.22 estimated on quarterly history, and the quota path taken from Home under ",
          "the scenario you select. Tighter supply pushes simulated premiums up; looser pulls them down."
        ),
        tags$li(
          tags$strong("Dealer book. "),
          "Orders are guaranteed, capped, or rebate contracts. Each order attempts successive tenders with a success ",
          "probability you set; the premium at first win settles it. Outputs are expectation, percentiles, VaR / CVaR, ",
          "P(loss) and the effect of shifting the whole book by one or two quarters."
        ),
        tags$li(
          tags$strong("Fleet. "),
          "A quarterly A/B replacement schedule chooses bid-new versus 5-year / 10-year renew under simulated QP / PQP, ",
          "with optional PARF and amortisation."
        )
      )
    ),

    section(
      "Scope and honesty",
      uiOutput(ns("honesty_block")),
      tags$p(class = "footnote", textOutput(ns("footer"), inline = TRUE))
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    spec <- store$meta$spec %||% list()

    score_of <- function(model, cat) {
      sc <- store$two_quota_scores
      if (is.null(sc)) return(NA_real_)
      v <- sc$MAPE[as.character(sc$model) == model & as.character(sc$category) == cat]
      if (length(v)) as.numeric(v[[1]]) else NA_real_
    }
    cal_of <- function(cat, horizon, col) {
      cal <- store$calibration
      if (is.null(cal)) return(NA_real_)
      v <- cal[[col]][as.character(cal$category) == cat & as.character(cal$horizon) == horizon]
      if (length(v)) as.numeric(v[[1]]) else NA_real_
    }
    alpha_of <- function(cat) {
      a <- store$alphas
      if (is.null(a)) return(NA_real_)
      as.numeric(a$alpha[as.character(a$category) == cat][[1]])
    }

    output$model_block <- renderUI({
      tags$ul(
        class = "about-list",
        tags$li(
          tags$strong("Estimator. "),
          tags$code("glmnet::cv.glmnet"), " with mixing parameter 0.5 (elastic net), MAE loss, penalty at ",
          tags$code(spec$lambda_rule %||% "lambda.min"),
          ". Predictions are floored at 20% of the trailing 12-month mean."
        ),
        tags$li(
          tags$strong("Two fits. "),
          sprintf(
            "An evaluation fit trained to %s and scored on %s onward (24 months), and a production fit on every month through %s that drives the forward path.",
            format(as.Date(spec$train_end %||% "2024-08-01"), "%b %Y"),
            format(as.Date(spec$test_start %||% "2024-09-01"), "%b %Y"),
            format(as.Date(spec$last_observed_month %||% "2026-08-01"), "%b %Y")
          )
        ),
        tags$li(
          tags$strong("Specification choice. "),
          "The collaborator realignment tried a cohort-only set at ",
          tags$code("lambda.min"),
          " for a more mechanical forward path. The live desk keeps the shipped held-out artefacts from the earlier due-stock + ",
          "short-memory fit at ",
          tags$code("lambda.1se"),
          " (quota MAPE about 12% on A and B). Scenarios, bootstrap bands and the forward engine from the realignment are kept; ",
          "refresh them with ",
          tags$code("scripts/rebuild_forward_from_shipped.R"),
          " without overwriting those scores."
        ),
        tags$li(
          tags$strong("One-step held-out quota error. "),
          sprintf(
            "Elastic net %s (A) and %s (B) against %s and %s for the trailing-average baseline.",
            pct(score_of("core_forecast", "A")), pct(score_of("core_forecast", "B")),
            pct(score_of("simple_average", "A")), pct(score_of("simple_average", "B"))
          )
        ),
        tags$li(
          tags$strong("Recursive 13-24 month error. "),
          sprintf(
            "Deregistrations %s (A, bias %+.0f%%) and %s (B, bias %+.0f%%); quota %s (A) and %s (B).",
            pct(cal_of("A", "13-24 months", "dereg_mape")), cal_of("A", "13-24 months", "dereg_bias_pct"),
            pct(cal_of("B", "13-24 months", "dereg_mape")), cal_of("B", "13-24 months", "dereg_bias_pct"),
            pct(cal_of("A", "13-24 months", "quota_mape")), pct(cal_of("B", "13-24 months", "quota_mape"))
          )
        ),
        tags$li(
          tags$strong("Quota mapping. "),
          sprintf(
            "Monthly quota = alpha x trailing 12-month deregistrations, alpha the training-window median of published / trailing exits: %.3f for A and %.3f for B (the textbook 25%% annual rule is 0.019 monthly).",
            alpha_of("A"), alpha_of("B")
          ),
          " LTA's discretion over the formula is measured as the relative residual of that mapping and resampled into the quota band."
        ),
        tags$li(
          tags$strong("Bands. "),
          sprintf(
            "%s recursive paths per category from three earned sources: each month's prediction is multiplied by one plus a block-resampled held-out relative residual; each path uses one of %s refits of the model on block-resampled training windows (parameter uncertainty); and each path carries its own PQP random walk built from historical monthly PQP changes. Quota paths then add the mapping residual (sd %s for A, %s for B) in six-month blocks. Reported bands are the 25-75 and 10-90 percentiles.",
            spec$bootstrap_sims %||% "500", spec$bootstrap_refits %||% "100", spec$mapping_resid_sd_A %||% "-", spec$mapping_resid_sd_B %||% "-"
          )
        )
      )
    })

    output$scenario_block <- renderUI({
      r <- store$renewal_response
      slope_txt <- if (is.null(r) || !nrow(r)) "" else sprintf(
        "On 2015-2026 LTA revalidations, renewal share falls %.1f points (A) and %.1f points (B) for every 10%% rise in PQP (R² %.2f and %.2f).",
        100 * abs(r$slope_log_pqp[r$category == "A"]) * log(1.1), 100 * abs(r$slope_log_pqp[r$category == "B"]) * log(1.1),
        r$r2[r$category == "A"], r$r2[r$category == "B"]
      )
      bf <- suppressWarnings(as.numeric(spec$brought_forward_since_2023 %||% NA))
      tags$ul(
        class = "about-list",
        tags$li(
          tags$strong("Baseline. "),
          "The production forward path with PQP held at its last value."
        ),
        tags$li(
          tags$strong("Renewal response. "),
          slope_txt,
          " The proposal assumed the opposite sign; the data decided. A scenario PQP path changes renewal share through ",
          "this slope, and each extra renewal removes one exit from the due cohort that month."
        ),
        tags$li(
          tags$strong("Levers. "),
          "A direct renewal shift in share points, premium glides to $80k or $160k by the end of the horizon, an LTA ",
          "injection of certificates per quarter, and a merged A+B view."
        ),
        tags$li(
          tags$strong("Clawback. "),
          if (is.finite(bf)) sprintf(
            "Since Feb 2023 LTA has published about %s more A/B certificates than the formula implies. The clawback preset removes that stock evenly over the next eight quarters.",
            num(bf)
          ) else "A preset removes the certificates LTA brought forward since 2023 over eight quarters."
        ),
        tags$li(
          tags$strong("Propagation. "),
          "Quota is rebuilt as alpha x trailing-12 exits after the change, so a renewal shift today reaches quota over the ",
          "following twelve months. Scenarios move the central path and carry the baseline band with it."
        )
      )
    })

    output$honesty_block <- renderUI({
      tags$ul(
        class = "about-list",
        tags$li(
          tags$strong("We forecast supply, not premiums. "),
          "Premiums also depend on demand, credit, and policy. Your risk stresses premiums with history and an optional supply drift; it does not predict the next clearing price."
        ),
        tags$li(
          tags$strong("Where the model is weak. "),
          sprintf(
            "In the recursive backtest the 80%% quota band covered %s (A) and %s (B) of months at 1-6 months, where LTA's 2024-25 releases ran above the formula. Category B exits were under-called by %+.0f%% in the first year. These are on Results, not hidden.",
            pct(cal_of("A", "1-6 months", "quota_cover80")), pct(cal_of("B", "1-6 months", "quota_cover80")),
            cal_of("B", "7-12 months", "dereg_bias_pct")
          )
        ),
        tags$li(
          tags$strong("Known data breaks. "),
          "A/B were redefined around EV power in May 2022; bidding was suspended Apr-Jun 2020. Both are handled explicitly."
        ),
        tags$li(
          tags$strong("Reproducible. "),
          tags$code("Rscript scripts/run_data_cleaning.R"), " then ", tags$code("Rscript scripts/fit_and_forecast.R"),
          " rebuild modelling artefacts. To refresh forward bands without overwriting the shipped held-out scores, use ",
          tags$code("scripts/rebuild_forward_from_shipped.R"),
          ". The app only loads artefacts; it does not refit."
        )
      )
    })

    output$footer <- renderText({
      sprintf(
        "DBA3702 - Cat A/B supply desk - open LTA / SingStat data - elastic net alpha 0.5, %s - last observed deregistrations %s.",
        spec$lambda_rule %||% "lambda.min", format(as.Date(spec$last_observed_month %||% "2026-08-01"), "%b %Y")
      )
    })
  })
}
