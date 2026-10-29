box::use(
  shiny[NS, moduleServer, div, tags],
)

section <- function(title, ...) {
  tags$section(
    class = "about-section",
    tags$h2(title),
    ...
  )
}

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk about-page",
    tags$p(class = "eyebrow", "About"),
    tags$h1("COE Intelligence"),
    tags$p(
      class = "lede lede--wide",
      "Category A/B supply forecasts for dealers, importers, and fleets, plus an exposure desk ",
      "for a locked order book. We forecast deregistrations and the quota they imply. ",
      "We do not forecast premiums; Your risk prices book risk under simulated premium paths."
    ),

    section(
      "Data cleaning",
      tags$p(
        "Raw extracts come from LTA DataMall (bidding results, PQP, LTA deregistrations by quota category, ",
        "population by age, COE revalidations, car-cost sheets) and SingStat / data.gov.sg ",
        "(long-run premiums, registrations, deregistrations, population under the Vehicle Quota System)."
      ),
      tags$ul(
        class = "about-list",
        tags$li(
          tags$strong("Bidding panel. "),
          "Category labels are normalised (e.g. “Category A” → A). Rows are completed over month × ",
          "bidding round × category so suspended exercises stay as missing values, not zeros — ",
          "COVID suspensions in Apr–Jun 2020 are flagged that way. Bid pressure is ",
          tags$code("bids_received / quota"),
          ". The app’s monthly view averages premiums and sums quota across the two exercises."
        ),
        tags$li(
          tags$strong("Regime markers. "),
          "Cat A’s power-cap change (Feb 2014) and the A/B EV redefinition (May 2022) are attached ",
          "as regime flags so later models can treat the post-2022 world separately."
        ),
        tags$li(
          tags$strong("Two deregistration series. "),
          "SingStat VQS flows are the modelling target (Cat A/B monthly exits from the road). ",
          "LTA M05 is kept in parallel and reconciled in an audit table — useful for formula-quota ",
          "checks, but not mixed into the response variable."
        ),
        tags$li(
          tags$strong("Age stock. "),
          "LTA age bands are mapped to numeric lower/upper ages; “20 & above” is unified. ",
          "Cars are stocked as a pooled A+B group in the source, so A vs B shares are inferred ",
          "from recent deregistration mix when we build due-stock features."
        ),
        tags$li(
          tags$strong("Revalidations. "),
          "Tenure is parsed from the revalidation type (5-year vs 10-year). Renewal rates are joined ",
          "to PQP for diagnostics; the live forecast path uses lagged revalidation counts as predictors, ",
          "not a separate renewal simulator."
        ),
        tags$li(
          tags$strong("Audits. "),
          "Registration − deregistration vs Δpopulation, and SingStat vs LTA deregistrations, ",
          "are written under ",
          tags$code("data/audit/"),
          " so identity breaks stay visible."
        )
      )
    ),

    section(
      "Feature engineering and selection",
      tags$p(
        "The unit of prediction is one category-month of SingStat deregistrations. ",
        "Every predictor is dated strictly before month ",
        tags$em("t"),
        " (age stock and PQP from ",
        tags$em("t−1"),
        ") so the fit cannot peek at the outcome."
      ),
      tags$p(tags$strong("Core block (both models)")),
      tags$ul(
        class = "about-list",
        tags$li(
          tags$code("due10"), " / ", tags$code("due15"), " / ", tags$code("due_near"),
          " — cars in the year-10, year-15, and nearby age bands at ",
          tags$em("t−1"),
          ", scaled by the A/B deregistration share and spread over 12 months. ",
          "This is the mechanical “due stock” that should scrap or renew soon."
        ),
        tags$li(
          tags$code("recent12"), ", ", tags$code("y_lag2"), ", ", tags$code("y_lag12"),
          " — short memory of the deregistration series itself (trailing mean and lags)."
        ),
        tags$li(
          tags$code("reval5_lag12/24/60"),
          " — five-year revalidations at those lags; a 5-year renewal today is a locked deregistration ",
          "five years later."
        ),
        tags$li(
          tags$code("lpqp"),
          " — log of lagged PQP relative to a S$50k reference; a cheap renewal incentive signal."
        ),
        tags$li(
          tags$code("post_2022"),
          " — indicator for the EV/category redefinition, plus month-of-year sine/cosine for seasonality."
        )
      ),
      tags$p(
        "Train-era exploratory screens (correlation and a pure lasso at ",
        tags$code("λ.1se"),
        ") consistently kept due-stock and recent lags; revalidation lags and ",
        tags$code("post_2022"),
        " mattered more for Cat B. The live model does not hard-zero those screens — ",
        "it passes the full feature matrix into an elastic net and lets shrinkage decide."
      )
    ),


    section(
      "Modelling",
      tags$ul(
        class = "about-list",
        tags$li(
          tags$strong("Estimator. "),
          tags$code("glmnet::cv.glmnet"),
          " with mixing parameter ",
          tags$code("α = 0.5"),
          " (elastic net: ridge stability + lasso sparsity), loss = MAE, ",
          "penalty chosen at ",
          tags$code("λ.1se"),
          ". Predictions are floored at 20% of ",
          tags$code("recent12"),
          " then rounded."
        ),
        tags$li(
          tags$strong("Split. "),
          "Fit through August 2024; held-out score from September 2024 onward (24 months in the shipped files). ",
          "Production rows start from 2018 so early sparse age history does not dominate."
        ),
        tags$li(
          tags$strong("Live model. "),
          "One elastic net on the core feature set. ",
          "Baseline on Results is the trailing 12-month mean of deregistrations — a naive comparator, not a competitor model."
        ),
        tags$li(
          tags$strong("Quota mapping. "),
          "Textbook LTA replacement is roughly a quarter of trailing 12-month deregistrations, with 10% of A/B ",
          "base shared into Cat E. The desk does not publish that identity raw. Instead we calibrate a category-specific ",
          "scale ",
          tags$code("α"),
          " as the train-era median of ",
          tags$em("published monthly quota / trailing-12m deregistrations"),
          " (~0.082 for A, ~0.078 for B). Outlook quota = ",
          tags$code("α × D₁₂"),
          " where ",
          tags$code("D₁₂"),
          " splices actual deregistrations before the test window with model predictions inside it."
        )
      )
    ),

    section(
      "Exposure calculator (Your risk)",
      tags$p(
        "Your risk does not invent a premium forecast. It resamples historical tender-to-tender premium shocks ",
        "(block bootstrap on log changes), optionally tilts those paths by a supply scenario, then settles ",
        "a dealer order book or fleet schedule against the simulated premiums."
      ),
      tags$ul(
        class = "about-list",
        tags$li(
          tags$strong("Premium paths. "),
          "Shocks come from published A/B round-to-round moves. Paths are built by accumulating demeaned ",
          "log shocks from the latest premium/PQP; PQP on the path is a trailing mean of recent premiums. ",
          "Horizon is measured in exercises (six per quarter)."
        ),
        tags$li(
          tags$strong("Supply tilt. "),
          "When enabled, each path gets a drift ",
          tags$code("elasticity × Δlog(Q) / 6"),
          " per exercise, using estimated quota→premium elasticities (about −0.22 for A/B) and a baseline ",
          "multi-quarter quota path. Tighter forecast supply → positive premium drift; looser supply → the opposite. ",
          "That is how predicted supply enters pricing — as a directional stress on simulated premiums, ",
          "not as a point forecast of the clearing price."
        ),
        tags$li(
          tags$strong("Dealer book. "),
          "Orders are guaranteed, capped, or rebate contracts. Each order attempts successive tenders with ",
          "a success probability you set; realised premium at the first win (or last attempt) determines ",
          "dealer loss vs the quoted package. Portfolio summaries are expectation, percentiles, VaR / CVaR, ",
          "and P(loss). Timing shift moves the whole book by 0–2 quarters on the same paths."
        ),
        tags$li(
          tags$strong("Fleet. "),
          "A quarterly A/B replacement schedule chooses bid-new vs 5y/10y renew under simulated QP/PQP, ",
          "with optional PARF and amortisation. Same supply-tilted paths, mid-quarter exercise index."
        )
      ),
      tags$p(
        class = "footnote",
        "DBA3702 · Cat A/B supply desk · models fitted offline; the app loads processed artefacts only."
      )
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    invisible(store)
  })
}
