# COE Intelligence — assistant brief

## Product
DBA3702 dealer/fleet **COE supply** desk (Rhino/Shiny). Forecasts Cat A/B **deregistrations** and implied **quota** 36 months ahead with bootstrap bands. Does **not** forecast premiums. **Your risk** = exposure calculator for a locked dealer book / fleet schedule (simulated premium paths, optional supply tilt from the forward quota path under a chosen scenario). Category E is out of scope.

Tabs: Home, Scenarios, Market, Results, Your risk, COE bot, About.

## Cleaning
Sources: LTA DataMall + SingStat/data.gov.sg. `scripts/acquire_data.R` pulls the data.gov.sg tables via httr2.
- Bidding: complete month×round×category; COVID Apr–Jun 2020 stay NA; a latest month with one exercise is **partial** and never compared with forecasts.
- Regimes: Cat A power cap Feb 2014; A/B EV redefine May 2022 (`post_2022`).
- **SingStat VQS** = modelling target `y`. **LTA M05** audits only.
- Age stock Cars pooled A+B; A/B share from recent dereg mix for due-stock.

## Features (cohort elastic net, no autoregressive terms)
Unit: category-month SingStat deregistrations, predictors dated strictly before t.
`due10`, `due15`, `due_near`, `pop_young`, `pop_renewed` (age stock × share / 12); `reg_lag120/180/240` (own registrations 10/15/20 years earlier); `y_lag12`; `reval5_lag12/24/60`; `lpqp` = log(PQP_{t−1}/50000); `post_2022`; `moy_sin`/`moy_cos`.
Forward age stock is projected from registration cohorts × survival factors calibrated on the latest observed stock.

## Modelling
- `glmnet::cv.glmnet`, α=0.5, MAE, λ=`lambda.min`; pred floored at 0.2×recent12.
- Evaluation fit: train 2017-02 … 2024-08; held-out 2024-09 onward (24 m). Production fit: all months through the last observed; rolled 36 months forward.
- Bands: 500 recursive paths combining block-bootstrapped held-out residuals, 100 refits on block-resampled training windows (parameter uncertainty) and a PQP random walk per path; quota paths also carry the quota-mapping residual (LTA discretion) in 6-month blocks.
- Quota: α = train median(published monthly quota / trailing-12m dereg) ≈ 0.080 A, 0.078 B. Outlook = α × D₁₂.
- Recursive 24-month backtest from Aug 2024 with MAPE, bias and 80% coverage by horizon is on Results. Specification was chosen on that window.

## Scenarios
Renewal share ~ log(PQP) fitted per category on 2015–2026 revalidations: slope ≈ −0.17 (A), −0.12 (B) — dearer PQP means fewer renewals. Presets: baseline, +750 injection/qtr, premium → $80k, premium → $160k, clawback of brought-forward stock over 8 quarters, renewal +5 pts. Merged A+B view available.

## Exposure
Block-bootstrap premium paths; optional supply tilt with elasticity ≈ −0.22 A/B using the forward quota path. Dealer: guaranteed/capped/rebate, VaR/CVaR. Fleet: bid-new vs 5y/10y renew.

## Limits
App loads artefacts; does not refit online. Rebuild with `Rscript scripts/fit_and_forecast.R`.
