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

## Features (live desk elastic net)
Unit: category-month SingStat deregistrations, predictors dated strictly before t.
`due10`, `due15`, `due_near`; `recent12`, `y_lag2`, `y_lag12`; `reval5_lag12/24/60`; `lpqp` = log(PQP_{t−1}/50000); `post_2022`; `moy_sin`/`moy_cos`.
(The cohort-only experiment without AR terms was tried and lost on held-out quota MAPE; scaffolding still projects age stock for bands/scenarios.)

## Modelling
- `glmnet::cv.glmnet`, α=0.5, MAE, λ=`lambda.1se`; pred floored at 0.2×recent12.
- **Held-out scores are the shipped artefacts** from the pre-realignment panel (`feature_panel.csv` + `two_models_*.csv`): quota MAPE ≈ **11.9% A / 11.6% B**. Do not overwrite those CSVs when refreshing forward bands.
- Evaluation window: train 2018-01 … 2024-08; held-out 2024-09 onward (24 m). Production fit on the shipped panel rolls ~36–40 months forward (`scripts/rebuild_forward_from_shipped.R`).
- Bands: recursive residual bootstrap + refits + PQP walk + quota-mapping residual.
- Quota: α = train median(published monthly quota / trailing-12m dereg). Outlook = α × D₁₂.
- Recursive backtest with MAPE / bias / 80% coverage is on Results.

## Scenarios
Renewal share ~ log(PQP) fitted per category on 2015–2026 revalidations: slope ≈ −0.17 (A), −0.12 (B) — dearer PQP means fewer renewals. Presets: baseline, +750 injection/qtr, premium → $80k, premium → $160k, clawback of brought-forward stock over 8 quarters, renewal +5 pts. Merged A+B view available.

## Exposure
Block-bootstrap premium paths; optional supply tilt with elasticity ≈ −0.22 A/B using the forward quota path. Dealer: guaranteed/capped/rebate, VaR/CVaR. Fleet: bid-new vs 5y/10y renew.

## Limits
App loads artefacts; does not refit online. Rebuild with `Rscript scripts/fit_and_forecast.R`.
