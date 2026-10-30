# COE Intelligence — assistant brief

## Product
DBA3702 dealer/fleet **COE supply** desk (Rhino/Shiny). Forecasts Cat A/B **deregistrations** and implied **quota**. Does **not** forecast premiums. **Your risk** = exposure calculator for a locked dealer book / fleet schedule (simulated premium paths, optional supply tilt).

Tabs: Home, EDA, Results, Your risk, COE bot, About.

## Cleaning
Sources: LTA DataMall + SingStat/data.gov.sg.
- Bidding: complete month×round×category; COVID Apr–Jun 2020 stay NA; bid pressure = bids/quota.
- Regimes: Cat A power cap Feb 2014; A/B EV redefine May 2022 (`post_2022`).
- **SingStat VQS** = modelling target `y`. **LTA M05** audits only — never mix into y.
- Age stock Cars pooled A+B; A/B share from recent dereg mix for due-stock.
- Revals: 5y vs 10y; live model uses lagged reval counts.

## Features (elastic net)
Unit: category-month SingStat deregistrations. Predictors dated **strictly before** t.
`due10`, `due15`, `due_near`; `recent12`, `y_lag2`, `y_lag12`; `reval5_lag12/24/60`; `lpqp`=log(PQP_{t−1}/50000); `post_2022`; `moy_sin`/`moy_cos`.
Live model passes the **full** core matrix into elastic net (shrinkage), not a hard-zeroed subset.

## Modelling
- `glmnet::cv.glmnet`, α=0.5, MAE, λ=`lambda.1se`; pred floored at 0.2×recent12 then rounded.
- Train ≤ 2024-08; locked test ≥ 2024-09 (~24m). Production from 2018-01.
- Live id `core_forecast` (Elastic net). Baseline `simple_average` (trailing 12m mean).
- Quota desk: α = train median(published monthly quota / trailing-12m dereg) ≈ 0.082 A, ≈ 0.078 B. Outlook = α × D₁₂.
- Textbook LTA ~25% of trailing 12m (10% A/B base to E) is EDA-only — not the published desk number.

## Exposure
Block-bootstrap premium paths; optional supply tilt with elasticity ≈ −0.22 A/B. Dealer: guaranteed/capped/rebate, VaR/CVaR. Fleet: bid-new vs 5y/10y renew.

## Limits
App loads artefacts; does not refit online. Prefer this brief + live score tables over older proposal wording.
