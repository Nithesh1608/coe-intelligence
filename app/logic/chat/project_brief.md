# COE Intelligence — project brief for the assistant

## Product
COE Intelligence is a DBA3702 dealer/fleet **COE supply** desk (Rhino/Shiny). It forecasts Category A and B **deregistrations** and the **quota** they imply. It does **not** forecast premiums. **Your risk** is an exposure calculator for a locked dealer order book or fleet schedule under simulated premium paths (optional supply tilt).

App tabs: Home (quota vs forecast), EDA, Results (deregistrations vs forecast), Your risk, COE bot, About.

## Problem
Dealers who lock COE-inclusive package prices and fleets that replace vehicles in cohorts need forward **supply** visibility with honest uncertainty — not a consumer “bid now” tip. Quota is largely mechanical: COEs last ~10 years; replacement quota tracks recent deregistrations; age stock and 5-year renewals lock part of the pipeline years ahead.

## Data cleaning
Sources: LTA DataMall (bidding results, PQP, LTA deregistrations by quota category, population by age, COE revalidations, car-cost) and SingStat / data.gov.sg (long-run premiums, registrations, deregistrations, population under VQS).

- Bidding: normalised categories; complete month × round × category grid; COVID Apr–Jun 2020 suspensions stay NA (not zero). Bid pressure = bids_received / quota. App monthly view: mean premium, sum quota.
- Regimes: Cat A power cap Feb 2014; A/B EV redefinition May 2022 (`post_2022`).
- Two dereg series: **SingStat VQS** = modelling target; **LTA M05** kept for audits / formula checks — do not mix into y.
- Age stock: numeric bands; Cars pooled A+B in source — A/B share inferred from recent dereg mix for due-stock.
- Revalidations: tenure 5y vs 10y; live model uses lagged reval counts, not a separate renewal simulator.
- Audits under `data/audit/`. Cleaning notebook: `data/data_cleaning.Rmd`.

## Features (live elastic net)
Unit: category-month SingStat deregistrations. Predictors dated strictly before month t (age/PQP from t−1).

Core features: `due10`, `due15`, `due_near` (due-stock / 12 after A/B share); `recent12`, `y_lag2`, `y_lag12`; `reval5_lag12/24/60`; `lpqp` = log(PQP_{t−1}/50000); `post_2022`; `moy_sin`/`moy_cos`.

Train screens (correlation + lasso λ.1se) kept due-stock and lags; reval lags and post_2022 stronger for Cat B. Live model passes the **full** core matrix into elastic net (shrinkage), not a hard-zeroed subset.

## Modelling
- Package: `glmnet::cv.glmnet`, mixing α = 0.5 (elastic net), MAE, λ at `lambda.1se`, pred floored at 0.2 × recent12 then rounded.
- Split: train ≤ 2024-08; locked test ≥ 2024-09 (~24 months). Production rows from 2018-01.
- Live model id: `core_forecast` (labelled Elastic net). Baseline: trailing 12-month mean (`simple_average`).
- Lab earlier also tried ridge, lasso, OLS variants, GAM, seasonal naive with expanding-origin CV inside train; primary pick was elastic net. Momentum variant was removed from the product.
- Quota desk link: calibrate category α = train median of published monthly quota / trailing-12m deregistrations (~0.082 A, ~0.078 B). Outlook = α × D₁₂ (actual dereg before test, model preds in test). Textbook LTA ~25% of trailing 12m with 10% of A/B base to E is for EDA/explanation, not the raw published desk number.

## Exposure (Your risk)
- Premium paths: block-bootstrap historical tender-to-tender log shocks; PQP path from trailing premiums; horizon in exercises (6 per quarter).
- Supply tilt (optional): drift ≈ elasticity × Δlog(Q) / 6 per exercise; estimated quota→premium elasticity ≈ −0.22 for A/B. Tighter supply → upward premium drift.
- Dealer book: guaranteed / capped / rebate contracts; i.i.d. bid success; VaR/CVaR/P(loss); timing shift 0–2 quarters.
- Fleet: quarterly A/B schedule; bid-new vs 5y/10y renew under simulated QP/PQP.

## Honest limits
App loads processed artefacts; it does not refit models online. Full multi-model experiment script/artefacts were stripped from the working tree (recoverable from older git); shipped CSVs are live elastic net + baseline. Proposal.md still mentions some earlier cohort/fan-chart language — prefer About + this brief + code/CSVs for what shipped.
