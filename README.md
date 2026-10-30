# COE Intelligence

DBA3702 dealer/fleet **COE supply** desk. Forecasts Category A/B deregistrations and the quota they imply twelve quarters ahead with bootstrap bands, lets you move that path with renewal, premium and policy scenarios, and prices the exposure of a locked order book or fleet schedule. It does **not** forecast premiums.

## Run

```r
renv::restore()
rhino::app()
```

## Tabs

| Tab | Proposal view | Purpose |
| --- | --- | --- |
| Home | Supply pipeline | Released quota vs the held-out model, then the production model rolled 36 months forward with 50% / 80% bootstrap bands and a quarterly table |
| Scenarios | Feedback and scenarios | Renewal share as a fitted function of PQP, renewal lever, premium glides, LTA injection or clawback, merged A+B |
| Market | Category dynamics | Cohort flow, within-quarter premium swings, A/B spread and volume shares, supporting EDA |
| Results | Calibration | One-step held-out scores, period totals where the model missed, 24-month recursive backtest with 80% coverage |
| Your risk | Exposure calculator | Dealer book and fleet schedule priced on simulated premium paths, optionally tilted by the forward quota path |
| COE bot | | Instant desk answers on forward quota / accuracy; optional NIM for open phrasing |
| About | | Data, cleaning, features, model, scenarios, exposure, limits |

## Rebuild everything

```r
Rscript scripts/acquire_data.R                  # data.gov.sg tables via httr2; prints the manual DataMall / SingStat steps
Rscript scripts/run_data_cleaning.R             # knits data/data_cleaning.Rmd -> data/processed/*.rds
Rscript scripts/fit_and_forecast.R              # full rebuild of model artefacts (overwrites held-out scores)
Rscript scripts/rebuild_forward_from_shipped.R  # refresh forward bands / scenarios without overwriting shipped scores
Rscript scripts/verify_exposure.R               # multi-case check of the exposure engine
```

Shipped held-out scores live under `data/processed/model_experiment/` (`two_models_*.csv`, `feature_panel.csv`). Prefer `rebuild_forward_from_shipped.R` for day-to-day forward refreshes. The app only loads artefacts; it never refits.

## Tests

```r
rhino::test_r()
```

Covers date helpers, feature engineering and cohort projection, quota mapping and band summaries, the scenario layer (renewal shift, premium glide, injection, clawback split), the exposure engine on deterministic cases, and the consistency of the shipped forward artefacts.

## Layout

| Path | Role |
| --- | --- |
| `app/logic/model/` | Features, elastic net + recursive bootstrap, scenario layer (shared by scripts, app and tests) |
| `app/logic/viewdata/` | Store loader and chart preparation |
| `app/logic/exposure/` | Exposure engine + API |
| `app/logic/eda/` | Static EDA sources and renderer |
| `app/view/` | Shiny modules, one per tab |
| `data/data_cleaning.Rmd` | Cleaning notebook |
| `data/raw/` | Official extracts (DataMall, SingStat); `data/raw/datagov/api/` holds httr2 pulls |
| `data/processed/` | Cleaned RDS + model artefacts (`model_experiment/`) + exposure inputs (`exposure/`) |

Proposal: `Project Proposal.md`.
