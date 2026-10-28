# COE Intelligence

DBA3702 dealer/fleet **COE supply** desk. Forecasts Category A/B exits and formula quota — not premiums.

## Data

| Path | Role |
| --- | --- |
| `data/data_cleaning.Rmd` | Cleaning notebook |
| `data/raw/` | Official extracts |
| `data/processed/` | Cleaned RDS tables |
| `data/processed/model_experiment/` | CV / test scores, forecasts, quota checks |
| `data/audit/` | Quality / reconcile checks |

```r
Rscript scripts/run_data_cleaning.R
Rscript scripts/run_model_experiment.R
Rscript scripts/summarise_dereg_examples.R
```

## App

```r
renv::restore()
rhino::app()
```

| Tab | Purpose |
| --- | --- |
| **Desk** | Landing dashboard — actual vs forecast at a glance |
| **Forecast** | Month examples + LTA-style quota from exits |
| **Method** | Technical ideas in plain language |
| **EDA** | Placeholder for exploratory charts |
| **Exposure** | Order-book cost under premium stress (not a premium forecast) |
| **About** | Product / problem brief |

Proposal: `Project Proposal.md`.
