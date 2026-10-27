# COE Intelligence

DBA3702 dealer / fleet **COE supply** desk for **Category A and Category B** only. Forecasts supply and sizes exposure — not premiums.

## App tabs

| Tab | Role |
| --- | --- |
| **Home** | Quota forecast vs released vs simple average (pick Core or Momentum) |
| **Results** | Exit-layer backtest for the selected forecast |
| **Your risk** | Locked book dollars; **stress derived from predicted quota** tightness |
| **How it works** | Method notes |
| **About** | Problem / honesty |
| **EDA** | Placeholder |

## Data

```r
Rscript scripts/run_data_cleaning.R
Rscript scripts/run_model_experiment.R
Rscript scripts/export_two_models_for_app.R
Rscript scripts/export_quota_two_models.R
```

```r
renv::restore()
rhino::app()
```

Proposal: `Project Proposal.md`.
