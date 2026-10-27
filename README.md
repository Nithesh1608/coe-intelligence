# COE Intelligence

DBA3702 dealer / fleet **COE supply** desk. Forecasts supply and sizes exposure — not premiums.

Aligned with `Project Proposal.md`:

| Proposal view | App tab |
| --- | --- |
| Supply pipeline | **Supply** |
| Feedback and scenarios | **Feedback** |
| Category dynamics | **Categories** |
| Exposure calculator | **Exposure** |

Plus **Backtest**, **How it works**, **About**, **EDA**.

## Data

| Path | Role |
| --- | --- |
| `data/data_cleaning.Rmd` | Cleaning notebook |
| `data/raw/` | Official extracts |
| `data/processed/` | Cleaned RDS tables |
| `data/processed/model_experiment/` | Forecast artefacts |

```r
Rscript scripts/run_data_cleaning.R
Rscript scripts/run_model_experiment.R
Rscript scripts/export_two_models_for_app.R
Rscript scripts/export_quota_two_models.R
```

## App

```r
renv::restore()
rhino::app()
```

**Honesty (from the proposal):** we forecast supply and quantify exposure under uncertainty. We do not claim to forecast premiums.
