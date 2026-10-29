# COE Intelligence

DBA3702 dealer/fleet **COE supply** desk. Forecasts Category A/B exits and formula quota — not premiums. **Your risk** is an exposure calculator for a locked order book / fleet schedule (simulated premium paths, optional supply tilt).

## Run

```r
renv::restore()
rhino::app()
```

## Tabs

| Tab | Purpose |
| --- | --- |
| Home | Desk landing — supply at a glance |
| Results | Core / Momentum forecasts + quota outlook |
| Your risk | Exposure calculator (dealer book + fleet) |
| How it works | Method in plain language |
| About | Product brief |
| EDA | Exploratory charts |

## Data

| Path | Role |
| --- | --- |
| `data/data_cleaning.Rmd` | Cleaning notebook |
| `data/raw/` | Official extracts |
| `data/processed/` | Cleaned RDS + app forecast CSVs |
| `data/processed/exposure/` | Exposure calculator inputs |
| `app/logic/exposure/` | Exposure engine + API |

```r
Rscript scripts/run_data_cleaning.R
Rscript scripts/export_two_models_for_app.R
Rscript scripts/build_quota_outlook.R
```

Proposal: `Project Proposal.md`. EDA: `EDA 1.Rmd`, `EDA 2.R`.
