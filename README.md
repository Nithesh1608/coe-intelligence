# COE Intelligence

DBA3702 dealer/fleet **COE supply** desk. Forecasts Category A/B deregistrations and formula quota — not premiums. **Your risk** is an exposure calculator for a locked order book / fleet schedule (simulated premium paths, optional supply tilt).

## Run

```r
renv::restore()
rhino::app()
```

## Tabs

| Tab | Purpose |
| --- | --- |
| Home | Desk landing — supply at a glance |
| EDA | Exploratory charts |
| Results | Elastic-net deregistrations + quota outlook |
| Your risk | Exposure calculator (dealer book + fleet) |
| COE bot | NVIDIA NIM assistant grounded on this project |
| About | Cleaning, features, models, exposure |

Local NIM (gitignored `.Renviron`): set `NVIDIA_API_KEY`, optionally `NVIDIA_NIM_MODEL`
(default `deepseek-ai/deepseek-v4.1-flash`). Open that model on [build.nvidia.com](https://build.nvidia.com)
once and accept / Get API Key so the account can call it.

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

Proposal: `Project Proposal.md`. EDA story lives in the app (`app/view/eda.R`, `app/logic/viewdata/eda_story.R`).
