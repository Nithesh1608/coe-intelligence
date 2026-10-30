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
| COE bot | Instant desk Q&A from the project brief + held-out scores |
| About | Cleaning, features, models, exposure |

### COE bot

Answers are built **in-app** (no remote LLM): a topic router over the fixed project brief
plus live tables from `two_models_quota_scores.csv` / hit rates. Fast on Posit and local;
no API key required. Optional NIM helpers remain under `app/logic/chat/nim.R` if you want
to wire a free-form model later.

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
