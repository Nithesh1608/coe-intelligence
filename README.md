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
| COE bot | Free-form Q&A via NVIDIA NIM, grounded on brief + held-out scores |
| About | Cleaning, features, models, exposure |

### COE bot / NVIDIA NIM

Free-form chat via NVIDIA NIM (`deepseek-ai/deepseek-v4.1-flash` by default). Each turn
sends a slim system pack: `project_brief.md` + live quota/hit-rate scores. Calls go through
system `curl` (R/libcurl often stalls on this host).

- **Local:** gitignored `.Renviron` with `NVIDIA_API_KEY=`… and optional `NVIDIA_NIM_MODEL=`.
- **Posit Connect Cloud:** content **Variables** → secret `NVIDIA_API_KEY` (do not commit the key).
- If NIM is unreachable, the bot posts the error in the thread instead of hanging silently.

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
