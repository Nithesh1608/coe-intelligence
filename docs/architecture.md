# Architecture

COE Intelligence is a Rhino 1.12 application on R 4.6.1. The Shiny desk has five views on the processed tables. Seasonal-naive bands on the Supply tab are an illustration, not a fitted Model B. Training still belongs in a later script.

## Layout

| Path | Role |
| --- | --- |
| `app/main.R` | Rhino root: Overview, Supply, Feedback, Categories, Exposure |
| `app/view/` | Shiny modules for those five desks |
| `app/logic/viewdata/` | Load processed CSVs, quota identity, seasonal-naive path |
| `app/logic/data/catalogue.R` | Official dataset IDs and DataMall URLs |
| `app/logic/data/ingest.R` | Download snapshots, or keep files already saved |
| `app/logic/data/parse.R` | Dates, missing tokens, wide SingStat files, bidding CSV, Excel grids |
| `app/logic/data/audit.R` | Measurements written to `data/audit/` |
| `app/logic/data/standardise.R` | Long extracts with stable category codes |
| `app/logic/data/build_processed.R` | Panel, bidding history, age, quota, model frame |
| `app/logic/data/checks.R` | Structural checks that stop the build |
| `scripts/run_ingest.R` | Command-line ingest |
| `scripts/run_audit.R` | Command-line audit |
| `scripts/build_processed.R` | Rebuild `data/processed/` from `data/raw/` |
| `data/raw/` | Unmodified official files |
| `data/audit/` | Quality tables produced by the audit |
| `data/processed/` | Modelling tables written by the build |
| `dependencies.R` | Packages renv and rsconnect should see |
| `renv.lock` | Locked package versions |

Modules use `box::use()`. Ingest and audit are batch steps. They are not called from `app/main.R`, so opening the app does not download data or fit a model.

## What starts with the app

`rhino::app()` loads `data/processed/` and renders the desk. It does not download data and does not train a model.

## Later boundaries

The processed tables are already under `data/processed/`. The Shiny process does not build them and does not fit a model.

Phase 3 should write model objects and backtest tables to disk from a script. The app should load those artifacts.

Quota rules, when encoded, belong in versioned functions under `app/logic/`, one function per rule period. The current LTA formula is documented in `docs/data_audit.md` and is not implemented yet.

Styles live in `app/styles/main.scss`. Charts are echarts4r. The generated GitHub workflow still runs R lint and R tests only.

## Reproduce the audit

From the project root, after `renv::restore()` on a clean machine:

```r
Rscript scripts/run_ingest.R
Rscript scripts/run_audit.R
```

`run_ingest.R` without `--refresh` records files that are already on disk and downloads only missing ones. `--refresh` calls data.gov.sg and DataMall again. The public API rate-limits unauthenticated calls, so a refresh should not be looped.
