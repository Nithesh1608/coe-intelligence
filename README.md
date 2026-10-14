# COE Intelligence

A predictive analytics prototype for Singapore's Certificate of Entitlement market, built as a Rhino application for DBA3702. The users in view are vehicle dealers, parallel importers, and fleet operators.

The desk is a Rhino / bslib / echarts4r app on the processed tables. No GAM or ARIMA has been fit. The Supply tab shows a seasonal-naive illustration with widening bands. Rebuild the tables with `Rscript scripts/build_processed.R`. That command reads `data/raw/` and does not call a government API. Launch with `rhino::app()`.

## Decision in brief

Monthly deregistrations by COE category can be forecast from a SingStat series that runs from May 1990 to August 2026. Cohort survival cannot. The published tables do not identify which vehicles, in which COE category, renewed or were deregistered. Monthly new registrations by COE category now run from May 1990 to August 2026, including May 2020–December 2024. Monthly age stocks run from January 2016, and monthly revalidations run from January 2015. Age is still by vehicle type, not by Category A versus B. Quota can later be translated from deregistrations with an explicit LTA rule. That translation is not a premium forecast.

## Reproduce the audit

R 4.6.1 is the version in `renv.lock`. Node is installed on the development machine and is not required for this phase.

```r
renv::restore()
Rscript scripts/run_ingest.R
Rscript scripts/run_audit.R
```

`run_ingest.R` reuses `data/raw/` unless you pass `--refresh`. Refreshes call data.gov.sg and LTA DataMall. The application itself does not.

Then:

```r
rhino::lint_r()
rhino::test_r()
rhino::app()
```

## Where to read

| Document | Contents |
| --- | --- |
| [docs/data_audit.md](docs/data_audit.md) | What was measured, and which models are feasible |
| [docs/data_engineering.md](docs/data_engineering.md) | How the processed tables are built |
| [docs/data_quality_report.md](docs/data_quality_report.md) | Checks that the build actually ran |
| [docs/modelling_readiness.md](docs/modelling_readiness.md) | Sample sizes and what not to fit yet |
| [docs/data_dictionary.md](docs/data_dictionary.md) | Columns, categories, and file layouts |
| [docs/data_sources.csv](docs/data_sources.csv) | Source catalogue |
| [docs/assumptions.md](docs/assumptions.md) | Assumptions that later code must not violate |
| [docs/architecture.md](docs/architecture.md) | Rhino layout |
| [docs/team_workflow.md](docs/team_workflow.md) | Branches and review rules |

Methodology, validation, and deployment notes will be written when a model and a deployment exist. Writing them now would invent results.

## Next phase

Build the monthly modelling panel, with a publication date on every predictor, and stop before fitting. Details are at the end of the data audit.
