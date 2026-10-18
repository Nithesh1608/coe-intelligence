# Cleaning report

`data/Project.Rmd` was run on the teammate files. Layout matches the earlier `data/raw`, `data/processed`, `data/audit` split. Full knit log: `reports/cleaning_report.md`. Row counts: `data/audit/data_quality_log.csv`.

## Folder layout

| Path | What it is |
| --- | --- |
| `data/raw/datagov/` | SingStat / data.gov extracts. Do not edit. |
| `data/raw/datamall/` | LTA DataMall extracts. Do not edit. |
| `data/Project.Rmd` | Cleaning notebook. |
| `data/processed/` | Cleaned tables from that notebook. |
| `data/audit/` | Quality log and reconcile checks. |
| `reports/` | This note plus the knit log. |

Re-run: `Rscript scripts/run_project_rmd.R`

## Checks that passed

- Apr–Jun 2020 bidding rows are missing premiums (not zeros).
- Age bands unify to 21 labels.
- Car Cost Update (Aug 2026): 217 models; ARF / GST / total cost match LTA within $2.
- Workbook Cat A/B premiums match the first exercise in `coe_results`: A $123,890, B $129,910.

## Cleaned tables

| Table | Rows | From | To | File |
| --- | ---: | --- | --- | --- |
| Bidding results | 2,020 | 2010-01 | 2026-10 | `data/processed/coe_results.rds` |
| PQP | 804 | 2010-02 | 2026-10 | `data/processed/pqp.rds` |
| SingStat quota / premium / PQP | 3,552 | 2002-02 | 2026-09 | `data/processed/singstat_coe.rds` |
| Registrations, exits, stock | 3,488 | 1990-05 | 2026-08 | `data/processed/vehicle_flows.rds` |
| Stock-flow residual check | — | — | — | `data/audit/vehicle_flow_check.rds` |
| LTA deregistrations | 912 | 2014-01 | 2026-08 | `data/processed/dereg_lta.rds` |
| LTA vs SingStat exits | — | — | — | `data/audit/dereg_reconcile.rds` |
| Population by age | 10,752 | 2016-01 | 2026-08 | `data/processed/pop_by_age.rds` |
| Revalidations | 1,112 | 2015-01 | 2026-07 | `data/processed/revalidation.rds` |
| Renewal rate vs PQP | — | — | — | `data/processed/renewal_rate.rds` |
| Car cost (Aug 2026) | 217 | 2026-08 | 2026-08 | `data/processed/car_cost_2026_08.rds` |
| Car cost archive | 217 | 2026-08 | 2026-08 | `data/processed/car_cost_archive.rds` |
| OMV by category | 2 | 2026-08 | 2026-08 | `data/processed/omv_by_category.rds` |

OMV medians (Aug 2026): Cat A $28,492 (58% EV among listed models); Cat B $44,383 (41% EV).

The hosted desk still expects the old CSV names under `data/processed/`. Those files are gone until the app is pointed at these RDS tables.
