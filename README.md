# COE Intelligence

DBA3702 prototype: a dealer/fleet **supply** desk for Singapore COE. Premiums are not forecast.

## Data

| Path | Role |
| --- | --- |
| `data/Project.Rmd` | Cleaning notebook |
| `data/raw/datagov/` | SingStat / data.gov extracts |
| `data/raw/datamall/` | LTA DataMall extracts |
| `data/processed/` | Cleaned tables (RDS) |
| `data/audit/` | Quality log and reconcile checks |
| `reports/cleaning_summary.md` | Cleaning report |

Rebuild cleaned tables from the project root:

```r
Rscript scripts/run_project_rmd.R
```

## App

```r
renv::restore()
rhino::app()
```

The desk still expects the previous CSV panel names. Point it at the new RDS tables before relying on the hosted app.

Proposal: `Project Proposal.md`.
