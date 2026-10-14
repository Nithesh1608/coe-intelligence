# Team workflow

The project is a Rhino application with one R codebase. Do not add a Python model service or a Next.js front end.

## Branches

Use `main` for reviewed work. Open feature branches from it:

| Branch | Work |
| --- | --- |
| `data-engineering` | Parsers, the monthly panel, validation of joins |
| `predictive-modelling` | Scripts that fit and backtest models and write artifacts |
| `application-development` | Rhino modules, charts, and the simulator |
| `business-analysis` | Quota scenarios, cost calculations, and the written business plan |

Keep model training on the modelling branch, in scripts. The application branch should read saved output.

## Rules that protect the statistics

- Do not edit files in `data/raw/`. Write derived tables to `data/processed/` or `data/audit/`.
- Do not impute a history that an agency did not publish.
- Do not use a value dated after the forecast origin as a predictor.
- Do not describe a quota scenario as a premium forecast.
- If a backtest does not favour the more complicated model, say so.

## Dependencies

Add packages to `dependencies.R`, then install them with renv and commit `renv.lock`. R 4.6.1 is the version recorded in the lockfile.

## Checks

`rhino::lint_r()` and `rhino::test_r()` are the current checks. GitHub Actions runs both on `main` and on pull requests. Frontend lint is intentionally absent until a view needs JavaScript or Sass.

## Commit

Phase 1 has not been committed. Commit when the group wants this audit on `main`, then branch from that commit.
