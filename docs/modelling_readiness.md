# Modelling readiness

No model has been trained. The target is monthly deregistrations. Categories A–D are the modelling categories. Category E has no deregistration series.

The model frame is one row per target month. Its features are known by the start of that month under the lags in `docs/data_engineering.md`. A multi-step backtest must not read a future row’s lags as if they were already known.

## Samples, from the built frame

| Sample | Rows | What is required | Span |
| --- | --- | --- | --- |
| Baseline | 1,743 | Observed deregistrations | A–C: May 1990–August 2026, 436 months each. D: 435 months, missing September 2002 |
| Seasonal lags | 1,679 | Baseline plus the two-month lag, the twelve-month lag, and the trailing mean | A–C from June 1991, 423 months. D: 410 months, because the September 2002 hole removes every window that contains it |
| Flow lags | 1,679 | Seasonal sample plus population and registrations at lag 2 | Same span. Those two series cover the deregistration history |
| Price lags | 1,151 | Flow sample plus PQP, lagged premium, and lagged bid ratio | A–C: March 2002–August 2026, 291 months. D: 278 months. April–June 2020 drop out because the premium is missing |
| Renewals | 536 | Price sample plus revalidations at lag 3 | April 2015–August 2026, 134 months in each category |
| Age share | 492 | Price sample plus the 8-to-under-11 age share at lag 2 | March 2016–August 2026, 123 months in each category |

A and B share one car age stock. That feature cannot separate the two categories.

## What can be fit next

- Seasonal naive, ETS, and ARIMA on Categories A–D, using the baseline sample. Compare a monthly fit with a quarterly sum. Keep September 2002 missing for Category D, and keep the 2020 deregistration months.
- A GAM or penalised regression on the price sample, with category indicators, seasonal lags, population, registrations, PQP, and lagged premiums. Renewal and age terms can be added only on their shorter samples, or with explicit missingness, not by filling the earlier years.
- Do not fit a survival or cohort model. The age file is by vehicle type, and renewals are not linked to a registration cohort.
- Do not fit XGBoost. Each category is one monthly series.
- Do not treat a quota scenario as a premium forecast. Premiums enter the deregistration model only as lagged, already published values. Future premiums in the business simulator stay assumptions.

## Usable forecast origin

The latest observed deregistration month in this snapshot is August 2026. With a two-month publication lag, that is the latest target month whose outcome is on the frame. September and October 2026 are on the panel for bidding only.
