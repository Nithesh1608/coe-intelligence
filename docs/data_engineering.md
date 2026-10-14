# Data engineering

Rebuild everything in `data/processed/` from the raw files:

```r
Rscript scripts/build_processed.R
```

The script does not download data and does not fit a model. If a structural check fails, it stops before writing the tables.

## What was joined

| Output | Grain | Sources |
| --- | --- | --- |
| `coe_monthly_panel.csv` | Month by category A–E | Deregistrations, VQS population, new registrations, revalidations, PQP, and monthly sums of bidding exercises |
| `coe_bidding_history.csv` | Exercise by category | SingStat quota file before January 2010; DataMall bidding file from January 2010 |
| `vehicle_age_distribution.csv` | Month by vehicle type by age band | DataMall monthly age CSV only |
| `coe_quota_history.csv` | Exercise, month, and quarter | The bidding history, summed. Official quarterly adjustments are not in the raw files |
| `coe_model_frame.csv` | Month by category A–D | The panel plus lagged features |

Category E is on the panel because it has a bidding record. Its deregistrations, population, and registrations are left missing. They are not estimated from Categories A–C.

Taxis, VQS-exempt vehicles, and weekend cars stay out of the panel. They are not bidding categories. Weekend cars end in 1998. Category C on the panel is the category total. Its Early Turnover parts are extra columns, not extra rows, and are not added to the total.

Pre-2010 bidding comes from the SingStat quota/premium extract. From February 2002 that series is complete: 95 months, two exercises, five categories, 950 rows.

## Overlap decisions

On every month, exercise, and category present in both bidding extracts, quota, successful bids, bids received, and premium match. The build stops if they do not. DataMall is kept from January 2010 because it includes October 2026 and the two corrected 2010 cells already checked against the LTA workbook.

PQP uses the DataMall month, which is the month the PQP applies. The SingStat PQP column is one month earlier. After that shift, the overlap matches. Months before February 2010 use the shifted SingStat values. There is no open-category PQP.

## Publication lags

A figure for month *m* may be used to predict month *t* only when *m + lag* is on or before *t*. These lags are in `publication_lags()`.

| Series | Lag | Why |
| --- | --- | --- |
| Deregistrations, VQS population, registrations, monthly age | 2 months | SingStat and DataMall monthly stocks are not treated as known in the following month. This snapshot’s deregistrations and age end in August 2026 |
| Revalidations | 3 months | LTA describes a two-month reporting lag, and this snapshot’s revalidations end in July 2026, one month behind deregistrations |
| Premiums, quotas, bids | 1 month | Exercise results are published in the bidding month, so they are not treated as known at the start of that month |
| PQP | 0 months | The DataMall month is the month the PQP is for, and the workbook shows it on the previous exercise sheet |

The shortest deregistration lag in the model frame is therefore two months. A one-month deregistration lag is not created. April–June 2020 premiums stay missing.

The model frame is safe for a forecast issued at the start of the target month. It is not safe to read a later row during a multi-step backtest. A forecast from origin *O* of target *O + h* must use only inputs whose source month plus publication lag is on or before *O*. Inside the horizon, deregistrations, population, registrations, renewals, premiums, and PQP have to be replaced by recursive forecasts or by scenarios. That step belongs to model training, which this build does not do.

## Features on the model frame

| Column | Definition | Scope |
| --- | --- | --- |
| `dereg_lag_2`, `dereg_lag_3`, `dereg_lag_12`, `dereg_lag_13` | Deregistrations at t−2, t−3, t−12, t−13 | Category |
| `dereg_roll_mean_12` | Mean of deregistrations from t−13 through t−2. Missing if any month in the window is missing | Category |
| `population_lag_2` | VQS population at t−2 | Category |
| `registrations_lag_2` | New registrations at t−2. Category C uses the category total | Category |
| `revalidations_total_lag_3` | 5-year plus 10-year revalidations at t−3 | Category |
| `pqp` | PQP applying in month t | Category. Missing for any month without a published PQP |
| `premium_mean_lag_1` | Mean of the exercise premiums in t−1. One exercise is enough; a missing exercise is not treated as zero | Category |
| `bid_ratio_lag_1` | Bids received divided by quota, summed across exercises in t−1 | Category |
| `quota_sum_lag_1` | Sum of exercise quotas in t−1 | Category |
| `age_share_8_to_11_lag_2` | Share of the vehicle-type stock aged 8 to under 11, at t−2. Missing if any band in that type-month is missing | A and B both use Cars. C uses goods and other vehicles. D uses motorcycles. Buses are not joined |
| `month_of_year` | Calendar month | Known |
| `rule_ab_power_cap` | 1 from February 2014 | Known from the effective month |
| `rule_ab_ev` | 1 from May 2022 | Known from the effective month |
| `rule_cat_e_shift` | 1 from May 2017 | Quota rule, not a deregistration measurement |
| `rule_growth_freeze` | 1 from February 2018 through January 2028 | Stated policy window |
| `bidding_suspended` | 1 in April, May, and June 2020 | Historical fact for those months |

Age shares for A and B are the same car stock. They do not identify which cars are Category A.

## What was not done

No value was interpolated. No extreme deregistration month was dropped. Category D in September 2002 stays missing. Blank bus ages stay missing. Policy-adjustment lines stay missing rather than zero. Individual vehicles were not reconstructed.
