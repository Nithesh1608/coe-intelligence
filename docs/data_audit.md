# Phase 1 data audit

Snapshot date: 14 October 2026. Measurements in this note come from the local files in `data/raw/` and the tables in `data/audit/`. No series was filled in, and no forecast was fit.

The workspace had no course brief in `docs/reference/`.

## Decision

Individual-level cohort survival is not identifiable from the published files. Do not build Model C.

The defensible predictive target is monthly vehicle deregistrations by COE category. The long series for that target is the SingStat extract, May 1990 through August 2026. Quota is a later accounting translation of those deregistrations, plus published policy adjustments. It is not a premium forecast.

Recommended next models, after a cleaned panel exists:

- Model A: seasonal naive, ETS, and ARIMA, fit separately for Categories A, B, C, and D. Compare a monthly fit with a quarterly aggregation, because the quota rule uses a trailing 12-month total.
- Model B: a GAM or penalised regression that may use only information dated on or before the forecast origin. Candidate predictors are lagged deregistrations, month-of-year, lagged category population, lagged premiums or PQP, lagged monthly revalidations from January 2015, and lagged monthly age-band stocks from January 2016. Those age stocks are by vehicle type, not by Category A versus Category B. XGBoost is not justified.
- Model C: do not fit. An aggregate stock-flow scenario may be shown later as an accounting identity, labelled as an assumption, not as a survival model.

No claim is made that Model B will beat Model A. That comparison is the point of the later backtest.

## What was obtained

| Role | File | What it actually covers |
| --- | --- | --- |
| Deregistrations | `datagov/deregistrations_vqs_monthly.csv` | 8 series, May 1990–August 2026, 436 months. The only deregistration file |
| VQS population | `datagov/population_vqs_monthly.csv` | Same span and categories, month-end stocks |
| Quota, bids, premium | `datagov/quota_premium_pqp.csv` | 50 series, February 2002–September 2026. Kept because it is the only quota and premium history before 2010 |
| Bidding, 2010 onward | `datamall/M11-coe_results.csv` | 1,985 rows, January 2010–October 2026, Categories A–E |
| PQP | `datamall/M11-coe_results_pqp.csv` | February 2010–October 2026. The month is the month the PQP applies |
| Monthly age | `datamall/M01-03M-Age.csv` | January 2016–August 2026, cars, motorcycles, goods vehicles, and buses |
| Revalidation | `datamall/M10-Monthly_COE_Revalidation.csv` | January 2015–July 2026, 5-year and 10-year, Categories A–D |
| New registrations | `datagov/new_registrations_vqs_monthly.csv` | May 1990–August 2026, 10 series. This is the only registration file |

Download uses the public data.gov.sg initiate/poll API and the xlsx links published on the LTA DataMall static-data page. `scripts/run_ingest.R` skips files that are already present. The app does not call these APIs on startup.

If a later refresh fails, download the same dataset IDs from the dataset pages in `docs/data_sources.csv`, or use the xlsx icons on https://datamall.lta.gov.sg/content/datamall/en/static-data.html, and save them under the local paths in that catalogue. Do not type in missing history.

## What is not available

- No vehicle-level file: no registration date, deregistration date, or renewal date for an individual vehicle.
- No age distribution split into Category A and Category B. The monthly age file is cars, motorcycles, goods and other vehicles, and buses. Engine capacity and power, which define A versus B, are not in it.
- No Category E registration or deregistration series. An open-category COE is used to register a vehicle that is later counted in A, B, or C.
- No historical panel of quota-adjustment components (taxi adjustment, Early Turnover Scheme, expired temporary COEs, guaranteed-deregistration redistribution, or the additional injection programme). Those lines are in quarterly LTA annexes, not in the CSVs.
- Pre-2010 bidding is taken from the SingStat quota/premium extract from February 2002. Earlier exercise sheets are not kept.

## Cohort survival

A survival model needs a cohort whose entry time and exit time are observed, or a reconstruction that does not depend on an arbitrary assumption about who renewed. The published tables do not provide that.

The closest stocks are:

- Monthly one-year age bands from January 2016 to August 2026 for cars, motorcycles, goods and other vehicles, and buses, with a top code at 20 years and above.
- Aggregate revalidations by category and by 5-year or 10-year term, monthly from January 2015 to July 2026, with no gap. Those counts are not linked to a registration cohort.

A car in the “9 – below 10” band may be Category A or B, may reach the COE decision in any month of the coming year, and may renew for 5 or 10 years or be deregistered. Renewed vehicles then sit in older age bands, so age is not the same thing as time since the original registration. Five-year COEs generally cannot be renewed again, which is a second exit process that is not separated in the age table.

That is why individual survival, and a fitted competing-risk model of renewal versus deregistration, is unsupported. Phase 2 should not manufacture cohort rows to fill the gap.

## Reconciliation and quality

Deregistrations. Before the duplicate files were removed, the LTA monthly extract for January 2014–August 2026 matched SingStat on every overlapping month for Categories A–D, taxis, and VQS-exempt vehicles. The SingStat file is the copy that was kept, because it also runs back to May 1990.

New registrations. The SingStat file `new_registrations_vqs_monthly.csv` runs from May 1990 to August 2026. Categories A, B, C, and D, taxis, and VQS-exempt vehicles have no missing month, including all 56 months from May 2020 through December 2024. Weekend cars are observed only from May 1991 to December 1998. The Early Turnover split of Category C is observed from May 2013, and the two parts add to Category C in every such month. The total equals the sum of its components in all 436 months. The earlier registration file, January 2014–April 2020, matches this file on all 76 overlapping months once its Category C is read as the non-ETS part and its Category C-ETS as the ETS part. The short 2025–2026 workbook matches the same way on all 20 months. Those two partial files were removed.

Bidding. A data.gov.sg bidding extract was compared with SingStat and with the LTA workbook, then deleted because `M11-coe_results.csv` is the same table with two corrections already applied: February 2010 first-exercise Category B quota is 693, and January 2010 second-exercise Category D premium is 852. The deleted file had 1,154 and 20,090 in those cells. Recent DataMall values can contain thousands separators.

Bidding coverage. Categories are A, B, C, D, and E. Successful bids do not exceed quota in any CSV row. The only missing months inside the span are April, May, and June 2020, the suspension. October 2026 has one exercise because this snapshot was taken after the first October exercise and before the second. That is a vintage effect, not a second suspension.

PQP. SingStat stores one monthly PQP per category except the open category, on a row named as the second bidding. `M11-coe_results_pqp.csv` is the same four series from February 2010 through October 2026, with no missing month. The LTA 2010 workbook labels the January block “PQP for Feb 2010”, and that value is the February row in the DataMall file. Shifting the DataMall month back by one month reproduces the SingStat series with no remaining differences on the overlap. The DataMall month is therefore the month the PQP is for. On that labelling, Category A is 32,875 in April, May, June, and July 2020, which matches the footnote that the April 2020 PQP also applied in May, June, and July. The SingStat July column is the August application value. Quota and premium themselves are absent for April–June 2020, which is correct: no exercise was held.

Category D deregistrations are missing for September 2002 only. Weekend cars are an observed series from 1991 to December 1998 and are `na` afterwards. They are not a current COE category.

Annual totals and labels such as “2026 (Jan–Aug)” appear above the months in the LTA workbooks. The parser keeps them in a separate `kind` so they cannot be added to the monthly rows.

The sources do not share one vintage. In this snapshot, bidding reaches October 2026, the SingStat quota and PQP extract reaches September 2026, the DataMall PQP file reaches October 2026, and deregistrations, population, and monthly age reach August 2026. Revalidations reach July 2026.

## Structural breaks to carry into modelling

- May 1990: deregistration and VQS population series start.
- February–April 2002: closed bidding ends and open bidding, two exercises a month, begins. The quota file starts in February 2002.
- August 2012: taxis pay the Category A PQP rather than bidding in the car categories.
- February 2014 first exercise: Category A/B gains the 97 kW power limit, in addition to 1,600 cc.
- May 2013 onward: the registration file separates Category C vehicles under the Early Turnover Scheme from the rest of Category C. The two parts add to the Category C total.
- May 2017: Category E receives 10% of Category A, B, and C deregistrations. Category D does not contribute.
- February 2018: allowable growth is 0% a year for A, B, and D, and 0.25% for C. LTA states that this remains until 31 January 2028.
- April–June 2020: bidding suspended. Deregistrations continued. Do not interpolate premiums for those months.
- May 2022 first exercise: fully electric cars use a 110 kW threshold. SingStat series names still say “1600cc and 97kW” across this break.
- From the second exercise of May 2023: some guaranteed future deregistrations are brought forward.
- From February 2025: LTA describes a progressive injection of up to about 20,000 additional COEs. The size and timing of the remainder are not known.

Category A and Category B are not a stable population across February 2014 and May 2022. A practical estimation window is February 2014 onward, with the earlier data kept for description. Whether May 2022 changes forecast errors is an empirical question for the backtest, not a conclusion of this audit.

## Current quota rule, not yet encoded

The August 2026–October 2026 LTA release, and the November 2025–January 2026 release, describe the quarterly quota as:

- 25% of effective deregistrations over the preceding 12 months
- Category E takes 10% of the Category A, B, and C replacement amounts; Category D does not contribute
- 0.25% a year growth for Category C, based on the previous year-end Category C population; 0% for A, B, and D
- separate adjustments for taxis, the Early Turnover Scheme, expired temporary COEs, redistribution of identified guaranteed deregistrations, and additional injections

The replacement piece can be computed from the deregistration series. The adjustment piece cannot be reconstructed from the CSVs. Historical backtests must use the rule that applied at that date. Rules before May 2017 and before the zero-growth decision are different, and they have not been encoded. Future injections after the announced quarter are scenarios.

## Eight- to twelve-quarter horizon

The sample length is not the constraint. Categories A–D have several hundred monthly observations. The constraint is what those observations identify at a two- to three-year horizon.

- A quota quarter depends on deregistrations over the previous 12 months. At an 8–12 quarter horizon, most of that window has not happened yet, so the quota interval stays wide even though the quota itself is a smoothed function of deregistrations.
- Vehicles that will face the 10-year decision inside that horizon are visible each month from 2016, but only as cars, motorcycles, goods vehicles, or buses. The Category A versus B mix inside a car age band is unknown. The oldest bus band, 20 and above, is blank from September 2018 through August 2026 except July–December 2020. Cars, motorcycles, and goods vehicles are complete.
- Renewal versus deregistration depends on the PQP, and the PQP depends on premiums. Using a premium assumption to move deregistrations, and then reading the result as a premium forecast, would be circular. Keep the deregistration forecast and the premium scenario separate.
- Growth rates are stated through January 2028. Beyond the announced quarter, injections and brought-forward deregistrations are policy choices. They should be labelled scenarios.
- Quarterly modelling may be the better grain for the quota identity. Monthly modelling is still the right grain for seeing bidding exercises and the 2020 suspension. Phase 3 should report both, not assume one is better.

## Phase 2 panel

The processed tables are built by `Rscript scripts/build_processed.R`. Raw files are not modified. The build stops if a structural check fails. Joins, publication lags, and sample sizes are in [data_engineering.md](data_engineering.md), [data_quality_report.md](data_quality_report.md), and [modelling_readiness.md](modelling_readiness.md).

No forecast has been fit. The next phase is model training on `data/processed/coe_model_frame.csv`, using only the sample flags documented there.
