# Data dictionary

Types below are the types after the Phase 1 parsers. Raw SingStat files store numbers as text in a wide layout, and raw LTA workbooks mix titles, year totals, year-to-date labels, and Excel dates. Missing tokens `na`, `-`, and blank become missing. Zeros stay zeros. Nothing is dropped.

## Bidding results

`data/raw/datamall/M11-coe_results.csv`

| Column | Type | Meaning |
| --- | --- | --- |
| month | month | Bidding month, `YYYY-MM` in the file |
| bidding_no | text | `1` or `2` |
| vehicle_class | text | `Category A` through `Category E` |
| quota | number | COEs offered in that exercise |
| bids_success | number | Successful bids |
| bids_received | number | Bids received |
| premium | number | Quota premium, Singapore dollars |

Category definitions, from the dataset page:

- Category A, before the May 2022 first exercise: cars up to 1,600 cc and 97 kW. From that exercise: non-electric cars on that rule, and fully electric cars up to 110 kW.
- Category B: cars above those thresholds, with the same May 2022 electric-car change.
- Category C: goods vehicles and buses.
- Category D: motorcycles.
- Category E: open category, any vehicle except a motorcycle.

The February 2014 first exercise is the earlier break, when the 97 kW limit was added to the 1,600 cc rule. The column `vehicle_class` does not record which definition was in force. Recent rows may store numbers with thousands separators. There is one bidding file for 2010–2026.

## Quota, premium, and PQP

`data/raw/datagov/quota_premium_pqp.csv`

| Column | Type | Meaning |
| --- | --- | --- |
| DataSeries | text | One row per measure. See `data/audit/quota_series.csv` |
| `YYYYMon` columns | number or missing | One month, newest first, from `2026Sep` back to `2002Feb` |

Measures, for the first and second exercise unless noted:

- Total quota, successful bids, bids received
- Category A, labelled “Cars Up To 1600cc And 97kW”: quota, successful bids, bids received, quota premium
- Category B, labelled “Cars Above 1600cc Or 97kW”: the same
- Goods vehicles and buses: the same
- Motorcycles: the same
- Open category: the same, without a PQP row
- PQP: one row per A, B, goods, and motorcycles, named as the second bidding

The PQP name is a layout choice. There is one PQP per month, not one per exercise. The label still uses the pre-2022 power threshold after the electric-car rule changed.

## Monthly deregistrations and VQS population

`deregistrations_vqs_monthly.csv` and `population_vqs_monthly.csv`

| Column | Type | Meaning |
| --- | --- | --- |
| DataSeries | text | Series name, with leading spaces in the raw file |
| `YYYYMon` columns | number or `na` | May 1990 through August 2026 |

Series after trimming:

- Total Motor Vehicles De-Registered, or Total Motor Vehicles for the population file
- Category A: Cars
- Category B: Cars
- Weekend Cars/Off Peak Cars
- Category C: Goods Vehicles & Buses
- Category D: Motorcycles & Scooters
- Taxis
- Vehicles Exempted From VQS

Deregistrations include weekend and off-peak cars and tax-exempt vehicles, per the SingStat footnote. Weekend cars are populated only in the 1990s. Taxis and exempt vehicles are not bidding categories. There is no Category E row.

## Monthly new registrations

`data/raw/datagov/new_registrations_vqs_monthly.csv`

Wide SingStat file, May 1990–August 2026. Ten series:

| Series | Role |
| --- | --- |
| Total New Motor Vehicles Registered | Sum of the components below. Weekend cars are omitted from the sum in months where that series is `na` |
| Category A: Cars | COE Category A |
| Category B: Cars | COE Category B |
| Weekend Cars/Off Peak Cars | Observed May 1991–December 1998 only |
| Category C: Goods Vehicles & Buses | COE Category C, including the Early Turnover Scheme |
| Category C: Goods Vehicles & Buses Not Under ETS | Part of Category C. Equals Category C in months before the scheme |
| Category C: Goods Vehicles & Buses Under ETS | Part of Category C. Observed May 2013–August 2026. The two parts add to Category C in every month where both are present |
| Category D: Motorcycles & Scooters | COE Category D |
| Taxis | Not a bidding category |
| Vehicles Exempted From VQS | Not a bidding category |

There is no Category E row. An open-category COE used to register a vehicle is counted in A, B, or C.

## DataMall CSV extracts

| File | Columns | Coverage |
| --- | --- | --- |
| `M11-coe_results.csv` | month, bidding_no, vehicle_class, quota, bids_success, bids_received, premium | January 2010–October 2026. February 2010 first-exercise Category B quota is 693. January 2010 second-exercise Category D premium is 852 |
| `M11-coe_results_pqp.csv` | month, vehicle_class, pqp | February 2010–October 2026. Categories A–D. The month is the month the PQP applies |
| `M01-03M-Age.csv` | month, type, age, number | January 2016–August 2026, every month. Types are Cars, Motorcycles, Goods and Other Vehicles, and Buses. Bands are `0 - < 1` through `19 - < 20`, then either `20 and above` or `20 & above`. The only blanks are Buses at 20 and above, from September 2018 through August 2026 except July–December 2020 |
| `M10-Monthly_COE_Revalidation.csv` | month, type, category, number | January 2015–July 2026, no missing month. Type is `5 Year` or `10 Year`. Categories A–D |

## Processed tables

Built by `scripts/build_processed.R`. Categories are `A`, `B`, `C`, `D`, and `E`. Dates are the first of the month. Blank cells are missing, not zero.

`coe_monthly_panel.csv` has one row per month per category from May 1990 to October 2026 (2,190 rows). Category E has bidding fields only. `registrations` for Category C is the category total. `registrations_not_ets` and `registrations_ets` are its parts, and only on Category C. `revalidations_total` is the sum of the 5-year and 10-year counts.

`coe_bidding_history.csv` has one row per exercise, category, and source period (2,935 rows). `source` is `singstat_quota_premium` before January 2010 and `datamall_m11` from January 2010.

`vehicle_age_distribution.csv` keeps the monthly age file’s vehicle types and bands (10,752 rows). `20 & above` and `20 and above` are stored as `20 and above`. Blank bus cells stay `not_published`.

`coe_quota_history.csv` stacks three grains: `exercise`, `month`, and `quarter` (4,900 rows). `official_quarterly_quota` and `policy_adjustment` are empty. A quarter with no bidding rows, including April–June 2020, is absent rather than recorded as zero.

`coe_model_frame.csv` has Categories A–D only (1,752 rows). Lagged columns are defined in `docs/data_engineering.md`. Sample flags mark which rows can support each proposed model.
