# Data quality report

These figures come from `Rscript scripts/build_processed.R` on 14 October 2026. The same structural checks run inside `rhino::test_r()`. The last test run was 21 passes, 0 failures.

## Structural checks

| Check | Result |
| --- | --- |
| Panel keys are unique | Pass. 0 duplicate month-category keys |
| Panel row count | Pass. 2,190 rows, equal to 438 months times 5 categories |
| Category E has no invented stock or flow | Pass. Deregistrations, population, and registrations are missing on every Category E row |
| Counts are not negative | Pass |
| Category C equals its ETS parts | Pass. 160 months, maximum absolute gap 0 |
| April–June 2020 premiums stay missing | Pass. Those months are marked suspended |
| Bidding keys are unique | Pass. 0 duplicate exercise keys |
| Monthly quota equals the exercises | Pass. 2,935 exercises and 1,470 month-category sums |
| Official quarterly adjustments were not filled in | Pass. Both adjustment columns are empty |
| Unpublished age cells stay missing | Pass. 90 bus cells aged 20 and above |
| Model-frame keys are unique and exclude E | Pass. 1,752 rows |
| Two-month deregistration lag matches the source month | Pass |
| One-month deregistration lag is absent | Pass |
| Registration total identity | Pass. 436 months, 0 differences |

The registration identity uses Category A, B, C, D, taxis, and exempt vehicles, plus weekend cars only in months where that series is published. Category C is included once.

## Coverage of the panel

| Column | Observed rows |
| --- | --- |
| Deregistrations | 1,743 |
| Population | 1,744 |
| Registrations | 1,744 |
| Revalidations | 556 |
| PQP | 1,184 |
| Premium and bidding quota | 1,470 |

Deregistrations are one short of the population count because Category D is unpublished for September 2002. That cell remains missing.

Bidding history runs from February 2002 to October 2026. The first 950 rows are the SingStat exercise file. The remaining 1,985 rows are the DataMall file. The two sources did not disagree on the overlap, so the build did not have to choose between conflicting numbers.

Quota history has 2,935 exercise rows, 1,470 monthly sums, and 495 quarterly sums. April–June 2020 do not appear as a zero quarter. The quarter is absent.

## Population change is not an identity

For Categories A–D, the month-to-month change in VQS population was compared with registrations minus deregistrations.

| Category | Months compared | Months with a zero gap | Largest absolute gap |
| --- | --- | --- | --- |
| A | 435 | 325 | 5,048 |
| B | 435 | 333 | 1,305 |
| C | 435 | 415 | 1,215 |
| D | 434 | 418 | 429 |

The median gap is 0 in every category. The non-zero months are kept. Population is not forced to equal the previous stock plus registrations minus deregistrations, because the published tables do not identify transfers or reclassification.

## Left unresolved on purpose

- No official quarterly quota series, and no historical file of taxi, Early Turnover, temporary-COE, brought-forward, or injection adjustments.
- No age split of Category A versus Category B.
- No vehicle-level registration or exit dates.
- Bidding exercises before February 2002 are not in the modelling tables.
