# Assumptions register

Phase 1 only. These are data and scope assumptions. They are not estimated parameters.

1. The predictive target is monthly deregistrations by COE category. Quota is derived from that target under a stated rule. Premium is not assumed to be determined by quota.
2. A predictor may be used at a forecast origin only if it would already have been published. Realised future deregistrations, renewals, and PQP values are not predictors.
3. Annual totals and year-to-date rows in LTA workbooks are not monthly observations.
4. The PQP column named as the second bidding is the monthly PQP. The open category has no PQP series. In `M11-coe_results_pqp.csv` the month is the month the PQP is for. The SingStat PQP column is one month earlier and matches the DataMall series after that shift.
5. April, May, and June 2020 have no quota premium because bidding was suspended. Those months stay missing. Deregistrations for those months are real and stay in the series.
6. Category A and Category B change definition in February 2014 and again for electric cars in May 2022. The series names do not change at those dates.
7. Weekend cars are a historical series through 1998, not a current modelling category.
8. Taxis and VQS-exempt vehicles are reported with deregistrations but are not bidding categories. Taxi registrations affect the Category E quota through a separate adjustment.
9. The modelling population is the VQS category stock. A separate vehicle-type population file is not kept.
10. Age bands describe a stock by vehicle type. The monthly file runs from January 2016 to August 2026. It does not identify Category A versus B, and it does not identify which vehicles renewed. The bus band “20 and above” is blank for most months from September 2018. The two labels “20 and above” and “20 & above” are the same band.
11. `M11-coe_results.csv` is the only 2010–2026 bidding file. February 2010 first-exercise Category B quota is 693, and January 2010 second-exercise Category D premium is 852.
12. The replacement component of the current quarterly quota is 25% of effective deregistrations over the previous 12 months, with 10% of the A, B, and C amounts assigned to Category E. Adjustment lines and any quota rule from before May 2017 are not treated as known until they are encoded from the announcement that applied at the time.
13. Growth of 0% for A, B, and D, and 0.25% for C, is a stated policy through 31 January 2028. Injections and brought-forward deregistrations beyond an announced quarter are scenarios.
14. No model has been trained, so there is no accuracy claim.
