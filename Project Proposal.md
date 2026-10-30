## **1\. What is the problem we want to tackle?**

Most discussion of Singapore's Certificate of Entitlement focuses on the individual car buyer. We are interested in a different party: the businesses that buy COEs continuously and in volume, and that carry real financial exposure to premium movements they cannot currently forecast.

**Problem Statement:** Can COE supply be forecast far enough ahead, and with honest uncertainty, for a dealer or fleet operator to price and time real financial exposure?

Two situations define the problem:

**Dealers and parallel importers quote COE-inclusive package prices.** When a customer orders a car, the dealer typically guarantees a total price and undertakes to bid on the customer's behalf. If the premium rises between order and successful bid, the dealer absorbs the difference. With premiums for cars currently above \$120,000 and capable of moving several thousand dollars between fortnightly exercises, a dealer holding fifty guaranteed orders is running an unhedged short position on COE — taken twice a month, with no analytical tooling.

**Fleet operators replace vehicles in cohorts.** A rental, leasing or ride-hail operator replacing several hundred vehicles is exposed to whether that replacement lands in a supply trough or a supply peak. Shifting a replacement cycle by two quarters can move total cost by seven figures.

Both parties need the same thing and cannot get it: a view of COE **supply** far enough ahead to plan against, expressed with honest uncertainty.

Our central observation is that supply is substantially mechanical. Every COE runs ten years. Quota is driven largely by deregistrations, and deregistrations are driven largely by registrations roughly ten years earlier. Those registration cohorts are already in the public record. The pipeline for the next two to three years is therefore partly knowable today — but nobody presents it in a form a business can plan against.

## **2\. What is the current market practice?**

**Trade buyers rely on intuition and relationships:**  Dealers form a view from experience and from talking to peers. There is no analytical tool built for them. Pricing of package deals is, in effect, a judgement call about a financial exposure.

**LTA publishes the raw material but not the analysis:** Bidding results, quota, and vehicle registration and deregistration statistics are all released promptly and in full, on data.gov.sg and LTA DataMall. They are tables and PDFs aimed at general publication, not decision support.

**Consumer tools exist and are well built:** [quota.sg](http://quota.sg) is the most developed: it publishes results after each exercise, a Buy/Wait/Caution signal derived from bid-to-quota ratios, cost-of-ownership and renew-versus-scrap calculators, historical premium heat maps, and — most relevantly — a quota forecast for the next three quarters, modelled from deregistration records. It is free and advertising-funded.

We regard quota.sg as validation rather than as an obstacle: its existence demonstrates commercial demand for COE analytics built on open data. But it is built for a person making one decision every ten years, and its design reflects that. It offers point estimates over a three-quarter horizon and answers the question "should I bid this round." A dealer pricing a forward order book, or a fleet planning a replacement cycle, is asking a different question over a longer horizon and needs a distribution rather than a number.

|  | Quota.sg | Our proposal |
| ----- | ----- | ----- |
| User | Individual buyer, one decision per decade | Dealer, importer, fleet operator — continuous exposure |
| Horizon | Three quarters | Twelve quarters |
| Method | Rolling average of realised deregistrations | Registration-cohort model: cohort survival projection of the fleet feeding an elastic net, rolled forward with a recursive bootstrap |
| Output | Point estimate, Buy / Wait / Caution signal | Forecast distribution (50% and 80% bands) plus modelled cost exposure |
| Question answered | "Should I bid now?" | "What is my COE cost exposure, and how do I time against it?" |
| Model | Free, advertising-funded | Subscription |

## **3\. What pain points does that practice create?**

* **Unpriced risk:** A dealer guaranteeing package prices is taking a position on COE without measuring it. There is no tool that converts a forward order book into an expected cost with a confidence range.  
* **No planning horizon:** Existing forecasts extend three quarters. Fleet replacement and inventory decisions are made over two to three years.  
* **Point estimates hide the downside:** A business needs the adverse case as much as the central one. A single projected number cannot support a decision about how much exposure to carry.  
* **Cross-category blindness:** Dealers allocate inventory across Categories A and B, but the categories are often analysed in isolation. Nothing shows how demand shifts between them as the spread narrows.  
* **No feedback modelling:** Renewal at the Prevailing Quota Premium competes with scrapping, so the renewal rate moves deregistrations and therefore future quota. This loop drives medium-term supply and is absent from every published forecast. (We expected dear premiums to raise renewals; LTA revalidation data since 2015 show the opposite, and the application models the fitted sign.)

## **4\. How are we going to tackle the problem?**

We will build an interactive R application that projects COE supply over a three-year horizon as a distribution, and translates it into cost exposure for a specific order book or fleet schedule.

Four views (tab names in the application in brackets):

1. **Supply pipeline (Home):** Registration cohorts traced forward into expected deregistrations and therefore quota, by category, over twelve quarters, presented as a distribution with 50% and 80% bootstrap bands rather than a single line, plus a quarterly table.  
2. **Feedback and scenarios (Scenarios):** Renewal share fitted as a function of PQP on LTA revalidation data, so the user can see how a premium path changes renewals, deregistrations and quota. Scenario controls for a direct renewal shift, premium glides, LTA injections, a clawback of the certificates LTA brought forward since 2023, and a merged A+B category.  
3. **Category dynamics (Market):** The A/B spread over time and the substitution behaviour it drives — how Category A demand migrates to B as the gap closes — with the cohort flow and within-quarter swing charts.  
4. **Exposure calculator (Your risk):** The user enters a forward order book or a fleet replacement schedule; the application returns expected COE cost with a confidence range, and shows how the expected cost changes if purchases are shifted by one or two quarters. Premium paths can be tilted by the forward quota path under a chosen scenario.

A fifth tab, **Results**, reports calibration: one-step held-out error, period totals where the model missed, and a 24-month recursive backtest with coverage of the 80% band.

### **What data will we use, and where will we obtain it?**

| Source | Dataset | Role |
| ----- | ----- | ----- |
| LTA, via data.gov.sg / DataMall | [COE Bidding Results / Prices](https://data.gov.sg/datasets/d_69b3380ad7e51aff3a7dcc84eba52b8a/view) (Jan 2010 to Oct 2026) | Per-exercise premium, quota and bids received by category. Primary price series; published quota is the calibration target. |
| SingStat, via data.gov.sg | [Motor Vehicle Quota, Quota Premium and PQP, Monthly](https://data.gov.sg/datasets/d_22094bf608253d36c0c63b52d852dd6e/view) (Feb 2002 to Sep 2026) | Monthly quota and PQP; PQP drives the renewal response. |
| SingStat Table Builder | Monthly new registrations, deregistrations and population under the Vehicle Quota System, by category (May 1990 to Aug 2026) | Registration cohorts and the modelling target (deregistrations). |
| LTA DataMall | [Monthly vehicle statistics](https://datamall.lta.gov.sg/content/datamall/en/static-data.html): population by type and age (M01-03M), deregistrations by quota category (M05), COE revalidations (M10), bidding results and PQP (M11), Car Cost Update (M032) | Age stock for the due cohort, renewal share, second independent deregistration series, and the ARF / OMV / dealer-price table that converts quota exposure into total vehicle cost. |
| ACRA, via data.gov.sg | [Entity information, filtered to motor trade SSIC codes](https://data.gov.sg/collections/1/view) | Sizes the addressable market of dealers and importers for the business plan (used in the deck, not in the application). |

The two data.gov.sg tables are pulled with `httr2` (`scripts/acquire_data.R`); DataMall and Table Builder exports need a browser session and are documented in the same script. The Car Cost Update workbook replaces the crawl of OneMotoring and trade listings that we first planned.

Two data issues we have already identified. Categories A and B were **redefined in May 2022** around electric vehicles' maximum power output, so a naive long series compares two different products. Bidding exercises were **suspended in April, May and June 2020**, leaving a structural gap.

### **What kinds of visualisation will we develop?**

* **Fan chart** of projected quota by category over twelve quarters, with 50% and 80% bootstrap bands widening at longer horizons.  
* **Cohort flow diagram** tracing registrations in year *t* through survival and renewal into deregistrations in year *t+10*, making the mechanics of the projection legible.  
* **Within-quarter volatility chart** showing the maximum premium swing inside each quarter historically — the chart that quantifies the dealer's exposure problem.  
* **Spread chart** of Cat A versus Cat B, with a linked view of A/B volume shares to show substitution.  
* **Scenario explorer**: renewal lever, premium glides and injections acting on the live forward path, with baseline and scenario quarterly quota side by side.  
* **Recursive backtest chart and calibration table**: the model rolled forward 24 months from the training end against realised deregistrations, with MAPE, bias and 80% coverage by horizon.  
* **Exposure waterfall** for the user's own order book: expected cost, confidence range, and the effect of shifting timing.

Live charts use `echarts4r` inside a Rhino / Shiny application; the static EDA figures are `ggplot2`. Acquisition uses `httr2` against the data.gov.sg API and `readxl` for the DataMall workbook; cleaning uses `dplyr` and `lubridate`. The supply model is an elastic net (`glmnet`) on registration-cohort, age-stock, renewal and PQP features, with the forward age stock projected from registration cohorts and calibrated survival factors, and intervals from a recursive block bootstrap of held-out residuals plus the quota-mapping residual.

### **Scope and honesty**

We forecast **supply**, which is largely mechanical, and we quantify **exposure** under uncertainty. We do not claim to forecast premiums, which depend additionally on demand, credit, and policy. This limitation is stated in the application itself. We backtest the supply model two ways — one step ahead on a locked 24-month window, and rolled forward recursively for 24 months from the training end — and report calibration openly on the Results tab, including the periods where the model under-called Category B and where LTA's discretionary releases sat outside the band. The model specification was chosen on that held-out window, and the application says so.

### **Changes since the submitted proposal**

* **Category E dropped.** The desk covers Categories A and B only; E is an open category with different demand and was removed from the model, the exposure engine and the charts.  
* **Method.** "Survival model" became a registration-cohort projection (cohort survival factors age the fleet forward) feeding an elastic net. Autoregressive lags were removed from the live specification because they under-extrapolated the 2016–17 cohort reaching year ten.  
* **Renewal feedback sign.** The data show dearer PQP lowers renewals; the scenario layer uses the fitted slope.  
* **Horizon.** Thirty-six months forward (twelve quarters), the upper end of the proposed range.  
* **Tooling.** `echarts4r` for live charts instead of `ggplot2` + `plotly`; the Car Cost Update workbook from DataMall instead of a web crawl.  
* **Backtest.** Twenty-four months held out rather than three years, because the age-stock series starts in 2016.

## **5\. Project plan**

### **Main tasks and owners**

| Task | Owner | Support |
| ----- | ----- | ----- |
| Data acquisition: data.gov.sg API, DataMall workbooks, web crawling | \[Name \] | \[Name \] |
| Data cleaning: category redefinition, 2020 gap, reconciliation across sources | \[Name \] | \[Name \] |
| Cohort survival model, renewal feedback, backtesting | \[Name \] | \[Name \] |
| Shiny architecture and interactive visualisations | \[Name \] | \[Name \] |
| Exposure calculator logic and cost build-up | \[Name \] | \[Name \] |
| Market sizing, dealer interviews, business plan | \[Name \] | \[Name \] |

### **Internal milestones**

| Week | Dates | Milestone |
| ----- | ----- | ----- |
| 6 | 22–26 Sep | Acquisition pipeline reproducible end to end. Clean master dataset for premiums, quota and registrations. First dealer interview attempted. |
| 7 | 29 Sep–3 Oct | Cohort survival model v1, backtested against the last three years of realised quota. Calibration reported. |
| 8 | 6–10 Oct | Shiny skeleton running with the supply pipeline and category views on real data. Feature set frozen. |
| 9 | 13–17 Oct | Renewal feedback model integrated. Exposure calculator specified and unit-tested against worked examples. |
| 10 | 20–24 Oct | Application feature-complete. Internal review and testing with five people outside the team. |
| 11 | 27–31 Oct | Business plan drafted, including market sizing from ACRA data. Charts polished. |
| 12 | 3–7 Nov | Deck complete; two full rehearsals timed to 15 minutes. Q\&A prepared, including the competitive positioning against quota.sg. |

