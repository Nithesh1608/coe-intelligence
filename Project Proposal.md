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
| Horizon | Three quarters | Eight to twelve quarters |
| Method | Rolling average of realised deregistrations | Registration-cohort survival model |
| Output | Point estimate, Buy / Wait / Caution signal | Forecast distribution plus modelled cost exposure |
| Question answered | "Should I bid now?" | "What is my COE cost exposure, and how do I time against it?" |
| Model | Free, advertising-funded | Subscription |

## **3\. What pain points does that practice create?**

* **Unpriced risk:** A dealer guaranteeing package prices is taking a position on COE without measuring it. There is no tool that converts a forward order book into an expected cost with a confidence range.  
* **No planning horizon:** Existing forecasts extend three quarters. Fleet replacement and inventory decisions are made over two to three years.  
* **Point estimates hide the downside:** A business needs the adverse case as much as the central one. A single projected number cannot support a decision about how much exposure to carry.  
* **Cross-category blindness:** Dealers allocate inventory across Categories A, B and E, but the categories are analysed separately. Nothing shows how demand shifts between them as the spread narrows.  
* **No feedback modelling:** When premiums are high, more owners renew at the Prevailing Quota Premium instead of scrapping, which suppresses future deregistrations and therefore future quota. This loop drives medium-term supply and is absent from every published forecast.

## **4\. How are we going to tackle the problem?**

We will build an interactive R application that projects COE supply over a two-to-three-year horizon as a distribution, and translates it into cost exposure for a specific order book or fleet schedule.

Four views:

1. **Supply pipeline:** Registration cohorts traced forward into expected deregistrations and therefore quota, by category, over eight to twelve quarters, presented as a distribution with uncertainty bands rather than a single line.  
2. **Feedback and scenarios:** Renewal rate modelled as a function of premium level, so the user can see how high premiums suppress future supply. Scenario controls for policy injections and for demand shocks.  
3. **Category dynamics:** The A/B/E spread over time, and the substitution behaviour it drives — how Category A demand migrates to B and E as the gap closes.  
4. **Exposure calculator:** The user enters a forward order book or a fleet replacement schedule; the application returns expected COE cost with a confidence range, and shows how the expected cost changes if purchases are shifted by one or two quarters.

### **What data will we use, and where will we obtain it?**

| Source | Dataset | Role |
| ----- | ----- | ----- |
| LTA, via data.gov.sg | [COE Bidding Results / Prices](https://data.gov.sg/datasets/d_69b3380ad7e51aff3a7dcc84eba52b8a/view)  (Data from Jan 2-1- to Sep 2026\) | Per-exercise premium, quota and bids received by category. Primary price series. |
| SingStat, via data.gov.sg | [Motor Vehicle Quota, Quota Premium and PQP, Monthly](https://data.gov.sg/datasets/d_22094bf608253d36c0c63b52d852dd6e/view)  (Data from Feb 2002 to Jul 2026\) | Monthly quota and PQP; PQP drives the renewal feedback model. |
| LTA DataMall | [Annual vehicle statistics: new registrations, deregistrations, vehicle population by type and age](https://datamall.lta.gov.sg/content/datamall/en/static-data.html) | Registration cohorts — the input to the survival model. Second, independent source. |
| ACRA, via data.gov.sg | [Entity information, filtered to motor trade SSIC codes](https://data.gov.sg/collections/1/view) | Sizes the addressable market of dealers and importers for the business plan. |
| OneMotoring / trade sources (crawled) | ARF and OMV schedules; COE-inclusive listing prices | Converts quota exposure into total vehicle cost. Source site cited in the report rather than submitted. |

Two data issues we have already identified. Categories A and B were **redefined in May 2022** around electric vehicles' maximum power output, so a naive long series compares two different products. Bidding exercises were **suspended in April, May and June 2020**, leaving a structural gap.

### **What kinds of visualisation will we develop?**

* **Fan chart** of projected quota by category over twelve quarters, with prediction intervals widening at longer horizons.  
* **Cohort flow diagram** tracing registrations in year *t* through survival and renewal into deregistrations in year *t+10*, making the mechanics of the projection legible.  
* **Within-quarter volatility chart** showing the maximum premium swing inside each quarter historically — the chart that quantifies the dealer's exposure problem.  
* **Spread chart** of Cat A versus Cat B versus Cat E, annotated with policy events, with a linked view of category volume shares to show substitution.  
* **Feedback loop explorer**: an interactive control on the renewal rate, showing how it propagates into supply two to three years out.  
* **Exposure waterfall** for the user's own order book: expected cost, confidence range, and the effect of shifting timing.

All charts in `ggplot2`, made interactive with `plotly`, delivered in a Shiny application. Acquisition uses `httr2` against the data.gov.sg API and `readxl` for DataMall workbooks; cleaning uses `dplyr` and `lubridate`; the cohort model uses survival-style attrition curves with bootstrapped intervals.

### **Scope and honesty**

We forecast **supply**, which is largely mechanical, and we quantify **exposure** under uncertainty. We do not claim to forecast premiums, which depend additionally on demand, credit conditions and policy. This limitation will be stated in the application itself. We will backtest the supply model against realised quota and report its calibration openly, including where it performs poorly.

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

