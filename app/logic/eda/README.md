# EDA charts

Market Deeper context uses **one notebook**, **one render script**, **one PNG folder**.

```
app/logic/eda/
  README.md
  render_gallery.R                 # writes Market gallery PNGs
  source/
    coe_eda_gallery.Rmd            # Market gallery source (g0–g7)

app/static/eda/                    # g0_… g7_ only
```

| File | Story |
|---|---|
| `g0_premium_history.png` | Long premium backdrop |
| `g1_quota_rule_topup.png` | Formula vs LTA top-up |
| `g2_age_heatmap.png` | Age wave on the road |
| `g3_locked_exits_lollipop.png` | Locked 5y exits |
| `g4_owner_price_response.png` | Owner response to price |
| `g7_supply_price.png` | Quota change vs premium change |
| `g5_bids_per_coe_violin.png` | Bidding pressure |
| `g6_dealer_risk_ridges.png` | Dealer wait risk |

```r
source("app/logic/eda/render_gallery.R")
```

Needs `ggplot2`, `scales`, `patchwork`, `ggridges`. Scope: **Cat A/B only.**
