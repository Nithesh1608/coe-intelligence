# EDA charts

All EDA visualisation code for the desk lives here — static ggplot sources next to the render entrypoint. Interactive echarts used in the Shiny app stay under `app/logic/viewdata/` (`proposal_viz.R`, `eda_story.R`, `charts.R`).

```
app/logic/eda/
  README.md
  render_static.R      # earlier static PNGs (e1_*, 01_* ...)
  render_gallery.R     # improved 6-chart gallery (g1_* ... g6_*)
  source/
    EDA1.Rmd           # exploratory notebook (Cat A/B)
    EDA2.R             # exploratory script (Cat A/B)
    coe_eda_gallery.Rmd  # improved gallery notebook (R Graph Gallery styles)

app/static/eda/        # committed PNG artefacts served by the Market tab
app/logic/viewdata/    # live echarts helpers used by Home / Market / Results / Your risk
```

Scope: **Categories A and B only.** Market "Deeper context" shows the **gallery** figures (`g1_`...`g6_`).

Regenerate from the repo root:

```r
source("app/logic/eda/render_gallery.R")   # preferred (live Market charts)
source("app/logic/eda/render_static.R")    # legacy artefacts
```

Needs `ggplot2`, `scales`, `patchwork`, and `ggridges` on `.libPaths()`.
