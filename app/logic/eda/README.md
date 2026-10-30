# EDA charts

All EDA visualisation code for the desk lives here — static ggplot sources next to the render entrypoint. Interactive echarts used in the Shiny app stay under `app/logic/viewdata/` (`proposal_viz.R`, `eda_story.R`, `charts.R`).

```
app/logic/eda/
  README.md
  render_static.R      # regenerates PNGs into app/static/eda/
  source/
    EDA1.Rmd           # exploratory notebook (Cat A/B)
    EDA2.R             # exploratory script (Cat A/B)

app/static/eda/        # committed PNG artefacts served by the EDA tab
app/logic/viewdata/    # live echarts helpers used by Home / EDA / Results / Your risk
```

Scope: **Categories A and B only.**

Regenerate static figures from the repo root:

```r
source("app/logic/eda/render_static.R")
```
