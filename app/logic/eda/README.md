# EDA / Market gallery

Live Market deeper-context charts are **echarts only**, built in:

```
app/logic/viewdata/gallery_echarts.R   # prep_gallery() + chart_g0…g6
app/view/eda.R                         # Market tab UI/server
```

Top-of-page Market blocks (cohort flow, within-quarter swing, A/B spread) live in `app/logic/viewdata/proposal_viz.R`.

Scope: **Cat A/B only.**
