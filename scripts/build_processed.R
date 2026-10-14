# Rebuild processed tables from data/raw. Does not call a government API.
# From the project root:
#   Rscript scripts/build_processed.R

source("renv/activate.R")
options(box.path = normalizePath("."))

box::use(
  app/logic/data/build_processed[build_all],
)

built <- build_all()
cat(
  "panel", nrow(built$panel),
  "bidding", nrow(built$history),
  "age", nrow(built$age),
  "quota", nrow(built$quota),
  "model", nrow(built$model_frame),
  "\n"
)
print(built$checks)
