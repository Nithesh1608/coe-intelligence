# Download or reuse official snapshots.
# From the project root:
#   Rscript scripts/run_ingest.R
#   Rscript scripts/run_ingest.R --refresh

source("renv/activate.R")
options(box.path = normalizePath("."))

box::use(
  app/logic/data/ingest[download_snapshots],
)

refresh <- "--refresh" %in% commandArgs(trailingOnly = TRUE)
download_snapshots(refresh = refresh)
