# Build the Phase 1 audit tables from data/raw.
# From the project root:
#   Rscript scripts/run_audit.R

source("renv/activate.R")
options(box.path = normalizePath("."))

box::use(
  app/logic/data/audit[run_audit],
)

run_audit()
