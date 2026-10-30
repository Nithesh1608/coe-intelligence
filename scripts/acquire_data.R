# Data acquisition.
#
# Two sources are open APIs and are downloaded here with httr2:
#   - data.gov.sg: LTA COE bidding results (per exercise) and the SingStat
#     monthly quota / premium / PQP table.
# The rest need a browser session and are documented, not scripted:
#   - LTA DataMall static tables (M01-03M-Age, M05, M10, M11, M032): the
#     download links sit behind a terms-of-use click.
#   - SingStat Table Builder exports for registrations, deregistrations and
#     population under the VQS (wide CSV, one column per month).
#
# Run from the repo root:  Rscript scripts/acquire_data.R
# Files land in data/raw/datagov/api/ and never overwrite the curated extracts
# in data/raw/datagov/ or data/raw/datamall/; data_cleaning.Rmd reads those.
# Compare the API pulls against the curated extracts with the audit at the end.

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(root, "rhino.yml"))) stop("Run from the repo root.")
out_dir <- file.path(root, "data", "raw", "datagov", "api")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

DATASETS <- list(
  coe_bidding_results = list(
    id = "d_69b3380ad7e51aff3a7dcc84eba52b8a",
    note = "LTA COE Bidding Results / Prices, per exercise, Jan 2010 onward"
  ),
  quota_premium_pqp = list(
    id = "d_22094bf608253d36c0c63b52d852dd6e",
    note = "SingStat Motor Vehicle Quota, Quota Premium and PQP, monthly, Feb 2002 onward"
  )
)

#' Page through the data.gov.sg datastore API for one dataset.
fetch_dataset <- function(id, page_size = 5000L) {
  rows <- list()
  offset <- 0L
  repeat {
    resp <- request("https://data.gov.sg/api/action/datastore_search") |>
      req_url_query(resource_id = id, limit = page_size, offset = offset) |>
      req_user_agent("coe-intelligence (DBA3702 project)") |>
      req_retry(max_tries = 3) |>
      req_perform()
    body <- resp_body_json(resp, simplifyVector = TRUE)
    if (!isTRUE(body$success)) stop("data.gov.sg returned success = FALSE for ", id)
    recs <- body$result$records
    if (is.null(recs) || !NROW(recs)) break
    rows[[length(rows) + 1L]] <- as.data.frame(recs, stringsAsFactors = FALSE)
    offset <- offset + NROW(recs)
    total <- body$result$total
    if (!is.null(total) && offset >= total) break
  }
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

for (name in names(DATASETS)) {
  d <- DATASETS[[name]]
  cat(sprintf("Fetching %s (%s)\n", name, d$note))
  df <- tryCatch(fetch_dataset(d$id), error = function(e) {
    message("  failed: ", conditionMessage(e))
    NULL
  })
  if (is.null(df) || !nrow(df)) next
  if ("_id" %in% names(df)) df[["_id"]] <- NULL
  path <- file.path(out_dir, paste0(name, ".csv"))
  utils::write.csv(df, path, row.names = FALSE)
  cat(sprintf("  %d rows -> %s\n", nrow(df), path))
}

# ---- Audit against the curated DataMall extract -------------------------------
api_path <- file.path(out_dir, "coe_bidding_results.csv")
curated_path <- file.path(root, "data", "raw", "datamall", "M11-coe_results.csv")
if (file.exists(api_path) && file.exists(curated_path)) {
  api <- utils::read.csv(api_path, stringsAsFactors = FALSE)
  cur <- utils::read.csv(curated_path, stringsAsFactors = FALSE)
  cat(sprintf("\nAPI exercises: %d rows, %s to %s\n", nrow(api), min(api$month), max(api$month)))
  cat(sprintf("Curated M11:   %d rows, %s to %s\n", nrow(cur), min(cur$month), max(cur$month)))
  key_api <- paste(api$month, api$bidding_no, api$vehicle_class)
  key_cur <- paste(cur$month, cur$bidding_no, cur$vehicle_class)
  common <- intersect(key_api, key_cur)
  if (length(common)) {
    a <- as.numeric(api$premium[match(common, key_api)])
    b <- as.numeric(cur$premium[match(common, key_cur)])
    cat(sprintf("Premium mismatches on %d common exercises: %d\n", length(common), sum(a != b, na.rm = TRUE)))
  }
}

cat("\nManual downloads (browser, accept terms once):\n")
cat("  LTA DataMall static datasets -> data/raw/datamall/\n")
cat("    M01-03M-Age.csv, M05-Dereg_by_Quota.csv, M10-Monthly_COE_Revalidation.csv,\n")
cat("    M11-coe_results.csv, M11-coe_results_pqp.csv, M032-Car_Cost_Update.xlsx\n")
cat("    https://datamall.lta.gov.sg/content/datamall/en/static-data.html\n")
cat("  SingStat Table Builder (M454871 family: VQS registrations, deregistrations, population) -> data/raw/datagov/\n")
cat("    https://tablebuilder.singstat.gov.sg/  (export as CSV, one column per month)\n")
cat("Then: Rscript scripts/run_data_cleaning.R; Rscript scripts/fit_and_forecast.R\n")
