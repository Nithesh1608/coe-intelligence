# Measure the local snapshots. This module does not fit a forecast and does
# not delete source rows.

box::use(
  app/logic/data/parse[
    parse_bidding_csv,
    parse_singstat_wide
  ],
)

write_out <- function(data, out_dir, name) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data, file.path(out_dir, name), row.names = FALSE)
  data
}

series_profile <- function(long, source_id) {
  rows <- lapply(unique(long$series), function(name) {
    part <- long[long$series == name, , drop = FALSE]
    data.frame(
      series = name,
      n = nrow(part),
      n_months = length(unique(part$month)),
      month_min = as.character(min(part$month, na.rm = TRUE)),
      month_max = as.character(max(part$month, na.rm = TRUE)),
      n_missing = sum(is.na(part$value)),
      n_unparsed_month = sum(is.na(part$month)),
      source_id = source_id,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

bidding_quality <- function(bidding) {
  key <- paste(bidding$month, bidding$bidding_no, bidding$vehicle_class, sep = "|")
  months <- sort(unique(bidding$month))
  expected <- seq(min(months), max(months), by = "month")
  missing <- as.character(expected)[!as.character(expected) %in% as.character(months)]
  exercises <- unique(bidding[, c("month", "bidding_no")])
  counts <- as.data.frame(table(exercises$month), stringsAsFactors = FALSE)
  one_exercise <- counts$Var1[counts$Freq == 1]
  data.frame(
    n_rows = nrow(bidding),
    n_duplicate_keys = sum(duplicated(key)),
    month_min = as.character(min(months)),
    month_max = as.character(max(months)),
    n_missing_months = length(missing),
    missing_months = paste(missing, collapse = "; "),
    classes = paste(sort(unique(bidding$vehicle_class)), collapse = "; "),
    n_success_above_quota = sum(bidding$bids_success > bidding$quota, na.rm = TRUE),
    n_one_exercise_months = length(one_exercise),
    one_exercise_months = paste(as.character(one_exercise), collapse = "; "),
    stringsAsFactors = FALSE
  )
}

quota_crosswalk <- function() {
  data.frame(
    series_prefix = c(
      "Cars Up To 1600cc And 97kW",
      "Cars Above 1600cc Or 97kW",
      "Goods Vehicles & Buses",
      "Motorcycles",
      "Open Category"
    ),
    vehicle_class = c(
      "Category A", "Category B", "Category C", "Category D", "Category E"
    ),
    stringsAsFactors = FALSE
  )
}

quota_mismatch_rows <- function(bidding, quota_long) {
  map <- quota_crosswalk()
  pieces <- lapply(seq_len(nrow(map)), function(i) {
    prefix <- map$series_prefix[[i]]
    one <- quota_long[startsWith(quota_long$series, paste0(prefix, ", Quota,")), ]
    one$vehicle_class <- map$vehicle_class[[i]]
    one$bidding_no <- ifelse(grepl("1st Bidding", one$series), "1", "2")
    one[, c("vehicle_class", "bidding_no", "month", "value")]
  })
  official <- do.call(rbind, pieces)
  names(official)[names(official) == "value"] <- "singstat_quota"
  bid <- bidding[, c("month", "bidding_no", "vehicle_class", "quota")]
  merged <- merge(official, bid, by = c("month", "bidding_no", "vehicle_class"))
  merged$diff <- merged$singstat_quota - merged$quota
  merged[!is.na(merged$diff) & merged$diff != 0, ]
}

reconcile_quota <- function(bidding, quota_long) {
  map <- quota_crosswalk()
  pieces <- lapply(seq_len(nrow(map)), function(i) {
    prefix <- map$series_prefix[[i]]
    one <- quota_long[startsWith(quota_long$series, paste0(prefix, ", Quota,")), ]
    one$vehicle_class <- map$vehicle_class[[i]]
    one$bidding_no <- ifelse(grepl("1st Bidding", one$series), "1", "2")
    one[, c("vehicle_class", "bidding_no", "month", "value")]
  })
  official <- do.call(rbind, pieces)
  names(official)[names(official) == "value"] <- "singstat_quota"
  bid <- bidding[, c("month", "bidding_no", "vehicle_class", "quota")]
  names(bid)[names(bid) == "quota"] <- "bidding_quota"
  merged <- merge(official, bid, by = c("month", "bidding_no", "vehicle_class"), all = TRUE)
  both <- !is.na(merged$singstat_quota) & !is.na(merged$bidding_quota)
  diff <- merged$singstat_quota - merged$bidding_quota
  data.frame(
    n_rows = nrow(merged),
    n_both = sum(both),
    n_equal = sum(both & diff == 0),
    n_differ = sum(both & diff != 0),
    max_abs_diff = if (any(both)) max(abs(diff[both])) else NA_real_,
    stringsAsFactors = FALSE
  )
}

csv_profile <- function(path, source_id) {
  if (!file.exists(path)) {
    return(data.frame(
      source_id = source_id,
      status = "missing",
      n_row = NA_integer_,
      n_col = NA_integer_,
      stringsAsFactors = FALSE
    ))
  }
  header <- names(utils::read.csv(path, nrows = 1, check.names = FALSE))
  n_row <- nrow(utils::read.csv(path, check.names = FALSE))
  data.frame(
    source_id = source_id,
    status = "present",
    n_row = n_row,
    n_col = length(header),
    columns = paste(header, collapse = " | "),
    stringsAsFactors = FALSE
  )
}

#' @export
run_audit <- function(raw_dir = "data/raw", out_dir = "data/audit") {
  gov <- function(name) file.path(raw_dir, "datagov", name)
  mall <- function(name) file.path(raw_dir, "datamall", name)

  dereg <- parse_singstat_wide(gov("deregistrations_vqs_monthly.csv"))
  quota <- parse_singstat_wide(gov("quota_premium_pqp.csv"))
  bidding <- parse_bidding_csv(mall("M11-coe_results.csv"))
  registrations <- parse_singstat_wide(gov("new_registrations_vqs_monthly.csv"))

  write_out(series_profile(dereg, "deregistrations_vqs_monthly"), out_dir, "dereg_series.csv")
  write_out(
    series_profile(registrations, "new_registrations_vqs_monthly"),
    out_dir,
    "registration_series.csv"
  )
  write_out(series_profile(quota, "quota_premium_pqp"), out_dir, "quota_series.csv")
  write_out(bidding_quality(bidding), out_dir, "bidding_quality.csv")
  write_out(reconcile_quota(bidding, quota), out_dir, "quota_reconciliation.csv")
  write_out(quota_mismatch_rows(bidding, quota), out_dir, "quota_mismatches.csv")
  pop_path <- gov("population_vqs_monthly.csv")
  if (file.exists(pop_path)) {
    pop <- parse_singstat_wide(pop_path)
    write_out(series_profile(pop, "population_vqs_monthly"), out_dir, "population_series.csv")
  }
  quota_gap <- quota[
    quota$series == "Total Quota, 1st Bidding" & is.na(quota$value),
    c("series", "month"),
    drop = FALSE
  ]
  write_out(quota_gap, out_dir, "quota_missing_months.csv")
  dereg_gap <- dereg[is.na(dereg$value) & dereg$series != "Weekend Cars/Off Peak Cars", ]
  write_out(
    dereg_gap[, c("series", "month")],
    out_dir,
    "dereg_missing_months.csv"
  )
  legacy_names <- c(
    "population_vqs_monthly.csv",
    "new_registrations_vqs_monthly.csv"
  )
  legacy <- lapply(legacy_names, function(name) csv_profile(gov(name), name))
  write_out(do.call(rbind, legacy), out_dir, "other_csv_profile.csv")
  message("Audit tables written to ", normalizePath(out_dir))
  invisible(out_dir)
}
