# Stop the build when a structural check fails.
# Informational checks are recorded and do not stop the build.

box::use(
  app/logic/data/parse[parse_singstat_wide],
  app/logic/data/standardise[shift_month],
)

check_row <- function(name, status, detail) {
  data.frame(
    check = name,
    status = status,
    detail = detail,
    stringsAsFactors = FALSE
  )
}

key_duplicates <- function(data, columns) {
  key <- do.call(paste, c(data[, columns, drop = FALSE], sep = "|"))
  sum(duplicated(key))
}

#' @export
assert_products <- function(panel, history, age, quota, model_frame, raw_dir) {
  bucket <- new.env(parent = emptyenv())
  bucket$rows <- list()
  add <- function(name, ok, detail) {
    bucket$rows[[length(bucket$rows) + 1]] <- check_row(
      name,
      if (ok) "pass" else "fail",
      detail
    )
  }
  add(
    "panel_unique_keys",
    key_duplicates(panel, c("month", "category")) == 0,
    paste("duplicate keys", key_duplicates(panel, c("month", "category")))
  )
  expected_rows <- length(unique(panel$month)) * length(unique(panel$category))
  add(
    "panel_row_count",
    nrow(panel) == expected_rows,
    paste("rows", nrow(panel), "expected", expected_rows)
  )
  category_e <- panel$category == "E"
  add(
    "category_e_has_no_stock_or_flow",
    all(is.na(panel$deregistrations[category_e])) &&
      all(is.na(panel$population[category_e])) &&
      all(is.na(panel$registrations[category_e])),
    "Category E deregistrations, population, and registrations are missing"
  )
  counts <- c(
    panel$deregistrations, panel$population, panel$registrations,
    panel$revalidations_5y, panel$revalidations_10y, panel$quota_sum,
    history$quota, history$bids_success, history$bids_received, age$number
  )
  add(
    "no_negative_counts",
    !any(counts < 0, na.rm = TRUE),
    "counts and quotas are non-negative where observed"
  )
  category_c <- panel$category == "C"
  both <- category_c & !is.na(panel$registrations_not_ets) & !is.na(panel$registrations_ets)
  ets_gap <- panel$registrations[both] -
    (panel$registrations_not_ets[both] + panel$registrations_ets[both])
  add(
    "category_c_ets_identity",
    length(ets_gap) > 0 && all(ets_gap == 0),
    paste("months", sum(both), "max abs gap", max(abs(ets_gap)))
  )
  suspended <- panel$month %in% as.Date(c("2020-04-01", "2020-05-01", "2020-06-01"))
  add(
    "bidding_suspension_stays_missing",
    all(is.na(panel$premium_mean[suspended])) &&
      all(panel$bidding_status[suspended] == "suspended"),
    "April-June 2020 premiums are missing"
  )
  add(
    "bidding_unique_keys",
    key_duplicates(history, c("month", "bidding_no", "category")) == 0,
    paste("duplicate exercise keys", key_duplicates(history, c("month", "bidding_no", "category")))
  )
  kept <- quota[quota$grain == "exercise", c("month", "category", "bidding_quota")]
  monthly <- quota[quota$grain == "month", , drop = FALSE]
  summed <- stats::aggregate(
    list(from_exercises = kept[["bidding_quota"]]),
    by = list(month = kept[["month"]], category = kept[["category"]]),
    FUN = function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
  )
  compared <- merge(monthly, summed, by = c("month", "category"))
  add(
    "monthly_quota_equals_exercises",
    nrow(kept) > 0 && all(same_number(compared$bidding_quota, compared$from_exercises)),
    paste("exercise rows", nrow(kept), "months", nrow(compared))
  )
  add(
    "official_quarterly_quota_not_invented",
    all(is.na(quota$official_quarterly_quota)) && all(is.na(quota$policy_adjustment)),
    "adjustment components remain unknown"
  )
  add(
    "age_keeps_unpublished_blanks",
    any(age$number_status == "not_published") &&
      all(is.na(age$number[age$number_status == "not_published"])),
    paste("unpublished age cells", sum(age$number_status == "not_published"))
  )
  add(
    "model_unique_keys",
    key_duplicates(model_frame, c("month", "category")) == 0 &&
      all(model_frame$category != "E"),
    paste("rows", nrow(model_frame))
  )
  lagged <- merge(
    model_frame[, c("month", "category", "dereg_lag_2")],
    panel[, c("month", "category", "deregistrations")],
    by.x = c("month", "category"),
    by.y = c("month", "category"),
    all.x = TRUE
  )
  lagged$source_month <- shift_month(lagged$month, -2L)
  source <- panel[, c("month", "category", "deregistrations")]
  names(source) <- c("source_month", "category", "source_value")
  lagged <- merge(lagged, source, by = c("source_month", "category"), all.x = TRUE)
  add(
    "dereg_lag_uses_published_month",
    all(same_number(lagged$dereg_lag_2, lagged$source_value)),
    "dereg_lag_2 equals deregistrations two months earlier"
  )
  add(
    "no_one_month_deregistration_lag",
    !"dereg_lag_1" %in% names(model_frame),
    "the one-month deregistration lag is excluded"
  )
  reg <- parse_singstat_wide(file.path(raw_dir, "datagov", "new_registrations_vqs_monthly.csv"))
  total_name <- "Total New Motor Vehicles Registered"
  parts <- c(
    "Category A: Cars",
    "Category B: Cars",
    "Weekend Cars/Off Peak Cars",
    "Category C: Goods Vehicles & Buses",
    "Category D: Motorcycles & Scooters",
    "Taxis",
    "Vehicles Exempted From VQS"
  )
  identity_bad <- 0L
  identity_n <- 0L
  for (month_value in unique(reg$month)) {
    slice <- reg[reg$month == month_value, , drop = FALSE]
    total <- slice$value[slice$series == total_name]
    values <- slice$value[match(parts, slice$series)]
    names(values) <- parts
    if (is.na(values[["Weekend Cars/Off Peak Cars"]])) {
      values <- values[names(values) != "Weekend Cars/Off Peak Cars"]
    }
    if (any(is.na(c(total, values)))) {
      next
    }
    identity_n <- identity_n + 1L
    if (total != sum(values)) {
      identity_bad <- identity_bad + 1L
    }
  }
  add(
    "registration_total_identity",
    identity_bad == 0 && identity_n > 0,
    paste("months", identity_n, "differences", identity_bad)
  )
  out <- do.call(rbind, bucket$rows)
  failed <- out$check[out$status == "fail"]
  if (length(failed)) {
    stop(
      "Processed-data checks failed: ",
      paste(failed, collapse = ", "),
      call. = FALSE
    )
  }
  out
}

same_number <- function(x, y) {
  (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
}
