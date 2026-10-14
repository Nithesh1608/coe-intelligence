# Build the processed analytical tables from local raw files.
# No download, no imputation, and no model fit.

box::use(
  app/logic/data/checks[assert_products],
  app/logic/data/standardise[
    age_long,
    bidding_from_datamall,
    bidding_from_singstat,
    pqp_from_datamall,
    pqp_from_singstat,
    revalidations_long,
    shift_month,
    singstat_flow
  ],
)

flow_series <- c(
  "A" = "Category A: Cars",
  "B" = "Category B: Cars",
  "C" = "Category C: Goods Vehicles & Buses",
  "D" = "Category D: Motorcycles & Scooters"
)

#' Publication lags, in months, used by the model frame.
#' A value for month m may be used from month m + lag onward.
#' @export
publication_lags <- function() {
  c(
    deregistrations = 2L,
    population = 2L,
    registrations = 2L,
    revalidations = 3L,
    age = 2L,
    bidding = 1L,
    pqp = 0L
  )
}

status_for <- function(category, value) {
  ifelse(
    category == "E",
    "not_applicable",
    ifelse(!is.na(value), "observed", "not_published")
  )
}

empty_grid <- function(months, categories) {
  data.frame(
    month = rep(months, each = length(categories)),
    category = rep(categories, times = length(months)),
    stringsAsFactors = FALSE
  )
}

pick_flow <- function(path, series_name, column) {
  part <- singstat_flow(path, series_name)
  names(part)[names(part) == "value"] <- column
  part[, c("category", "month", column)]
}

same_number <- function(x, y) {
  both_na <- is.na(x) & is.na(y)
  both_na | (!is.na(x) & !is.na(y) & x == y)
}

#' @export
bidding_conflicts <- function(datamall, singstat) {
  both <- merge(
    datamall,
    singstat,
    by = c("month", "bidding_no", "category"),
    suffixes = c("_datamall", "_singstat")
  )
  fields <- c("quota", "bids_success", "bids_received", "premium")
  pieces <- lapply(fields, function(field) {
    left <- both[[paste0(field, "_datamall")]]
    right <- both[[paste0(field, "_singstat")]]
    bad <- !same_number(left, right)
    if (!any(bad)) {
      return(NULL)
    }
    data.frame(
      month = both$month[bad],
      bidding_no = both$bidding_no[bad],
      category = both$category[bad],
      field = field,
      datamall = left[bad],
      singstat = right[bad],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, pieces)
  if (is.null(out)) {
    return(both[0, c("month", "bidding_no", "category"), drop = FALSE])
  }
  out
}

#' @export
build_bidding_history <- function(raw_dir = "data/raw") {
  datamall <- bidding_from_datamall(
    file.path(raw_dir, "datamall", "M11-coe_results.csv")
  )
  singstat <- bidding_from_singstat(
    file.path(raw_dir, "datagov", "quota_premium_pqp.csv")
  )
  conflicts <- bidding_conflicts(datamall, singstat)
  if (nrow(conflicts) > 0) {
    stop("Bidding sources disagree on ", nrow(conflicts), " cells.", call. = FALSE)
  }
  early <- singstat[singstat$month < as.Date("2010-01-01"), , drop = FALSE]
  history <- rbind(early, datamall)
  history[order(history$month, history$bidding_no, history$category), , drop = FALSE]
}

pqp_conflicts <- function(datamall, singstat) {
  both <- merge(
    datamall[, c("month", "category", "pqp")],
    singstat[, c("month", "category", "pqp")],
    by = c("month", "category"),
    suffixes = c("_datamall", "_singstat")
  )
  bad <- !same_number(both$pqp_datamall, both$pqp_singstat)
  both[bad, , drop = FALSE]
}

#' @export
build_pqp <- function(raw_dir = "data/raw") {
  datamall <- pqp_from_datamall(
    file.path(raw_dir, "datamall", "M11-coe_results_pqp.csv")
  )
  singstat <- pqp_from_singstat(
    file.path(raw_dir, "datagov", "quota_premium_pqp.csv")
  )
  conflicts <- pqp_conflicts(datamall, singstat)
  if (nrow(conflicts) > 0) {
    stop("PQP sources disagree on ", nrow(conflicts), " cells.", call. = FALSE)
  }
  early <- singstat[singstat$month < min(datamall$month), , drop = FALSE]
  rbind(early, datamall)
}

#' @export
build_age <- function(raw_dir = "data/raw") {
  age <- age_long(file.path(raw_dir, "datamall", "M01-03M-Age.csv"))
  age[order(age$month, age$vehicle_type, age$age_band), , drop = FALSE]
}

age_share <- function(age) {
  bands <- c("8 - < 9", "9 - < 10", "10 - < 11")
  types <- unique(age$vehicle_type)
  months <- unique(age$month)
  rows <- lapply(types, function(type_name) {
    part <- age[age$vehicle_type == type_name, , drop = FALSE]
    lapply(months, function(month_value) {
      slice <- part[part$month == month_value, , drop = FALSE]
      chosen <- slice$number[match(bands, slice$age_band)]
      total <- if (any(is.na(slice$number))) NA_real_ else sum(slice$number)
      share <- if (any(is.na(chosen)) || is.na(total) || total == 0) {
        NA_real_
      } else {
        sum(chosen) / total
      }
      data.frame(
        month = month_value,
        vehicle_type = type_name,
        age_share_8_to_11 = share,
        stringsAsFactors = FALSE
      )
    })
  })
  do.call(rbind, unlist(rows, recursive = FALSE))
}

sum_exercises <- function(history) {
  keys <- unique(history[, c("month", "category")])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    part <- history[
      history$month == keys$month[[i]] & history$category == keys$category[[i]],
      ,
      drop = FALSE
    ]
    quota <- if (all(is.na(part$quota))) NA_real_ else sum(part$quota, na.rm = TRUE)
    success <- if (all(is.na(part$bids_success))) {
      NA_real_
    } else {
      sum(part$bids_success, na.rm = TRUE)
    }
    received <- if (all(is.na(part$bids_received))) {
      NA_real_
    } else {
      sum(part$bids_received, na.rm = TRUE)
    }
    data.frame(
      month = keys$month[[i]],
      category = keys$category[[i]],
      quota_sum = quota,
      n_exercises = sum(!is.na(part$quota)),
      bids_success_sum = success,
      bids_received_sum = received,
      premium_1 = part$premium[match("1", part$bidding_no)],
      premium_2 = part$premium[match("2", part$bidding_no)],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out$bid_ratio <- out$bids_received_sum / out$quota_sum
  out$premium_mean <- rowMeans(
    cbind(out$premium_1, out$premium_2),
    na.rm = TRUE
  )
  out$premium_mean[is.nan(out$premium_mean)] <- NA_real_
  out
}

#' @export
build_quota_history <- function(history) {
  exercise <- data.frame(
    grain = "exercise",
    month = history$month,
    quarter = NA_character_,
    bidding_no = history$bidding_no,
    category = history$category,
    bidding_quota = history$quota,
    n_exercises = 1L,
    official_quarterly_quota = NA_real_,
    policy_adjustment = NA_real_,
    adjustment_status = "unknown",
    source = history$source,
    stringsAsFactors = FALSE
  )
  monthly_values <- sum_exercises(history)
  monthly <- data.frame(
    grain = "month",
    month = monthly_values$month,
    quarter = NA_character_,
    bidding_no = NA_character_,
    category = monthly_values$category,
    bidding_quota = monthly_values$quota_sum,
    n_exercises = monthly_values$n_exercises,
    official_quarterly_quota = NA_real_,
    policy_adjustment = NA_real_,
    adjustment_status = "unknown",
    source = "sum_of_exercises",
    stringsAsFactors = FALSE
  )
  quarter_start <- as.Date(paste0(
    format(history$month, "%Y"),
    "-",
    sprintf("%02d", ((as.integer(format(history$month, "%m")) - 1L) %/% 3L) * 3L + 1L),
    "-01"
  ))
  history$quarter_start <- quarter_start
  groups <- unique(history[, c("quarter_start", "category")])
  quarterly <- lapply(seq_len(nrow(groups)), function(i) {
    part <- history[
      history$quarter_start == groups$quarter_start[[i]] &
        history$category == groups$category[[i]],
      ,
      drop = FALSE
    ]
    data.frame(
      grain = "quarter",
      month = groups$quarter_start[[i]],
      quarter = paste0(
        format(groups$quarter_start[[i]], "%Y"),
        "-Q",
        (as.integer(format(groups$quarter_start[[i]], "%m")) - 1L) %/% 3L + 1L
      ),
      bidding_no = NA_character_,
      category = groups$category[[i]],
      bidding_quota = sum(part$quota, na.rm = TRUE),
      n_exercises = sum(!is.na(part$quota)),
      official_quarterly_quota = NA_real_,
      policy_adjustment = NA_real_,
      adjustment_status = "unknown",
      source = "sum_of_exercises",
      stringsAsFactors = FALSE
    )
  })
  out <- rbind(exercise, monthly, do.call(rbind, quarterly))
  out[order(out$grain, out$month, out$category, out$bidding_no), , drop = FALSE]
}

spread_term <- function(revalidations, term, column) {
  part <- revalidations[revalidations$term == term, c("month", "category", "value")]
  names(part)[names(part) == "value"] <- column
  part
}

#' @export
build_panel <- function(raw_dir = "data/raw", history = NULL, pqp = NULL) {
  if (is.null(history)) {
    history <- build_bidding_history(raw_dir)
  }
  if (is.null(pqp)) {
    pqp <- build_pqp(raw_dir)
  }
  gov <- function(name) file.path(raw_dir, "datagov", name)
  dereg <- pick_flow(gov("deregistrations_vqs_monthly.csv"), unname(flow_series), "deregistrations")
  population <- pick_flow(gov("population_vqs_monthly.csv"), unname(flow_series), "population")
  registrations <- pick_flow(
    gov("new_registrations_vqs_monthly.csv"),
    unname(flow_series),
    "registrations"
  )
  reg_not <- singstat_flow(
    gov("new_registrations_vqs_monthly.csv"),
    "Category C: Goods Vehicles & Buses Not Under ETS"
  )
  reg_ets <- singstat_flow(
    gov("new_registrations_vqs_monthly.csv"),
    "Category C: Goods Vehicles & Buses Under ETS"
  )
  reg_not <- data.frame(
    month = reg_not$month,
    category = "C",
    registrations_not_ets = reg_not$value,
    stringsAsFactors = FALSE
  )
  reg_ets <- data.frame(
    month = reg_ets$month,
    category = "C",
    registrations_ets = reg_ets$value,
    stringsAsFactors = FALSE
  )
  renewals <- revalidations_long(
    file.path(raw_dir, "datamall", "M10-Monthly_COE_Revalidation.csv")
  )
  months <- seq(as.Date("1990-05-01"), as.Date("2026-10-01"), by = "month")
  panel <- empty_grid(months, c("A", "B", "C", "D", "E"))
  panel <- merge(panel, dereg, by = c("month", "category"), all.x = TRUE)
  panel <- merge(panel, population, by = c("month", "category"), all.x = TRUE)
  panel <- merge(panel, registrations, by = c("month", "category"), all.x = TRUE)
  panel <- merge(panel, reg_not, by = c("month", "category"), all.x = TRUE)
  panel <- merge(panel, reg_ets, by = c("month", "category"), all.x = TRUE)
  five_year <- spread_term(renewals, "5y", "revalidations_5y")
  ten_year <- spread_term(renewals, "10y", "revalidations_10y")
  panel <- merge(panel, five_year, by = c("month", "category"), all.x = TRUE)
  panel <- merge(panel, ten_year, by = c("month", "category"), all.x = TRUE)
  panel$revalidations_total <- panel$revalidations_5y + panel$revalidations_10y
  pqp_cols <- pqp[, c("month", "category", "pqp", "source")]
  names(pqp_cols)[names(pqp_cols) == "source"] <- "pqp_source"
  panel <- merge(panel, pqp_cols, by = c("month", "category"), all.x = TRUE)
  panel <- merge(panel, sum_exercises(history), by = c("month", "category"), all.x = TRUE)
  suspended <- panel$month %in% as.Date(c("2020-04-01", "2020-05-01", "2020-06-01"))
  panel$bidding_status <- ifelse(
    suspended,
    "suspended",
    ifelse(is.na(panel$n_exercises), "not_held_or_not_published", "observed")
  )
  panel$dereg_status <- status_for(panel$category, panel$deregistrations)
  panel$population_status <- status_for(panel$category, panel$population)
  panel$registrations_status <- status_for(panel$category, panel$registrations)
  panel[order(panel$month, panel$category), , drop = FALSE]
}

attach_lag <- function(frame, source, value_column, lag, new_name) {
  lookup <- source[, c("month", "category", value_column)]
  lookup$month <- shift_month(lookup$month, lag)
  names(lookup)[names(lookup) == value_column] <- new_name
  merge(frame, lookup, by = c("month", "category"), all.x = TRUE)
}

#' @export
build_model_frame <- function(panel, age, lags = publication_lags()) {
  frame <- panel[panel$category != "E", c("month", "category", "deregistrations")]
  frame <- attach_lag(frame, panel, "deregistrations", lags[["deregistrations"]], "dereg_lag_2")
  frame <- attach_lag(frame, panel, "deregistrations", 3L, "dereg_lag_3")
  frame <- attach_lag(frame, panel, "deregistrations", 12L, "dereg_lag_12")
  frame <- attach_lag(frame, panel, "deregistrations", 13L, "dereg_lag_13")
  frame <- attach_lag(frame, panel, "population", lags[["population"]], "population_lag_2")
  frame <- attach_lag(frame, panel, "registrations", lags[["registrations"]], "registrations_lag_2")
  frame <- attach_lag(
    frame, panel, "revalidations_total", lags[["revalidations"]], "revalidations_total_lag_3"
  )
  frame <- attach_lag(frame, panel, "pqp", lags[["pqp"]], "pqp")
  frame <- attach_lag(frame, panel, "premium_mean", lags[["bidding"]], "premium_mean_lag_1")
  frame <- attach_lag(frame, panel, "bid_ratio", lags[["bidding"]], "bid_ratio_lag_1")
  frame <- attach_lag(frame, panel, "quota_sum", lags[["bidding"]], "quota_sum_lag_1")
  roll_source <- panel[, c("month", "category", "deregistrations")]
  roll_rows <- lapply(seq_len(nrow(frame)), function(i) {
    target <- frame$month[[i]]
    category <- frame$category[[i]]
    lag_start <- lags[["deregistrations"]]
    window <- shift_month(target, -seq.int(lag_start, lag_start + 11L))
    values <- roll_source$deregistrations[
      roll_source$category == category & roll_source$month %in% window
    ]
    if (length(values) < 12L || any(is.na(values))) NA_real_ else mean(values)
  })
  frame$dereg_roll_mean_12 <- unlist(roll_rows)
  shares <- age_share(age)
  scope <- c(A = "Cars", B = "Cars", C = "Goods and Other Vehicles", D = "Motorcycles")
  frame$age_share_scope <- unname(scope[frame$category])
  share_lookup <- shares
  share_lookup$month <- shift_month(share_lookup$month, lags[["age"]])
  names(share_lookup)[names(share_lookup) == "vehicle_type"] <- "age_share_scope"
  names(share_lookup)[names(share_lookup) == "age_share_8_to_11"] <- "age_share_8_to_11_lag_2"
  frame <- merge(
    frame,
    share_lookup,
    by = c("month", "age_share_scope"),
    all.x = TRUE
  )
  frame$month_of_year <- as.integer(format(frame$month, "%m"))
  frame$rule_ab_power_cap <- as.integer(frame$month >= as.Date("2014-02-01"))
  frame$rule_ab_ev <- as.integer(frame$month >= as.Date("2022-05-01"))
  frame$rule_cat_e_shift <- as.integer(frame$month >= as.Date("2017-05-01"))
  frame$rule_growth_freeze <- as.integer(
    frame$month >= as.Date("2018-02-01") & frame$month <= as.Date("2028-01-01")
  )
  frame$bidding_suspended <- as.integer(
    frame$month %in% as.Date(c("2020-04-01", "2020-05-01", "2020-06-01"))
  )
  frame$sample_baseline <- !is.na(frame$deregistrations)
  frame$sample_seasonal <- frame$sample_baseline &
    !is.na(frame$dereg_lag_2) & !is.na(frame$dereg_lag_12) & !is.na(frame$dereg_roll_mean_12)
  frame$sample_flow <- frame$sample_seasonal &
    !is.na(frame$population_lag_2) & !is.na(frame$registrations_lag_2)
  frame$sample_price <- frame$sample_flow &
    !is.na(frame$pqp) & !is.na(frame$premium_mean_lag_1) & !is.na(frame$bid_ratio_lag_1)
  frame$sample_renewal <- frame$sample_price & !is.na(frame$revalidations_total_lag_3)
  frame$sample_age <- frame$sample_price & !is.na(frame$age_share_8_to_11_lag_2)
  frame[order(frame$month, frame$category), , drop = FALSE]
}

write_table <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data, path, row.names = FALSE, na = "")
  invisible(path)
}

#' @export
build_all <- function(raw_dir = "data/raw", out_dir = "data/processed") {
  history <- build_bidding_history(raw_dir)
  pqp <- build_pqp(raw_dir)
  age <- build_age(raw_dir)
  panel <- build_panel(raw_dir, history, pqp)
  quota <- build_quota_history(history)
  model_frame <- build_model_frame(panel, age)
  checks <- assert_products(panel, history, age, quota, model_frame, raw_dir)
  write_table(panel, file.path(out_dir, "coe_monthly_panel.csv"))
  write_table(history, file.path(out_dir, "coe_bidding_history.csv"))
  write_table(age, file.path(out_dir, "vehicle_age_distribution.csv"))
  write_table(quota, file.path(out_dir, "coe_quota_history.csv"))
  write_table(model_frame, file.path(out_dir, "coe_model_frame.csv"))
  write_table(checks, file.path(out_dir, "build_checks.csv"))
  invisible(list(
    panel = panel,
    history = history,
    age = age,
    quota = quota,
    model_frame = model_frame,
    checks = checks
  ))
}
