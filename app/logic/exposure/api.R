# Load exposure calculator functions + supporting tables.

box::use(
  app/logic/viewdata/store[project_root],
)

exposure_dir <- function(root = project_root()) {
  file.path(root, "data", "processed", "exposure")
}

#' Source exposure_functions.R into an environment
#' @export
load_exposure_funs <- function(root = project_root()) {
  path <- file.path(root, "app", "logic", "exposure", "exposure_functions.R")
  if (!file.exists(path)) stop("Missing ", path)
  needed <- c("dplyr", "tidyr", "purrr", "tibble", "lubridate", "stringr")
  missing <- needed[!vapply(needed, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing)) stop("Missing packages for exposure engine: ", paste(missing, collapse = ", "))
  # Ensure verbs resolve when the sourced file uses bare dplyr/purrr names.
  for (pkg in needed) {
    if (!paste0("package:", pkg) %in% search()) {
      suppressPackageStartupMessages(base::library(pkg, character.only = TRUE))
    }
  }
  e <- new.env(parent = globalenv())
  sys.source(path, envir = e)
  e
}

#' @export
load_exposure_data <- function(root = project_root()) {
  d <- exposure_dir(root)
  read_rds <- function(f) {
    p <- file.path(d, f)
    if (!file.exists(p)) return(NULL)
    readRDS(p)
  }
  read_csv <- function(f) {
    p <- file.path(d, f)
    if (!file.exists(p)) return(NULL)
    utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
  }

  list(
    example_book = read_csv("example_order_book.csv"),
    example_fleet = read_csv("example_fleet_schedule.csv"),
    premium_moves = read_rds("premium_moves.rds"),
    price_sensitivity = read_rds("price_sensitivity.rds"),
    scenario_examples = read_rds("scenario_examples.rds"),
    premium_sims_baseline = read_rds("premium_sims_baseline.rds")
  )
}

#' Build a simple pooled guaranteed book from desk inputs
#' @export
make_pooled_book <- function(n_cars, category, quoted_premium, max_attempts = 4L) {
  n <- as.integer(n_cars)
  data.frame(
    order_id = sprintf("ORD%03d", seq_len(n)),
    category = as.character(category),
    target_exercise = rep(seq_len(6), length.out = n),
    max_attempts = as.integer(max_attempts),
    contract = "guaranteed",
    cap_topup = 5000,
    quoted_premium = as.numeric(quoted_premium),
    stringsAsFactors = FALSE
  )
}

BOOK_REQUIRED <- c(
  "order_id", "category", "quoted_premium", "target_exercise", "max_attempts", "contract"
)
FLEET_REQUIRED <- c("quarter", "category", "n_vehicles", "parf_rebate")

#' Validate / normalise an uploaded dealer order book (Doris schema).
#' @export
parse_order_book <- function(path_or_df) {
  b <- if (is.character(path_or_df)) {
    utils::read.csv(path_or_df, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    as.data.frame(path_or_df, stringsAsFactors = FALSE)
  }
  names(b) <- trimws(names(b))
  missing <- setdiff(BOOK_REQUIRED, names(b))
  if (length(missing)) {
    stop("Order book CSV needs columns: ", paste(BOOK_REQUIRED, collapse = ", "),
         ". Missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!"cap_topup" %in% names(b)) b$cap_topup <- 5000
  b$order_id <- as.character(b$order_id)
  b$category <- toupper(trimws(as.character(b$category)))
  b$quoted_premium <- as.numeric(b$quoted_premium)
  b$target_exercise <- as.integer(b$target_exercise)
  b$max_attempts <- as.integer(b$max_attempts)
  b$contract <- tolower(trimws(as.character(b$contract)))
  b$cap_topup <- as.numeric(b$cap_topup)
  b <- b[b$category %in% c("A", "B"), , drop = FALSE]
  if (!nrow(b)) stop("No Cat A/B rows in the order book.", call. = FALSE)
  bad_c <- setdiff(unique(b$contract), c("guaranteed", "capped", "rebate"))
  if (length(bad_c)) {
    stop("Unknown contract(s): ", paste(bad_c, collapse = ", "),
         ". Use guaranteed, capped, or rebate.", call. = FALSE)
  }
  if (any(!is.finite(b$quoted_premium)) || any(!is.finite(b$target_exercise)) ||
        any(!is.finite(b$max_attempts)) || any(b$max_attempts < 1L)) {
    stop("quoted_premium / target_exercise / max_attempts must be valid numbers.", call. = FALSE)
  }
  b[, c(BOOK_REQUIRED, "cap_topup"), drop = FALSE]
}

#' Validate / normalise an uploaded fleet schedule (Doris schema).
#' @export
parse_fleet_schedule <- function(path_or_df) {
  f <- if (is.character(path_or_df)) {
    utils::read.csv(path_or_df, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    as.data.frame(path_or_df, stringsAsFactors = FALSE)
  }
  names(f) <- trimws(names(f))
  missing <- setdiff(FLEET_REQUIRED, names(f))
  if (length(missing)) {
    stop("Fleet CSV needs columns: ", paste(FLEET_REQUIRED, collapse = ", "),
         ". Missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  f$quarter <- as.integer(f$quarter)
  f$category <- toupper(trimws(as.character(f$category)))
  f$n_vehicles <- as.integer(f$n_vehicles)
  f$parf_rebate <- as.numeric(f$parf_rebate)
  if (!"new_vehicle_cost" %in% names(f)) f$new_vehicle_cost <- 0
  if (!"old_vehicle_residual" %in% names(f)) f$old_vehicle_residual <- 0
  f$new_vehicle_cost <- as.numeric(f$new_vehicle_cost)
  f$old_vehicle_residual <- as.numeric(f$old_vehicle_residual)
  f <- f[f$category %in% c("A", "B"), , drop = FALSE]
  if (!nrow(f)) stop("No Cat A/B rows in the fleet schedule.", call. = FALSE)
  if (any(!is.finite(f$quarter)) || any(!is.finite(f$n_vehicles)) || any(f$n_vehicles < 1L)) {
    stop("quarter / n_vehicles must be valid positive integers.", call. = FALSE)
  }
  f
}
