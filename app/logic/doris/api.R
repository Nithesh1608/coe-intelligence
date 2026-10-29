# Load Doris exposure functions + supporting tables for the calculator.

box::use(
  app/logic/viewdata/store[project_root],
)

doris_dir <- function(root = project_root()) {
  file.path(root, "data", "processed", "doris")
}

#' Source exposure_functions.R into an environment
#' @export
load_doris_funs <- function(root = project_root()) {
  path <- file.path(root, "app", "logic", "doris", "exposure_functions.R")
  if (!file.exists(path)) stop("Missing ", path)
  e <- new.env(parent = globalenv())
  sys.source(path, envir = e)
  e
}

#' @export
load_doris_data <- function(root = project_root()) {
  d <- doris_dir(root)
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
    premium_sims_baseline = read_rds("premium_sims_baseline.rds"),
    car_cost = read_rds("car_cost_2026_08.rds")
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
