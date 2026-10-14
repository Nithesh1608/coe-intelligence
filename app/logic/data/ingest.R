# Download official snapshots. Existing files are kept unless refresh = TRUE,
# so a later run does not depend on the government API being reachable.

box::use(
  httr2[req_perform, req_retry, req_user_agent, request, resp_body_json],
)
box::use(
  app/logic/data/catalogue[
    datagov_sources,
    datamall_sources,
    initiate_url,
    poll_url
  ],
)

user_agent <- "COE-Intelligence-audit (NUS DBA3702 educational project)"

or_default <- function(x, fallback) {
  if (is.null(x) || length(x) == 0) {
    return(fallback)
  }
  x
}

perform_json <- function(url) {
  response <- request(url) |>
    req_user_agent(user_agent) |>
    req_retry(max_tries = 6, backoff = function(attempt) 10 * attempt) |>
    req_perform()
  resp_body_json(response)
}

download_file <- function(url, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  request(url) |>
    req_user_agent(user_agent) |>
    req_retry(max_tries = 4, backoff = function(attempt) 5 * attempt) |>
    req_perform(path = dest)
  dest
}

#' @export
download_datagov <- function(dataset_id, dest) {
  perform_json(initiate_url(dataset_id))
  url <- NULL
  status <- NA_character_
  for (attempt in seq_len(6)) {
    Sys.sleep(3)
    body <- perform_json(poll_url(dataset_id))
    status <- or_default(body$data$status, NA_character_)
    url <- body$data$url
    if (!is.null(url) && nzchar(url)) {
      break
    }
  }
  if (is.null(url) || !nzchar(url)) {
    stop(
      "data.gov.sg did not return a file URL for ",
      dataset_id, " (", status, ")."
    )
  }
  download_file(url, dest)
  Sys.sleep(8)
  status
}

log_row <- function(source_id, dest, status, url) {
  size <- if (file.exists(dest)) file.info(dest)$size else NA_real_
  data.frame(
    source_id = source_id,
    path = dest,
    status = status,
    bytes = size,
    url = url,
    recorded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    stringsAsFactors = FALSE
  )
}

#' @export
download_snapshots <- function(raw_dir = "data/raw", refresh = FALSE) {
  gov_dir <- file.path(raw_dir, "datagov")
  mall_dir <- file.path(raw_dir, "datamall")
  dir.create(gov_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(mall_dir, recursive = TRUE, showWarnings = FALSE)
  gov <- datagov_sources()
  mall <- datamall_sources()
  rows <- list()

  for (i in seq_len(nrow(gov))) {
    dest <- file.path(gov_dir, paste0(gov$source_id[[i]], ".csv"))
    page <- initiate_url(gov$dataset_id[[i]])
    if (file.exists(dest) && !refresh) {
      rows[[length(rows) + 1]] <- log_row(gov$source_id[[i]], dest, "cached", page)
      next
    }
    message("Downloading ", gov$source_id[[i]])
    status <- download_datagov(gov$dataset_id[[i]], dest)
    rows[[length(rows) + 1]] <- log_row(gov$source_id[[i]], dest, status, page)
  }

  for (i in seq_len(nrow(mall))) {
    dest <- file.path(mall_dir, mall$file_name[[i]])
    if (file.exists(dest) && !refresh) {
      rows[[length(rows) + 1]] <- log_row(
        mall$source_id[[i]], dest, "cached", mall$url[[i]]
      )
      next
    }
    message("Downloading ", mall$file_name[[i]])
    download_file(mall$url[[i]], dest)
    rows[[length(rows) + 1]] <- log_row(
      mall$source_id[[i]], dest, "downloaded", mall$url[[i]]
    )
  }

  log <- do.call(rbind, rows)
  utils::write.csv(log, file.path(raw_dir, "download_log.csv"), row.names = FALSE)
  log
}
