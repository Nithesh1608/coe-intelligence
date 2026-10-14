# Official sources used by the Phase 1 audit.
# URLs are the public download endpoints or the DataMall file links that the
# static-data page exposes. Nothing here is inferred from an unpublished API.

datagov_api <- "https://api-open.data.gov.sg/v1/public/api/datasets"

#' @export
datagov_sources <- function() {
  data.frame(
    source_id = c(
      "quota_premium_pqp",
      "deregistrations_vqs_monthly",
      "population_vqs_monthly",
      "new_registrations_vqs_monthly"
    ),
    dataset_id = c(
      "d_22094bf608253d36c0c63b52d852dd6e",
      "d_d520d6034b5e0c4f883b4e480de28f97",
      "d_ede1a559013d10f234d209ac5e9fd9b4",
      "d_d94cf5d839fc11a144f24ef971705d3e"
    ),
    publisher = c(
      "SingStat via data.gov.sg",
      "SingStat via data.gov.sg",
      "SingStat via data.gov.sg",
      "SingStat via data.gov.sg"
    ),
    stringsAsFactors = FALSE
  )
}

#' @export
datamall_sources <- function() {
  data.frame(
    source_id = character(),
    file_name = character(),
    url = character(),
    publisher = character(),
    stringsAsFactors = FALSE
  )
}

#' @export
page_url <- function(dataset_id) {
  paste0("https://data.gov.sg/datasets/", dataset_id, "/view")
}

#' @export
initiate_url <- function(dataset_id) {
  paste0(datagov_api, "/", dataset_id, "/initiate-download")
}

#' @export
poll_url <- function(dataset_id) {
  paste0(datagov_api, "/", dataset_id, "/poll-download")
}
