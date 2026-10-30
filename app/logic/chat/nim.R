# NVIDIA NIM chat completions (OpenAI-compatible).

NIM_BASE <- "https://integrate.api.nvidia.com/v1"

# Hosted Llama 3.1/3.3 instruct IDs hit EOL on NVIDIA's free catalog (Aug 2026).
# Prefer a current instruct model; override with NVIDIA_NIM_MODEL in .Renviron.
NIM_MODEL_DEFAULT <- "deepseek-ai/deepseek-v4.1-flash"
NIM_MODEL_FALLBACKS <- c(
  "google/gemma-3-12b-it",
  "nv-mistralai/mistral-nemo-12b-instruct",
  "nvidia/mistral-nemo-minitron-8b-8k-instruct",
  "ibm/granite-3.0-8b-instruct",
  "aisingapore/sea-lion-7b-instruct"
)

#' Load project .Renviron once per process if present.
#' @export
ensure_nvidia_env <- function(root = NULL) {
  if (!nzchar(Sys.getenv("NVIDIA_API_KEY", ""))) {
    if (is.null(root)) {
      box::use(app/logic/viewdata/store[project_root])
      root <- project_root()
    }
    path <- file.path(root, ".Renviron")
    if (file.exists(path)) readRenviron(path)
  }
  invisible(nzchar(Sys.getenv("NVIDIA_API_KEY", "")))
}

#' @export
nvidia_key_ok <- function() {
  ensure_nvidia_env()
  nzchar(Sys.getenv("NVIDIA_API_KEY", ""))
}

#' @export
nim_model_id <- function() {
  ensure_nvidia_env()
  m <- Sys.getenv("NVIDIA_NIM_MODEL", "")
  if (nzchar(m)) m else NIM_MODEL_DEFAULT
}

#' @export
nim_model_candidates <- function() {
  unique(c(nim_model_id(), NIM_MODEL_FALLBACKS))
}

parse_nim_content <- function(body) {
  if (is.null(body$choices) || !length(body$choices)) {
    err <- body$error$message
    if (is.null(err) || !nzchar(as.character(err))) {
      err <- body$detail
    }
    if (is.null(err) || !nzchar(as.character(err))) err <- "Empty response from NIM."
    stop(as.character(err)[[1]], call. = FALSE)
  }
  ch <- body$choices[[1]]
  msg <- ch$message
  if (is.null(msg$content) || !nzchar(msg$content)) {
    stop("Model returned no text.", call. = FALSE)
  }
  msg$content
}

nim_request <- function(messages, model, key, temperature = 0.25, max_tokens = 1200L) {
  box::use(httr2[
    request, req_headers, req_body_json, req_options, req_timeout,
    req_error, req_perform, resp_body_json, resp_status
  ])

  # Same OpenAI-compatible body as build.nvidia.com samples (text chat).
  # CURL_HTTP_VERSION_1_1 = 2 — HTTP/2 POSTs to integrate.api.nvidia.com hang
  # under R/libcurl on Windows; GET works, PowerShell/curl.exe also fine.
  # CURL_IPRESOLVE_V4 = 1 as a belt-and-suspenders for dual-stack stalls.
  resp <- request(paste0(NIM_BASE, "/chat/completions")) |>
    req_headers(
      Authorization = paste("Bearer", key),
      `Content-Type` = "application/json",
      Accept = "application/json"
    ) |>
    req_body_json(
      list(
        model = model,
        messages = messages,
        temperature = temperature,
        top_p = 0.95,
        max_tokens = as.integer(max_tokens),
        stream = FALSE
      ),
      auto_unbox = TRUE
    ) |>
    req_options(ipresolve = 1L, http_version = 2L) |>
    req_timeout(90) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()

  status <- resp_status(resp)
  body <- tryCatch(resp_body_json(resp), error = function(e) NULL)
  list(status = status, body = body)
}

activation_hint <- function(model) {
  paste0(
    "Model '", model, "' is not enabled for this NVIDIA API key (HTTP 404). ",
    "Open https://build.nvidia.com/", model, " , click Get API Key / accept the model terms, ",
    "then retry. Or set NVIDIA_NIM_MODEL in .Renviron to another enabled model id."
  )
}

#' Chat with NVIDIA NIM. `messages` = list of list(role=, content=).
#' @export
nim_chat <- function(messages, temperature = 0.25, max_tokens = 1200L) {
  ensure_nvidia_env()
  key <- Sys.getenv("NVIDIA_API_KEY", "")
  if (!nzchar(key)) {
    stop(
      "NVIDIA_API_KEY is missing. Add it to the project .Renviron and restart the app.",
      call. = FALSE
    )
  }

  clean <- lapply(messages, function(m) {
    list(role = as.character(m$role), content = as.character(m$content))
  })

  last_err <- NULL
  for (model in nim_model_candidates()) {
    res <- tryCatch(
      nim_request(clean, model, key, temperature, max_tokens),
      error = function(e) list(status = 0L, body = list(detail = conditionMessage(e)))
    )
    if (res$status >= 200L && res$status < 300L) {
      return(list(ok = TRUE, text = parse_nim_content(res$body), model = model))
    }
    detail <- NULL
    if (!is.null(res$body$detail)) detail <- as.character(res$body$detail)[[1]]
    if (is.null(detail) && !is.null(res$body$error$message)) {
      detail <- as.character(res$body$error$message)[[1]]
    }
    if (is.null(detail)) detail <- paste("HTTP", res$status)

    if (res$status == 404L || grepl("Not found for account", detail, fixed = TRUE)) {
      last_err <- activation_hint(model)
      next
    }
    if (res$status == 410L || grepl("end of life", detail, ignore.case = TRUE)) {
      last_err <- paste0(model, " is EOL on NIM. Set NVIDIA_NIM_MODEL to a current id.")
      next
    }
    stop(sprintf("NIM failed (%s): %s", model, detail), call. = FALSE)
  }

  stop(last_err %||% "No NIM model responded.", call. = FALSE)
}

`%||%` <- function(a, b) if (is.null(a) || !length(a) || !nzchar(a[[1]])) b else a
