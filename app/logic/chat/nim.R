# NVIDIA NIM chat completions (OpenAI-compatible), sync + async.

NIM_BASE <- "https://integrate.api.nvidia.com/v1"

# Hosted Llama 3.1/3.3 instruct IDs hit EOL on NVIDIA's free catalog (Aug 2026).
# Prefer a current instruct model; override with NVIDIA_NIM_MODEL in .Renviron.
NIM_MODEL_DEFAULT <- "deepseek-ai/deepseek-v4.1-flash"

`%||%` <- function(a, b) if (is.null(a) || !length(a) || !nzchar(a[[1]])) b else a

renviron_candidate_dirs <- function(root = NULL) {
  starts <- character()
  if (!is.null(root) && nzchar(as.character(root)[[1]])) {
    starts <- c(starts, as.character(root)[[1]])
  }
  bp <- getOption("box.path", "")
  if (is.character(bp) && length(bp) && nzchar(bp[[1]])) {
    starts <- c(starts, bp[[1]])
  }
  starts <- c(starts, getwd())
  tryCatch({
    box::use(app/logic/viewdata/store[project_root])
    starts <- c(starts, project_root())
  }, error = function(e) NULL)

  dirs <- character()
  for (start in unique(starts[nzchar(starts)])) {
    here <- normalizePath(start, winslash = "/", mustWork = FALSE)
    for (i in seq_len(12)) {
      dirs <- c(dirs, here)
      parent <- dirname(here)
      if (identical(parent, here)) break
      here <- parent
    }
  }
  unique(dirs)
}

#' Load project .Renviron if NVIDIA_API_KEY is not already set.
#' @export
ensure_nvidia_env <- function(root = NULL) {
  if (nzchar(Sys.getenv("NVIDIA_API_KEY", ""))) {
    return(invisible(TRUE))
  }
  for (dir in renviron_candidate_dirs(root)) {
    path <- file.path(dir, ".Renviron")
    if (!file.exists(path)) next
    readRenviron(path)
    if (nzchar(Sys.getenv("NVIDIA_API_KEY", ""))) {
      return(invisible(TRUE))
    }
  }
  invisible(FALSE)
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

#' Short display label for the preferred model.
#' @export
nim_model_label <- function() {
  id <- nim_model_id()
  base <- sub("^.*/", "", id)
  base <- gsub("[-_]+", " ", base)
  parts <- strsplit(base, " ", fixed = TRUE)[[1]]
  paste(
    vapply(parts, function(p) {
      if (grepl("^[0-9]", p) || grepl("\\.", p)) return(toupper(p))
      paste0(toupper(substr(p, 1L, 1L)), substr(p, 2L, nchar(p)))
    }, character(1)),
    collapse = " "
  )
}

parse_nim_content <- function(body) {
  if (is.null(body$choices) || !length(body$choices)) {
    err <- body$error$message
    if (is.null(err) || !nzchar(as.character(err))) err <- body$detail
    if (is.null(err) || !nzchar(as.character(err))) err <- "Empty response from NIM."
    stop(as.character(err)[[1]], call. = FALSE)
  }
  msg <- body$choices[[1]]$message
  if (is.null(msg$content) || !nzchar(msg$content)) {
    stop("Model returned no text.", call. = FALSE)
  }
  msg$content
}

error_detail <- function(body, status) {
  detail <- NULL
  if (!is.null(body$detail)) detail <- as.character(body$detail)[[1]]
  if (is.null(detail) && !is.null(body$error$message)) {
    detail <- as.character(body$error$message)[[1]]
  }
  if (is.null(detail) || !nzchar(detail)) detail <- paste("HTTP", status)
  detail
}

activation_hint <- function(model) {
  paste0(
    "Model '", model, "' is not enabled for this API key (HTTP 404). ",
    "Open https://build.nvidia.com/", model, " once, accept / Get API Key, then retry. ",
    "Or set NVIDIA_NIM_MODEL to another enabled id."
  )
}

build_nim_request <- function(messages, model, key, temperature, max_tokens) {
  box::use(httr2[
    request, req_headers, req_body_json, req_options, req_timeout, req_error
  ])

  # CURL_HTTP_VERSION_1_1 = 2 — HTTP/2 POSTs hang under R/libcurl on Windows.
  # CURL_IPRESOLVE_V4 = 1 avoids dual-stack stalls.
  request(paste0(NIM_BASE, "/chat/completions")) |>
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
        top_p = 0.9,
        max_tokens = as.integer(max_tokens),
        stream = FALSE
      ),
      auto_unbox = TRUE
    ) |>
    req_options(ipresolve = 1L, http_version = 2L) |>
    req_timeout(60) |>
    req_error(is_error = function(resp) FALSE)
}

interpret_nim_response <- function(resp, model) {
  box::use(httr2[resp_body_json, resp_status])
  status <- resp_status(resp)
  body <- tryCatch(resp_body_json(resp), error = function(e) NULL)
  if (status >= 200L && status < 300L) {
    return(list(ok = TRUE, text = parse_nim_content(body), model = model))
  }
  detail <- error_detail(body, status)
  if (status == 404L || grepl("Not found for account", detail, fixed = TRUE)) {
    return(list(ok = FALSE, error = activation_hint(model)))
  }
  if (status == 410L || grepl("end of life", detail, ignore.case = TRUE)) {
    return(list(
      ok = FALSE,
      error = paste0(model, " is EOL on NIM. Set NVIDIA_NIM_MODEL to a current id.")
    ))
  }
  list(ok = FALSE, error = sprintf("NIM failed (%s): %s", model, detail))
}

prepare_messages <- function(messages) {
  lapply(messages, function(m) {
    list(role = as.character(m$role), content = as.character(m$content))
  })
}

require_api_key <- function() {
  ensure_nvidia_env()
  key <- Sys.getenv("NVIDIA_API_KEY", "")
  if (!nzchar(key)) {
    stop(
      "NVIDIA_API_KEY is missing. Local: .Renviron. Posit Connect Cloud: content Variables.",
      call. = FALSE
    )
  }
  key
}

#' Sync chat (tests / scripts). Prefer nim_chat_async in the Shiny UI.
#' @export
nim_chat <- function(messages, temperature = 0.2, max_tokens = 1800L) {
  box::use(httr2[req_perform])
  key <- require_api_key()
  model <- nim_model_id()
  clean <- prepare_messages(messages)
  resp <- build_nim_request(clean, model, key, temperature, max_tokens) |>
    req_perform()
  out <- interpret_nim_response(resp, model)
  if (!isTRUE(out$ok)) stop(out$error %||% "NIM failed.", call. = FALSE)
  out
}

#' Non-blocking NIM call via httr2 + promises (Shiny can paint while waiting).
#' @export
nim_chat_async <- function(messages, temperature = 0.2, max_tokens = 1800L) {
  box::use(
    httr2[req_perform_promise],
    promises[then, catch]
  )
  key <- require_api_key()
  model <- nim_model_id()
  clean <- prepare_messages(messages)

  req_perform_promise(
    build_nim_request(clean, model, key, temperature, max_tokens)
  ) |>
    then(function(resp) interpret_nim_response(resp, model)) |>
    catch(function(e) list(ok = FALSE, error = conditionMessage(e)))
}
