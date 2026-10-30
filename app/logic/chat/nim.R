# NVIDIA NIM chat completions (OpenAI-compatible).
# Transport: system `curl` — R/libcurl POSTs to integrate.api.nvidia.com often
# stall with 0 bytes on Windows; curl.exe reaches the API in <1s.

NIM_BASE <- "https://integrate.api.nvidia.com/v1"
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

find_curl <- function() {
  for (cand in c("curl", "curl.exe")) {
    path <- Sys.which(cand)
    if (nzchar(path)) return(unname(path))
  }
  stop(
    "curl is required for the COE bot NIM calls but was not found on PATH.",
    call. = FALSE
  )
}

interpret_status_body <- function(status, body, model) {
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

#' POST chat/completions via system curl (reliable where R libcurl stalls).
#' Auth goes in a temp header file so the key never appears on the argv.
nim_curl_exchange <- function(messages, model, key, temperature, max_tokens) {
  box::use(jsonlite[toJSON, fromJSON])
  payload <- list(
    model = model,
    messages = messages,
    temperature = temperature,
    top_p = 0.9,
    max_tokens = as.integer(max_tokens),
    stream = FALSE
  )
  body_path <- tempfile("nim_req_", fileext = ".json")
  out_path <- tempfile("nim_resp_", fileext = ".json")
  hdr_path <- tempfile("nim_hdr_", fileext = ".txt")
  on.exit({
    unlink(c(body_path, out_path, hdr_path), force = TRUE)
  }, add = TRUE)

  writeLines(
    toJSON(payload, auto_unbox = TRUE, null = "null"),
    body_path,
    useBytes = TRUE
  )
  writeLines(
    c(
      paste0("Authorization: Bearer ", key),
      "Content-Type: application/json",
      "Accept: application/json"
    ),
    hdr_path,
    useBytes = TRUE
  )

  curl_bin <- find_curl()
  body_arg <- paste0("@", normalizePath(body_path, winslash = "/", mustWork = TRUE))
  hdr_arg <- paste0("@", normalizePath(hdr_path, winslash = "/", mustWork = TRUE))
  out_arg <- normalizePath(out_path, winslash = "/", mustWork = FALSE)

  # http1.1 avoids the hang class seen with R/libcurl HTTP/2 on some networks.
  args <- c(
    "-sS", "--http1.1",
    "-m", "40",
    "-X", "POST",
    paste0(NIM_BASE, "/chat/completions"),
    "-H", hdr_arg,
    "--data-binary", body_arg,
    "-o", out_arg,
    "-w", "%{http_code}"
  )

  code_chr <- suppressWarnings(
    system2(curl_bin, args, stdout = TRUE, stderr = FALSE)
  )
  status_line <- utils::tail(code_chr[nzchar(code_chr)], 1L)
  status <- suppressWarnings(as.integer(status_line))
  if (is.na(status) || identical(status, 0L)) {
    err <- if (is.na(status)) {
      "curl failed (no HTTP status) — check network / DNS to integrate.api.nvidia.com"
    } else {
      "curl returned HTTP 0 — connection failed before a response"
    }
    return(list(ok = FALSE, error = err))
  }

  raw <- if (file.exists(out_path)) {
    paste(readLines(out_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  } else {
    ""
  }
  body <- tryCatch(fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
  interpret_status_body(status, body, model)
}

#' Sync chat used by the Shiny bot (after pending UI flush).
#' @export
nim_chat <- function(messages, temperature = 0.2, max_tokens = 1800L, retries = 1L) {
  key <- require_api_key()
  model <- nim_model_id()
  clean <- prepare_messages(messages)
  last_err <- NULL
  for (attempt in seq_len(as.integer(retries) + 1L)) {
    out <- tryCatch(
      nim_curl_exchange(clean, model, key, temperature, max_tokens),
      error = function(e) list(ok = FALSE, error = conditionMessage(e))
    )
    if (isTRUE(out$ok)) return(out)
    last_err <- out$error %||% "NIM failed."
    if (!grepl("Timeout|timed out|0 bytes|curl failed|Failed to perform HTTP", last_err, ignore.case = TRUE)) {
      break
    }
  }
  stop(last_err, call. = FALSE)
}

#' @export
nim_chat_async <- function(messages, temperature = 0.2, max_tokens = 1800L) {
  # Kept for callers; bot uses sync nim_chat after onFlushed for reliable UI.
  box::use(promises[promise])
  promise(function(resolve, reject) {
    out <- tryCatch(
      nim_chat(messages, temperature = temperature, max_tokens = max_tokens),
      error = function(e) list(ok = FALSE, error = conditionMessage(e))
    )
    resolve(out)
  })
}
