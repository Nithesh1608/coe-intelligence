# NVIDIA NIM chat completions (OpenAI-compatible).
# Transport: system curl via --config file (reliable argv/quoting on Windows).

NIM_BASE <- "https://integrate.api.nvidia.com/v1"
# deepseek-v4.1-flash hangs with 0-byte timeouts on this account; bolt is live.
NIM_MODEL_DEFAULT <- "nvidia/nemotron-3.5-bolt-e4b"
NIM_MODEL_FALLBACKS <- c(
  "nvidia/nemotron-3.5-bolt-e4b",
  "nvidia/nemotron-nano-3-30b-a3b"
)

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

#' Ordered model candidates: preferred first, then fallbacks (deduped).
nim_model_candidates <- function() {
  unique(c(nim_model_id(), NIM_MODEL_DEFAULT, NIM_MODEL_FALLBACKS))
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
    if (is.null(err) || !nzchar(as.character(err))) err <- body$title
    if (is.null(err) || !nzchar(as.character(err))) err <- "Empty response from NIM."
    stop(as.character(err)[[1]], call. = FALSE)
  }
  msg <- body$choices[[1]]$message
  content <- msg$content
  if (is.null(content) || !nzchar(as.character(content)[[1]])) {
    # Some Nemotron builds put text in reasoning_content only — rare, but recover.
    alt <- msg$reasoning_content
    if (!is.null(alt) && nzchar(as.character(alt)[[1]])) return(as.character(alt)[[1]])
    stop("Model returned no text.", call. = FALSE)
  }
  as.character(content)[[1]]
}

error_detail <- function(body, status) {
  detail <- NULL
  if (!is.null(body$detail)) detail <- as.character(body$detail)[[1]]
  if (is.null(detail) && !is.null(body$error$message)) {
    detail <- as.character(body$error$message)[[1]]
  }
  if (is.null(detail) && !is.null(body$title)) detail <- as.character(body$title)[[1]]
  if (is.null(detail) || !nzchar(detail)) detail <- paste("HTTP", status)
  detail
}

activation_hint <- function(model) {
  paste0(
    "Model '", model, "' is not enabled for this API key (HTTP 404). ",
    "Open https://build.nvidia.com/", model, " once, accept / Get API Key, then retry. ",
    "Or set NVIDIA_NIM_MODEL to another enabled id (default: ", NIM_MODEL_DEFAULT, ")."
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
    return(list(ok = FALSE, error = activation_hint(model), retryable = TRUE))
  }
  if (status == 410L || grepl("end of life", detail, ignore.case = TRUE)) {
    return(list(
      ok = FALSE,
      retryable = TRUE,
      error = paste0(model, " is EOL on NIM. Set NVIDIA_NIM_MODEL to a current id.")
    ))
  }
  list(ok = FALSE, error = sprintf("NIM failed (%s): %s", model, detail), retryable = FALSE)
}

escape_curl_cfg <- function(x) {
  gsub("([\"\\\\])", "\\\\\\1", as.character(x), perl = TRUE)
}

#' POST chat/completions via curl --config (Windows-safe quoting).
nim_curl_exchange <- function(messages, model, key, temperature, max_tokens,
                              timeout_sec = 35L) {
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
  cfg_path <- tempfile("nim_cfg_", fileext = ".txt")
  on.exit({
    unlink(c(body_path, out_path, cfg_path), force = TRUE)
  }, add = TRUE)

  writeLines(
    toJSON(payload, auto_unbox = TRUE, null = "null"),
    body_path,
    useBytes = TRUE
  )

  body_norm <- normalizePath(body_path, winslash = "/", mustWork = TRUE)
  out_norm <- normalizePath(out_path, winslash = "/", mustWork = FALSE)
  writeLines(
    c(
      paste0('url = "', NIM_BASE, '/chat/completions"'),
      "request = POST",
      "http1.1",
      "silent",
      "show-error",
      paste0("max-time = ", as.integer(timeout_sec)),
      paste0('header = "Authorization: Bearer ', escape_curl_cfg(key), '"'),
      'header = "Content-Type: application/json"',
      'header = "Accept: application/json"',
      paste0('data-binary = "@', body_norm, '"'),
      paste0('output = "', out_norm, '"'),
      'write-out = "%{http_code}"'
    ),
    cfg_path,
    useBytes = TRUE
  )

  curl_bin <- find_curl()
  cfg_arg <- normalizePath(cfg_path, winslash = "/", mustWork = TRUE)
  code_chr <- suppressWarnings(
    system2(curl_bin, c("-K", cfg_arg), stdout = TRUE, stderr = TRUE)
  )
  status_line <- utils::tail(code_chr[nzchar(code_chr)], 1L)
  # Prefer trailing numeric http code; curl errors also land in stdout with stderr merged.
  num_hits <- grep("^[0-9]{3}$", code_chr, value = TRUE)
  if (length(num_hits)) status_line <- utils::tail(num_hits, 1L)
  status <- suppressWarnings(as.integer(status_line))
  exit_st <- attr(code_chr, "status")

  if (is.na(status) || identical(status, 0L)) {
    err_txt <- paste(code_chr[!grepl("^[0-9]{3}$", code_chr)], collapse = " ")
    if (!nzchar(trimws(err_txt))) err_txt <- paste("curl exit", exit_st %||% "?")
    if (!is.null(exit_st) && identical(as.integer(exit_st), 28L)) {
      return(list(
        ok = FALSE,
        retryable = TRUE,
        error = sprintf("NIM timed out after %ss on %s (0 bytes).", timeout_sec, model)
      ))
    }
    return(list(
      ok = FALSE,
      retryable = TRUE,
      error = paste("curl failed:", trimws(err_txt))
    ))
  }

  raw <- if (file.exists(out_path)) {
    paste(readLines(out_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  } else {
    ""
  }
  body <- tryCatch(fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
  interpret_status_body(status, body, model)
}

is_retryable_nim <- function(out) {
  if (isTRUE(out$ok)) return(FALSE)
  if (isTRUE(out$retryable)) return(TRUE)
  err <- out$error %||% ""
  grepl(
    "Timeout|timed out|0 bytes|curl failed|Failed to perform HTTP|Not found for account|EOL|404",
    err,
    ignore.case = TRUE
  )
}

#' Sync chat used by the Shiny bot (after pending UI flush).
#' Tries preferred model then fallbacks when the endpoint hangs / 404s.
#' @export
nim_chat <- function(messages, temperature = 0.25, max_tokens = 1100L,
                     retries = 0L, timeout_sec = 35L) {
  key <- require_api_key()
  clean <- prepare_messages(messages)
  models <- nim_model_candidates()
  last_err <- NULL

  for (model in models) {
    # Prefer a short timeout on known-dead deepseek so we fail over fast.
    to <- if (grepl("deepseek-v4\\.1-flash", model, ignore.case = TRUE)) {
      min(as.integer(timeout_sec), 12L)
    } else {
      as.integer(timeout_sec)
    }
    attempts <- as.integer(retries) + 1L
    for (attempt in seq_len(attempts)) {
      out <- tryCatch(
        nim_curl_exchange(clean, model, key, temperature, max_tokens, timeout_sec = to),
        error = function(e) list(ok = FALSE, error = conditionMessage(e), retryable = TRUE)
      )
      if (isTRUE(out$ok)) {
        out$model <- model
        return(out)
      }
      last_err <- out$error %||% "NIM failed."
      if (!is_retryable_nim(out)) {
        stop(last_err, call. = FALSE)
      }
      # Next attempt / next model.
    }
  }
  stop(last_err %||% "NIM failed on all candidate models.", call. = FALSE)
}

#' @export
nim_chat_async <- function(messages, temperature = 0.25, max_tokens = 1100L) {
  box::use(promises[promise])
  promise(function(resolve, reject) {
    out <- tryCatch(
      nim_chat(messages, temperature = temperature, max_tokens = max_tokens),
      error = function(e) list(ok = FALSE, error = conditionMessage(e))
    )
    resolve(out)
  })
}
