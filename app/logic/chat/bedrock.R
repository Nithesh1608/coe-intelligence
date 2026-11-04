# AWS Bedrock Converse for COE bot.
# Credentials: COEINTEL_AWS_* from project .Renviron only (IAM: coeintel_cursor).
# Never writes ~/.aws and never selects another named profile.
# Transport: AWS CLI with process-scoped env (same reliability pattern as NIM/curl).

BEDROCK_MODEL_DEFAULT <- "apac.amazon.nova-pro-v1:0"
# Amazon Nova on-demand inference profiles — no marketplace / use-case request.
BEDROCK_MODEL_FALLBACKS <- c(
  "apac.amazon.nova-pro-v1:0",
  "global.amazon.nova-2-lite-v1:0",
  "apac.amazon.nova-lite-v1:0"
)

`%||%` <- function(a, b) if (is.null(a) || !length(a) || !nzchar(a[[1]])) b else a

#' Load COEINTEL_AWS_* / BEDROCK_* from .Renviron into this process only.
#' @export
ensure_aws_env <- function(root = NULL) {
  box::use(app/logic/chat/env[ensure_project_renviron, coeintel_aws_ready])
  ensure_project_renviron(root)
  invisible(coeintel_aws_ready())
}

#' @export
bedrock_ok <- function(root = NULL) {
  ensure_aws_env(root)
  box::use(app/logic/chat/env[coeintel_aws_ready])
  coeintel_aws_ready()
}

bedrock_region <- function() {
  ensure_aws_env()
  r <- Sys.getenv("COEINTEL_AWS_REGION", "")
  if (nzchar(r)) r else "ap-southeast-1"
}

#' @export
bedrock_model_id <- function() {
  ensure_aws_env()
  m <- Sys.getenv("BEDROCK_MODEL_ID", "")
  if (nzchar(m)) m else BEDROCK_MODEL_DEFAULT
}

bedrock_model_candidates <- function() {
  unique(c(bedrock_model_id(), BEDROCK_MODEL_DEFAULT, BEDROCK_MODEL_FALLBACKS))
}

#' @export
bedrock_model_label <- function() {
  id <- bedrock_model_id()
  if (grepl("nova-pro", id, ignore.case = TRUE)) return("Amazon Nova Pro")
  if (grepl("nova-2-lite", id, ignore.case = TRUE)) return("Amazon Nova 2 Lite")
  if (grepl("nova-lite", id, ignore.case = TRUE)) return("Amazon Nova Lite")
  if (grepl("nova-micro", id, ignore.case = TRUE)) return("Amazon Nova Micro")
  base <- sub("^[^.]+\\.", "", id)
  base <- sub(":.*$", "", base)
  gsub("[-_]+", " ", base)
}

find_aws <- function() {
  for (cand in c("aws", "aws.exe")) {
    path <- Sys.which(cand)
    if (nzchar(path)) return(unname(path))
  }
  stop(
    "AWS CLI is required for Bedrock calls but was not found on PATH.",
    call. = FALSE
  )
}

require_coeintel_creds <- function() {
  ensure_aws_env()
  key <- Sys.getenv("COEINTEL_AWS_ACCESS_KEY_ID", "")
  secret <- Sys.getenv("COEINTEL_AWS_SECRET_ACCESS_KEY", "")
  if (!nzchar(key) || !nzchar(secret)) {
    stop(
      "COEINTEL_AWS_ACCESS_KEY_ID / COEINTEL_AWS_SECRET_ACCESS_KEY missing. ",
      "Add coeintel_cursor keys to .Renviron (never other IAM profiles).",
      call. = FALSE
    )
  }
  list(key = key, secret = secret, region = bedrock_region())
}

#' Split OpenAI-style messages into Converse system + messages.
prepare_converse <- function(messages) {
  system_parts <- character()
  turns <- list()
  for (m in messages) {
    role <- as.character(m$role %||% "user")
    text <- as.character(m$content %||% "")
    if (!nzchar(text)) next
    if (identical(role, "system")) {
      system_parts <- c(system_parts, text)
    } else if (role %in% c("user", "assistant")) {
      turns[[length(turns) + 1L]] <- list(
        role = role,
        content = list(list(text = text))
      )
    }
  }
  while (length(turns) && identical(turns[[1]]$role, "assistant")) {
    turns <- turns[-1L]
  }
  if (!length(turns)) {
    turns <- list(list(role = "user", content = list(list(text = "Hello"))))
  }
  list(
    system = if (length(system_parts)) {
      list(list(text = paste(system_parts, collapse = "\n\n")))
    } else {
      NULL
    },
    messages = turns
  )
}

parse_converse_text <- function(body) {
  content <- body$output$message$content
  if (is.null(content) || !length(content)) {
    err <- body$message %||% body$Message %||% "Empty response from Bedrock."
    stop(as.character(err)[[1]], call. = FALSE)
  }
  texts <- vapply(content, function(part) {
    t <- part$text
    if (is.null(t)) "" else as.character(t)[[1]]
  }, character(1))
  out <- paste(texts[nzchar(texts)], collapse = "\n")
  if (!nzchar(out)) stop("Model returned no text.", call. = FALSE)
  out
}

error_from_cli <- function(lines, status) {
  txt <- paste(lines, collapse = "\n")
  if (grepl("AccessDenied|UnrecognizedClient|InvalidClientTokenId|ExpiredToken", txt)) {
    return(paste0(
      "Bedrock rejected coeintel_cursor credentials. ",
      "Confirm the access key is active and has bedrock:InvokeModel. ",
      trimws(txt)
    ))
  }
  if (grepl("ValidationException|isn't supported|inference profile", txt, ignore.case = TRUE)) {
    return(paste("Bedrock model/profile error:", trimws(txt)))
  }
  if (grepl("Throttl|Too many requests|ServiceUnavailable", txt, ignore.case = TRUE)) {
    return(paste("Bedrock busy:", trimws(txt)))
  }
  if (!nzchar(trimws(txt))) {
    return(sprintf("AWS CLI failed (exit %s).", status %||% "?"))
  }
  trimws(txt)
}

is_retryable_bedrock <- function(err) {
  grepl(
    "Throttl|Too many|ServiceUnavailable|timed out|Timeout|Temporary|busy|ValidationException|isn't supported|inference profile",
    err %||% "",
    ignore.case = TRUE
  )
}

#' Converse via AWS CLI with env scoped to this child process only.
bedrock_cli_exchange <- function(messages, model, creds, temperature, max_tokens,
                                 timeout_sec = 45L) {
  box::use(jsonlite[toJSON, fromJSON])
  prepared <- prepare_converse(messages)
  payload <- list(
    messages = prepared$messages,
    inferenceConfig = list(
      maxTokens = as.integer(max_tokens),
      temperature = as.numeric(temperature)
    )
  )
  if (!is.null(prepared$system)) payload$system <- prepared$system

  body_path <- tempfile("bedrock_req_", fileext = ".json")
  on.exit(unlink(body_path, force = TRUE), add = TRUE)

  writeLines(
    toJSON(payload, auto_unbox = TRUE, null = "null"),
    body_path,
    useBytes = TRUE
  )

  aws_bin <- find_aws()
  args <- c(
    "bedrock-runtime", "converse",
    "--model-id", model,
    "--cli-input-json", paste0("file://", normalizePath(body_path, winslash = "/", mustWork = TRUE)),
    "--region", creds$region,
    "--output", "json"
  )

  # Process-scoped creds only for this call. Avoid system2(env=) on Windows —
  # Cursor/sandbox env blobs get mis-parsed as AWS CLI argv.
  aws_vars <- c(
    "AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_SESSION_TOKEN",
    "AWS_SECURITY_TOKEN", "AWS_PROFILE", "AWS_DEFAULT_PROFILE",
    "AWS_DEFAULT_REGION", "AWS_REGION", "AWS_EC2_METADATA_DISABLED"
  )
  old <- Sys.getenv(aws_vars, unset = NA_character_)
  on.exit({
    for (nm in names(old)) {
      if (is.na(old[[nm]])) Sys.unsetenv(nm) else do.call(Sys.setenv, setNames(list(old[[nm]]), nm))
    }
  }, add = TRUE)

  Sys.setenv(
    AWS_ACCESS_KEY_ID = creds$key,
    AWS_SECRET_ACCESS_KEY = creds$secret,
    AWS_DEFAULT_REGION = creds$region,
    AWS_REGION = creds$region,
    AWS_EC2_METADATA_DISABLED = "true"
  )
  Sys.unsetenv(c(
    "AWS_PROFILE", "AWS_DEFAULT_PROFILE",
    "AWS_SESSION_TOKEN", "AWS_SECURITY_TOKEN"
  ))

  code_chr <- suppressWarnings(
    system2(
      aws_bin,
      args,
      stdout = TRUE,
      stderr = TRUE,
      timeout = as.integer(timeout_sec)
    )
  )
  exit_st <- attr(code_chr, "status")

  if (!is.null(exit_st) && !identical(as.integer(exit_st), 0L)) {
    err <- error_from_cli(code_chr, exit_st)
    return(list(ok = FALSE, error = err, retryable = is_retryable_bedrock(err)))
  }

  raw <- paste(code_chr, collapse = "\n")
  body <- tryCatch(fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(body)) {
    return(list(
      ok = FALSE,
      retryable = TRUE,
      error = paste("Could not parse Bedrock JSON:", substr(raw, 1L, 400L))
    ))
  }

  tryCatch(
    list(ok = TRUE, text = parse_converse_text(body), model = model),
    error = function(e) list(ok = FALSE, error = conditionMessage(e), retryable = FALSE)
  )
}

#' Sync Bedrock chat used by the Shiny bot.
#' @export
bedrock_chat <- function(messages, temperature = 0.25, max_tokens = 1600L,
                         retries = 0L, timeout_sec = 45L) {
  creds <- require_coeintel_creds()
  models <- bedrock_model_candidates()
  last_err <- NULL

  for (model in models) {
    attempts <- as.integer(retries) + 1L
    for (attempt in seq_len(attempts)) {
      out <- tryCatch(
        bedrock_cli_exchange(
          messages, model, creds, temperature, max_tokens,
          timeout_sec = as.integer(timeout_sec)
        ),
        error = function(e) list(ok = FALSE, error = conditionMessage(e), retryable = TRUE)
      )
      if (isTRUE(out$ok)) {
        out$model <- model
        return(out)
      }
      last_err <- out$error %||% "Bedrock failed."
      if (!isTRUE(out$retryable) && !is_retryable_bedrock(last_err)) {
        if (!grepl("ValidationException|isn't supported|inference profile", last_err, ignore.case = TRUE)) {
          stop(last_err, call. = FALSE)
        }
      }
    }
  }
  stop(last_err %||% "Bedrock failed on all candidate models.", call. = FALSE)
}
