# Chat backend: Bedrock (coeintel_cursor / Amazon Nova) preferred.
# When COE_CHAT_PROVIDER=bedrock, never fall back to NIM — status must match reality.

`%||%` <- function(a, b) if (is.null(a) || !length(a) || !nzchar(as.character(a)[[1]])) b else a

#' @export
ensure_chat_env <- function(root = NULL) {
  box::use(
    app/logic/chat/bedrock[ensure_aws_env],
    app/logic/chat/nim[ensure_nvidia_env]
  )
  ensure_aws_env(root)
  ensure_nvidia_env(root)
  invisible(TRUE)
}

provider_pref <- function() {
  tolower(Sys.getenv("COE_CHAT_PROVIDER", "bedrock"))
}

#' Resolved provider: "bedrock", "nim", or "none".
#' @export
chat_provider <- function() {
  box::use(
    app/logic/chat/bedrock[bedrock_ok],
    app/logic/chat/nim[nvidia_key_ok]
  )
  pref <- provider_pref()
  if (identical(pref, "bedrock")) {
    if (bedrock_ok()) return("bedrock")
    return("none")
  }
  if (identical(pref, "nim")) {
    if (nvidia_key_ok()) return("nim")
    return("none")
  }
  # auto
  if (bedrock_ok()) return("bedrock")
  if (nvidia_key_ok()) return("nim")
  "none"
}

#' @export
chat_ready <- function() !identical(chat_provider(), "none")

#' Human label for the live status line (configured model — no silent NIM rename).
#' @export
chat_model_label <- function() {
  box::use(
    app/logic/chat/bedrock[bedrock_model_label],
    app/logic/chat/nim[nim_model_label]
  )
  switch(
    chat_provider(),
    bedrock = bedrock_model_label(),
    nim = nim_model_label(),
    "No model configured"
  )
}

#' Route a chat turn. No NIM fallback when COE_CHAT_PROVIDER=bedrock.
#' @export
chat_complete <- function(messages, temperature = 0.25, max_tokens = 1600L,
                          timeout_sec = 45L) {
  box::use(
    app/logic/chat/bedrock[bedrock_chat],
    app/logic/chat/nim[nim_chat]
  )
  prov <- chat_provider()
  pref <- provider_pref()

  if (identical(prov, "bedrock")) {
    out <- tryCatch(
      bedrock_chat(
        messages,
        temperature = temperature,
        max_tokens = max_tokens,
        timeout_sec = timeout_sec
      ),
      error = function(e) list(ok = FALSE, error = conditionMessage(e), provider = "bedrock")
    )
    if (isTRUE(out$ok)) {
      out$provider <- "bedrock"
      return(out)
    }
    # Explicit bedrock preference: do not silently switch to Nemotron.
    if (identical(pref, "bedrock")) {
      out$provider <- "bedrock"
      return(out)
    }
    # auto mode only: optional NIM fallback
    box::use(app/logic/chat/nim[nvidia_key_ok])
    if (nvidia_key_ok()) {
      fb <- tryCatch(
        nim_chat(
          messages,
          temperature = temperature,
          max_tokens = min(as.integer(max_tokens), 1100L),
          timeout_sec = min(as.integer(timeout_sec), 35L)
        ),
        error = function(e) list(ok = FALSE, error = conditionMessage(e))
      )
      if (isTRUE(fb$ok)) {
        fb$provider <- "nim"
        fb$fallback_from <- out$error %||% "bedrock"
        return(fb)
      }
    }
    return(out)
  }

  if (identical(prov, "nim")) {
    out <- nim_chat(
      messages,
      temperature = temperature,
      max_tokens = min(as.integer(max_tokens), 1100L),
      timeout_sec = min(as.integer(timeout_sec), 35L)
    )
    out$provider <- "nim"
    return(out)
  }

  list(
    ok = FALSE,
    error = paste(
      "No chat backend configured.",
      "Set COEINTEL_AWS_* and COE_CHAT_PROVIDER=bedrock for Amazon Nova."
    )
  )
}
