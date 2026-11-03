# Chat backend selection: Bedrock (coeintel_cursor / Amazon Nova) first, NIM fallback.

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

#' Resolved provider: "bedrock", "nim", or "none".
#' @export
chat_provider <- function() {
  box::use(
    app/logic/chat/bedrock[bedrock_ok],
    app/logic/chat/nim[nvidia_key_ok]
  )
  pref <- tolower(Sys.getenv("COE_CHAT_PROVIDER", "auto"))
  if (identical(pref, "bedrock") && bedrock_ok()) return("bedrock")
  if (identical(pref, "nim") && nvidia_key_ok()) return("nim")
  if (bedrock_ok()) return("bedrock")
  if (nvidia_key_ok()) return("nim")
  "none"
}

#' @export
chat_ready <- function() !identical(chat_provider(), "none")

#' Human label for the live status line.
#' @export
chat_model_label <- function() {
  box::use(
    app/logic/chat/bedrock[bedrock_model_label],
    app/logic/chat/nim[nim_model_label]
  )
  switch(
    chat_provider(),
    bedrock = paste0("Bedrock · ", bedrock_model_label()),
    nim = paste0("NIM · ", nim_model_label()),
    "No model configured"
  )
}

#' Route a chat turn to Bedrock or NIM.
#' @export
chat_complete <- function(messages, temperature = 0.25, max_tokens = 1600L,
                          timeout_sec = 45L) {
  box::use(
    app/logic/chat/bedrock[bedrock_chat],
    app/logic/chat/nim[nim_chat]
  )
  prov <- chat_provider()
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
    # Soft-fallback to NIM when Bedrock fails and a NIM key exists.
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
      return(list(
        ok = FALSE,
        error = paste0(
          "Bedrock: ", out$error %||% "failed",
          " | NIM: ", fb$error %||% "failed"
        )
      ))
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
      "Set COEINTEL_AWS_* for Bedrock (coeintel_cursor) or NVIDIA_API_KEY for NIM."
    )
  )
}
