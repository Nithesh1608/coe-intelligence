# Live COE bot smoke tests (Bedrock first, NIM fallback). Run: Rscript scripts/test_coe_bot.R
setwd("C:/COE Intelligence")
if (file.exists(".Renviron")) readRenviron(".Renviron")
.libPaths(c("C:/Users/Optiplex/AppData/Local/R/win-library/4.6", .libPaths()))

`%||%` <- function(a, b) {
  if (is.null(a) || !length(a) || !nzchar(as.character(a)[[1]])) b else a
}

ok <- function(label, cond, detail = "") {
  cat(if (isTRUE(cond)) "PASS" else "FAIL", "-", label)
  if (nzchar(detail)) cat(" |", detail)
  cat("\n")
  invisible(isTRUE(cond))
}

box::use(
  app/logic/chat/provider[ensure_chat_env, chat_ready, chat_provider, chat_model_label, chat_complete],
  app/logic/chat/bedrock[bedrock_ok, bedrock_model_id],
  app/logic/chat/context[system_prompt, build_project_context],
  app/logic/viewdata/store[load_store]
)

ensure_chat_env()
ok("Chat backend ready", chat_ready(), chat_provider())
ok("Bedrock creds (coeintel_cursor)", bedrock_ok())
ok("Bedrock model id", nzchar(bedrock_model_id()), bedrock_model_id())
ok("Status label", nzchar(chat_model_label()), chat_model_label())

store <- load_store()
sys_full <- paste(system_prompt(), "\n\n", build_project_context(store), sep = "")
sys_q <- paste(
  system_prompt(), "\n\n",
  build_project_context(store, question = "What is the Cat A quota forecast?"),
  sep = ""
)
ok("Full context under 60k", nchar(sys_full) < 60000L, paste0(nchar(sys_full), " chars"))
ok("Smart context smaller or equal", nchar(sys_q) <= nchar(sys_full) + 50L, paste0(nchar(sys_q), " vs ", nchar(sys_full)))
ok("Context has forward quota", grepl("Forward", sys_q, fixed = TRUE))
ok("Context has MAPE / scores", grepl("MAPE|Live quota scores", sys_full))
ok("Capability map present", grepl("App capability map", sys_full, fixed = TRUE))

t0 <- Sys.time()
ping <- tryCatch(
  chat_complete(
    list(
      list(role = "system", content = "Reply with exactly: PONG"),
      list(role = "user", content = "ping")
    ),
    max_tokens = 32L,
    timeout_sec = 40L
  ),
  error = function(e) list(ok = FALSE, error = conditionMessage(e))
)
ping_s <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
ok(
  "Chat ping ok",
  isTRUE(ping$ok),
  sprintf(
    "%.1fs provider=%s model=%s %s",
    ping_s, ping$provider %||% "?", ping$model %||% "?",
    substr(ping$text %||% ping$error %||% "", 1, 80)
  )
)

qs <- c(
  "What is the Cat A quota forecast?",
  "How accurate is the model?",
  "What can Scenarios stress?",
  "What does Your risk do?"
)
for (q in qs) {
  t1 <- Sys.time()
  sys <- paste(system_prompt(), "\n\n", build_project_context(store, question = q), sep = "")
  out <- tryCatch(
    chat_complete(
      list(
        list(role = "system", content = sys),
        list(role = "user", content = q)
      ),
      max_tokens = 1200L,
      timeout_sec = 50L
    ),
    error = function(e) list(ok = FALSE, error = conditionMessage(e))
  )
  elapsed <- as.numeric(difftime(Sys.time(), t1, units = "secs"))
  snippet <- if (isTRUE(out$ok)) substr(gsub("\\s+", " ", out$text), 1, 140) else out$error
  ok(
    paste0("Q: ", q),
    isTRUE(out$ok) && nchar(out$text %||% "") > 40L,
    sprintf("%.1fs | %s", elapsed, snippet)
  )
}

cat("\nDone.\n")
