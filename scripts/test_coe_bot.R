# Live COE bot / NIM smoke tests. Run: Rscript scripts/test_coe_bot.R
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

box::use(app/logic/chat/nim[nim_chat, nvidia_key_ok, nim_model_id, ensure_nvidia_env])
box::use(app/logic/chat/context[system_prompt, build_project_context])
box::use(app/logic/viewdata/store[load_store])

ensure_nvidia_env()
ok("API key present", nvidia_key_ok())
ok("Model id set", nzchar(nim_model_id()), nim_model_id())

store <- load_store()
sys <- paste(system_prompt(), "\n\n", build_project_context(store), sep = "")
ok("Context size under 8k chars", nchar(sys) < 8000L, paste0(nchar(sys), " chars"))
ok("Context has forward quota", grepl("Forward quota", sys, fixed = TRUE))
ok("Context has MAPE / scores", grepl("MAPE|Live quota scores", sys))

t0 <- Sys.time()
ping <- tryCatch(
  nim_chat(
    list(
      list(role = "system", content = "Reply with exactly: PONG"),
      list(role = "user", content = "ping")
    ),
    max_tokens = 32L,
    timeout_sec = 25L
  ),
  error = function(e) list(ok = FALSE, error = conditionMessage(e))
)
ping_s <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
ok(
  "NIM ping ok",
  isTRUE(ping$ok),
  sprintf("%.1fs model=%s %s", ping_s, ping$model %||% "?", substr(ping$text %||% ping$error %||% "", 1, 80))
)

qs <- c(
  "What is the Cat A quota forecast?",
  "How accurate is the model?",
  "What does Your risk do?"
)
for (q in qs) {
  t1 <- Sys.time()
  out <- tryCatch(
    nim_chat(
      list(
        list(role = "system", content = sys),
        list(role = "user", content = q)
      ),
      max_tokens = 900L,
      timeout_sec = 35L
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
