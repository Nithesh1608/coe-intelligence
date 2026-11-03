# Desk runner smoke (no LLM). Run: Rscript scripts/test_desk_run.R
setwd("C:/COE Intelligence")
if (file.exists(".Renviron")) readRenviron(".Renviron")
.libPaths(c("C:/Users/Optiplex/AppData/Local/R/win-library/4.6", .libPaths()))

`%||%` <- function(a, b) {
  if (is.null(a) || !length(a) || !nzchar(as.character(a)[[1]])) b else a
}
ok <- function(label, cond, detail = "") {
  cat(if (isTRUE(cond)) "PASS" else "FAIL", "-", label)
  if (nzchar(detail)) cat(" |", substr(detail, 1, 120))
  cat("\n")
}

box::use(
  app/logic/viewdata/store[load_store],
  app/logic/chat/desk_run[handle_desk_turn]
)

store <- load_store()

# Forecast-style should fall through to chat
r0 <- handle_desk_turn("What is the Cat A quota forecast?", store)
ok("forecast -> chat", identical(r0$action, "chat"))

# Refuse premium forecast
r1 <- handle_desk_turn("What will next tender premium be for Cat A?", store)
ok("refuse premium forecast", identical(r1$action, "reply") && grepl("clearing premium", r1$text, ignore.case = TRUE))

# Scenario slot ask
r2 <- handle_desk_turn("Run inject 750", store)
ok("scenario asks category", identical(r2$action, "reply") && !is.null(r2$pending) && grepl("A.*or.*B", r2$text))

r3 <- handle_desk_turn("A", store, pending = r2$pending)
ok(
  "scenario completes -> generate",
  identical(r3$action, "generate") && isTRUE(r3$ok) && !is.null(r3$viz) && nzchar(r3$facts %||% ""),
  substr(r3$facts %||% "", 1, 80)
)

# Exposure one-shot
r4 <- handle_desk_turn("Price 30 Cat A orders at $130k", store)
ok(
  "exposure one-shot -> generate",
  identical(r4$action, "generate") && isTRUE(r4$ok) && !is.null(r4$viz),
  substr(r4$facts %||% "", 1, 100)
)

# Exposure slot fill
r5 <- handle_desk_turn("Price my risk for 40 cars", store)
ok("exposure asks category/quote", !is.null(r5$pending))
r6 <- handle_desk_turn("B", store, pending = r5$pending)
ok("exposure asks quote", !is.null(r6$pending) && grepl("quoted premium", r6$text, ignore.case = TRUE))
r7 <- handle_desk_turn("145000", store, pending = r6$pending)
ok(
  "exposure completes after slots",
  identical(r7$action, "generate") && isTRUE(r7$ok) && identical(r7$viz$type, "exposure"),
  substr(r7$facts %||% "", 1, 100)
)

r9 <- handle_desk_turn("What does Your risk do?", store)
ok("explain your risk -> chat", identical(r9$action, "chat"))

# Cancel
r8 <- handle_desk_turn("cancel", store, pending = r5$pending)
ok("cancel clears", isTRUE(r8$clear_pending) && grepl("Cancelled", r8$text))

cat("\nDone.\n")
