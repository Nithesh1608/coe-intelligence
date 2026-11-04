# Thorough COE bot Q&A battery: intents, slots, engines, live model style checks.
# Run: Rscript scripts/test_bot_qa_battery.R
setwd("C:/COE Intelligence")
if (file.exists(".Renviron")) readRenviron(".Renviron")
.libPaths(c("C:/Users/Optiplex/AppData/Local/R/win-library/4.6", .libPaths()))
options(box.path = "C:/COE Intelligence")

`%||%` <- function(a, b) {
  if (is.null(a) || !length(a) || (length(a) == 1L && is.na(a))) b else a
}

n_pass <- 0L
n_fail <- 0L
fails <- character()

ok <- function(label, cond, detail = "") {
  hit <- isTRUE(cond)
  if (hit) n_pass <<- n_pass + 1L else {
    n_fail <<- n_fail + 1L
    fails <<- c(fails, label)
  }
  cat(if (hit) "PASS" else "FAIL", "-", label)
  if (nzchar(as.character(detail)[[1]])) {
    cat(" |", substr(gsub("\\s+", " ", as.character(detail)[[1]]), 1L, 140L))
  }
  cat("\n")
  invisible(hit)
}

clip <- function(x, n = 220L) {
  x <- gsub("\\s+", " ", as.character(x %||% ""))
  if (nchar(x) <= n) x else paste0(substr(x, 1L, n - 1L), "…")
}

bad_style <- function(text) {
  t <- tolower(as.character(text %||% ""))
  checks <- c(
    grepl("step[- ]by[- ]step|here'?s how you can|navigate to|click on the", t),
    grepl("feel free to ask|let me know if|happy to help|further assistance", t),
    grepl("example output:|var:\\s*\\$20,?000|cvar:\\s*\\$25,?000", t),
    grepl("go to the ['\"]?your risk", t) && grepl("upload", t)
  )
  any(checks)
}

ask_model <- function(q, store, facts = NULL, max_tokens = 700L) {
  box::use(
    app/logic/chat/provider[chat_complete],
    app/logic/chat/context[system_prompt, build_project_context]
  )
  sys <- paste(system_prompt(), "\n\n", build_project_context(store, question = q), sep = "")
  if (nzchar(facts %||% "")) sys <- paste(sys, "\n\n", facts, sep = "")
  tryCatch(
    chat_complete(
      list(
        list(role = "system", content = sys),
        list(role = "user", content = q)
      ),
      temperature = 0.2,
      max_tokens = as.integer(max_tokens),
      timeout_sec = 55L
    ),
    error = function(e) list(ok = FALSE, error = conditionMessage(e))
  )
}

cat("=== COE bot Q&A battery ===\n")

box::use(
  app/logic/viewdata/store[load_store],
  app/logic/chat/desk_run[handle_desk_turn, detect_desk_intent],
  app/logic/chat/provider[ensure_chat_env, chat_ready, chat_model_label]
)

ensure_chat_env()
store <- load_store()
ok("chat ready", chat_ready(), chat_model_label())

cat("\n-- Intent matrix --\n")
intent_cases <- list(
  list(q = "What is the Cat A quota forecast?", want = "chat"),
  list(q = "How accurate is the model?", want = "chat"),
  list(q = "What does Your risk do?", want = "chat"),
  list(q = "how does exposure work", want = "chat"),
  list(q = "i want to get a financial exposure", want = "exposure"),
  list(q = "show me exposure", want = "exposure"),
  list(q = "Price 30 Cat A orders at $130k", want = "exposure"),
  list(q = "I need financial exposure numbers", want = "exposure"),
  list(q = "Run inject 750 on Cat A", want = "scenario"),
  list(q = "PQP to 80k for Cat B", want = "scenario"),
  list(q = "clawback Cat A", want = "scenario"),
  list(q = "What will next tender premium be?", want = "refuse")
)
for (cs in intent_cases) {
  got <- detect_desk_intent(cs$q)
  ok(sprintf("intent: %s", cs$q), identical(got, cs$want), paste(got, "vs", cs$want))
}

cat("\n-- Refuse / cancel --\n")
r_prem <- handle_desk_turn("What will Cat A clearing be next month?", store)
ok(
  "refuse premium forecast",
  identical(r_prem$action, "reply") && grepl("clearing premium", r_prem$text, ignore.case = TRUE),
  r_prem$text %||% ""
)
r_prem2 <- handle_desk_turn("What will next tender premium be for Cat A?", store)
ok(
  "refuse next tender premium",
  identical(r_prem2$action, "reply") && grepl("clearing premium", r_prem2$text, ignore.case = TRUE)
)

r_slot <- handle_desk_turn("financial exposure", store)
ok("exposure starts slots", identical(r_slot$action, "reply") && !is.null(r_slot$pending))
r_cancel <- handle_desk_turn("cancel", store, pending = r_slot$pending)
ok("cancel clears", identical(r_cancel$action, "reply") && isTRUE(r_cancel$clear_pending))

cat("\n-- Multi-turn exposure slots --\n")
p <- NULL
t1 <- handle_desk_turn("i want to get a financial exposure", store, pending = p)
ok("turn1 asks orders", identical(t1$action, "reply") && grepl("orders", t1$text, ignore.case = TRUE), t1$text)
p <- t1$pending
t_bad <- handle_desk_turn("500", store, pending = p)
ok(
  "500 still asks orders (out of range)",
  identical(t_bad$action, "reply") && grepl("orders", t_bad$text, ignore.case = TRUE),
  t_bad$text
)
# 500 is parsed as n_cars by parse_orders only if 1-200; 500 should not fill
# confirm slots still missing n_cars
ok("500 did not set n_cars", is.null(t_bad$pending$slots$n_cars) || t_bad$pending$slots$n_cars != 500L)

t2 <- handle_desk_turn("200", store, pending = p)
ok("turn2 asks category", identical(t2$action, "reply") && grepl("Category", t2$text), t2$text)
p <- t2$pending
t3 <- handle_desk_turn("A", store, pending = p)
ok("turn3 asks quote", identical(t3$action, "reply") && grepl("quoted premium", t3$text, ignore.case = TRUE), t3$text)
p <- t3$pending
t4 <- handle_desk_turn("130000", store, pending = p)
ok(
  "turn4 generates exposure",
  identical(t4$action, "generate") && isTRUE(t4$ok) && identical(t4$viz$type, "exposure"),
  clip(t4$facts %||% t4$error %||% "")
)
ok("exposure facts have var95", grepl("var95_loss=", t4$facts %||% "", fixed = TRUE))
ok("exposure facts have timing", grepl("timing:", t4$facts %||% "", fixed = TRUE))

cat("\n-- One-shot exposure + cheap quote --\n")
cheap <- handle_desk_turn("Price 30 Cat A orders at $50k", store)
ok("cheap quote runs", identical(cheap$action, "generate") && isTRUE(cheap$ok))
ok(
  "cheap quote high loss signal in facts",
  grepl("prob_loss=100|prob_loss=9[0-9]", cheap$facts %||% "") ||
    grepl("expected_coe_cost=", cheap$facts %||% "", fixed = TRUE),
  clip(cheap$facts %||% "")
)

cat("\n-- Scenario one-shot / slots --\n")
sc1 <- handle_desk_turn("Run inject 750", store)
ok("inject asks category", identical(sc1$action, "reply") && !is.null(sc1$pending))
sc2 <- handle_desk_turn("A", store, pending = sc1$pending)
ok(
  "inject A generates scenario",
  identical(sc2$action, "generate") && isTRUE(sc2$ok) && identical(sc2$viz$type, "scenario"),
  clip(sc2$facts %||% "")
)
sc3 <- handle_desk_turn("PQP to 80k for Cat B", store)
ok(
  "pqp 80k Cat B one-shot",
  identical(sc3$action, "generate") && isTRUE(sc3$ok),
  clip(sc3$facts %||% "")
)

cat("\n-- Live model answers --\n")
qa <- list(
  list(
    id = "forecast",
    q = "What is the Cat A quota forecast for the next few months?",
    facts = NULL,
    must = c("2026-|forward|outlook|forecast|quota|Cat A"),
    must_not_style = TRUE
  ),
  list(
    id = "mape",
    q = "How accurate is the model?",
    facts = NULL,
    must = c("MAPE|11\\.|elastic net|12-month"),
    must_not_style = TRUE
  ),
  list(
    id = "your_risk_explain",
    q = "What does Your risk do?",
    facts = NULL,
    must = c("bootstrap|VaR|P&L|book|premium"),
    must_not_style = TRUE
  ),
  list(
    id = "exposure_mkt",
    q = "Summarise this pooled desk run.",
    facts = t4$facts,
    must = c("200|Cat A|\\$|VaR|P&L|loss|cost"),
    must_not_style = TRUE
  ),
  list(
    id = "exposure_cheap",
    q = "Is this quote cheap? when should I bid given supply next 12 months?",
    facts = cheap$facts,
    must = c("cheap|under|loss|quote|fix|timing|\\+0Q|forward|quota"),
    must_not_style = TRUE
  ),
  list(
    id = "scenario",
    q = "What happens to Cat A quota under inject 750?",
    facts = sc2$facts,
    must = c("inject|quota|delta|Cat A"),
    must_not_style = TRUE
  ),
  list(
    id = "published_discipline",
    q = "What is the published Cat A quota for December 2026?",
    facts = NULL,
    must = c("not released|NOT_RELEASED|forecast|outlook|not in context"),
    must_not_style = TRUE
  ),
  list(
    id = "no_tutorial",
    q = "can i get financial exposure here",
    facts = NULL,
    must = c("order|category|quote|premium|pooled|here|A or B"),
    must_not_style = TRUE
  )
)

for (item in qa) {
  cat(sprintf("\n>> %s: %s\n", item$id, item$q))
  out <- ask_model(item$q, store, facts = item$facts)
  ok(sprintf("model ok [%s]", item$id), isTRUE(out$ok), out$error %||% "")
  txt <- out$text %||% ""
  cat("   ANS:", clip(txt, 280L), "\n")
  ok(sprintf("nonempty [%s]", item$id), nchar(txt) > 40L, sprintf("n=%s", nchar(txt)))
  ok(sprintf("style clean [%s]", item$id), !isTRUE(item$must_not_style) || !bad_style(txt), clip(txt, 100L))
  for (pat in item$must) {
    ok(
      sprintf("content/%s [%s]", pat, item$id),
      grepl(pat, txt, ignore.case = TRUE, perl = TRUE),
      clip(txt, 100L)
    )
  }
  # Prefer short answers
  ok(sprintf("length<=1200 [%s]", item$id), nchar(txt) <= 1200L, sprintf("n=%s", nchar(txt)))
}

cat("\n=== Summary: ", n_pass, " passed, ", n_fail, " failed ===\n", sep = "")
if (length(fails)) {
  cat("Failed labels:\n")
  cat(paste0(" - ", unique(fails), collapse = "\n"), "\n")
}
if (n_fail > 0L) quit(status = 1L)
cat("Done.\n")
