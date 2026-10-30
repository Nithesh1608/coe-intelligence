box::use(
  shiny[
    NS, moduleServer, div, tags, tagList, HTML,
    textAreaInput, actionButton, observeEvent,
    reactiveVal, renderUI, uiOutput,
    updateTextAreaInput, req
  ],
)
box::use(
  app/logic/chat/nim[nim_chat, nvidia_key_ok, ensure_nvidia_env, nim_model_id],
  app/logic/chat/context[system_prompt, build_project_context],
)

`%||%` <- function(a, b) {
  if (is.null(a) || !length(a) || (length(a) == 1L && is.na(a))) b else a
}

bubble <- function(role, text) {
  cls <- if (identical(role, "user")) {
    "coe-bot-bubble coe-bot-bubble--user"
  } else {
    "coe-bot-bubble coe-bot-bubble--bot"
  }
  who <- if (identical(role, "user")) "You" else "COE bot"
  div(
    class = cls,
    tags$span(class = "coe-bot-who", who),
    tags$div(class = "coe-bot-text", HTML(format_bot_text(text)))
  )
}

format_bot_text <- function(text) {
  esc <- function(s) {
    s <- gsub("&", "&amp;", s, fixed = TRUE)
    s <- gsub("<", "&lt;", s, fixed = TRUE)
    s <- gsub(">", "&gt;", s, fixed = TRUE)
    s
  }
  parts <- strsplit(as.character(text), "\n\n", fixed = TRUE)[[1]]
  paste(
    vapply(parts, function(p) {
      p <- esc(p)
      p <- gsub("\n", "<br/>", p, fixed = TRUE)
      sprintf("<p>%s</p>", p)
    }, character(1)),
    collapse = ""
  )
}

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk coe-bot-page",
    tags$p(class = "eyebrow", "COE bot"),
    tags$h1("Ask the desk"),
    tags$p(
      class = "lede lede--wide",
      "Grounded on this project's cleaning, features, elastic-net supply model, quota mapping, ",
      "and exposure calculator — via NVIDIA NIM."
    ),
    uiOutput(ns("status")),
    div(
      class = "coe-bot-shell",
      div(class = "coe-bot-thread", uiOutput(ns("thread"))),
      div(
        class = "coe-bot-composer",
        textAreaInput(
          ns("prompt"),
          label = NULL,
          rows = 3,
          width = "100%",
          placeholder = "e.g. How does due-stock enter the elastic net? How does Your risk use supply?"
        ),
        div(
          class = "coe-bot-actions",
          actionButton(ns("send"), "Send", class = "btn btn-primary coe-bot-send"),
          actionButton(ns("clear"), "Clear", class = "btn btn-outline-secondary")
        )
      )
    ),
    tags$p(
      class = "footnote",
      "Answers use a fixed project brief plus held-out scores. Not financial advice."
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    ensure_nvidia_env()
    history <- reactiveVal(list())
    busy <- reactiveVal(FALSE)
    ctx <- build_project_context(store)
    sys <- paste(system_prompt(), "\n\n", ctx, sep = "")

    output$status <- renderUI({
      if (nvidia_key_ok()) {
        tags$p(
          class = "coe-bot-status coe-bot-status--ok",
          paste0(
            "NIM key loaded · preferred model ", nim_model_id(),
            " (open that model on build.nvidia.com once to enable it)"
          )
        )
      } else {
        tags$p(
          class = "coe-bot-status coe-bot-status--bad",
          "NVIDIA_API_KEY missing. Add it to .Renviron in the project root and restart the app."
        )
      }
    })

    output$thread <- renderUI({
      msgs <- history()
      if (!length(msgs)) {
        return(div(
          class = "coe-bot-empty",
          tags$p("Ask how supply is forecast, what features mean, or how exposure uses predicted quota.")
        ))
      }
      do.call(tagList, lapply(msgs, function(m) bubble(m$role, m$content)))
    })

    observeEvent(input$clear, {
      history(list())
      updateTextAreaInput(session, "prompt", value = "")
    })

    observeEvent(input$send, {
      req(!isTRUE(busy()))
      q <- trimws(input$prompt %||% "")
      if (!nzchar(q)) return()
      if (!nvidia_key_ok()) {
        history(c(history(), list(list(
          role = "assistant",
          content = "NVIDIA_API_KEY is not loaded. Save it in .Renviron and restart."
        ))))
        return()
      }

      prior <- history()
      prior <- c(prior, list(list(role = "user", content = q)))
      history(prior)
      updateTextAreaInput(session, "prompt", value = "")
      busy(TRUE)

      turns <- utils::tail(prior, 8L)
      api_msgs <- c(
        list(list(role = "system", content = sys)),
        turns
      )

      out <- tryCatch(
        nim_chat(api_msgs),
        error = function(e) list(ok = FALSE, error = conditionMessage(e))
      )

      if (isTRUE(out$ok)) {
        history(c(history(), list(list(role = "assistant", content = out$text))))
      } else {
        history(c(history(), list(list(
          role = "assistant",
          content = paste0("Request failed: ", out$error %||% "unknown error")
        ))))
      }
      busy(FALSE)
    })
  })
}
