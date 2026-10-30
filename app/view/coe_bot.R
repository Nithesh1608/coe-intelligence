box::use(
  shiny[
    NS, moduleServer, div, tags, tagList, HTML,
    textAreaInput, actionButton, observeEvent,
    reactiveVal, renderUI, uiOutput,
    updateTextAreaInput, updateActionButton
  ],
  promises[then, finally, promise_resolve],
)
box::use(
  app/logic/chat/nim[
    nim_chat_async, nvidia_key_ok, ensure_nvidia_env, nim_model_label
  ],
  app/logic/chat/context[system_prompt, build_project_context],
)

`%||%` <- function(a, b) {
  if (is.null(a) || !length(a) || (length(a) == 1L && is.na(a))) b else a
}

esc_html <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  s <- gsub(">", "&gt;", s, fixed = TRUE)
  s
}

inline_md <- function(s) {
  s <- esc_html(s)
  s <- gsub("`([^`]+)`", "<code>\\1</code>", s, perl = TRUE)
  s <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", s, perl = TRUE)
  s <- gsub("(?<!\\*)\\*([^*]+)\\*(?!\\*)", "<em>\\1</em>", s, perl = TRUE)
  s
}

is_table_sep <- function(line) {
  grepl("^\\s*\\|?\\s*:?-{3,}", line)
}

parse_table_row <- function(line) {
  line <- trimws(line)
  line <- sub("^\\|", "", line)
  line <- sub("\\|$", "", line)
  trimws(strsplit(line, "|", fixed = TRUE)[[1]])
}

format_bot_text <- function(text) {
  lines <- strsplit(as.character(text %||% ""), "\n", fixed = TRUE)[[1]]
  if (!length(lines)) return("")
  out <- character()
  i <- 1L
  n <- length(lines)

  while (i <= n) {
    line <- lines[[i]]

    if (!nzchar(trimws(line))) {
      i <- i + 1L
      next
    }

    if (grepl("^\\s*\\|", line) && i < n && is_table_sep(lines[[i + 1L]])) {
      headers <- parse_table_row(line)
      i <- i + 2L
      rows <- list()
      while (i <= n && grepl("^\\s*\\|", lines[[i]]) && !is_table_sep(lines[[i]])) {
        rows[[length(rows) + 1L]] <- parse_table_row(lines[[i]])
        i <- i + 1L
      }
      th <- paste0(
        "<th>", vapply(headers, inline_md, character(1)), "</th>",
        collapse = ""
      )
      trs <- vapply(rows, function(r) {
        cells <- r
        if (length(cells) < length(headers)) {
          cells <- c(cells, rep("", length(headers) - length(cells)))
        }
        paste0(
          "<tr>",
          paste0(
            "<td>",
            vapply(cells[seq_along(headers)], inline_md, character(1)),
            "</td>",
            collapse = ""
          ),
          "</tr>"
        )
      }, character(1))
      out <- c(out, paste0(
        "<div class=\"coe-bot-table-wrap\"><table class=\"coe-bot-table\"><thead><tr>",
        th, "</tr></thead><tbody>", paste(trs, collapse = ""), "</tbody></table></div>"
      ))
      next
    }

    if (grepl("^###\\s+", line)) {
      out <- c(out, paste0("<h4 class=\"coe-bot-h\">", inline_md(sub("^###\\s+", "", line)), "</h4>"))
      i <- i + 1L
      next
    }
    if (grepl("^##\\s+", line)) {
      out <- c(out, paste0("<h3 class=\"coe-bot-h\">", inline_md(sub("^##\\s+", "", line)), "</h3>"))
      i <- i + 1L
      next
    }
    if (grepl("^#\\s+", line)) {
      out <- c(out, paste0("<h3 class=\"coe-bot-h\">", inline_md(sub("^#\\s+", "", line)), "</h3>"))
      i <- i + 1L
      next
    }

    if (grepl("^\\s*[-*]\\s+", line)) {
      items <- character()
      while (i <= n && grepl("^\\s*[-*]\\s+", lines[[i]])) {
        items <- c(items, paste0("<li>", inline_md(sub("^\\s*[-*]\\s+", "", lines[[i]])), "</li>"))
        i <- i + 1L
      }
      out <- c(out, paste0("<ul class=\"coe-bot-list\">", paste(items, collapse = ""), "</ul>"))
      next
    }

    if (grepl("^\\s*\\d+\\.\\s+", line)) {
      items <- character()
      while (i <= n && grepl("^\\s*\\d+\\.\\s+", lines[[i]])) {
        items <- c(items, paste0("<li>", inline_md(sub("^\\s*\\d+\\.\\s+", "", lines[[i]])), "</li>"))
        i <- i + 1L
      }
      out <- c(out, paste0("<ol class=\"coe-bot-list\">", paste(items, collapse = ""), "</ol>"))
      next
    }

    if (grepl("^---\\s*$", line)) {
      out <- c(out, "<hr class=\"coe-bot-hr\"/>")
      i <- i + 1L
      next
    }

    buf <- line
    i <- i + 1L
    while (
      i <= n &&
        nzchar(trimws(lines[[i]])) &&
        !grepl("^#{1,3}\\s+", lines[[i]]) &&
        !grepl("^\\s*[-*]\\s+", lines[[i]]) &&
        !grepl("^\\s*\\d+\\.\\s+", lines[[i]]) &&
        !grepl("^\\s*\\|", lines[[i]]) &&
        !grepl("^---\\s*$", lines[[i]])
    ) {
      buf <- paste(buf, lines[[i]])
      i <- i + 1L
    }
    out <- c(out, paste0("<p>", inline_md(buf), "</p>"))
  }

  paste(out, collapse = "")
}

bubble <- function(role, text, pending = FALSE) {
  cls <- if (identical(role, "user")) {
    "coe-bot-bubble coe-bot-bubble--user"
  } else if (isTRUE(pending)) {
    "coe-bot-bubble coe-bot-bubble--bot coe-bot-bubble--pending"
  } else {
    "coe-bot-bubble coe-bot-bubble--bot"
  }
  who <- if (identical(role, "user")) "You" else "COE bot"
  body <- if (isTRUE(pending)) {
    tags$div(
      class = "coe-bot-pending",
      tags$span(class = "coe-bot-pulse", `aria-hidden` = "true"),
      tags$span("Writing a reply…")
    )
  } else {
    tags$div(class = "coe-bot-text", HTML(format_bot_text(text)))
  }
  div(class = cls, tags$span(class = "coe-bot-who", who), body)
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
      "Free-form Q&A grounded on this project's brief, elastic-net setup, and held-out scores."
    ),
    uiOutput(ns("status")),
    div(
      class = "coe-bot-shell",
      div(class = "coe-bot-thread", id = ns("thread_scroll"), uiOutput(ns("thread"))),
      div(
        class = "coe-bot-composer",
        textAreaInput(
          ns("prompt"),
          label = NULL,
          rows = 2,
          width = "100%",
          placeholder = "Ask anything about the desk — supply model, features, scores, quota, Your risk…"
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
      "Answers from the project context pack + model. Not financial advice. Enter to send · Shift+Enter for a new line."
    ),
    tags$script(HTML(sprintf(
      "(function(){
        var ns='%s';
        function scrollThread(){
          var el=document.getElementById(ns+'thread_scroll');
          if(el) el.scrollTop=el.scrollHeight;
        }
        $(document).on('shiny:value', function(e){
          if(e.name===ns+'thread') setTimeout(scrollThread, 40);
        });
        $(document).on('keydown', '#'+ns+'prompt', function(ev){
          if(ev.key!=='Enter' || ev.shiftKey) return;
          ev.preventDefault();
          var ta=ev.target;
          if(!(ta.value||'').trim()) return;
          var id=ns+'send';
          var v=(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$inputValues[id]) || 0;
          Shiny.setInputValue(id, Number(v)+1, {priority:'event'});
        });
      })();",
      ns("")
    )))
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    root <- if (!is.null(store$root)) store$root else NULL
    ensure_nvidia_env(root)
    history <- reactiveVal(list())
    busy <- reactiveVal(FALSE)
    ctx <- build_project_context(store)
    sys <- paste(system_prompt(), "\n\n", ctx, sep = "")

    output$status <- renderUI({
      ensure_nvidia_env(root)
      if (nvidia_key_ok()) {
        tags$p(
          class = "coe-bot-status coe-bot-status--ok",
          paste("Model:", nim_model_label())
        )
      } else {
        tags$p(
          class = "coe-bot-status coe-bot-status--bad",
          "API key missing — set NVIDIA_API_KEY in .Renviron (local) or Posit Connect Cloud Variables."
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
      do.call(tagList, lapply(msgs, function(m) {
        bubble(m$role, m$content %||% "", pending = isTRUE(m$pending))
      }))
    })

    observeEvent(input$clear, {
      if (isTRUE(busy())) return()
      history(list())
      updateTextAreaInput(session, "prompt", value = "")
    })

    observeEvent(input$send, {
      if (isTRUE(busy())) return()
      q <- trimws(input$prompt %||% "")
      if (!nzchar(q)) return()

      if (!nvidia_key_ok()) {
        history(c(history(), list(
          list(role = "user", content = q),
          list(
            role = "assistant",
            content = "API key missing — set NVIDIA_API_KEY locally (.Renviron) or as a Posit Connect Cloud variable, then restart/republish."
          )
        )))
        updateTextAreaInput(session, "prompt", value = "")
        return()
      }

      busy(TRUE)
      updateActionButton(session, "send", label = "Writing…")
      history(c(
        history(),
        list(list(role = "user", content = q)),
        list(list(role = "assistant", content = "", pending = TRUE))
      ))
      updateTextAreaInput(session, "prompt", value = "")

      turns <- utils::tail(
        Filter(function(m) !isTRUE(m$pending), history()),
        6L
      )
      api_msgs <- c(
        list(list(role = "system", content = sys)),
        turns
      )

      # Async HTTP so Shiny can paint the user bubble + "Writing…" while NIM runs.
      p <- tryCatch(
        nim_chat_async(api_msgs, max_tokens = 1800L),
        error = function(e) {
          promises::promise_resolve(list(ok = FALSE, error = conditionMessage(e)))
        }
      )

      then(
        p,
        onFulfilled = function(out) {
          done <- Filter(function(m) !isTRUE(m$pending), history())
          if (isTRUE(out$ok)) {
            history(c(done, list(list(role = "assistant", content = out$text))))
          } else {
            history(c(done, list(list(
              role = "assistant",
              content = paste0("Request failed: ", out$error %||% "unknown error")
            ))))
          }
        },
        onRejected = function(err) {
          done <- Filter(function(m) !isTRUE(m$pending), history())
          history(c(done, list(list(
            role = "assistant",
            content = paste0("Request failed: ", conditionMessage(err))
          ))))
        }
      ) |>
        finally(function() {
          busy(FALSE)
          updateActionButton(session, "send", label = "Send")
        })
    }, ignoreInit = TRUE)
  })
}
