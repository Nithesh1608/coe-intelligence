box::use(
  shiny[
    NS, moduleServer, div, tags, tagList, HTML,
    textAreaInput, actionButton, observeEvent,
    reactiveVal, renderUI, uiOutput,
    updateTextAreaInput, updateActionButton
  ],
)
box::use(
  app/logic/chat/nim[nim_chat, nvidia_key_ok, ensure_nvidia_env, nim_model_label],
  app/logic/chat/context[system_prompt, build_project_context],
  app/logic/chat/desk_answers[try_desk_answer, suggestion_chips],
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
  chips <- suggestion_chips()
  div(
    class = "page-stack desk coe-bot-page",
    tags$p(class = "eyebrow", "COE bot"),
    tags$h1("Ask the desk"),
    tags$p(
      class = "lede lede--wide",
      "Ask anything about the supply desk. Chips and free-form questions both go through NIM when a key is set; otherwise they answer from the live artefacts."
    ),
    uiOutput(ns("status")),
    div(
      class = "coe-bot-chips",
      # Plain buttons (not actionButton) so only our JS send path fires.
      lapply(chips, function(label) {
        tags$button(
          type = "button",
          class = "coe-bot-chip",
          `data-q` = label,
          label
        )
      })
    ),
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
          placeholder = "e.g. What is the Cat A quota forecast? How accurate is the model?"
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
      "NIM answers chips and typed questions when NVIDIA_API_KEY is set; otherwise desk artefacts answer. Not financial advice. Enter to send · Shift+Enter for a new line."
    ),
    tags$script(HTML(sprintf(
      "(function(){
        var ns='%s';
        var gate=false;
        function threadHost(){ return document.getElementById(ns+'thread'); }
        function scrollThread(){
          var el=document.getElementById(ns+'thread_scroll');
          if(el) el.scrollTop=el.scrollHeight;
        }
        function hasPending(){
          var host=threadHost();
          return !!(host && host.querySelector('.coe-bot-bubble--pending'));
        }
        function unlockIfIdle(){
          if(!hasPending()) gate=false;
        }
        function paintPending(q){
          var host=threadHost();
          if(!host || !q) return;
          var empty=host.querySelector('.coe-bot-empty');
          if(empty) empty.remove();
          host.querySelectorAll('[data-opt=\"1\"]').forEach(function(n){ n.remove(); });
          function add(who, cls, html){
            var d=document.createElement('div');
            d.className='coe-bot-bubble '+cls;
            d.setAttribute('data-opt','1');
            d.innerHTML='<span class=\"coe-bot-who\">'+who+'</span>'+html;
            host.appendChild(d);
          }
          var esc=document.createElement('div');
          esc.textContent=q;
          add('You','coe-bot-bubble--user','<div class=\"coe-bot-text\"><p>'+esc.innerHTML+'</p></div>');
          add('COE bot','coe-bot-bubble--bot coe-bot-bubble--pending',
            '<div class=\"coe-bot-pending\"><span class=\"coe-bot-pulse\"></span><span>Writing a reply…</span></div>');
          scrollThread();
        }
        function sendOnce(forced){
          if(gate) return;
          var ta=document.getElementById(ns+'prompt');
          var q=forced || (ta ? (ta.value||'').trim() : '');
          if(!q) return;
          gate=true;
          paintPending(q);
          Shiny.setInputValue(ns+'ask', {q:q, nonce: Date.now()}, {priority:'event'});
          if(ta) ta.value='';
        }
        // Shiny replaces the thread after every answer; unlock once pending is gone.
        $(document).on('shiny:value', function(e){
          if(e.name!==ns+'thread') return;
          setTimeout(function(){ scrollThread(); unlockIfIdle(); }, 50);
          setTimeout(unlockIfIdle, 250);
        });
        $(document).on('shiny:idle', unlockIfIdle);
        $(document).on('click', '#'+ns+'send', function(ev){
          ev.preventDefault();
          ev.stopImmediatePropagation();
          sendOnce();
          return false;
        });
        $(document).on('click', '.coe-bot-chip', function(ev){
          ev.preventDefault();
          ev.stopImmediatePropagation();
          var btn=ev.currentTarget;
          var t=(btn && (btn.getAttribute('data-q') || btn.innerText)) || '';
          if(t) sendOnce(t.trim());
          return false;
        });
        $(document).on('keydown', '#'+ns+'prompt', function(ev){
          if(ev.key!=='Enter' || ev.shiftKey) return;
          ev.preventDefault();
          sendOnce();
        });
      })();",
      ns("")
    )))
  )
}

finish_turn <- function(history, busy, session, out) {
  done <- Filter(function(m) !isTRUE(m$pending), history())
  if (isTRUE(out$ok)) {
    history(c(done, list(list(role = "assistant", content = as.character(out$text)))))
  } else {
    history(c(done, list(list(
      role = "assistant",
      content = paste0("Request failed: ", out$error %||% "unknown error")
    ))))
  }
  busy(FALSE)
  updateActionButton(session, "send", label = "Send")
}

local_or_help <- function(q, store) {
  local <- tryCatch(try_desk_answer(q, store), error = function(e) NULL)
  if (!is.null(local) && nzchar(local)) return(local)
  paste(
    "I can answer forecasts, accuracy, model setup, and Your risk from the live artefacts — try those chips.",
    "For NIM phrasing on every question, set NVIDIA_API_KEY in .Renviron (local) or Posit Connect Cloud Variables.",
    sep = "\n\n"
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
          paste("NIM ready:", nim_model_label(), "· chips and free-form")
        )
      } else {
        tags$p(
          class = "coe-bot-status coe-bot-status--ok",
          "Desk artefact answers ready. Set NVIDIA_API_KEY for NIM on every question."
        )
      }
    })

    output$thread <- renderUI({
      msgs <- history()
      if (!length(msgs)) {
        return(div(
          class = "coe-bot-empty",
          tags$p("Tap a chip above, or ask for the Cat A / B quota outlook, MAPE, or how Your risk works.")
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

    observeEvent(input$ask, {
      if (isTRUE(busy())) return()
      ask <- input$ask
      q <- if (is.list(ask)) trimws(ask$q %||% "") else trimws(ask %||% "")
      if (!nzchar(q)) return()

      # Prefer NIM for chips and free-form alike; fall back to local artefacts.
      if (!nvidia_key_ok()) {
        history(c(
          history(),
          list(list(role = "user", content = q)),
          list(list(role = "assistant", content = local_or_help(q, store)))
        ))
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

      session$onFlushed(function() {
        out <- tryCatch(
          nim_chat(api_msgs, max_tokens = 1800L),
          error = function(e) list(ok = FALSE, error = conditionMessage(e))
        )
        if (!isTRUE(out$ok)) {
          soft <- local_or_help(q, store)
          out <- list(ok = TRUE, text = soft)
        }
        finish_turn(history, busy, session, out)
      }, once = TRUE)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  })
}
