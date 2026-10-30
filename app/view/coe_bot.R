box::use(
  shiny[
    NS, moduleServer, div, tags, tagList, HTML,
    textAreaInput, actionButton, observeEvent,
    reactiveVal, renderUI, uiOutput,
    updateTextAreaInput
  ],
)
box::use(
  app/logic/chat/local_answers[answer_question],
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

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk coe-bot-page",
    tags$p(class = "eyebrow", "COE bot"),
    tags$h1("Ask the desk"),
    tags$p(
      class = "lede lede--wide",
      "Instant answers from this project's brief and held-out scores — supply, features, quota, exposure."
    ),
    tags$p(class = "coe-bot-status coe-bot-status--ok", "Local desk answers · live score tables"),
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
          placeholder = "e.g. How is supply forecast? What do the features mean? Show held-out scores."
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
      "No remote model call — replies are built in-app from the brief + CSVs. Not financial advice. Enter to send."
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
    history <- reactiveVal(list())

    output$thread <- renderUI({
      msgs <- history()
      if (!length(msgs)) {
        return(div(
          class = "coe-bot-empty",
          tags$p("Try: “How is supply forecast?”, “What do the features mean?”, or “Show the held-out scores.”")
        ))
      }
      do.call(tagList, lapply(msgs, function(m) bubble(m$role, m$content %||% "")))
    })

    observeEvent(input$clear, {
      history(list())
      updateTextAreaInput(session, "prompt", value = "")
    })

    observeEvent(input$send, {
      q <- trimws(input$prompt %||% "")
      if (!nzchar(q)) return()

      out <- answer_question(q, store)
      history(c(
        history(),
        list(list(role = "user", content = q)),
        list(list(role = "assistant", content = out$text))
      ))
      updateTextAreaInput(session, "prompt", value = "")
    }, ignoreInit = TRUE)
  })
}
