box::use(
  shiny[
    NS, moduleServer, div, tags, tagList, HTML,
    textAreaInput, actionButton, observeEvent,
    reactiveVal, renderUI, uiOutput,
    updateTextAreaInput, updateActionButton, req, outputOptions
  ],
  echarts4r[renderEcharts4r, echarts4rOutput, e_charts, e_bar, e_tooltip, e_color, e_legend, e_grid, e_x_axis, e_y_axis],
)
box::use(
  app/logic/chat/provider[ensure_chat_env, chat_ready, chat_complete, chat_model_label],
  app/logic/chat/context[system_prompt, build_project_context],
  app/logic/chat/desk_answers[suggestion_chips],
  app/logic/chat/desk_run[handle_desk_turn],
  app/logic/viewdata/scenario_viz[chart_scenario_quota],
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
      tags$span("Thinking…")
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
    tags$h1("Ask about your supply outlook"),
    tags$p(
      class = "lede lede--wide",
      "Forecast Q&A from the live model. Scenarios and pooled Your risk runs use the same desk engines as those tabs — not free-form app control."
    ),
    uiOutput(ns("status")),
    div(
      class = "coe-bot-chips",
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
    # Always-mounted chart host (never height:0 / display:none — that breaks echarts).
    div(
      class = "coe-bot-desk-panel",
      uiOutput(ns("desk_title")),
      echarts4rOutput(ns("desk_chart"), height = "340px")
    ),
    tags$p(
      class = "footnote",
      "Enter to send · Shift+Enter for a new line. Method detail lives in About."
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
        function setChipsDisabled(on){
          document.querySelectorAll('.coe-bot-chip').forEach(function(b){ b.disabled=!!on; });
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
            '<div class=\"coe-bot-pending\"><span class=\"coe-bot-pulse\"></span><span>Thinking…</span></div>');
          scrollThread();
        }
        function sendOnce(forced){
          if(gate) return;
          var ta=document.getElementById(ns+'prompt');
          var q=forced || (ta ? (ta.value||'').trim() : '');
          if(!q) return;
          gate=true;
          setChipsDisabled(true);
          paintPending(q);
          Shiny.setInputValue(ns+'ask', {q:q, nonce: Date.now()}, {priority:'event'});
          if(ta) ta.value='';
        }
        $(document).on('shiny:value', function(e){
          if(e.name!==ns+'thread') return;
          setTimeout(function(){ scrollThread(); unlockIfIdle(); setChipsDisabled(hasPending()); }, 40);
          setTimeout(function(){ unlockIfIdle(); setChipsDisabled(hasPending()); }, 220);
        });
        $(document).on('shiny:idle', function(){
          unlockIfIdle();
          setChipsDisabled(hasPending());
        });
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
          if(btn && btn.disabled) return false;
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

friendly_model_from_out <- function(out) {
  if (is.null(out) || !isTRUE(out$ok)) return(NULL)
  id <- as.character(out$model %||% "")
  prov <- as.character(out$provider %||% "")
  if (grepl("nova-pro", id, ignore.case = TRUE)) return("Amazon Nova Pro")
  if (grepl("nova-2-lite", id, ignore.case = TRUE)) return("Amazon Nova 2 Lite")
  if (grepl("nova-lite", id, ignore.case = TRUE)) return("Amazon Nova Lite")
  if (grepl("nemotron-3\\.5-bolt|nemotron-3.5-bolt", id, ignore.case = TRUE)) return("Nemotron 3.5 Bolt")
  if (grepl("nemotron", id, ignore.case = TRUE)) return("Nemotron")
  if (nzchar(id)) {
    base <- sub("^.*/", "", id)
    base <- sub(":.*$", "", base)
    return(gsub("[-_]+", " ", base))
  }
  if (identical(prov, "bedrock")) return("Amazon Nova Pro")
  if (identical(prov, "nim")) return("NIM")
  NULL
}

finish_turn <- function(history, busy, session, out, live_label = NULL) {
  done <- Filter(function(m) !isTRUE(m$pending), history())
  if (isTRUE(out$ok)) {
    history(c(done, list(list(role = "assistant", content = as.character(out$text)))))
    nm <- friendly_model_from_out(out)
    if (!is.null(live_label) && !is.null(nm)) live_label(nm)
  } else {
    history(c(done, list(list(
      role = "assistant",
      content = paste0(
        "I could not reach the model just now. ",
        out$error %||% "unknown error",
        "\n\nTry again in a moment."
      )
    ))))
  }
  busy(FALSE)
  updateActionButton(session, "send", label = "Send")
}

key_missing_msg <- function() {
  paste(
    "The bot needs a live model to answer.",
    "Preferred: **COEINTEL_AWS_ACCESS_KEY_ID** / **COEINTEL_AWS_SECRET_ACCESS_KEY** (IAM `coeintel_cursor`) for Bedrock Amazon Nova.",
    "Fallback: **NVIDIA_API_KEY** for NIM. Add keys to `.Renviron` (or Posit Variables), then reload.",
    sep = "\n\n"
  )
}

build_system <- function(store, question = NULL) {
  ctx <- build_project_context(store, question = question)
  paste(system_prompt(), "\n\n", ctx, sep = "")
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    root <- if (!is.null(store$root)) store$root else NULL
    ensure_chat_env(root)
    history <- reactiveVal(list())
    busy <- reactiveVal(FALSE)
    desk_pending <- reactiveVal(NULL)
    desk_viz <- reactiveVal(NULL)
    live_label <- reactiveVal(NULL)

    output$status <- renderUI({
      ensure_chat_env(root)
      if (!chat_ready()) {
        return(tags$p(class = "coe-bot-status coe-bot-status--bad", "Model offline"))
      }
      nm <- live_label()
      if (is.null(nm) || !nzchar(nm)) {
        # Preferred configured model until the first successful reply.
        lab <- chat_model_label()
        nm <- sub("^Bedrock · ", "", lab)
        nm <- sub("^NIM · ", "", nm)
      }
      tags$p(class = "coe-bot-status coe-bot-status--ok", paste0("Live · ", nm))
    })

    output$thread <- renderUI({
      msgs <- history()
      if (!length(msgs)) {
        return(div(
          class = "coe-bot-empty",
          tags$p("Try a forecast question, “Run inject 750 on Cat A”, or “Price 30 Cat A orders at $130k”.")
        ))
      }
      do.call(tagList, lapply(msgs, function(m) {
        bubble(m$role, m$content %||% "", pending = isTRUE(m$pending))
      }))
    })

    output$desk_title <- renderUI({
      viz <- desk_viz()
      if (is.null(viz)) {
        return(tags$span(
          class = "nx-kicker",
          "Desk chart — run a scenario or price a pooled book to populate"
        ))
      }
      tags$span(
        class = "nx-kicker",
        if (identical(viz$type, "scenario")) "Desk chart — Scenarios" else "Desk chart — Your risk"
      )
    })

    output$desk_chart <- renderEcharts4r({
      viz <- desk_viz()
      shiny::validate(shiny::need(!is.null(viz), "Run a scenario or pooled risk price to draw a chart."))
      if (identical(viz$type, "scenario")) {
        shiny::validate(shiny::need(
          !is.null(viz$prep) && !is.null(viz$prep$quarters) && nrow(viz$prep$quarters) > 0,
          "Scenario path has no quarters to chart."
        ))
        ch <- tryCatch(chart_scenario_quota(viz$prep), error = function(e) {
          warning("scenario chart: ", conditionMessage(e))
          NULL
        })
        shiny::validate(shiny::need(!is.null(ch), "Could not build the scenario chart."))
        return(ch)
      }
      if (identical(viz$type, "exposure")) {
        r <- viz$result
        shiny::validate(shiny::need(
          !is.null(r) && !is.null(r$base$portfolio$dealer_pnl),
          "Exposure run has no P&L paths to chart."
        ))
        pnl <- as.numeric(r$base$portfolio$dealer_pnl) / 1000
        h <- graphics::hist(pnl, breaks = 24, plot = FALSE)
        d <- data.frame(mid = as.numeric(h$mids), n = as.numeric(h$counts))
        return(
          d |>
            e_charts(mid) |>
            e_bar(n, name = "Simulations") |>
            e_tooltip() |>
            e_color("#ff5c1a") |>
            e_legend(show = FALSE) |>
            e_grid(left = 48, right = 16, top = 28, bottom = 56, containLabel = FALSE) |>
            e_x_axis(
              name = "Dealer P&L vs quote ($k)", nameLocation = "middle", nameGap = 28,
              axisLabel = list(color = "#a3a3a3"), splitLine = list(show = FALSE),
              axisLine = list(show = FALSE), axisTick = list(show = FALSE)
            ) |>
            e_y_axis(
              name = "Count", nameLocation = "middle", nameGap = 36,
              axisLabel = list(color = "#a3a3a3"), splitLine = list(show = FALSE),
              axisLine = list(show = FALSE), axisTick = list(show = FALSE)
            )
        )
      }
      shiny::validate(shiny::need(FALSE, "Unknown desk chart type."))
    })
    # Bot tab is often hidden at startup; don't freeze the first chart paint.
    outputOptions(output, "desk_chart", suspendWhenHidden = FALSE)

    observeEvent(input$clear, {
      if (isTRUE(busy())) return()
      history(list())
      desk_pending(NULL)
      desk_viz(NULL)
      updateTextAreaInput(session, "prompt", value = "")
    })

    observeEvent(input$ask, {
      if (isTRUE(busy())) return()
      ask <- input$ask
      q <- if (is.list(ask)) trimws(ask$q %||% "") else trimws(ask %||% "")
      if (!nzchar(q)) return()

      busy(TRUE)
      updateActionButton(session, "send", label = "Thinking…")
      prior <- Filter(function(m) !isTRUE(m$pending), history())
      history(c(prior, list(list(role = "user", content = q))))
      updateTextAreaInput(session, "prompt", value = "")

      # 1) Constrained desk runner (engines only) — may ask slots or supply facts.
      desk <- tryCatch(
        handle_desk_turn(q, store, pending = desk_pending()),
        error = function(e) list(action = "chat", error = conditionMessage(e))
      )

      if (identical(desk$action, "reply")) {
        if (isTRUE(desk$clear_pending)) desk_pending(NULL)
        if (!is.null(desk$pending)) desk_pending(desk$pending)
        finish_turn(history, busy, session, list(ok = TRUE, text = desk$text %||% ""), live_label)
        return()
      }

      if (identical(desk$action, "generate")) {
        if (isTRUE(desk$clear_pending)) desk_pending(NULL)
        if (!is.null(desk$viz)) desk_viz(desk$viz)
      }

      # 2) Live model always writes the user-facing answer (chips included).
      ensure_chat_env(root)
      if (!chat_ready()) {
        finish_turn(history, busy, session, list(ok = TRUE, text = key_missing_msg()), live_label)
        return()
      }

      turns <- utils::tail(prior, 4L)
      sys <- build_system(store, question = q)
      if (identical(desk$action, "generate") && nzchar(desk$facts %||% "")) {
        sys <- paste(sys, "\n\n", desk$facts, sep = "")
      }
      api_msgs <- c(
        list(list(role = "system", content = sys)),
        turns,
        list(list(role = "user", content = q))
      )
      out <- tryCatch(
        chat_complete(api_msgs, max_tokens = 1600L, timeout_sec = 45L),
        error = function(e) list(ok = FALSE, error = conditionMessage(e))
      )
      finish_turn(history, busy, session, out, live_label)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  })
}
