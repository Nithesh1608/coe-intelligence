box::use(
  shiny[
    NS, moduleServer, div, tags, selectInput, sliderInput, numericInput,
    reactive, renderText, renderUI, uiOutput, textOutput, req
  ],
  bslib[card, card_header, card_body],
  echarts4r[renderEcharts4r, echarts4rOutput],
)
box::use(
  app/logic/viewdata/scenario_viz[
    preset_choices, prep_scenario, chart_scenario_quota, chart_scenario_dereg
  ],
)

`%||%` <- function(a, b) {
  if (is.null(a) || !length(a) || (length(a) == 1L && is.na(a))) b else a
}

fmt_int <- function(x) format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
fmt_sgd <- function(x) paste0("$", fmt_int(x))

#' Feedback and scenarios — renewal response, demand shocks, policy injections
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$p(class = "eyebrow", "Feedback and scenarios"),
    tags$h2("How behaviour and policy move the supply path"),
    tags$p(
      class = "lede lede--wide",
      "The baseline is the mechanical forward path: registration cohorts reaching their COE decision, ",
      "with PQP held where it is today. Here you change behaviour and policy on top of it. ",
      "Renewal share is a fitted function of PQP, so a premium path changes renewals, deregistrations, ",
      "and then quota twelve months later. Injections add certificates directly."
    ),
    div(
      class = "desk-controls desk-controls--row",
      selectInput(
        ns("cat"), "Category",
        choices = c("Category A" = "A", "Category B" = "B", "Merged A + B" = "AB"),
        selected = "A", width = "170px"
      ),
      selectInput(ns("preset"), "Scenario", choices = preset_choices(), selected = "baseline", width = "330px"),
      sliderInput(
        ns("renew"), "Extra renewal-rate shift (share points)",
        min = -0.15, max = 0.15, value = 0, step = 0.01, width = "280px"
      ),
      numericInput(ns("inject"), "Extra car COEs per quarter", 0, min = -2000, max = 5000, step = 50, width = "190px")
    ),
    uiOutput(ns("lead")),
    card(
      class = "desk-card",
      full_screen = TRUE,
      card_header("Quarterly quota: baseline versus scenario (next 12 quarters)"),
      card_body(class = "chart-frame chart-frame--tall", echarts4rOutput(ns("quota"), height = "400px"))
    ),
    tags$p(class = "footnote", textOutput(ns("note"), inline = TRUE)),
    tags$section(
      class = "viz-explain",
      tags$span(class = "nx-kicker", "Mechanics"),
      tags$h2("What the lever does"),
      tags$ul(
        class = "viz-explain__list",
        tags$li(tags$strong("Renewal response - "), textOutput(ns("slope_text"), inline = TRUE)),
        tags$li(tags$strong("Due cohort - "), "cars reaching the year-10 decision each month, projected from registrations ten years earlier and age stock."),
        tags$li(tags$strong("Quota - "), "calibrated share of trailing twelve-month deregistrations, so a change in exits feeds quota over the following year."),
        tags$li(tags$strong("Injection - "), "added to published quota in proportion to recent A/B shares; it does not change deregistrations."),
        tags$li(tags$strong("Merged A + B - "), "sums both categories, the shape a single car category would take if LTA merges them.")
      )
    ),
    card(
      class = "desk-card",
      full_screen = TRUE,
      card_header("Quarterly deregistrations: baseline versus scenario"),
      card_body(class = "chart-frame", echarts4rOutput(ns("dereg"), height = "320px"))
    ),
    tags$p(
      class = "footnote",
      "Scenarios shift the central path and carry the baseline bootstrap band with it. ",
      "They are supply scenarios, not premium forecasts."
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    prep <- reactive({
      tryCatch(
        prep_scenario(
          store,
          category = input$cat %||% "A",
          preset_id = input$preset %||% "baseline",
          renewal_shift = as.numeric(input$renew %||% 0),
          injection_per_qtr = as.numeric(input$inject %||% 0),
          horizon_quarters = 12L
        ),
        error = function(e) {
          warning("Scenario prep failed: ", conditionMessage(e))
          NULL
        }
      )
    })

    output$quota <- renderEcharts4r({
      p <- prep()
      req(!is.null(p), nrow(p$quarters) > 0)
      chart_scenario_quota(p)
    })

    output$dereg <- renderEcharts4r({
      p <- prep()
      req(!is.null(p), nrow(p$quarters) > 0)
      chart_scenario_dereg(p)
    })

    output$lead <- renderUI({
      p <- prep()
      if (is.null(p) || !nrow(p$quarters)) {
        return(tags$p(class = "footnote", "Forward path unavailable - run scripts/fit_and_forecast.R."))
      }
      q <- p$quarters
      first_year <- q[seq_len(min(4L, nrow(q))), ]
      total_delta <- sum(q$delta)
      div(
        class = "nx-metrics nx-metrics--3",
        div(
          class = "nx-metric",
          tags$span(class = "nx-kicker", "Baseline, next 4 quarters"),
          div(class = "nx-num", fmt_int(sum(first_year$quota_base))),
          tags$span(class = "nx-hint", "certificates, central path")
        ),
        div(
          class = "nx-metric nx-metric--accent",
          tags$span(class = "nx-kicker", "Scenario change over 12 quarters"),
          div(class = "nx-num", sprintf("%s%s", if (total_delta >= 0) "+" else "", fmt_int(total_delta))),
          tags$span(class = "nx-hint", sprintf("%+.1f%% versus baseline", 100 * total_delta / max(sum(q$quota_base), 1)))
        ),
        div(
          class = "nx-metric",
          tags$span(class = "nx-kicker", "Largest quarterly move"),
          div(class = "nx-num", sprintf("%+.0f%%", q$delta_pct[which.max(abs(q$delta_pct))])),
          tags$span(class = "nx-hint", q$label[which.max(abs(q$delta_pct))])
        )
      )
    })

    output$note <- renderText({
      p <- prep()
      if (is.null(p)) return("")
      pqp_end <- utils::tail(p$quarters$pqp_scenario, 1)
      sprintf(
        "%s | %s | renewal shift %+.0f share points | injection %s per quarter | PQP ends near %s. %s",
        if (identical(p$category, "AB")) "Merged A + B" else paste("Category", p$category),
        p$preset$label, 100 * p$renewal_shift, fmt_int(p$injection_per_qtr), fmt_sgd(pqp_end), p$preset$note
      )
    })

    output$slope_text <- renderText({
      p <- prep()
      r <- store$renewal_response
      if (is.null(r) || !nrow(r)) return("fitted on LTA revalidations against PQP.")
      if (!is.null(p) && identical(p$category, "AB")) {
        return(sprintf(
          "fitted on 2015-2026 revalidations: slopes %.2f (A) and %.2f (B) share points per log-unit of PQP. In the data, dearer PQP goes with fewer renewals, not more.",
          r$slope_log_pqp[r$category == "A"], r$slope_log_pqp[r$category == "B"]
        ))
      }
      cat <- p$category %||% "A"
      row <- r[as.character(r$category) == cat, ]
      sprintf(
        "fitted on %d months (%s to %s): renewal share falls %.1f points for every 10%% rise in PQP (R² %.2f). The data contradict the common claim that dear premiums push owners to renew.",
        row$n, row$from, row$to, 100 * abs(row$slope_log_pqp) * log(1.1), row$r2
      )
    })
  })
}
