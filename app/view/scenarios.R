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
    tags$p(class = "eyebrow", "Scenarios"),
    tags$h2("What if renewals, premiums or LTA policy change?"),
    tags$p(
      class = "lede lede--wide",
      "Start from today’s outlook, then stress the levers that move certificate supply: ",
      "more renewals, a different premium path, extra certificates from LTA, or a merged A+B category. ",
      "See the impact on the next twelve quarters before you lock packages or replacement windows."
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
        ns("renew"), "Renewal rate change",
        min = -0.15, max = 0.15, value = 0, step = 0.01, width = "280px"
      ),
      numericInput(ns("inject"), "Extra certificates / quarter", 0, min = -2000, max = 5000, step = 50, width = "190px")
    ),
    uiOutput(ns("lead")),
    card(
      class = "desk-card",
      full_screen = TRUE,
      card_header("Quota: baseline versus your scenario"),
      card_body(class = "chart-frame chart-frame--tall", echarts4rOutput(ns("quota"), height = "400px"))
    ),
    tags$p(class = "footnote", textOutput(ns("note"), inline = TRUE)),
    tags$section(
      class = "viz-explain",
      tags$span(class = "nx-kicker", "What you can change"),
      tags$ul(
        class = "viz-explain__list",
        tags$li(tags$strong("Renewals — "), "more owners keeping cars means fewer certificates soon after."),
        tags$li(tags$strong("Premium path — "), "cheaper or dearer renewals shift how many cars leave."),
        tags$li(tags$strong("LTA injection — "), "extra certificates added straight into the quota."),
        tags$li(tags$strong("Merged A + B — "), "one combined car category, if LTA joins them.")
      )
    ),
    card(
      class = "desk-card",
      full_screen = TRUE,
      card_header("Deregistrations: baseline versus your scenario"),
      card_body(class = "chart-frame", echarts4rOutput(ns("dereg"), height = "320px"))
    ),
    tags$p(
      class = "footnote",
      "Use this to pressure-test timing. How the levers are modelled is in About."
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
          tags$span(class = "nx-hint", "certificates")
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
      sprintf(
        "%s · %s · renewal %+.0f pts · injection %s / quarter",
        if (identical(p$category, "AB")) "Merged A + B" else paste("Category", p$category),
        p$preset$label, 100 * p$renewal_shift, fmt_int(p$injection_per_qtr)
      )
    })
  })
}
