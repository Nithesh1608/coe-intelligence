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

#' Feedback and scenarios - renewal response, demand shocks, policy injections
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$section(
      class = "desk-hero desk-hero--pitch",
      div(
        class = "desk-hero__copy",
        tags$p(class = "eyebrow", "Scenarios"),
        tags$h1("Stress today's certificate outlook."),
        tags$p(
          class = "lede lede--wide",
          "You get two twelve-quarter paths side by side: ",
          tags$strong("today's baseline"),
          " and ",
          tags$strong("your stressed case"),
          ". Move renewals, premiums, or LTA policy, then read the certificate gap before you lock packages or a replacement window."
        )
      )
    ),

    tags$section(
      class = "viz-explain",
      tags$span(class = "nx-kicker", "Step 1"),
      tags$h2("Set the stress"),
      tags$p(
        "Pick a ready-made scenario, then fine-tune. Every control changes supply over the next twelve quarters; nothing here prices premiums as a forecast."
      )
    ),
    div(
      class = "desk-controls desk-controls--row",
      selectInput(
        ns("cat"), "Category",
        choices = c("Category A" = "A", "Category B" = "B", "Merged A + B" = "AB"),
        selected = "A", width = "170px"
      ),
      selectInput(
        ns("preset"), "Ready-made scenario",
        choices = preset_choices(), selected = "baseline", width = "330px"
      ),
      sliderInput(
        ns("renew"), "Extra renewal shift (pts)",
        min = -0.15, max = 0.15, value = 0, step = 0.01, width = "280px"
      ),
      numericInput(
        ns("inject"), "Extra certificates / quarter",
        0, min = -2000, max = 5000, step = 50, width = "190px"
      )
    ),
    tags$ul(
      class = "viz-explain__list scenario-lever-list",
      tags$li(
        tags$strong("Renewals. "),
        "Owners who renew keep the car; fewer exits soon after, so fewer certificates in the following year."
      ),
      tags$li(
        tags$strong("Premium path (in the scenario pack). "),
        "Cheaper or dearer PQP changes renewal share through the fitted response, then exits and quota."
      ),
      tags$li(
        tags$strong("LTA injection / clawback. "),
        "Certificates added or removed straight into quota, on top of the formula path."
      ),
      tags$li(
        tags$strong("Merged A + B. "),
        "One combined car category if LTA joins them - same levers, pooled supply."
      )
    ),

    tags$section(
      class = "viz-explain",
      tags$span(class = "nx-kicker", "Step 2"),
      tags$h2("Read the impact")
    ),
    uiOutput(ns("lead")),
    card(
      class = "desk-card",
      full_screen = TRUE,
      card_header("Certificate supply: baseline vs your scenario"),
      card_body(class = "chart-frame chart-frame--tall", echarts4rOutput(ns("quota"), height = "400px"))
    ),
    tags$p(class = "footnote", textOutput(ns("note"), inline = TRUE)),

    tags$section(
      class = "viz-explain",
      tags$span(class = "nx-kicker", "Why the path moved"),
      tags$h2("Exits under the same stress"),
      tags$p(
        "Quota is mapped from trailing exits. This chart shows the deregistration path that sits under the certificate chart above."
      )
    ),
    card(
      class = "desk-card",
      full_screen = TRUE,
      card_header("Deregistrations: baseline vs your scenario"),
      card_body(class = "chart-frame", echarts4rOutput(ns("dereg"), height = "320px"))
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
        return(tags$p(class = "footnote", "Forward path unavailable - rebuild forward artefacts, then reload."))
      }
      q <- p$quarters
      first_year <- q[seq_len(min(4L, nrow(q))), ]
      total_delta <- sum(q$delta)
      hit <- which.max(abs(q$delta_pct))
      div(
        class = "nx-metrics nx-metrics--3",
        div(
          class = "nx-metric",
          tags$span(class = "nx-kicker", "Baseline, next 4 quarters"),
          div(class = "nx-num", fmt_int(sum(first_year$quota_base))),
          tags$span(class = "nx-hint", "certificates under today's outlook")
        ),
        div(
          class = "nx-metric nx-metric--accent",
          tags$span(class = "nx-kicker", "Your scenario vs baseline"),
          div(class = "nx-num", sprintf("%s%s", if (total_delta >= 0) "+" else "", fmt_int(total_delta))),
          tags$span(
            class = "nx-hint",
            sprintf("%+.1f%% certificates over 12 quarters", 100 * total_delta / max(sum(q$quota_base), 1))
          )
        ),
        div(
          class = "nx-metric",
          tags$span(class = "nx-kicker", "Biggest quarterly swing"),
          div(class = "nx-num", sprintf("%+.0f%%", q$delta_pct[hit])),
          tags$span(class = "nx-hint", q$label[hit])
        )
      )
    })

    output$note <- renderText({
      p <- prep()
      if (is.null(p)) return("")
      sprintf(
        "Active: %s | %s | renewal %+0.f pts | injection %s / quarter",
        if (identical(p$category, "AB")) "Merged A + B" else paste("Category", p$category),
        p$preset$label, 100 * p$renewal_shift, fmt_int(p$injection_per_qtr)
      )
    })
  })
}
