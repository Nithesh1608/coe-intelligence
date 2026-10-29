box::use(
  shiny[
    NS, moduleServer, div, tags, selectInput, reactive, renderUI, uiOutput,
    req
  ],
  bslib[card, card_header, card_body],
  echarts4r[renderEcharts4r, echarts4rOutput],
)
box::use(
  app/logic/viewdata/eda_story[
    prep_supply_swings, chart_supply_swings,
    prep_exits_to_quota, chart_exits_to_quota,
    prep_age_wave, chart_age_wave,
    prep_renewal_price, chart_renewal_price,
    prep_supply_premium, chart_supply_premium,
    fmt_int
  ],
)

beat <- function(n, kicker, title, body, chart_id, height = "360px", aside = NULL) {
  div(
    class = "eda-beat",
    div(
      class = "eda-beat__copy",
      tags$span(class = "eda-beat__num", sprintf("%02d", n)),
      tags$span(class = "nx-kicker", kicker),
      tags$h3(title),
      tags$p(body),
      if (!is.null(aside)) tags$p(class = "eda-beat__aside", aside)
    ),
    card(
      class = "desk-card",
      full_screen = TRUE,
      card_body(
        class = "chart-frame",
        echarts4rOutput(chart_id, height = height)
      )
    )
  )
}

#' Exploratory story — supply → cohorts → renewals → why exposure
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk eda-page",
    tags$p(class = "eyebrow", "EDA"),
    tags$h2("How COE supply actually behaves"),
    tags$p(
      class = "lede lede--wide",
      "Before the forecast: five charts that explain why Category A and B certificates ",
      "move in waves, what locks those waves in, and why timing a locked book is a money problem."
    ),
    div(
      class = "eda-spine",
      tags$span("Supply swings"),
      tags$span(class = "eda-spine__dot", "→"),
      tags$span("Exits feed quota"),
      tags$span(class = "eda-spine__dot", "→"),
      tags$span("Age cliff"),
      tags$span(class = "eda-spine__dot", "→"),
      tags$span("Renewals bend timing"),
      tags$span(class = "eda-spine__dot", "→"),
      tags$span("Tight supply, dear COEs")
    ),

    beat(
      1, "The market dealers buy into",
      "Certificate supply is not a smooth line",
      paste(
        "Monthly auction quotas for Categories A and B swing for years at a time.",
        "A dealer who prices packages against “average supply” is pricing against a fiction."
      ),
      ns("c_supply"),
      aside = "Monthly sum of both bidding rounds when both are complete."
    ),

    div(
      class = "eda-beat",
      div(
        class = "eda-beat__copy",
        tags$span(class = "eda-beat__num", "02"),
        tags$span(class = "nx-kicker", "The formula link"),
        tags$h3("Cars leaving the road set up the next certificates"),
        tags$p(
          "LTA’s replacement rule looks back at recent deregistrations. ",
          "When exits rise, the next quota quarter usually follows — that is the lever our supply models learn."
        ),
        selectInput(
          ns("exit_cat"), "Show category",
          choices = c("Category A" = "A", "Category B" = "B"),
          selected = "A", width = "200px"
        ),
        tags$p(
          class = "eda-beat__aside",
          "Grey: exits in the prior 12 months. Orange: certificates in the following quarter."
        )
      ),
      card(
        class = "desk-card",
        full_screen = TRUE,
        card_body(class = "chart-frame", echarts4rOutput(ns("c_exits"), height = "360px"))
      )
    ),

    uiOutput(ns("age_beat")),

    beat(
      4, "Behaviour on top of the cohort",
      "When renewing gets expensive, more cars leave",
      paste(
        "Owners facing year-10 can pay PQP to renew or scrap and free a certificate.",
        "Higher prevailing PQP lines up with lower renewal rates — exits are partly a price decision, not only age."
      ),
      ns("c_renew"),
      aside = "Yearly Cat A+B. Solid: renewal rate. Dashed: average PQP."
    ),

    beat(
      5, "Why Your risk exists",
      "Loose supply and cheap premiums rarely travel together",
      paste(
        "We do not forecast the next premium. We do show that certificate scarcity and clearing prices move together.",
        "If your book is locked to a quote, a tight quarter is a P&L event — that is the exposure calculator."
      ),
      ns("c_prem"),
      aside = "Category A monthly certificates vs average clearing premium."
    ),

    tags$p(
      class = "footnote eda-close",
      "Next: Results shows the held-out supply forecasts. Your risk prices a locked book under those supply paths."
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    swings <- reactive({
      req(!is.null(store$bids_raw))
      prep_supply_swings(store$bids_raw)
    })
    exits <- reactive({
      req(!is.null(store$dereg_lta), !is.null(store$bids_raw))
      prep_exits_to_quota(store$dereg_lta, store$bids_raw)
    })
    age <- reactive({
      req(!is.null(store$pop_by_age))
      prep_age_wave(store$pop_by_age)
    })
    renew <- reactive({
      req(!is.null(store$renewal_rate))
      prep_renewal_price(store$renewal_rate)
    })
    prem <- reactive({
      req(!is.null(store$bids_raw))
      prep_supply_premium(store$bids_raw, "A")
    })

    output$c_supply <- renderEcharts4r({
      chart_supply_swings(swings())
    })
    output$c_exits <- renderEcharts4r({
      chart_exits_to_quota(exits(), input$exit_cat)
    })
    output$age_beat <- renderUI({
      a <- age()
      beat(
        3, "What is already on the road",
        paste0(
          fmt_int(a$due), " cars hit the 10-year decision by ~2029 — only ",
          fmt_int(a$thin), " follow in the thin 2–4 year band"
        ),
        paste(
          "Age structure is the slow clock behind exits. A large block near year 10 means elevated deregistrations",
          "and quota for a few years; the thin cohorts behind them point to a later trough.",
          paste0("Snapshot: ", format(a$latest, "%B %Y"), ".")
        ),
        session$ns("c_age"),
        aside = "Orange: ages 7–9. Lime: ages 2–4 (bought in the tight-quota years)."
      )
    })
    output$c_age <- renderEcharts4r({
      chart_age_wave(age())
    })
    output$c_renew <- renderEcharts4r({
      chart_renewal_price(renew())
    })
    output$c_prem <- renderEcharts4r({
      chart_supply_premium(prem())
    })
  })
}
