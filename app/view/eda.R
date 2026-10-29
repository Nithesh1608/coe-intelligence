box::use(
  shiny[
    NS, moduleServer, div, tags, selectInput, reactive, renderUI, uiOutput, req
  ],
  bslib[card, card_body],
  echarts4r[renderEcharts4r, echarts4rOutput],
)
box::use(
  app/logic/viewdata/eda_story[
    prep_age_wave, chart_age_wave,
    prep_locked_exits, chart_locked_exits,
    prep_quota_discretion, chart_quota_discretion,
    prep_renewal_scatter, chart_renewal_scatter2,
    prep_supply_price_scatter, chart_supply_price_scatter,
    fmt_int
  ],
)

beat <- function(n, kicker, title, body, chart_ui, aside = NULL, extra = NULL) {
  div(
    class = "eda-beat",
    div(
      class = "eda-beat__copy",
      tags$span(class = "eda-beat__num", sprintf("%02d", n)),
      tags$span(class = "nx-kicker", kicker),
      if (inherits(title, "shiny.tag") || inherits(title, "shiny.tag.list")) title else tags$h3(title),
      tags$p(body),
      if (!is.null(extra)) extra,
      if (!is.null(aside)) tags$p(class = "eda-beat__aside", aside)
    ),
    card(
      class = "desk-card",
      full_screen = TRUE,
      card_body(class = "chart-frame", chart_ui)
    )
  )
}

#' Exploratory story before the models
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk eda-page",
    tags$p(class = "eyebrow", "EDA"),
    tags$h2("What the data already says about supply"),
    tags$p(
      class = "lede lede--wide",
      "Five views — bars, stacks, and scatters — that motivate forecasting exits and quota ",
      "before we show any model. Category A and B only."
    ),
    div(
      class = "eda-spine",
      tags$span("Age cliff"),
      tags$span(class = "eda-spine__dot", "→"),
      tags$span("Locked exits"),
      tags$span(class = "eda-spine__dot", "→"),
      tags$span("Formula vs LTA"),
      tags$span(class = "eda-spine__dot", "→"),
      tags$span("Renewal vs PQP"),
      tags$span(class = "eda-spine__dot", "→"),
      tags$span("Supply vs price")
    ),

    beat(
      1, "Cohorts already on the road",
      uiOutput(ns("age_title")),
      paste(
        "Age structure is the slow clock. A large block near year 10 means elevated deregistrations",
        "and quota for a few years; the thin 2–4 year band points to a later trough."
      ),
      echarts4rOutput(ns("c_age"), height = "380px"),
      aside = "Stacked bars by vehicle age. Orange = ages 7–9. Lime = ages 2–4."
    ),

    beat(
      2, "Some exits are already booked",
      uiOutput(ns("locked_title")),
      paste(
        "A 5-year renewal cannot be renewed again — those cars must leave five years later.",
        "That cushion is knowable today and shrinks in the late 2020s."
      ),
      echarts4rOutput(ns("c_locked"), height = "360px"),
      aside = "Grouped bars: cars that must exit because they took a 5-year COE renewal."
    ),

    beat(
      3, "Quota is formula plus LTA choice",
      uiOutput(ns("disc_title")),
      paste(
        "The 25% replacement rule sets a baseline from recent deregistrations.",
        "Since 2023 LTA has also added or held back certificates — so released quota is not pure mechanics."
      ),
      echarts4rOutput(ns("c_disc"), height = "360px"),
      aside = "Stacked bars per quota quarter. Grey = 25% rule. Blue = LTA added. Orange = held back.",
      extra = selectInput(
        ns("disc_cat"), "Show category",
        choices = c("Category A" = "A", "Category B" = "B"),
        selected = "A", width = "200px"
      )
    ),

    beat(
      4, "Renewal is a price decision",
      uiOutput(ns("ren_title")),
      paste(
        "Each point is a year. Further right = higher PQP; higher = more owners renewed at year 10.",
        "Expensive renewal lines up with lower renewal shares — more cars scrap and free certificates."
      ),
      echarts4rOutput(ns("c_ren"), height = "380px"),
      aside = "Scatter: average PQP vs renewal share of cars turning 10 (Cat A+B)."
    ),

    beat(
      5, "Why a locked book cares about supply",
      "When quarterly supply rises, clearing premiums usually ease",
      paste(
        "Dots are year-on-year changes in quarterly certificates vs average premium (May 2022 regime).",
        "We do not forecast premiums — but supply timing still shows up in the price a dealer pays."
      ),
      echarts4rOutput(ns("c_scatter"), height = "380px"),
      aside = "Scatter since May 2022. Tooltip shows category. Next: Results for the elastic-net forecasts."
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    age <- reactive({
      req(!is.null(store$pop_by_age))
      prep_age_wave(store$pop_by_age)
    })
    locked <- reactive({
      req(!is.null(store$revalidation))
      prep_locked_exits(store$revalidation)
    })
    disc <- reactive({
      req(!is.null(store$bids_raw), !is.null(store$dereg_lta))
      prep_quota_discretion(store$bids_raw, store$dereg_lta)
    })
    ren <- reactive({
      req(!is.null(store$revalidation), !is.null(store$pop_by_age), !is.null(store$pqp))
      prep_renewal_scatter(store$revalidation, store$pop_by_age, store$pqp)
    })
    scat <- reactive({
      req(!is.null(store$bids_raw))
      prep_supply_price_scatter(store$bids_raw)
    })

    output$age_title <- renderUI({
      a <- age()
      tags$h3(paste0(
        fmt_int(a$due), " cars hit year-10 by ~2029 — only ",
        fmt_int(a$thin), " sit in the thin 2–4 band (", format(a$latest, "%b %Y"), ")"
      ))
    })
    output$locked_title <- renderUI({
      L <- locked()
      tags$h3(paste0(
        "Locked-in exits ~", fmt_int(L$avg_past), "/yr in 2021–24 vs ~",
        fmt_int(L$avg_fut), "/yr in 2026–28"
      ))
    })
    output$disc_title <- renderUI({
      d <- disc()
      tags$h3(paste0(
        "About ", sprintf("%.0f%%", 100 * abs(d$share)),
        " of Cat A+B certificates since 2023 sat off the pure 25% rule"
      ))
    })
    output$ren_title <- renderUI({
      r <- ren()
      cor_txt <- if (is.finite(r$cor)) sprintf("r = %.2f", r$cor) else "n small"
      tags$h3(paste0("Renewal shares fall when PQP rises (", cor_txt, ")"))
    })

    output$c_age <- renderEcharts4r(chart_age_wave(age()))
    output$c_locked <- renderEcharts4r(chart_locked_exits(locked()))
    output$c_disc <- renderEcharts4r(chart_quota_discretion(disc(), input$disc_cat))
    output$c_ren <- renderEcharts4r(chart_renewal_scatter2(ren()))
    output$c_scatter <- renderEcharts4r(chart_supply_price_scatter(scat()))
  })
}
