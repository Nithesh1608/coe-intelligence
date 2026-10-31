box::use(
  shiny[NS, moduleServer, div, tags, addResourcePath, selectInput, reactive],
  bslib[card, card_header, card_body],
  echarts4r[renderEcharts4r, echarts4rOutput],
)
box::use(
  app/logic/viewdata/store[project_root],
  app/logic/viewdata/proposal_viz[
    load_premium_moves, prep_cohort_flow, chart_cohort_flow, cohort_year_choices,
    prep_within_quarter_swing, chart_within_quarter_swing,
    prep_spread_and_shares, chart_spread, chart_volume_shares
  ],
)

`%||%` <- function(a, b) {
  if (is.null(a) || !length(a) || (length(a) == 1L && is.na(a))) b else a
}

eda_fig <- function(file, alt = "") {
  tags$figure(
    class = "eda-fig",
    tags$img(src = file.path("eda-assets", file), alt = alt, class = "eda-fig__img")
  )
}

eda_section <- function(n, kicker, title, body, files, aside = NULL) {
  figs <- lapply(files, function(f) eda_fig(f, alt = title))
  div(
    class = "eda-beat eda-beat--full",
    div(
      class = "eda-beat__copy eda-beat__copy--wide",
      tags$span(class = "eda-beat__num", sprintf("%02d", n)),
      tags$span(class = "nx-kicker", kicker),
      tags$h3(title),
      tags$p(body),
      if (!is.null(aside)) tags$p(class = "eda-beat__aside", aside)
    ),
    div(class = "eda-fig-stack", figs)
  )
}

viz_block <- function(num, kicker, title, body, bullets, controls = NULL, charts) {
  div(
    class = "eda-beat eda-beat--full viz-proposal",
    div(
      class = "eda-beat__copy eda-beat__copy--wide",
      tags$span(class = "eda-beat__num", sprintf("%02d", num)),
      tags$span(class = "nx-kicker", kicker),
      tags$h3(title),
      tags$p(body),
      if (length(bullets)) tags$ul(class = "viz-explain__list", lapply(bullets, tags$li)),
      if (!is.null(controls)) div(class = "desk-controls desk-controls--row", controls)
    ),
    div(class = "viz-proposal__charts", charts)
  )
}

#' Market context and supply mechanics
#' @export
ui <- function(id) {
  ns <- NS(id)
  root <- tryCatch(project_root(), error = function(e) getwd())
  tryCatch(
    addResourcePath("eda-assets", file.path(root, "app", "static", "eda")),
    error = function(e) NULL
  )
  cy <- tryCatch(
    cohort_year_choices(root),
    error = function(e) list(choices = stats::setNames(2024L, "2024"), selected = 2024L)
  )
  if (is.null(cy$choices) || !length(cy$choices)) {
    cy <- list(choices = stats::setNames(2024L, "2024"), selected = 2024L)
  }

  div(
    class = "page-stack desk eda-page",
    tags$p(class = "eyebrow", "Market"),
    tags$h2("Market context before you lock packages"),
    tags$p(
      class = "lede lede--wide",
      "See the pipeline of cars coming due, how much premiums can move inside a quarter, ",
      "and how Categories A and B trade against each other - so timing and mix decisions are grounded in the market."
    ),

    viz_block(
      1, "Pipeline",
      "Where next year's certificates come from",
      paste(
        "Cars registered about ten years ago are reaching their COE decision now.",
        "This view shows that pipeline: who renews, who leaves, and how that feeds A and B quota."
      ),
      list(),
      controls = selectInput(
        ns("cohort_year"), "Exit year (year-10 decision)",
        choices = cy$choices,
        selected = cy$selected,
        width = "220px"
      ),
      charts = card(
        class = "desk-card",
        card_header("Registration -> renewal / deregistration -> quota"),
        card_body(class = "chart-frame chart-frame--tall", echarts4rOutput(ns("cohort"), height = "440px"))
      )
    ),

    viz_block(
      2, "Dealer risk",
      "How far premiums can move inside a quarter",
      paste(
        "You quote a package, then bid twice a month. This chart shows the largest premium swing inside each quarter -",
        "the move your open book is exposed to until it clears."
      ),
      list(),
      controls = selectInput(
        ns("swing_cat"), "Category",
        choices = c("Category A" = "A", "Category B" = "B"),
        selected = "A", width = "180px"
      ),
      charts = list(
        card(
          class = "desk-card",
          card_header("Max premium swing inside each quarter"),
          card_body(class = "chart-frame chart-frame--tall", echarts4rOutput(ns("swing"), height = "380px"))
        ),
        tags$p(class = "footnote", shiny::textOutput(ns("swing_note"), inline = TRUE))
      )
    ),

    viz_block(
      3, "Category mix",
      "A / B spread and volume shares",
      paste(
        "When the A/B premium gap narrows, buyers can shift between categories.",
        "Watch the spread and the certificate mix together when you allocate inventory."
      ),
      list(),
      charts = div(
        class = "timing-compare",
        style = "grid-template-columns: 1fr;",
        card(
          class = "desk-card",
          card_header("Premiums - Cat A vs Cat B"),
          card_body(class = "chart-frame chart-frame--tall", echarts4rOutput(ns("spread"), height = "360px"))
        ),
        card(
          class = "desk-card",
          card_header("Certificate volume shares (A vs B)"),
          card_body(class = "chart-frame", echarts4rOutput(ns("shares"), height = "320px"))
        )
      )
    ),

    div(
      class = "eda-beat eda-beat--full eda-beat--cta",
      div(
        class = "eda-beat__copy eda-beat__copy--wide",
        tags$span(class = "eda-beat__num", "04"),
        tags$span(class = "nx-kicker", "Feedback"),
        tags$h3("Stress renewals and policy in Scenarios"),
        tags$p(
          "Ready to move the forward path? Open ",
          tags$strong("Scenarios"),
          " to test renewals, premium paths and LTA injections against the live outlook."
        )
      )
    ),

    tags$hr(class = "eda-divider"),
    tags$p(class = "nx-kicker", "Deeper context"),
    tags$p(
      class = "lede lede--wide",
      "Extra market background: supply shape, age structure, renewals, and how quota has been set."
    ),
    div(
      class = "eda-spine",
      tags$span("Supply shape"),
      tags$span(class = "eda-spine__dot", "->"),
      tags$span("Season & cycles"),
      tags$span(class = "eda-spine__dot", "->"),
      tags$span("Deregistrations -> quota"),
      tags$span(class = "eda-spine__dot", "->"),
      tags$span("Age & lock-in"),
      tags$span(class = "eda-spine__dot", "->"),
      tags$span("Renewals"),
      tags$span(class = "eda-spine__dot", "->"),
      tags$span("Formula & price")
    ),

    eda_section(
      1, "How much supply is there?",
      "Certificate supply moves in multi-year waves",
      paste(
        "A and B quotas rise and fall for years at a time.",
        "Plan packages against that cycle, not against a flat average."
      ),
      c("01_COE_supply_over_time.png", "02_COE_supply_volatility.png"),
      "Level and volatility of released certificates."
    ),

    eda_section(
      2, "Within the year and across years",
      "The multi-year cycle matters more than the calendar month",
      paste(
        "Month-of-year patterns are mild. The longer boom-bust is what shapes fleet replacement windows."
      ),
      c("04_Monthly_patterns.png", "05_Long_term_movements.png"),
      "Monthly patterns and annual averages."
    ),

    eda_section(
      3, "The link to next quota",
      "When more cars leave, more certificates usually follow",
      paste(
        "Recent deregistrations feed the next quota. Rising exits are an early signal for looser certificate supply."
      ),
      "03_Deregistrations_and_supply.png",
      "Prior 12-month deregistrations vs subsequent quarterly quota."
    ),

    eda_section(
      4, "What is already on the road",
      "A large age cliff is due by ~2029; a thinner cohort follows",
      paste(
        "Cars aged 7-9 hit the year-10 decision soon. Ages 2-4 were bought in tight-quota years and leave a thinner pipeline behind."
      ),
      c("e1_02_age_profile.png", "08_Vehicle_age_distribution.png"),
      "Two views of the same age snapshot."
    ),

    eda_section(
      5, "Some exits are already booked",
      "Five-year renewals lock scrap dates years ahead",
      paste(
        "Those cars cannot renew again. That locked cushion was thicker in 2021-24 than in 2026-28 - useful for planning today."
      ),
      "e1_04_locked_exits.png",
      "Deregistrations implied by completed 5-year revalidations."
    ),

    eda_section(
      6, "Renewals and price",
      "When renewing gets expensive, more owners leave instead",
      paste(
        "Renewal activity and the renewal-PQP link show how price behaviour can free certificates for the market."
      ),
      c("06_Renewal_activity.png", "e1_05_renewal_pqp.png"),
      "Renewal activity and renewal share vs PQP."
    ),

    eda_section(
      7, "How quota has been set",
      "Formula supply versus LTA adjustments since 2023",
      paste(
        "Grey is the replacement rule; blue is extra certificates added; red is held back.",
        "Useful context when you stress policy levers in Scenarios."
      ),
      "e1_03_quota_rule.png",
      "Quarterly stacked bars from Feb 2023."
    ),

    eda_section(
      8, "Premiums in context",
      "Supply timing shows up in clearing prices",
      paste(
        "Long premium history and supply-price charts explain why a locked order book cares about certificate timing.",
        "Price your book in Your risk."
      ),
      c("e1_01_premiums.png", "07_Supply_and_premiums.png", "e1_06_supply_price.png"),
      "Premium history and supply-price relationships."
    ),

    tags$p(
      class = "footnote eda-close",
      "Home = outlook  |  Scenarios = stress tests  |  Results = track record  |  Your risk = price the book  |  About = method."
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    addResourcePath("eda-assets", file.path(project_root(), "app", "static", "eda"))
    root <- project_root()
    pm <- load_premium_moves(root)

    output$cohort <- renderEcharts4r({
      yr <- suppressWarnings(as.integer(input$cohort_year))
      prep <- prep_cohort_flow(store, exit_year = if (is.finite(yr)) yr else NULL)
      if (is.null(prep)) return(NULL)
      chart_cohort_flow(prep)
    })

    swing_prep <- reactive({
      prep_within_quarter_swing(pm, category = input$swing_cat %||% "A")
    })

    output$swing <- renderEcharts4r({
      prep <- swing_prep()
      if (is.null(prep)) return(NULL)
      chart_within_quarter_swing(prep)
    })

    output$swing_note <- shiny::renderText({
      prep <- swing_prep()
      if (is.null(prep) || is.null(prep$summary) || !nrow(prep$summary)) {
        return("Within-quarter swing data unavailable.")
      }
      s <- prep$summary[1, ]
      sprintf(
        "Cat %s | median quarterly swing ~ $%s (p90 ~ $%s) across %s quarters.",
        prep$category,
        format(round(s$median_swing), big.mark = ",", trim = TRUE),
        format(round(s$p90_swing), big.mark = ",", trim = TRUE),
        s$n
      )
    })

    spread_prep <- reactive({
      prep_spread_and_shares(store$bidding, from = as.Date("2016-01-01"))
    })

    output$spread <- renderEcharts4r({
      prep <- spread_prep()
      if (is.null(prep)) return(NULL)
      chart_spread(prep)
    })

    output$shares <- renderEcharts4r({
      prep <- spread_prep()
      if (is.null(prep)) return(NULL)
      chart_volume_shares(prep)
    })

    # Do not force-render EDA charts while Home is open - heavy prep + echarts
    # init on hidden panels can crash the client and blank Home charts.
  })
}
