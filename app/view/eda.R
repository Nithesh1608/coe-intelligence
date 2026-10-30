box::use(
  shiny[
    NS, moduleServer, div, tags, addResourcePath, selectInput, sliderInput,
    reactive
  ],
  bslib[card, card_header, card_body],
  echarts4r[renderEcharts4r, echarts4rOutput],
)
box::use(
  app/logic/viewdata/store[project_root],
  app/logic/viewdata/proposal_viz[
    load_premium_moves, prep_cohort_flow, chart_cohort_flow,
    prep_within_quarter_swing, chart_within_quarter_swing,
    prep_spread_and_shares, chart_spread, chart_volume_shares,
    prep_feedback_scenario, chart_feedback_scenario
  ],
)

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

viz_block <- function(ns, num, kicker, title, body, bullets, controls = NULL, charts) {
  div(
    class = "eda-beat eda-beat--full viz-proposal",
    div(
      class = "eda-beat__copy eda-beat__copy--wide",
      tags$span(class = "eda-beat__num", sprintf("%02d", num)),
      tags$span(class = "nx-kicker", kicker),
      tags$h3(title),
      tags$p(body),
      tags$ul(class = "viz-explain__list", lapply(bullets, tags$li)),
      if (!is.null(controls)) div(class = "desk-controls desk-controls--row", controls)
    ),
    div(class = "viz-proposal__charts", charts)
  )
}

#' EDA — proposal visuals first, then notebook story
#' @export
ui <- function(id) {
  ns <- NS(id)
  addResourcePath("eda-assets", file.path(project_root(), "app", "static", "eda"))
  div(
    class = "page-stack desk eda-page",
    tags$p(class = "eyebrow", "EDA"),
    tags$h2("Supply mechanics and category dynamics"),
    tags$p(
      class = "lede lede--wide",
      "Interactive charts from the project proposal — cohort flow, within-quarter swings, A/B/E spreads, ",
      "and the renewal feedback loop — each with a plain-language explanation. Scroll further for the full notebook story."
    ),

    tags$p(class = "nx-kicker", style = "margin-top:1.5rem;", "Proposal visuals"),

    viz_block(
      ns, 2, "Proposal · 2 of 6",
      "Cohort flow — why the next few years are partly knowable",
      paste(
        "Every COE runs about ten years. Registrations in year t become the stock that decides to renew or scrap in year t+10.",
        "Scrapping feeds deregistrations, and deregistrations feed the replacement quota. This diagram makes that pipeline legible."
      ),
      list(
        tags$span(tags$strong("Left → right — "), "one registration cohort moving through survival, renewal, scrap, and quota."),
        tags$span(tags$strong("Renewed (PQP) — "), "owners who pay the Prevailing Quota Premium and keep the car off the scrap heap."),
        tags$span(tags$strong("Cat A/B quota / E pool — "), "how deregistrations are split under the replacement rule.")
      ),
      controls = selectInput(
        ns("cohort_year"), "Exit year (year-10 decision)",
        choices = c("Loading…" = ""),
        width = "220px"
      ),
      charts = card(
        class = "desk-card",
        card_header("Registration → renewal / deregistration → quota"),
        card_body(class = "chart-frame chart-frame--tall", echarts4rOutput(ns("cohort"), height = "440px"))
      )
    ),

    viz_block(
      ns, 3, "Proposal · 3 of 6",
      "Within-quarter volatility — the dealer’s fortnight problem",
      paste(
        "Dealers guarantee package prices, then bid twice a month. Premiums can move thousands of dollars inside a single quarter.",
        "This chart is the historical size of that swing — the exposure problem in dollars, before any model."
      ),
      list(
        tags$span(tags$strong("Bar height — "), "maximum premium swing across exercises inside that quarter ($ thousands)."),
        tags$span(tags$strong("Why it matters — "), "a locked order book is short that swing until every order clears.")
      ),
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
      ns, 4, "Proposal · 4 of 6",
      "A / B / E spread and volume shares — substitution in view",
      paste(
        "Dealers allocate inventory across Categories A, B and E. When the A–B gap narrows, demand can migrate.",
        "Premiums show the spread; certificate volume shares show whether the mix of supply moved with it. Policy events are marked."
      ),
      list(
        tags$span(tags$strong("Premium lines — "), "Cat A, B, and open Cat E clearing prices."),
        tags$span(tags$strong("Dashed markers — "), "COVID bidding suspension (2020) and the May 2022 A/B redefinition."),
        tags$span(tags$strong("Share areas — "), "how A/B/E split monthly certificate volume (substitution / mix).")
      ),
      charts = div(
        class = "timing-compare",
        style = "grid-template-columns: 1fr;",
        card(
          class = "desk-card",
          card_header("Premiums — Cat A vs B vs E"),
          card_body(class = "chart-frame chart-frame--tall", echarts4rOutput(ns("spread"), height = "360px"))
        ),
        card(
          class = "desk-card",
          card_header("Certificate volume shares"),
          card_body(class = "chart-frame", echarts4rOutput(ns("shares"), height = "320px"))
        )
      )
    ),

    viz_block(
      ns, 5, "Proposal · 5 of 6",
      "Feedback loop explorer — renewals suppress future supply",
      paste(
        "When premiums (and PQP) are high, more owners renew instead of scrapping. That cuts near-term deregistrations,",
        "which cuts future quota. Move the renewal shift to see how supply two to three years out responds."
      ),
      list(
        tags$span(tags$strong("Baseline (dashed) — "), "seasonal-naive deregistrations mapped through the replacement quota identity."),
        tags$span(tags$strong("Orange — "), "same pipeline after your renewal-rate shift (positive = more renewals = less scrap = less future quota)."),
        tags$span(tags$strong("Honesty — "), "this is a scenario lever on the mechanical pipeline, not a fitted premium forecast.")
      ),
      controls = list(
        selectInput(
          ns("fb_cat"), "Category",
          choices = c("Category A" = "A", "Category B" = "B"),
          selected = "A", width = "180px"
        ),
        sliderInput(
          ns("fb_shift"), "Renewal-rate shift",
          min = -0.25, max = 0.25, value = 0.10, step = 0.05, width = "260px"
        )
      ),
      charts = list(
        card(
          class = "desk-card",
          card_header("Projected quarterly quota under renewal scenarios"),
          card_body(class = "chart-frame chart-frame--tall", echarts4rOutput(ns("feedback"), height = "380px"))
        ),
        tags$p(class = "footnote", shiny::textOutput(ns("fb_note"), inline = TRUE))
      )
    ),

    tags$hr(class = "eda-divider"),
    tags$p(class = "nx-kicker", "Notebook story"),
    tags$p(
      class = "lede lede--wide",
      "All charts from our EDA notebooks, in reading order. Light figures keep the original ggplot look."
    ),
    div(
      class = "eda-spine",
      tags$span("Supply shape"),
      tags$span(class = "eda-spine__dot", "→"),
      tags$span("Season & cycles"),
      tags$span(class = "eda-spine__dot", "→"),
      tags$span("Deregistrations → quota"),
      tags$span(class = "eda-spine__dot", "→"),
      tags$span("Age & lock-in"),
      tags$span(class = "eda-spine__dot", "→"),
      tags$span("Renewals"),
      tags$span(class = "eda-spine__dot", "→"),
      tags$span("Formula & price")
    ),

    eda_section(
      1, "How much supply is there?",
      "Certificate quotas swing for years — they are not a smooth line",
      paste(
        "Monthly auction quotas for Categories A, B and E show long waves and deep troughs.",
        "A dealer pricing packages against “average supply” is pricing against a fiction."
      ),
      c("01_COE_supply_over_time.png", "02_COE_supply_volatility.png"),
      "EDA 2 · RQ1 — level and rolling volatility."
    ),

    eda_section(
      2, "Within the year and across years",
      "Seasonality is mild; the multi-year cycle is the real story",
      paste(
        "Month-of-year patterns exist but are secondary. Calendar-year averages show the longer boom–bust",
        "that matters for fleet replacement windows."
      ),
      c("04_Monthly_patterns.png", "05_Long_term_movements.png"),
      "EDA 2 · RQ3 — monthly patterns and annual averages."
    ),

    eda_section(
      3, "The mechanical link",
      "Deregistrations set up the next certificates",
      paste(
        "LTA’s replacement rule looks back at recent deregistrations. When deregistrations rise, the next quota quarter",
        "usually follows — that is the lever our elastic-net models learn."
      ),
      "03_Deregistrations_and_supply.png",
      "EDA 2 · RQ2 — prior 12-month deregistrations vs subsequent quarterly quota."
    ),

    eda_section(
      4, "What is already on the road",
      "A large age cliff is due by ~2029; a thin cohort follows",
      paste(
        "Age structure is the slow clock. Cars aged 7–9 hit the year-10 decision soon; ages 2–4 were bought",
        "in the tight-quota years and leave a thinner pipeline behind."
      ),
      c("e1_02_age_profile.png", "08_Vehicle_age_distribution.png"),
      "EDA 1 chart 2 and EDA 2 · RQ5 — two views of the same age snapshot."
    ),

    eda_section(
      5, "Some deregistrations are already booked",
      "Five-year renewals lock scrap dates five years ahead",
      paste(
        "Those cars cannot renew again. The locked-in deregistration cushion was thicker in 2021–24 than in 2026–28 —",
        "knowable today, before any model."
      ),
      "e1_04_locked_exits.png",
      "EDA 1 chart 4 — deregistrations implied by completed 5-year revalidations."
    ),

    eda_section(
      6, "Renewal is a price decision",
      "When renewing gets expensive, more owners scrap instead",
      paste(
        "Renewal activity over time, and the yearly renewal–PQP scatter, both show behaviour on top of the cohort.",
        "Expensive PQP lines up with lower renewal shares — freeing certificates rather than holding them back."
      ),
      c("06_Renewal_activity.png", "e1_05_renewal_pqp.png"),
      "EDA 2 · RQ4 and EDA 1 chart 5."
    ),

    eda_section(
      7, "Quota is formula plus LTA choice",
      "Since 2023 a large share of A/B certificates sat off the pure 25% rule",
      paste(
        "Grey is what the replacement rule gives; blue is extra LTA added; red is held back.",
        "Supply is partly mechanical — and partly discretionary."
      ),
      "e1_03_quota_rule.png",
      "EDA 1 chart 3 — quarterly stacked bars from Feb 2023."
    ),

    eda_section(
      8, "Premiums in context — not our forecast target",
      "Supply timing still shows up in clearing prices",
      paste(
        "Long premium history with regime shading, monthly supply vs premium, and YoY quota–premium scatters",
        "explain why a locked order book cares about supply. We still do not forecast the next premium."
      ),
      c("e1_01_premiums.png", "07_Supply_and_premiums.png", "e1_06_supply_price.png"),
      "EDA 1 charts 1 & 6 and EDA 2 · RQ5."
    ),

    tags$p(
      class = "footnote eda-close",
      "Fan chart lives on Home. Exposure waterfall lives on Your risk after you price a book. Results shows held-out elastic-net forecasts."
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    addResourcePath("eda-assets", file.path(project_root(), "app", "static", "eda"))
    root <- project_root()
    pm <- load_premium_moves(root)

    # Cohort year choices
    cohort0 <- prep_cohort_flow(store)
    if (!is.null(cohort0) && length(cohort0$years)) {
      shiny::updateSelectInput(
        session, "cohort_year",
        choices = as.character(cohort0$years),
        selected = as.character(cohort0$exit_year)
      )
    }

    output$cohort <- renderEcharts4r({
      yr <- suppressWarnings(as.integer(input$cohort_year))
      prep <- prep_cohort_flow(store, exit_year = if (is.finite(yr)) yr else NULL)
      if (is.null(prep)) return(NULL)
      chart_cohort_flow(prep)
    })

    swing_prep <- reactive({
      prep_within_quarter_swing(pm, category = input$swing_cat)
    })

    output$swing <- renderEcharts4r({
      prep <- swing_prep()
      if (is.null(prep)) return(NULL)
      chart_within_quarter_swing(prep)
    })

    output$swing_note <- shiny::renderText({
      prep <- swing_prep()
      if (is.null(prep) || is.null(prep$summary) || !nrow(prep$summary)) {
        return("Within-quarter swing table missing.")
      }
      s <- prep$summary[1, ]
      sprintf(
        "Cat %s · median quarterly swing ≈ $%s (p90 ≈ $%s). Across %s quarters with exercise-level premiums.",
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

    fb_prep <- reactive({
      prep_feedback_scenario(
        store,
        renewal_shift = as.numeric(input$fb_shift),
        category = input$fb_cat,
        horizon_months = 36L
      )
    })

    output$feedback <- renderEcharts4r({
      prep <- fb_prep()
      if (is.null(prep)) return(NULL)
      chart_feedback_scenario(prep)
    })

    output$fb_note <- shiny::renderText({
      prep <- fb_prep()
      if (is.null(prep)) return("")
      shift <- prep$renewal_shift
      direction <- if (shift > 0) "more renewals → less scrap → tighter future quota"
      else if (shift < 0) "fewer renewals → more scrap → looser future quota"
      else "no shift (scenario = baseline)"
      sprintf(
        "Cat %s · renewal shift %+.0f%% · %s. Horizon 12 quarters via seasonal-naive deregistrations → replacement quota.",
        prep$category, 100 * shift, direction
      )
    })
  })
}
