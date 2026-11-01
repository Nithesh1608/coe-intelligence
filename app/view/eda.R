box::use(
  shiny[NS, moduleServer, div, tags, selectInput, reactive, uiOutput, renderUI],
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
  app/logic/viewdata/gallery_echarts[
    prep_gallery,
    chart_g0_premium, chart_g1_topup, chart_g2_heatmap,
    chart_g4_owner, chart_g7_supply_price, chart_g5_bids, chart_g6_risk
  ],
)

`%||%` <- function(a, b) {
  if (is.null(a) || !length(a) || (length(a) == 1L && is.na(a))) b else a
}

viz_block <- function(num, kicker, title, body, bullets = list(), controls = NULL, charts) {
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

echart_card <- function(outputId, height = "520px", title = NULL, note = NULL) {
  card(
    class = "desk-card eda-chart-card",
    if (!is.null(title)) card_header(class = "eda-chart-card__title", title),
    if (!is.null(note)) tags$p(class = "eda-chart-card__note", note),
    card_body(
      class = "chart-frame eda-chart-card__frame",
      style = sprintf("height:%s;min-height:%s;", height, height),
      echarts4rOutput(outputId, height = height, width = "100%")
    )
  )
}

#' Market context and supply mechanics
#' @export
ui <- function(id) {
  ns <- NS(id)
  root <- tryCatch(project_root(), error = function(e) getwd())
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

    tags$hr(class = "eda-divider"),
    tags$p(class = "nx-kicker", "Deeper context"),
    tags$p(
      class = "lede lede--wide",
      "Start from the price backdrop, then the cars aging into certificates, how LTA builds quota, ",
      "whether supply eases premiums, how owners respond, why bidding pressure is a weak signal, ",
      "and finally the cost of waiting ? which leads into Your risk."
    ),
    div(
      class = "eda-spine",
      tags$span("Premium history"),
      tags$span(class = "eda-spine__dot", "->"),
      tags$span("Age wave"),
      tags$span(class = "eda-spine__dot", "->"),
      tags$span("Formula vs top-up"),
      tags$span(class = "eda-spine__dot", "->"),
      tags$span("Supply vs price"),
      tags$span(class = "eda-spine__dot", "->"),
      tags$span("Owner response"),
      tags$span(class = "eda-spine__dot", "->"),
      tags$span("Bidding"),
      tags$span(class = "eda-spine__dot", "->"),
      tags$span("Dealer risk")
    ),

    viz_block(
      4, "Premium backdrop",
      "Cat A and B premiums across every bidding round",
      paste(
        "Clearing prices move in long waves, not a flat trend. Shading marks how Cat A was defined;",
        "dotted lines are policy changes. Use this as context before you read supply mechanics below."
      ),
      charts = list(
        uiOutput(ns("g0_title")),
        echart_card(ns("g0"), height = "620px")
      )
    ),

    viz_block(
      5, "The wave on the road",
      "A large 2017-19 cohort reaches year 10 by 2029; a thinner cohort follows",
      paste(
        "Each row is age, each column a month. Bright bands move up the heatmap as cars age.",
        "Expect more certificates into 2029, then a tighter early-2030s window for fleets."
      ),
      charts = list(
        uiOutput(ns("g2_title")),
        uiOutput(ns("g2_note")),
        echart_card(ns("g2"), height = "640px")
      )
    ),

    viz_block(
      6, "Where supply comes from",
      "Formula quota versus LTA top-ups since 2023",
      paste(
        "Muted area is the 25% rule; orange is LTA top-up.",
        "Top-ups peaked and are thinning - recent supply growth is more structural."
      ),
      controls = selectInput(
        ns("g1_cat"), "Category",
        choices = c("Category A" = "A", "Category B" = "B"),
        selected = "A", width = "180px"
      ),
      charts = list(
        uiOutput(ns("g1_title")),
        echart_card(ns("g1"), height = "560px")
      )
    ),

    viz_block(
      7, "Supply and clearing prices",
      "When the quota rises, do premiums fall? Only partly",
      paste(
        "Each point is one quarter versus the same quarter a year earlier (percent changes).",
        "More certificates often go with softer premiums, but only partly. Price a locked book in Your risk."
      ),
      charts = list(
        uiOutput(ns("g7_title")),
        echart_card(ns("g7"), height = "540px")
      )
    ),

    viz_block(
      8, "How owners react to price",
      "Dear COEs slow early scrapping; dear PQP pushes more year-10 exits",
      paste(
        "Left: share of cars aged 1-9 scrapped vs premium. Right: share scrapped at year 10 vs renewal price.",
        "High prices can cut early exits and keep supply tight - a feedback loop, not a quick mean reversion."
      ),
      charts = list(
        uiOutput(ns("g4_title")),
        div(
          class = "eda-chart-pair",
          div(uiOutput(ns("g4a_title")), echart_card(ns("g4a"), height = "480px")),
          div(uiOutput(ns("g4b_title")), echart_card(ns("g4b"), height = "480px"))
        )
      )
    ),

    viz_block(
      9, "Bidding pressure vs price",
      "Bids per COE sits near the same level whether premiums are cheap or dear",
      paste(
        "Orange band = 10th-90th percentile of bids per COE in that premium band; white bar = median.",
        "Pressure does not warn you about the next price move."
      ),
      charts = list(
        uiOutput(ns("g5_title")),
        echart_card(ns("g5"), height = "580px")
      )
    ),

    viz_block(
      10, "Dealer risk between order and COE",
      "The longer the wait, the larger the 1-in-10 bad case",
      paste(
        "Cat A and Cat B side by side. Hills show how often the premium moved since May 2022,",
        "scaled to today's price. Price the open book in Your risk."
      ),
      charts = list(
        uiOutput(ns("g6_title")),
        div(
          class = "eda-chart-pair",
          echart_card(ns("g6a"), height = "520px"),
          echart_card(ns("g6b"), height = "520px")
        )
      )
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    root <- project_root()
    pm <- load_premium_moves(root)

    gallery <- reactive({
      tryCatch(prep_gallery(root), error = function(e) {
        warning("gallery prep failed: ", conditionMessage(e))
        NULL
      })
    })

    gallery_title <- function(key) {
      force(key)
      renderUI({
        g <- gallery()
        txt <- if (!is.null(g) && !is.null(g$titles[[key]])) g$titles[[key]] else NULL
        if (is.null(txt) || !nzchar(txt)) return(NULL)
        tags$h4(class = "eda-chart-card__title", txt)
      })
    }
    output$g0_title <- gallery_title("g0")
    output$g1_title <- gallery_title("g1")
    output$g2_title <- gallery_title("g2")
    output$g2_note <- renderUI({
      g <- gallery()
      txt <- if (!is.null(g)) g$titles$g2_note else NULL
      if (is.null(txt)) return(NULL)
      tags$p(class = "eda-chart-card__note", txt)
    })
    output$g4_title <- gallery_title("g4")
    output$g4a_title <- gallery_title("g4a")
    output$g4b_title <- gallery_title("g4b")
    output$g7_title <- gallery_title("g7")
    output$g5_title <- gallery_title("g5")
    output$g6_title <- gallery_title("g6")

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

    output$g0 <- renderEcharts4r({
      g <- gallery()
      if (is.null(g)) return(NULL)
      chart_g0_premium(g)
    })
    output$g1 <- renderEcharts4r({
      g <- gallery()
      if (is.null(g)) return(NULL)
      chart_g1_topup(g, category = input$g1_cat %||% "A")
    })
    output$g2 <- renderEcharts4r({
      g <- gallery()
      if (is.null(g)) return(NULL)
      chart_g2_heatmap(g)
    })
    output$g4a <- renderEcharts4r({
      g <- gallery()
      if (is.null(g)) return(NULL)
      chart_g4_owner(g, "early")
    })
    output$g4b <- renderEcharts4r({
      g <- gallery()
      if (is.null(g)) return(NULL)
      chart_g4_owner(g, "y10")
    })
    output$g7 <- renderEcharts4r({
      g <- gallery()
      if (is.null(g)) return(NULL)
      chart_g7_supply_price(g)
    })
    output$g5 <- renderEcharts4r({
      g <- gallery()
      if (is.null(g)) return(NULL)
      chart_g5_bids(g)
    })
    output$g6a <- renderEcharts4r({
      g <- gallery()
      if (is.null(g)) return(NULL)
      chart_g6_risk(g, category = "A")
    })
    output$g6b <- renderEcharts4r({
      g <- gallery()
      if (is.null(g)) return(NULL)
      chart_g6_risk(g, category = "B")
    })
  })
}
