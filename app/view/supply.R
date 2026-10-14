box::use(
  shiny[NS, moduleServer, div, tags, sliderInput, selectInput],
  bslib[layout_sidebar, sidebar, card, card_header, card_body],
  echarts4r[renderEcharts4r, echarts4rOutput],
)
box::use(
  app/logic/viewdata/pipeline[naive_path, quota_path, year_end_pop_c],
  app/logic/viewdata/charts[fan_chart, bar_chart],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Pipeline",
      selectInput(
        ns("category"),
        "Category",
        choices = c("A", "B", "C", "D"),
        selected = "A"
      ),
      sliderInput(
        ns("horizon"),
        "Horizon (months)",
        min = 12,
        max = 36,
        value = 24,
        step = 3
      ),
      sliderInput(
        ns("renewal"),
        "Extra renewals (fraction of exits withheld)",
        min = 0,
        max = 0.25,
        value = 0,
        step = 0.01
      ),
      tags$p(
        class = "footnote",
        "Bands are ±1.96 year-on-year residual scale, widening with horizon. ",
        "Quota uses 25% of trailing 12-month exits, 10% of A/B/C to E, ",
        "and 0.25% annual growth on C. Adjustments (taxis, injections) are not in the CSVs."
      )
    ),
    div(
      class = "page-stack",
      tags$h2("Supply pipeline"),
      tags$p(
        class = "lede",
        "Seasonal naive on deregistrations, then the published replacement identity. ",
        "Not a fitted survival model."
      ),
      card(
        full_screen = TRUE,
        card_header("Deregistrations — history and illustrated path"),
        card_body(echarts4rOutput(ns("fan"), height = "400px"))
      ),
      card(
        full_screen = TRUE,
        card_header("Implied quarterly replacement quota"),
        card_body(echarts4rOutput(ns("quota"), height = "360px"))
      )
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    output$fan <- renderEcharts4r({
      cat <- input$category
      fwd <- naive_path(store, cat, input$horizon, input$renewal)
      hist <- store$panel
      hist <- hist[hist$category == cat & !is.na(hist$deregistrations), ]
      hist <- hist[hist$month >= as.Date("2016-01-01"), ]
      hist <- data.frame(
        month = hist$month,
        mid = hist$deregistrations,
        stringsAsFactors = FALSE
      )
      fan_chart(hist, fwd)
    })

    output$quota <- renderEcharts4r({
      cats <- c("A", "B", "C", "D")
      paths <- lapply(cats, function(cat) {
        naive_path(store, cat, input$horizon, input$renewal)
      })
      names(paths) <- cats
      q <- quota_path(store, paths, year_end_pop_c(store))
      q$quarter <- format(as.Date(q$quarter), "%Y-%m")
      one <- q[q$category == input$category, ]
      one <- stats::aggregate(quota ~ quarter, one, sum)
      bar_chart(one, "quarter", "quota")
    })
  })
}
