box::use(
  shiny[NS, moduleServer, div, tags, selectInput, reactive, renderText],
  bslib[card, card_header, card_body],
  echarts4r[renderEcharts4r, echarts4rOutput],
)
box::use(
  app/logic/viewdata/charts[quota_track_chart],
)

#' Proposal view 1 — Supply pipeline
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$section(
      class = "desk-hero desk-hero--pitch",
      div(
        class = "desk-hero__copy",
        tags$p(class = "eyebrow", "Supply pipeline"),
        tags$h1("What is my COE cost exposure, and how do I time against it?"),
        tags$p(
          class = "lede lede--wide",
          "We forecast Category A and Category B supply — certificates coming to market — ",
          "so dealers and fleets can plan. We do not forecast premiums."
        )
      ),
      div(
        class = "desk-controls",
        selectInput(
          ns("model"),
          "Forecast",
          choices = c(
            "Core supply forecast" = "core_forecast",
            "Momentum supply forecast" = "momentum_forecast"
          ),
          selected = "core_forecast",
          width = "100%"
        ),
        selectInput(
          ns("cat"),
          "Category",
          choices = c("Category A" = "A", "Category B" = "B"),
          selected = "A",
          width = "100%"
        )
      )
    ),
    div(
      class = "nx-metrics nx-metrics--3",
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "Typical miss versus released quota"),
        div(class = "nx-num", shiny::textOutput(ns("kpi_mape"), inline = TRUE)),
        tags$span(class = "nx-hint", "Held-out backtest · selected forecast")
      ),
      div(
        class = "nx-metric nx-metric--accent",
        tags$span(class = "nx-kicker", "We forecast"),
        div(class = "nx-num nx-num--sm", "Supply"),
        tags$span(class = "nx-hint", "Quota driven largely by exits")
      ),
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "We do not forecast"),
        div(class = "nx-num nx-num--sm", "Premiums"),
        tags$span(class = "nx-hint", "Stated in the proposal · kept here")
      )
    ),
    card(
      class = "desk-card",
      full_screen = TRUE,
      card_header("Certificates released versus supply forecast"),
      card_body(
        class = "chart-frame chart-frame--tall",
        echarts4rOutput(ns("quota"), height = "420px")
      )
    ),
    tags$p(
      class = "footnote",
      "White = certificates released. Orange = selected forecast. Grey dashed = simple twelve-month average. ",
      "Open Exposure to turn a forward book into dollars and shift timing by quarters."
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    model_label <- reactive({
      if (identical(input$model, "momentum_forecast")) {
        "Momentum supply forecast"
      } else {
        "Core supply forecast"
      }
    })

    output$kpi_mape <- renderText({
      sc <- store$two_quota_scores
      if (is.null(sc)) return("—")
      row <- sc[sc$model == input$model & sc$category == input$cat, ]
      if (!nrow(row)) return("—")
      sprintf("%.0f%%", 100 * row$MAPE[[1]])
    })

    output$quota <- renderEcharts4r({
      q <- store$two_quota
      if (is.null(q)) return(NULL)
      one <- q[q$model == input$model & q$category == input$cat, ]
      base <- q[q$model == "simple_average" & q$category == input$cat, ]
      one <- one[order(one$month), ]
      df <- data.frame(
        month = one$month,
        published = one$published,
        outlook = one$outlook,
        baseline = base$outlook[match(one$month, base$month)],
        stringsAsFactors = FALSE
      )
      quota_track_chart(df, model_label())
    })
  })
}
