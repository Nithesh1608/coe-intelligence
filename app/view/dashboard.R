box::use(
  shiny[NS, moduleServer, div, tags, selectInput, renderText, reactive],
  bslib[card, card_header, card_body],
  echarts4r[renderEcharts4r, echarts4rOutput],
)
box::use(
  app/logic/viewdata/charts[quota_track_chart],
  app/logic/viewdata/proposal_viz[prep_quota_fan, chart_quota_fan],
)

LIVE_MODEL <- "core_forecast"

#' Home - supply forecast vs released quota (+ fan chart)
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$section(
      class = "desk-hero desk-hero--pitch",
      div(
        class = "desk-hero__copy",
        tags$p(class = "eyebrow", "COE Intelligence"),
        tags$h1("Forecast COE supply. Price the risk."),
        tags$p(
          class = "lede lede--wide",
          "Built for dealers and fleets that buy certificates every month. We project Category A and B quota ",
          "so you can time purchases and size exposure - instead of guessing the next premium."
        )
      ),
      div(
        class = "desk-controls",
        tags$p(
          class = "footnote",
          style = "margin:0 0 0.6rem;",
          "Live model: elastic net on due-stock and recent flow features, mapped to quota."
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
        tags$span(class = "nx-hint", "Out-of-sample months | elastic net")
      ),
      div(
        class = "nx-metric nx-metric--accent",
        tags$span(class = "nx-kicker", "What we forecast"),
        div(class = "nx-num nx-num--sm", "Supply"),
        tags$span(class = "nx-hint", "Certificates coming to market")
      ),
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "What we do not forecast"),
        div(class = "nx-num nx-num--sm", "Premiums"),
        tags$span(class = "nx-hint", "Stress them in Your risk")
      )
    ),
    card(
      class = "desk-card",
      full_screen = TRUE,
      card_header("Certificates released versus forecast"),
      card_body(
        class = "chart-frame chart-frame--tall",
        echarts4rOutput(ns("quota"), height = "420px")
      )
    ),
    tags$p(
      class = "footnote",
      "White = certificates released. Orange = elastic net. Grey dashed = 12-month average."
    ),

    tags$section(
      class = "viz-explain",
      tags$span(class = "nx-kicker", "Supply outlook"),
      tags$h2("Quota path with a real uncertainty band"),
      tags$p(
        class = "lede lede--wide",
        "A single number is not enough to price a forward book. The fan shows our central supply path ",
        "and a band that widens further out - so you can plan against a range."
      ),
      tags$ul(
        class = "viz-explain__list",
        tags$li(tags$strong("White - "), "certificates actually released."),
        tags$li(tags$strong("Orange - "), "central projected quota from the live supply model."),
        tags$li(tags$strong("Band - "), "interval that grows with horizon, scaled from out-of-sample error.")
      )
    ),
    card(
      class = "desk-card",
      full_screen = TRUE,
      card_header("Projected quota fan (next ~2 years)"),
      card_body(
        class = "chart-frame chart-frame--tall",
        echarts4rOutput(ns("fan"), height = "420px")
      )
    ),
    tags$p(class = "footnote", shiny::textOutput(ns("fan_note"), inline = TRUE))
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    live_label <- store$meta$model_labels[[LIVE_MODEL]]
    if (is.null(live_label) || is.na(live_label)) live_label <- "Elastic net"

    output$kpi_mape <- renderText({
      sc <- store$two_quota_scores
      if (is.null(sc)) return("-")
      row <- sc[sc$model == LIVE_MODEL & sc$category == input$cat, ]
      if (!nrow(row)) return("-")
      sprintf("%.0f%%", 100 * row$MAPE[[1]])
    })

    output$quota <- renderEcharts4r({
      q <- store$two_quota
      if (is.null(q)) return(NULL)
      one <- q[q$model == LIVE_MODEL & q$category == input$cat, ]
      base <- q[q$model == "simple_average" & q$category == input$cat, ]
      one <- one[order(one$month), ]
      df <- data.frame(
        month = one$month,
        published = one$published,
        outlook = one$outlook,
        baseline = base$outlook[match(one$month, base$month)],
        stringsAsFactors = FALSE
      )
      quota_track_chart(df, live_label)
    })

    fan_prep <- reactive({
      prep_quota_fan(store, category = input$cat, horizon_months = 24L)
    })

    output$fan <- renderEcharts4r({
      prep <- fan_prep()
      if (is.null(prep)) return(NULL)
      chart_quota_fan(prep)
    })

    output$fan_note <- renderText({
      prep <- fan_prep()
      if (is.null(prep)) return("Fan chart unavailable - missing quota outlook.")
      sprintf(
        "Category %s | band uses out-of-sample MAPE ~ %.0f%%. As of %s. We forecast supply, not premiums.",
        prep$category, 100 * prep$mape, format(prep$as_of, "%b %Y")
      )
    })

    shiny::outputOptions(output, "quota", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "fan", suspendWhenHidden = FALSE)
  })
}
