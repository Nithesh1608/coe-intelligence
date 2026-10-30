box::use(
  shiny[NS, moduleServer, div, tags, selectInput, renderText, reactive, req],
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
        echarts4rOutput(ns("quota"), height = "400px", width = "100%")
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
        echarts4rOutput(ns("fan"), height = "400px", width = "100%")
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

    cat_code <- reactive({
      c <- input$cat
      if (is.null(c) || !nzchar(as.character(c)[[1]])) "A" else as.character(c)[[1]]
    })

    output$kpi_mape <- renderText({
      sc <- store$two_quota_scores
      if (is.null(sc) || !nrow(sc)) return("-")
      row <- sc[as.character(sc$model) == LIVE_MODEL & as.character(sc$category) == cat_code(), ]
      if (!nrow(row) || !is.finite(row$MAPE[[1]])) return("-")
      sprintf("%.0f%%", 100 * as.numeric(row$MAPE[[1]]))
    })

    output$quota <- renderEcharts4r({
      q <- store$two_quota
      req(!is.null(q), nrow(q) > 0)
      code <- cat_code()
      one <- q[as.character(q$model) == LIVE_MODEL & as.character(q$category) == code, , drop = FALSE]
      base <- q[as.character(q$model) == "simple_average" & as.character(q$category) == code, , drop = FALSE]
      req(nrow(one) > 0)
      one <- one[order(one$month), , drop = FALSE]
      df <- data.frame(
        month = one$month,
        published = as.numeric(one$published),
        outlook = as.numeric(one$outlook),
        baseline = as.numeric(base$outlook[match(one$month, base$month)]),
        stringsAsFactors = FALSE
      )
      df <- df[is.finite(df$published) | is.finite(df$outlook), , drop = FALSE]
      req(nrow(df) > 0)
      tryCatch(
        quota_track_chart(df, live_label),
        error = function(e) {
          warning("Home quota chart failed: ", conditionMessage(e))
          NULL
        }
      )
    })

    fan_prep <- reactive({
      tryCatch(
        prep_quota_fan(store, category = cat_code(), horizon_months = 24L),
        error = function(e) {
          warning("Fan prep failed: ", conditionMessage(e))
          NULL
        }
      )
    })

    output$fan <- renderEcharts4r({
      prep <- fan_prep()
      req(!is.null(prep), !is.null(prep$df), nrow(prep$df) > 0)
      tryCatch(
        chart_quota_fan(prep),
        error = function(e) {
          warning("Fan chart failed: ", conditionMessage(e))
          NULL
        }
      )
    })

    output$fan_note <- renderText({
      prep <- fan_prep()
      if (is.null(prep)) return("Fan chart unavailable - missing quota outlook.")
      sprintf(
        "Category %s | band uses out-of-sample MAPE ~ %.0f%%. As of %s. We forecast supply, not premiums.",
        prep$category, 100 * prep$mape, format(prep$as_of, "%b %Y")
      )
    })

    # Keep Home charts alive even when other nav panels are selected (Posit/bslib).
    shiny::outputOptions(output, "quota", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "fan", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "kpi_mape", suspendWhenHidden = FALSE)
  })
}
