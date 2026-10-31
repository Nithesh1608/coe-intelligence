box::use(
  shiny[NS, moduleServer, div, tags, selectInput, renderText, renderUI, uiOutput, reactive, req],
  bslib[card, card_header, card_body],
  echarts4r[renderEcharts4r, echarts4rOutput],
)
box::use(
  app/logic/viewdata/charts[quota_track_chart],
  app/logic/viewdata/proposal_viz[prep_quota_fan, chart_quota_fan, forward_quarters],
  app/logic/viewdata/pipeline[quarter_label],
)

fmt_int <- function(x) format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)

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
        tags$h1("Know your certificate supply three years out."),
        tags$p(
          class = "lede lede--wide",
          "For dealers and fleets that buy every month. See how many Category A and B certificates are ",
          "coming to market, plan purchase timing, and size exposure with a clear range - not a single guess."
        )
      ),
      div(
        class = "desk-controls",
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
        tags$span(class = "nx-kicker", "Planning horizon"),
        div(class = "nx-num nx-num--sm", "12 quarters"),
        tags$span(class = "nx-hint", "Forward certificate outlook")
      ),
      div(
        class = "nx-metric nx-metric--accent",
        tags$span(class = "nx-kicker", "Typical accuracy"),
        div(class = "nx-num", shiny::textOutput(ns("kpi_mape"), inline = TRUE)),
        tags$span(class = "nx-hint", "Versus released quota")
      ),
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "What you get"),
        div(class = "nx-num nx-num--sm", "A range"),
        tags$span(class = "nx-hint", "Central path plus bands")
      )
    ),
    card(
      class = "desk-card",
      full_screen = TRUE,
      card_header("Released certificates versus our outlook"),
      card_body(
        class = "chart-frame chart-frame--tall",
        echarts4rOutput(ns("quota"), height = "400px", width = "100%")
      )
    ),


    tags$section(
      class = "viz-explain",
      tags$span(class = "nx-kicker", "Forward outlook"),
      tags$h2("Certificate supply for the next twelve quarters")
    ),
    card(
      class = "desk-card",
      full_screen = TRUE,
      card_header("Projected quota"),
      card_body(
        class = "chart-frame chart-frame--tall",
        echarts4rOutput(ns("fan"), height = "400px", width = "100%")
      )
    ),

    tags$p(class = "footnote", shiny::textOutput(ns("fan_note"), inline = TRUE)),
    card(
      class = "desk-card",
      card_header("Quarterly outlook"),
      card_body(uiOutput(ns("qtr_table")))
    ),
    tags$p(
      class = "footnote",
      "Stress this path in Scenarios. Price a locked book in Your risk. Method detail lives in About."
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    live_label <- "Our outlook"

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
        prep_quota_fan(store, category = cat_code(), horizon_months = 36L),
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
      if (is.null(prep)) return("Outlook unavailable - rebuild the forward artefacts, then reload.")
      sprintf(
        "Category %s  |  outlook from %s  |  %d months ahead  |  released quota through %s.",
        prep$category, format(prep$as_of, "%b %Y"), prep$horizon, format(prep$last_published, "%b %Y")
      )
    })

    output$qtr_table <- renderUI({
      q <- tryCatch(forward_quarters(store, cat_code(), n = 12L), error = function(e) NULL)
      if (is.null(q) || !nrow(q)) return(tags$p(class = "footnote", "No forward quarters available."))
      rows <- lapply(seq_len(nrow(q)), function(i) {
        tags$tr(
          tags$td(quarter_label(q$quarter[i])),
          tags$td(fmt_int(q$mid[i])),
          tags$td(paste0(fmt_int(q$lo80[i]), " - ", fmt_int(q$hi80[i]))),
          tags$td(if (is.finite(q$published[i])) fmt_int(q$published[i]) else "-")
        )
      })
      tags$table(
        class = "table table-sm",
        style = "color:#fafafa;",
        tags$thead(tags$tr(tags$th("Quarter"), tags$th("Central"), tags$th("80% band"), tags$th("Published so far"))),
        tags$tbody(rows)
      )
    })

  })
}
