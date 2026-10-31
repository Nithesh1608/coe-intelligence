box::use(
  shiny[NS, moduleServer, div, tags, selectInput, renderText, renderUI, uiOutput, reactive, req],
  bslib[card, card_header, card_body],
  echarts4r[renderEcharts4r, echarts4rOutput],
)
box::use(
  app/logic/viewdata/proposal_viz[prep_quota_fan, chart_quota_fan, forward_quarters],
  app/logic/viewdata/pipeline[quarter_label],
)

fmt_int <- function(x) format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)

LIVE_MODEL <- "core_forecast"

#' Home - single supply outlook chart (released history + forward fan)
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
        tags$span(class = "nx-kicker", "Typical error"),
        div(class = "nx-num", shiny::textOutput(ns("kpi_mape"), inline = TRUE)),
        tags$span(class = "nx-hint", "MAPE vs released quota")
      ),
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "What you get"),
        div(class = "nx-num nx-num--sm", "A  Range"),
        tags$span(class = "nx-hint", "Central path plus bands")
      )
    ),
    tags$section(
      class = "viz-explain",
      tags$span(class = "nx-kicker", "Supply outlook"),
      tags$h2("Released certificates and the supply outlook")
    ),
    card(
      class = "desk-card",
      full_screen = TRUE,
      card_header("Certificate supply"),
      card_body(
        class = "chart-frame chart-frame--tall",
        echarts4rOutput(ns("supply"), height = "440px", width = "100%")
      )
    ),
    card(
      class = "desk-card",
      card_header(uiOutput(ns("qtr_header"))),
      card_body(uiOutput(ns("qtr_table")))
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
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

    supply_prep <- reactive({
      tryCatch(
        prep_quota_fan(store, category = cat_code(), horizon_months = 36L, history_months = 48L),
        error = function(e) {
          warning("Supply prep failed: ", conditionMessage(e))
          NULL
        }
      )
    })

    output$supply <- renderEcharts4r({
      prep <- supply_prep()
      req(!is.null(prep), !is.null(prep$df), nrow(prep$df) > 0)
      tryCatch(
        chart_quota_fan(prep),
        error = function(e) {
          warning("Supply chart failed: ", conditionMessage(e))
          NULL
        }
      )
    })

    output$qtr_header <- renderUI({
      prep <- supply_prep()
      when <- if (!is.null(prep) && length(prep$last_published) && is.finite(prep$last_published)) {
        format(as.Date(prep$last_published), "%b %Y")
      } else {
        "latest release"
      }
      paste0("Quarterly outlook - after ", when)
    })

    output$qtr_table <- renderUI({
      q <- tryCatch(forward_quarters(store, cat_code(), n = 12L), error = function(e) NULL)
      if (is.null(q) || !nrow(q)) return(tags$p(class = "footnote", "No forward quarters available."))
      rows <- lapply(seq_len(nrow(q)), function(i) {
        tags$tr(
          tags$td(quarter_label(q$quarter[i])),
          tags$td(fmt_int(q$mid[i])),
          tags$td(paste0(fmt_int(q$lo80[i]), " - ", fmt_int(q$hi80[i])))
        )
      })
      tags$table(
        class = "desk-table",
        tags$thead(tags$tr(tags$th("Quarter"), tags$th("Central"), tags$th("80% band"))),
        tags$tbody(rows)
      )
    })
  })
}
