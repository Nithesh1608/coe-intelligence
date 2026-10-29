box::use(
  shiny[NS, moduleServer, div, tags, selectInput, renderUI, uiOutput],
  bslib[card, card_header, card_body],
  echarts4r[renderEcharts4r, echarts4rOutput],
)
box::use(
  app/logic/viewdata/charts[exit_track_chart],
)

LIVE_MODEL <- "core_forecast"

fmt_int <- function(x) {
  format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
}

#' Results — deregistrations behind the quota
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$p(class = "eyebrow", "Results"),
    tags$h2("How supply is built"),
    tags$p(
      class = "lede",
      "Quota follows deregistrations. Live model is an elastic net; pick a category."
    ),
    div(
      class = "desk-controls desk-controls--row",
      selectInput(
        ns("cat"),
        "Category",
        choices = c("Category A" = "A", "Category B" = "B"),
        selected = "A",
        width = "180px"
      )
    ),
    tags$p(
      class = "footnote",
      "Predicts monthly deregistrations from due-stock and recent flow features, then maps to quota."
    ),
    uiOutput(ns("example_grid")),
    card(
      class = "desk-card",
      full_screen = TRUE,
      card_header("Deregistrations each month"),
      card_body(
        class = "chart-frame chart-frame--tall",
        echarts4rOutput(ns("exits"), height = "400px")
      )
    ),
    tags$p(
      class = "footnote",
      "White = actual deregistrations. Orange = elastic net. Grey dashed = 12-month average."
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    live_label <- store$meta$model_labels[[LIVE_MODEL]]
    if (is.null(live_label) || is.na(live_label)) live_label <- "Elastic net"

    output$example_grid <- renderUI({
      dm <- store$two_monthly
      if (is.null(dm)) return(NULL)
      months <- as.Date(c(
        "2024-09-01", "2025-01-01", "2025-05-01",
        "2025-08-01", "2026-01-01", "2026-05-01"
      ))
      cards <- lapply(months, function(m) {
        f <- dm[dm$category == input$cat & dm$model == LIVE_MODEL & dm$month == m, ]
        s <- dm[dm$category == input$cat & dm$model == "simple_average" & dm$month == m, ]
        if (!nrow(f)) return(NULL)
        better <- abs(f$predicted[[1]] - f$actual[[1]]) < abs(s$predicted[[1]] - s$actual[[1]])
        div(
          class = paste("ex-tile", if (better) "ex-tile--win" else "ex-tile--miss"),
          tags$span(class = "ex-month", strftime(as.Date(m), "%b %Y")),
          tags$div(class = "ex-row", tags$span("Actual"), tags$strong(fmt_int(f$actual[[1]]))),
          tags$div(
            class = "ex-row ex-row--accent",
            tags$span("Forecast"),
            tags$strong(fmt_int(f$predicted[[1]]))
          ),
          tags$div(
            class = "ex-row muted",
            tags$span("Average"),
            tags$strong(fmt_int(s$predicted[[1]]))
          )
        )
      })
      div(class = "ex-grid", cards)
    })

    output$exits <- renderEcharts4r({
      dm <- store$two_monthly
      f <- dm[dm$category == input$cat & dm$model == LIVE_MODEL, ]
      s <- dm[dm$category == input$cat & dm$model == "simple_average", ]
      df <- data.frame(
        month = f$month,
        actual = f$actual,
        forecast = f$predicted,
        simple = s$predicted[match(f$month, s$month)],
        stringsAsFactors = FALSE
      )
      exit_track_chart(df, live_label)
    })
  })
}
