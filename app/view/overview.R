box::use(
  shiny[NS, moduleServer, div, tags, renderText],
  bslib[layout_columns, card, card_header, card_body],
  echarts4r[renderEcharts4r, echarts4rOutput],
)
box::use(
  app/logic/viewdata/pipeline[quarter_start, add_months],
  app/logic/viewdata/charts[spread_chart, bar_chart],
  app/logic/viewdata/premiums[premium_wide],
)

fmt_int <- function(x) {
  format(round(x), big.mark = ",", scientific = FALSE)
}

fmt_sgd <- function(x) {
  paste0("$", format(round(x), big.mark = ",", scientific = FALSE))
}

trailing_a <- function(panel) {
  a <- panel[panel$category == "A" & !is.na(panel$deregistrations), ]
  end <- max(a$month)
  start <- add_months(end, -11L)
  sum(a$deregistrations[a$month >= start], na.rm = TRUE)
}

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack",
    tags$section(
      class = "hero",
      div(
        tags$p(class = "eyebrow", "v0 · dealer desk"),
        tags$h1("Price the pipeline. Not the rumour."),
        tags$p(
          class = "lede",
          "Official LTA series only. Quota is mostly last year's exits. ",
          "Premiums stay a stress you apply — never a claimed forecast."
        )
      ),
      div(
        class = "hero-aside",
        "Last Cat A print",
        tags$strong(shiny::textOutput(ns("kpi_prem"), inline = TRUE)),
        "Mechanical supply · 8–12 quarter horizon · no survival fiction"
      )
    ),
    div(
      class = "nx-metrics",
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "A − B spread"),
        div(class = "nx-num", shiny::textOutput(ns("kpi_spread"), inline = TRUE)),
        tags$span(class = "nx-hint", "Substitution pressure")
      ),
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "Cat A exits / 12m"),
        div(class = "nx-num", shiny::textOutput(ns("kpi_dereg"), inline = TRUE)),
        tags$span(class = "nx-hint", "Replacement fuel")
      ),
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "Bid / quota"),
        div(class = "nx-num", shiny::textOutput(ns("kpi_ratio"), inline = TRUE)),
        tags$span(class = "nx-hint", "Round tightness")
      ),
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "Model status"),
        div(class = "nx-num", "Naive"),
        tags$span(class = "nx-hint", "GAM / ARIMA not fit")
      )
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(
        full_screen = TRUE,
        card_header("Premium path — Categories A, B, E"),
        card_body(class = "chart-frame", echarts4rOutput(ns("spread"), height = "340px"))
      ),
      card(
        full_screen = TRUE,
        card_header("Cat A swing inside each quarter"),
        card_body(class = "chart-frame", echarts4rOutput(ns("swing"), height = "340px"))
      )
    ),
    tags$p(
      class = "footnote",
      "Published LTA / SingStat values only. Supply tab uses a seasonal-naive ",
      "illustration until a fitted model is saved."
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    bidding <- store$bidding
    panel <- store$panel
    a <- bidding[bidding$category == "A", ]
    b <- bidding[bidding$category == "B", ]
    last_a <- a[nrow(a), ]
    last_b <- b[nrow(b), ]
    last_ratio <- last_a$bids_received / last_a$quota

    output$kpi_prem <- renderText(fmt_sgd(last_a$premium))
    output$kpi_spread <- renderText(fmt_sgd(last_a$premium - last_b$premium))
    output$kpi_dereg <- renderText(fmt_int(trailing_a(panel)))
    output$kpi_ratio <- renderText(sprintf("%.2f x", last_ratio))

    output$spread <- renderEcharts4r({
      spread_chart(premium_wide(bidding, from = as.Date("2018-01-01")))
    })

    output$swing <- renderEcharts4r({
      aa <- a
      aa$quarter <- quarter_start(aa$month)
      swing <- stats::aggregate(premium ~ quarter, aa, function(z) diff(range(z)))
      names(swing) <- c("quarter", "swing")
      swing$quarter <- format(as.Date(swing$quarter), "%Y-%m")
      swing <- utils::tail(swing, 24)
      bar_chart(swing, "quarter", "swing")
    })
  })
}
