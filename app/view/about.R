box::use(
  shiny[NS, moduleServer, div, tags],
  bslib[layout_columns, card, card_header, card_body],
)

#' Startup / product brief
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$section(
      class = "about-hero",
      tags$p(class = "eyebrow", "About"),
      tags$h1("COE Intelligence"),
      tags$p(
        class = "lede lede--wide",
        "A supply desk for dealers, parallel importers, and fleets who buy ",
        "Certificates of Entitlement every month — and carry real money risk ",
        "when the market tightens."
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        class = "desk-card",
        card_header("The problem"),
        card_body(
          tags$ul(
            class = "about-list",
            tags$li("You lock a COE-inclusive price today and bid later — an unhedged short."),
            tags$li("Fleet replacement cycles are multi-year; two quarters of timing can move seven figures."),
            tags$li("Retail tools answer one bid. You need a planning horizon and a dollar figure.")
          )
        )
      ),
      card(
        class = "desk-card",
        card_header("What you get"),
        card_body(
          tags$ul(
            class = "about-list",
            tags$li("Exit forecast for Category A and B — ahead of the quota print."),
            tags$li("Quota outlook on the same scale as monthly tender releases."),
            tags$li("Exposure desk: convert a locked book into COE dollars under your stress.")
          )
        )
      )
    ),
    div(
      class = "about-promise",
      tags$h3("Our line in the sand"),
      tags$p(
        "We forecast ", tags$strong("supply"), ". We do ", tags$strong("not"),
        " forecast premiums. Demand, credit, and policy still move the print — ",
        "and we would rather say that than sell a tip."
      )
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      div(
        class = "about-stat",
        tags$span(class = "nx-kicker", "Data"),
        tags$p("Official LTA / SingStat only")
      ),
      div(
        class = "about-stat",
        tags$span(class = "nx-kicker", "Market"),
        tags$p("Singapore dealer & fleet")
      ),
      div(
        class = "about-stat",
        tags$span(class = "nx-kicker", "Focus"),
        tags$p("Continuous COE exposure")
      )
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    invisible(store)
  })
}
