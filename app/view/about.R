box::use(
  shiny[NS, moduleServer, div, tags],
  bslib[layout_columns, card, card_header, card_body],
)

#' What is this startup / product
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$section(
      class = "about-hero",
      tags$p(class = "eyebrow", "Product"),
      tags$h1("COE Intelligence"),
      tags$p(
        class = "lede lede--wide",
        "A supply desk for Singapore dealers, parallel importers, and fleet operators — ",
        "the people who buy COEs continuously and carry real money risk when premiums move."
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        class = "desk-card",
        card_header("The pain"),
        card_body(
          tags$ul(
            class = "about-list",
            tags$li("Dealers lock package prices, then bid later — an unhedged short on COE."),
            tags$li("Fleets replace cohorts over years; shifting two quarters can move seven figures."),
            tags$li("Public tools answer “should I bid this round?” for one buyer. We answer planning questions.")
          )
        )
      ),
      card(
        class = "desk-card",
        card_header("What we ship"),
        card_body(
          tags$ul(
            class = "about-list",
            tags$li("Forecast of Category A/B deregistrations with a clear baseline comparison."),
            tags$li("Quota via the published replacement identity (exits → supply)."),
            tags$li("Exposure calculator: your book × last premium + stress — not a premium prophecy.")
          )
        )
      )
    ),
    div(
      class = "about-promise",
      tags$h3("Honesty clause"),
      tags$p(
        "We forecast ", tags$strong("supply"), ", which is largely mechanical. ",
        "We do ", tags$strong("not"), " claim to forecast premiums. Demand, credit, and policy still move the print. ",
        "That limit is intentional — and stated in the product."
      )
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      div(
        class = "about-stat",
        tags$span(class = "nx-kicker", "Data"),
        tags$p("Official LTA / SingStat / DataMall only")
      ),
      div(
        class = "about-stat",
        tags$span(class = "nx-kicker", "Course"),
        tags$p("DBA3702 · NUS Business Analytics")
      ),
      div(
        class = "about-stat",
        tags$span(class = "nx-kicker", "Contrast"),
        tags$p("Built for continuous exposure, not one retail bid")
      )
    ),
    tags$p(
      class = "footnote",
      "Full brief: Project Proposal.md in the repo."
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    invisible(store)
  })
}
