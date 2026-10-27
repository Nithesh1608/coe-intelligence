box::use(
  shiny[NS, moduleServer, div, tags],
  bslib[layout_columns, card, card_header, card_body],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$p(class = "eyebrow", "About"),
    tags$h1("COE Intelligence"),
    tags$p(
      class = "lede lede--wide",
      "Built for dealers, parallel importers, and fleet operators who buy Certificates of Entitlement ",
      "continuously — not for a one-time retail bidder."
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        class = "desk-card",
        card_header("Problem statement"),
        card_body(
          tags$p(
            "Can COE supply be forecast far enough ahead, and with honest uncertainty, ",
            "for a dealer or fleet operator to price and time real financial exposure?"
          )
        )
      ),
      card(
        class = "desk-card",
        card_header("Question we answer"),
        card_body(
          tags$p("What is my COE cost exposure, and how do I time against it?")
        )
      )
    ),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      div(class = "about-stat", tags$span(class = "nx-kicker", "1"), tags$p("Supply pipeline")),
      div(class = "about-stat", tags$span(class = "nx-kicker", "2"), tags$p("Feedback")),
      div(class = "about-stat", tags$span(class = "nx-kicker", "3"), tags$p("Categories")),
      div(class = "about-stat", tags$span(class = "nx-kicker", "4"), tags$p("Exposure"))
    ),
    div(
      class = "about-promise",
      tags$h3("Scope and honesty"),
      tags$p(
        "We forecast ", tags$strong("supply"), ", which is largely mechanical, ",
        "and we quantify ", tags$strong("exposure"), " under uncertainty. ",
        "We do not claim to forecast premiums."
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
