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
      "Supply visibility for dealers and fleets who buy Certificates of Entitlement continuously — ",
      "Category A and Category B — and carry exposure when they lock package prices."
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        class = "desk-card",
        card_header("Problem"),
        card_body(
          tags$p(
            "Can COE supply be forecast far enough ahead, with honest uncertainty, ",
            "to price and time real financial exposure?"
          )
        )
      ),
      card(
        class = "desk-card",
        card_header("What we ship"),
        card_body(
          tags$p(
            "Category A and Category B supply forecasts, then Your risk — where predicted quota ",
            "sets how hard to stress a locked order book. Not a premium tip. Not Category E."
          )
        )
      )
    ),
    div(
      class = "about-promise",
      tags$h3("Honesty"),
      tags$p(
        "We forecast ", tags$strong("supply"), " and quantify ", tags$strong("exposure"),
        " under stress. We do not claim to forecast premiums."
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
