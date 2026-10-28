box::use(
  shiny[NS, moduleServer, div, tags],
  bslib[layout_columns, card, card_header, card_body],
)

#' Technical notes — keep off the Home pitch
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$p(class = "eyebrow", "How it works"),
    tags$h2("Method notes"),
    tags$p(
      class = "lede",
      "Product question from the proposal: can COE supply be forecast far enough ahead ",
      "for a dealer or fleet to price and time real financial exposure?"
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        class = "desk-card gloss",
        card_header("Core supply forecast"),
        card_body(
          tags$p("Elastic net on Category A and Category B monthly exits, then mapped to tender-scale quota.")
        )
      ),
      card(
        class = "desk-card gloss",
        card_header("Momentum supply forecast"),
        card_body(
          tags$p("Same family, plus year-on-year due-stock and short-run exit acceleration.")
        )
      )
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        class = "desk-card gloss",
        card_header("Fit"),
        card_body(tags$p("Through August 2024."))
      ),
      card(
        class = "desk-card gloss",
        card_header("Prove"),
        card_body(tags$p("September 2024 onward only."))
      ),
      card(
        class = "desk-card gloss",
        card_header("Compare"),
        card_body(tags$p("Simple twelve-month average of exits."))
      )
    ),
    tags$p(
      class = "footnote",
      "We forecast supply. We do not forecast premiums. Exposure is priced under user stress on Your risk."
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    invisible(store)
  })
}
