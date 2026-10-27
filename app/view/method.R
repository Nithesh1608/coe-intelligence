box::use(
  shiny[NS, moduleServer, div, tags],
  bslib[layout_columns, card, card_header, card_body],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$p(class = "eyebrow", "How it works"),
    tags$h2("Method notes"),
    tags$p(
      class = "lede",
      "Quota is driven largely by deregistrations; deregistrations are driven largely by older registration cohorts. ",
      "We forecast Category A and Category B exits, map them to tender-scale quota, and backtest against released certificates."
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        class = "desk-card gloss",
        card_header("Core supply forecast"),
        card_body(tags$p("Elastic net on monthly exits, then mapped to monthly certificate volume."))
      ),
      card(
        class = "desk-card gloss",
        card_header("Momentum supply forecast"),
        card_body(tags$p("Same family, plus year-on-year due-stock and short-run exit acceleration."))
      )
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(class = "desk-card gloss", card_header("Fit"), card_body(tags$p("Through August 2024."))),
      card(class = "desk-card gloss", card_header("Prove"), card_body(tags$p("September 2024 onward only."))),
      card(class = "desk-card gloss", card_header("Baseline"), card_body(tags$p("Simple twelve-month average.")))
    ),
    tags$p(
      class = "footnote",
      "Compared with quota.sg: we serve continuous dealer or fleet exposure over a longer planning horizon, ",
      "with an exposure calculator — not a retail Buy / Wait signal."
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    invisible(store)
  })
}
