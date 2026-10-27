box::use(
  shiny[NS, moduleServer, div, tags],
  bslib[layout_columns, card, card_header, card_body],
)

#' Technical page — separate from the front story
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$p(class = "eyebrow", "How it works · technical"),
    tags$h2("Detail for people who want the method"),
    tags$p(
      class = "lede lede--wide",
      "The front pages stay in plain language. This page is the engineering note: ",
      "what we fit, how we prove it, and what the two forecasts are."
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        class = "desk-card gloss",
        card_header("Core supply forecast"),
        card_body(
          tags$p(
            "Regularised regression (elastic net) on Category A and Category B monthly exits."
          ),
          tags$p(
            "Inputs known before the month: due stock near ten-year expiry, recent exits, ",
            "five-year renewals, prevailing quota premium, and the May 2022 category regime flag."
          )
        )
      ),
      card(
        class = "desk-card gloss",
        card_header("Momentum supply forecast"),
        card_body(
          tags$p(
            "Same elastic-net family, plus momentum features: year-on-year change in due stock, ",
            "short-run exit acceleration."
          ),
          tags$p(
            "Same locked test window. Shown beside Core so the audience can see both tracks."
          )
        )
      )
    ),
    tags$h3(class = "section-title", "Proof rules"),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        class = "desk-card gloss",
        card_header("Fit window"),
        card_body(
          tags$p("History through August 2024 only.")
        )
      ),
      card(
        class = "desk-card gloss",
        card_header("Test window"),
        card_body(
          tags$p("September 2024 onward — never used to choose the method.")
        )
      ),
      card(
        class = "desk-card gloss",
        card_header("Comparison"),
        card_body(
          tags$p("Simple average of the last twelve months of exits — a common desk rule of thumb.")
        )
      )
    ),
    tags$h3(class = "section-title", "Quota map"),
    tags$p(
      class = "lede lede--wide",
      "Monthly published tender quotas do not sit on the same scale as the textbook ",
      "“twenty-five percent of annual exits” identity. We map trailing twelve-month exits ",
      "to monthly certificate volume with a category factor fit on pre-test history only, ",
      "then apply that map to forecasted exits."
    ),
    tags$h3(class = "section-title", "What we refuse to claim"),
    tags$p(
      class = "lede lede--wide",
      "We do not forecast the next Certificate of Entitlement premium. ",
      "Demand, credit, and policy still move the price. ",
      "Our product is supply timing and the dollar exposure on a locked order book."
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    invisible(store)
  })
}
