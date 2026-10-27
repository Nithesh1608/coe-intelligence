box::use(
  shiny[NS, moduleServer, div, tags],
  bslib[layout_columns, card, card_header, card_body],
)

#' Single production engine — startup product tone
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$p(class = "eyebrow", "Engine"),
    tags$h2("One supply engine. Two outputs."),
    tags$p(
      class = "lede lede--wide",
      "COE Intelligence forecasts how many Category A and B cars leave the fleet, ",
      "then turns that into a monthly quota outlook dealers can put next to real LTA releases. ",
      "Premiums are not forecast — they stay a stress you apply on the Exposure desk."
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        class = "desk-card gloss",
        card_header("1 · Exit forecast"),
        card_body(
          tags$p(
            tags$strong("Monthly deregistrations"),
            " for Cat A and Cat B — the flow that largely sets future supply."
          ),
          tags$p(
            "Built from due stock, recent exits, renewals, and the post-2022 category regime. ",
            "Validated on held-out months from September 2024 onward."
          )
        )
      ),
      card(
        class = "desk-card gloss",
        card_header("2 · Quota outlook"),
        card_body(
          tags$p(
            tags$strong("Monthly tender-scale outlook"),
            " from trailing exits — matched to the size of certificates LTA actually puts up."
          ),
          tags$p(
            "Not a claim to own LTA’s internal rule. An exit-driven map so supply and releases ",
            "can be read on the same chart."
          )
        )
      )
    ),
    tags$h3(class = "section-title", "What goes into the exit forecast"),
    tags$p(
      class = "lede lede--wide",
      "Only information known before the month starts."
    ),
    div(
      class = "feat-grid",
      div(class = "feat-chip", tags$strong("10-year due stock"), " Vehicles approaching first COE expiry."),
      div(class = "feat-chip", tags$strong("Near-term due stock"), " Later expiries and near-window cohorts."),
      div(class = "feat-chip", tags$strong("Recent exits"), " Last year and short lags of deregistrations."),
      div(class = "feat-chip", tags$strong("5-year renewals"), " Revalidations that push exits later."),
      div(class = "feat-chip", tags$strong("Prevailing Quota Premium"), " Price level linked to renewal behaviour."),
      div(class = "feat-chip", tags$strong("Post-May 2022 regime"), " A/B redefinition around EV power.")
    ),
    tags$h3(class = "section-title", "How we know it works"),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        class = "desk-card gloss",
        card_header("Fit vs prove"),
        card_body(
          tags$p(tags$strong("Fit"), " on history through August 2024."),
          tags$p(tags$strong("Prove"), " only on September 2024 onward — never used to tune.")
        )
      ),
      card(
        class = "desk-card gloss",
        card_header("Beat the rule of thumb"),
        card_body(
          tags$p("Compared against a trailing twelve-month average of exits."),
          tags$p("That is what many desks already use. We have to beat it.")
        )
      ),
      card(
        class = "desk-card gloss",
        card_header("Where we still miss"),
        card_body(
          tags$p("The sharpest 2026 exit spikes still run hotter than the forecast."),
          tags$p("We under-shoot less than the rule of thumb — and we show the miss on the Desk.")
        )
      )
    ),
    tags$h3(class = "section-title", "What we refuse to sell"),
    tags$p(
      class = "lede lede--wide",
      "We do not forecast the next COE premium. Demand, credit, and policy still move the print. ",
      "Our job is supply timing and the dollar exposure on a locked book — not a price tip."
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    invisible(store)
  })
}
