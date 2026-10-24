box::use(
  shiny[NS, moduleServer, div, tags, renderUI, uiOutput],
  bslib[layout_columns, card, card_header, card_body],
)

#' Technical ideas in plain language
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$p(class = "eyebrow", "Method · simple words"),
    tags$h2("What we tried, and what we kept"),
    tags$p(
      class = "lede lede--wide",
      "This page is for markers and curious dealers. Same story as Forecast, with the names of the tools."
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        class = "desk-card gloss",
        card_header("Target"),
        card_body(
          tags$p(tags$strong("Monthly deregistrations"), " for Cat A and Cat B."),
          tags$p("How many cars in that category leave the fleet that month.")
        )
      ),
      card(
        class = "desk-card gloss",
        card_header("Baseline"),
        card_body(
          tags$p(tags$strong("Trailing 12-month average."), " Next month ≈ average of the last year."),
          tags$p("Simple. Often used in practice. Our model has to beat it.")
        )
      ),
      card(
        class = "desk-card gloss",
        card_header("Train / test"),
        card_body(
          tags$p(tags$strong("Train"), " on data up to Aug 2024."),
          tags$p(tags$strong("Test"), " only Sep 2024 onward — never used to pick the model.")
        )
      )
    ),
    tags$h3(class = "section-title", "Features we feed the model"),
    tags$p(
      class = "lede lede--wide",
      "Only things known before the month starts — no peeking at the answer."
    ),
    div(
      class = "feat-grid",
      div(class = "feat-chip", tags$strong("due10"), " Cars about to hit the 10-year COE expiry."),
      div(class = "feat-chip", tags$strong("due15 / due_near"), " Later expiry cohorts + near-term due stock."),
      div(class = "feat-chip", tags$strong("recent12 / lags"), " Recent exit history (last year, last few months)."),
      div(class = "feat-chip", tags$strong("reval5 lags"), " 5-year renewals — high renewals delay exits."),
      div(class = "feat-chip", tags$strong("lpqp"), " Prevailing Quota Premium — renewal feedback cue."),
      div(class = "feat-chip", tags$strong("post_2022"), " Flag after May 2022 A/B EV-power redefinition.")
    ),
    tags$h3(class = "section-title", "Models we compared"),
    div(
      class = "model-grid",
      div(
        class = "model-card",
        tags$h4("Ridge / Lasso / Elastic net"),
        tags$p(
          "Modern regularised regression. They shrink noisy coefficients so a small monthly ",
          "dataset does not overfit. Elastic net mixes ridge and lasso — our primary pick."
        )
      ),
      div(
        class = "model-card",
        tags$h4("OLS (due / lean / full)"),
        tags$p(
          "Ordinary linear regression with different feature sets. ",
          "Full set overfit; due-stock version stayed honest and made the shortlist."
        )
      ),
      div(
        class = "model-card",
        tags$h4("GAM + seasonal naive"),
        tags$p(
          "Smooth curves on due stock, and “same month last year.” ",
          "Useful checks — not the winners on the locked test."
        )
      )
    ),
    tags$h3(class = "section-title", "Cross-validation (in one sentence)"),
    tags$p(
      class = "lede lede--wide",
      "Inside the train window we walk forward in time: train on the past, predict the next six months, ",
      "repeat. That picks stable models before we ever touch 2024–26."
    ),
    tags$h3(class = "section-title", "Shortlist that beat the baseline on both A and B"),
    uiOutput(ns("shortlist")),
    tags$h3(class = "section-title", "Why training looked “fast”"),
    tags$p(
      class = "lede lede--wide",
      "We have a few hundred monthly rows and about a dozen features. ",
      "These are linear models with shrinkage — not image neural nets. ",
      "Seconds on a laptop is normal and correct for this problem size."
    ),
    tags$p(
      class = "footnote",
      "Artifacts live in data/processed/model_experiment/. Re-run: Rscript scripts/run_model_experiment.R"
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    output$shortlist <- renderUI({
      sl <- store$shortlist
      if (is.null(sl) || !nrow(sl)) {
        return(tags$p("Shortlist not loaded — run the experiment script."))
      }
      tiles <- lapply(seq_len(nrow(sl)), function(i) {
        row <- sl[i, ]
        div(
          class = paste("short-tile", if (i == 1) "short-tile--primary"),
          tags$span(class = "nx-kicker", if (i == 1) "Primary" else "Shortlist"),
          tags$p(class = "short-name", row$model),
          tags$p(
            class = "short-meta",
            sprintf(
              "Avg miss ~%.0f cars/mo (A %.0f · B %.0f)",
              row$MAE_mean, row$A, row$B
            )
          )
        )
      })
      div(class = "short-grid", tiles)
    })
  })
}
