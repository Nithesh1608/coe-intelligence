box::use(
  shiny[NS, moduleServer, div, tags, renderUI, uiOutput],
  bslib[layout_columns, card, card_header, card_body],
)

#' How the supply engine works — product tone
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$p(class = "eyebrow", "Method"),
    tags$h2("How the supply engine works"),
    tags$p(
      class = "lede lede--wide",
      "We estimate how many Category A and B cars leave the fleet each month, ",
      "then translate those exits into replacement quota using LTA’s published identity. ",
      "Everything below is designed to be auditable by a dealer ops or finance lead."
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        class = "desk-card gloss",
        card_header("What we forecast"),
        card_body(
          tags$p(tags$strong("Monthly deregistrations"), " for Cat A and Cat B."),
          tags$p("The flow that largely determines how many new COEs come back.")
        )
      ),
      card(
        class = "desk-card gloss",
        card_header("What we compare against"),
        card_body(
          tags$p(tags$strong("Trailing twelve-month average"), " of exits."),
          tags$p("A practical rule of thumb. The production model must beat it on held-out months.")
        )
      ),
      card(
        class = "desk-card gloss",
        card_header("Validation window"),
        card_body(
          tags$p(tags$strong("Fit"), " on history through August 2024."),
          tags$p(tags$strong("Score"), " only on September 2024 onward — unused when choosing the model.")
        )
      )
    ),
    tags$h3(class = "section-title", "Signals used (known before the month)"),
    tags$p(
      class = "lede lede--wide",
      "No look-ahead. Each input is observable from the official series before the month opens."
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
    tags$h3(class = "section-title", "Model family"),
    div(
      class = "model-grid",
      div(
        class = "model-card",
        tags$h4("Regularised regression"),
        tags$p(
          "Ridge, lasso, and elastic net. Shrinkage keeps the fit stable when predictors move together. ",
          "Elastic net is the production choice."
        )
      ),
      div(
        class = "model-card",
        tags$h4("Linear benchmarks"),
        tags$p(
          "Ordinary least squares with lean and due-stock feature sets. ",
          "A compact due-stock specification remains on the shortlist."
        )
      ),
      div(
        class = "model-card",
        tags$h4("Reference checks"),
        tags$p(
          "Seasonal carry-forward and a smooth due-stock curve. ",
          "Useful diagnostics; not selected for production."
        )
      )
    ),
    tags$h3(class = "section-title", "How we validate"),
    tags$p(
      class = "lede lede--wide",
      "Inside the fit window we roll forward in time: train on the past, score the next six months, repeat. ",
      "Only after that do we evaluate September 2024–August 2026 once."
    ),
    tags$h3(class = "section-title", "Production shortlist"),
    tags$p(
      class = "lede lede--wide",
      "Models that beat the trailing-average baseline on both Category A and B in the held-out window."
    ),
    uiOutput(ns("shortlist")),
    tags$h3(class = "section-title", "Quota layer"),
    tags$p(
      class = "lede lede--wide",
      "After exits are forecast, we apply the mechanical replacement rule ",
      "(about 25% of trailing twelve-month exits; A/B retain 90%, 10% feeds Category E). ",
      "Published tender quotas can still differ when LTA adjusts for policy — the Forecast tab shows both."
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    output$shortlist <- renderUI({
      sl <- store$shortlist
      if (is.null(sl) || !nrow(sl)) {
        return(tags$p(class = "footnote", "Shortlist unavailable."))
      }
      # Keep primary + top two others for a product feel
      show <- utils::head(sl, 3)
      tiles <- lapply(seq_len(nrow(show)), function(i) {
        row <- show[i, ]
        label <- switch(
          as.character(row$model),
          elastic_net = "Elastic net",
          ols_due = "Due-stock linear",
          lasso = "Lasso",
          gam_due = "Smooth due-stock",
          ridge = "Ridge",
          ols_lean = "Lean linear",
          as.character(row$model)
        )
        div(
          class = paste("short-tile", if (i == 1) "short-tile--primary"),
          tags$span(class = "nx-kicker", if (i == 1) "Production" else "Reserve"),
          tags$p(class = "short-name", label),
          tags$p(
            class = "short-meta",
            sprintf(
              "Typical monthly miss · A %s · B %s",
              format(round(row$A), big.mark = ","),
              format(round(row$B), big.mark = ",")
            )
          )
        )
      })
      div(class = "short-grid", tiles)
    })
  })
}
