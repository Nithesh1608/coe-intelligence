box::use(
  shiny[NS, moduleServer, div, tags],
)

#' Placeholder for exploratory analysis page
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$p(class = "eyebrow", "EDA · coming next"),
    tags$h2("Exploratory data analysis"),
    tags$p(
      class = "lede lede--wide",
      "This page is reserved for charts that show the raw series, cohort shapes, ",
      "renewals, and regime breaks — before the model story."
    ),
    div(
      class = "eda-placeholder",
      tags$div(
        class = "eda-placeholder__inner",
        tags$span(class = "nx-kicker", "Scaffold ready"),
        tags$h3("Drop EDA modules here"),
        tags$p(
          "Planned: age-band due stock, revalidation vs PQP, May 2022 A/B break, ",
          "bid-to-quota tightness. Content to be filled once the narrative charts are chosen."
        )
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
