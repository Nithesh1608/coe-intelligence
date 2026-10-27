box::use(
  shiny[NS, moduleServer, div, tags, renderText, selectInput, reactive],
  bslib[layout_columns, card, card_header, card_body],
  echarts4r[renderEcharts4r, echarts4rOutput],
)
box::use(
  app/logic/viewdata/charts[two_model_chart],
)

fmt_int <- function(x) {
  format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
}

#' Front page — instantly clear for any audience
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$section(
      class = "desk-hero desk-hero--pitch",
      div(
        class = "desk-hero__copy",
        tags$p(class = "eyebrow", "COE Intelligence"),
        tags$h1("We tell dealers how many cars will leave — before new COE certificates are released."),
        tags$p(
          class = "lede lede--wide",
          "Every month, cars in Category A and Category B leave the road. ",
          "Those exits largely decide how many new certificates come back. ",
          "We forecast the exits. You plan the business."
        )
      ),
      div(
        class = "desk-hero__status",
        tags$span(class = "status-pill status-pill--live", "What you are looking at"),
        tags$p(class = "status-value status-value--sm", "Cars leaving the fleet"),
        tags$p(
          class = "status-note",
          "White = real numbers. Orange and green = our two forecasts. Grey = a simple average people already use."
        )
      )
    ),
    div(
      class = "pitch-strip",
      div(
        class = "pitch-item",
        tags$span(class = "pitch-n", "1"),
        tags$div(
          tags$strong("Predict exits"),
          tags$p("How many Category A and Category B cars leave each month.")
        )
      ),
      div(
        class = "pitch-item",
        tags$span(class = "pitch-n", "2"),
        tags$div(
          tags$strong("Read quota"),
          tags$p("Turn those exits into an outlook for certificates released.")
        )
      ),
      div(
        class = "pitch-item",
        tags$span(class = "pitch-n", "3"),
        tags$div(
          tags$strong("Size your risk"),
          tags$p("If you already sold cars, see the COE cost if the price moves.")
        )
      )
    ),
    div(
      class = "nx-metrics",
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "Full year 2025 · Category A"),
        div(class = "nx-num", shiny::textOutput(ns("kpi_a"), inline = TRUE)),
        tags$span(class = "nx-hint", shiny::textOutput(ns("kpi_a_note"), inline = TRUE))
      ),
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "Full year 2025 · Category B"),
        div(class = "nx-num", shiny::textOutput(ns("kpi_b"), inline = TRUE)),
        tags$span(class = "nx-hint", shiny::textOutput(ns("kpi_b_note"), inline = TRUE))
      ),
      div(
        class = "nx-metric nx-metric--accent",
        tags$span(class = "nx-kicker", "Months we were within 10%"),
        div(class = "nx-num", shiny::textOutput(ns("kpi_hit"), inline = TRUE)),
        tags$span(class = "nx-hint", "Core forecast · versus the simple average")
      ),
      div(
        class = "nx-metric",
        tags$span(class = "nx-kicker", "We do not forecast"),
        div(class = "nx-num nx-num--sm", "COE prices"),
        tags$span(class = "nx-hint", "Only supply and your exposure")
      )
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(
        class = "desk-card",
        full_screen = TRUE,
        card_header(
          div(
            class = "card-head-row",
            tags$span("Cars leaving each month — real versus forecast"),
            selectInput(
              ns("cat"),
              NULL,
              choices = c("Category A" = "A", "Category B" = "B"),
              selected = "A",
              width = "160px"
            )
          )
        ),
        card_body(
          class = "chart-frame chart-frame--tall",
          echarts4rOutput(ns("track"), height = "420px")
        )
      ),
      card(
        class = "desk-card desk-card--story",
        card_header("Read this in ten seconds"),
        card_body(
          tags$ol(
            class = "story-list",
            tags$li(
              tags$strong("White line"),
              " — how many cars actually left that month."
            ),
            tags$li(
              tags$strong("Orange line"),
              " — our Core supply forecast."
            ),
            tags$li(
              tags$strong("Green line"),
              " — our Momentum supply forecast (same idea, adds recent acceleration)."
            ),
            tags$li(
              tags$strong("Grey dashed line"),
              " — simple average of the last twelve months (what many people use today)."
            ),
            tags$li(
              "When orange or green stays close to white, planning is possible. ",
              "Grey falling behind is the gap we close."
            )
          ),
          tags$p(
            class = "footnote",
            "Next: open ", tags$strong("Results"), " for month examples and quota. ",
            "Open ", tags$strong("Your risk"), " if you have cars already sold. ",
            "Open ", tags$strong("How it works"), " only if you want the technical detail."
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    roll <- store$two_rollup
    hits <- store$two_hits

    pick_roll <- function(cat, period_pat, model) {
      if (is.null(roll)) return(NULL)
      hit <- roll$category == cat & grepl(period_pat, roll$period) & roll$model == model
      if (!any(hit)) return(NULL)
      roll[hit, ][1, ]
    }

    output$kpi_a <- renderText({
      r <- pick_roll("A", "2025", "core_forecast")
      if (is.null(r)) "—" else fmt_int(r$actual)
    })
    output$kpi_a_note <- renderText({
      r <- pick_roll("A", "2025", "core_forecast")
      s <- pick_roll("A", "2025", "simple_average")
      if (is.null(r)) return("—")
      sprintf(
        "Core forecast %+.0f%% · simple average %+.0f%%",
        r$pct_off, s$pct_off
      )
    })
    output$kpi_b <- renderText({
      r <- pick_roll("B", "2025", "core_forecast")
      if (is.null(r)) "—" else fmt_int(r$actual)
    })
    output$kpi_b_note <- renderText({
      r <- pick_roll("B", "2025", "core_forecast")
      s <- pick_roll("B", "2025", "simple_average")
      if (is.null(r)) return("—")
      sprintf(
        "Core forecast %+.0f%% · simple average %+.0f%%",
        r$pct_off, s$pct_off
      )
    })
    output$kpi_hit <- renderText({
      if (is.null(hits)) return("—")
      a <- hits[hits$model == "core_forecast" & hits$category == "A", ]
      b <- hits[hits$model == "core_forecast" & hits$category == "B", ]
      sa <- hits[hits$model == "simple_average" & hits$category == "A", ]
      sb <- hits[hits$model == "simple_average" & hits$category == "B", ]
      sprintf(
        "%s / %s  (avg %s / %s)",
        paste0(round(100 * a$within_10pct[[1]]), "%"),
        paste0(round(100 * b$within_10pct[[1]]), "%"),
        paste0(round(100 * sa$within_10pct[[1]]), "%"),
        paste0(round(100 * sb$within_10pct[[1]]), "%")
      )
    })

    chart_df <- reactive({
      dm <- store$two_monthly
      cat <- input$cat
      if (is.null(dm)) return(NULL)
      core <- dm[dm$category == cat & dm$model == "core_forecast", ]
      mom <- dm[dm$category == cat & dm$model == "momentum_forecast", ]
      sim <- dm[dm$category == cat & dm$model == "simple_average", ]
      data.frame(
        month = core$month,
        actual = core$actual,
        core = core$predicted,
        momentum = mom$predicted[match(core$month, mom$month)],
        simple = sim$predicted[match(core$month, sim$month)],
        stringsAsFactors = FALSE
      )
    })

    output$track <- renderEcharts4r({
      two_model_chart(chart_df())
    })
  })
}
