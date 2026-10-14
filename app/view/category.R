box::use(
  shiny[NS, moduleServer, div, tags],
  bslib[card, card_header, card_body, layout_columns],
  echarts4r[renderEcharts4r, echarts4rOutput, e_charts, e_line, e_area, e_datazoom, e_color],
)
box::use(
  app/logic/viewdata/charts[style_chart, spread_chart],
  app/logic/viewdata/premiums[premium_wide],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack",
    tags$h2("Category dynamics"),
    tags$p(
      class = "lede",
      "A, B and E are substitutes. Watch the spread and the share of successful ",
      "bids. May 2022 redefined A/B around EV power — the labels stay, the product changed."
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        full_screen = TRUE,
        card_header("Premium spread"),
        card_body(echarts4rOutput(ns("spread"), height = "380px"))
      ),
      card(
        full_screen = TRUE,
        card_header("Share of successful bids (A / B / E)"),
        card_body(echarts4rOutput(ns("share"), height = "380px"))
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Cars aged 8 to under 11 (A and B share this stock)"),
      card_body(echarts4rOutput(ns("age"), height = "320px"))
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    output$spread <- renderEcharts4r({
      spread_chart(premium_wide(store$bidding, from = as.Date("2016-01-01")))
    })

    output$share <- renderEcharts4r({
      b <- store$bidding
      b <- b[b$category %in% c("A", "B", "E") & b$month >= as.Date("2016-01-01"), ]
      tot <- stats::aggregate(bids_success ~ month, b, sum)
      names(tot)[2] <- "total"
      m <- merge(b, tot, by = "month")
      m$share <- 100 * m$bids_success / m$total
      wide <- stats::reshape(
        m[, c("month", "category", "share")],
        idvar = "month",
        timevar = "category",
        direction = "wide"
      )
      names(wide) <- gsub("share.", "", names(wide), fixed = TRUE)
      wide <- wide[order(wide$month), ]
      wide$month <- format(as.Date(wide$month), "%Y-%m")
      wide |>
        e_charts(month) |>
        e_area(A, stack = "share", smooth = TRUE) |>
        e_area(B, stack = "share", smooth = TRUE) |>
        e_area(E, stack = "share", smooth = TRUE) |>
        e_color(c("#7dd3fc", "#c4b5fd", "#fb7185")) |>
        style_chart() |>
        e_datazoom(type = "inside")
    })

    output$age <- renderEcharts4r({
      age <- store$age
      age <- age[age$vehicle_type == "Cars", ]
      bands <- c("8 - < 9", "9 - < 10", "10 - < 11")
      age <- age[age$age_band %in% bands, ]
      tot <- stats::aggregate(number ~ month, age, sum)
      names(tot) <- c("month", "near_expiry")
      tot$month <- format(as.Date(tot$month), "%Y-%m")
      tot |>
        e_charts(month) |>
        e_line(near_expiry, symbol = "none", smooth = TRUE) |>
        style_chart() |>
        e_datazoom(type = "inside")
    })
  })
}
