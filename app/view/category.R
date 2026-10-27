box::use(
  shiny[NS, moduleServer, div, tags],
  bslib[card, card_header, card_body, layout_columns],
  echarts4r[renderEcharts4r, echarts4rOutput, e_charts, e_line, e_area, e_datazoom, e_color],
)
box::use(
  app/logic/viewdata/charts[style_chart, spread_chart],
  app/logic/viewdata/premiums[premium_wide],
)

#' Proposal view 3 — Category dynamics (A / B / E)
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page-stack desk",
    tags$p(class = "eyebrow", "Categories"),
    tags$h2("Category A, Category B, and Category E move together"),
    tags$p(
      class = "lede",
      "Dealers allocate across substitutes. Watch the premium spread and bid shares. ",
      "May 2022 redefined Category A and Category B around electric-vehicle power — labels stayed, the product changed."
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        class = "desk-card",
        full_screen = TRUE,
        card_header("Premium spread"),
        card_body(class = "chart-frame", echarts4rOutput(ns("spread"), height = "340px"))
      ),
      card(
        class = "desk-card",
        full_screen = TRUE,
        card_header("Share of successful bids"),
        card_body(class = "chart-frame", echarts4rOutput(ns("share"), height = "340px"))
      )
    ),
    card(
      class = "desk-card",
      full_screen = TRUE,
      card_header("Cars aged 8 to under 11 (near-expiry stock)"),
      card_body(class = "chart-frame", echarts4rOutput(ns("age"), height = "340px"))
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
      tot <- stats::aggregate(bids_success ~ month, b, sum, na.rm = TRUE)
      names(tot)[2] <- "total"
      m <- merge(b, tot, by = "month")
      m$share <- 100 * m$bids_success / pmax(m$total, 1)
      wide <- stats::reshape(
        m[, c("month", "category", "share")],
        idvar = "month",
        timevar = "category",
        direction = "wide"
      )
      names(wide) <- gsub("share.", "", names(wide), fixed = TRUE)
      wide <- wide[order(wide$month), ]
      wide$month <- format(as.Date(wide$month), "%Y-%m")
      for (col in c("A", "B", "E")) {
        if (!col %in% names(wide)) wide[[col]] <- NA_real_
      }
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
      if (is.null(age) || !nrow(age)) return(NULL)
      age <- age[age$vehicle_type == "Cars", ]
      if ("age_lower" %in% names(age)) {
        age <- age[age$age_lower %in% 8:10, ]
        tot <- stats::aggregate(number ~ month, age, sum, na.rm = TRUE)
      } else {
        bands <- c("8 - < 9", "9 - < 10", "10 - < 11")
        age <- age[age$age_band %in% bands, ]
        tot <- stats::aggregate(number ~ month, age, sum, na.rm = TRUE)
      }
      names(tot) <- c("month", "near_expiry")
      tot$month <- format(as.Date(tot$month), "%Y-%m")
      tot <- tot[order(tot$month), ]
      tot |>
        e_charts(month) |>
        e_line(near_expiry, symbol = "none", smooth = TRUE) |>
        style_chart() |>
        e_datazoom(type = "inside")
    })
  })
}
