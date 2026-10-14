box::use(
  shiny[NS, moduleServer, tags, HTML],
  bslib[page_navbar, nav_panel, nav_spacer, nav_item, bs_theme, bs_add_rules, font_google],
)
box::use(
  app/logic/viewdata/store[load_store, project_root],
  app/view/overview,
  app/view/supply,
  app/view/feedback,
  app/view/category,
  app/view/exposure,
)

read_asset <- function(rel) {
  paste(readLines(file.path(project_root(), rel), warn = FALSE), collapse = "\n")
}

theme <- bs_theme(
  version = 5,
  bg = "#050505",
  fg = "#fafafa",
  primary = "#ff5c1a",
  secondary = "#a3a3a3",
  success = "#c8f542",
  base_font = font_google("IBM Plex Sans"),
  heading_font = font_google("Syne"),
  code_font = font_google("IBM Plex Mono"),
  "card-bg" = "#0a0a0a",
  "border-color" = "rgba(255,255,255,0.08)"
) |>
  bs_add_rules(read_asset("app/styles/main.scss"))

#' @export
inline_assets <- function() {
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$script(HTML(read_asset("app/js/index.js")))
  )
}

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    title = tags$span(class = "brand", "COE Intelligence"),
    theme = theme,
    fillable = FALSE,
    window_title = "COE Intelligence",
    header = inline_assets(),
    nav_panel("Overview", overview$ui(ns("overview"))),
    nav_panel("Supply", supply$ui(ns("supply"))),
    nav_panel("Feedback", feedback$ui(ns("feedback"))),
    nav_panel("Categories", category$ui(ns("category"))),
    nav_panel("Exposure", exposure$ui(ns("exposure"))),
    nav_spacer(),
    nav_item(tags$span(class = "badge-live", "Supply, not premiums"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    store <- load_store()
    overview$server("overview", store)
    supply$server("supply", store)
    feedback$server("feedback", store)
    category$server("category", store)
    exposure$server("exposure", store)
  })
}
