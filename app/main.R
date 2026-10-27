box::use(
  shiny[NS, moduleServer, tags, HTML],
  bslib[page_navbar, nav_panel, nav_spacer, nav_item, bs_theme, bs_add_rules, font_google],
)
box::use(
  app/logic/viewdata/store[load_store, project_root],
  app/view/dashboard,
  app/view/forecast,
  app/view/method,
  app/view/eda,
  app/view/exposure,
  app/view/about,
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
    nav_panel("Home", dashboard$ui(ns("dashboard"))),
    nav_panel("Results", forecast$ui(ns("forecast"))),
    nav_panel("Your risk", exposure$ui(ns("exposure"))),
    nav_panel("How it works", method$ui(ns("method"))),
    nav_panel("About", about$ui(ns("about"))),
    nav_panel("EDA", eda$ui(ns("eda"))),
    nav_spacer(),
    nav_item(tags$span(class = "badge-live", "Supply desk · not prices"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    store <- load_store()
    dashboard$server("dashboard", store)
    forecast$server("forecast", store)
    method$server("method", store)
    eda$server("eda", store)
    exposure$server("exposure", store)
    about$server("about", store)
  })
}
