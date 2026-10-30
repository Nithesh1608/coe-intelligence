box::use(
  shiny[NS, moduleServer, tags, HTML],
  bslib[page_navbar, nav_panel, nav_spacer, nav_item, bs_theme, bs_add_rules, font_google],
)
box::use(
  app/logic/viewdata/store[load_store, project_root],
  app/logic/chat/nim[ensure_nvidia_env],
  app/view/dashboard,
  app/view/scenarios,
  app/view/forecast,
  app/view/eda,
  app/view/exposure,
  app/view/coe_bot,
  app/view/about,
)

read_asset <- function(rel) {
  root <- tryCatch(project_root(), error = function(e) getwd())
  path <- file.path(root, rel)
  if (!file.exists(path)) return("")
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

safe_font <- function(family, fallback = NULL) {
  tryCatch(
    font_google(family),
    error = function(e) fallback
  )
}

theme <- tryCatch({
  bs_theme(
    version = 5,
    bg = "#050505",
    fg = "#fafafa",
    primary = "#ff5c1a",
    secondary = "#a3a3a3",
    success = "#c8f542",
    base_font = safe_font("IBM Plex Sans"),
    heading_font = safe_font("Syne"),
    code_font = safe_font("IBM Plex Mono"),
    "card-bg" = "#0a0a0a",
    "border-color" = "rgba(255,255,255,0.08)"
  ) |>
    bs_add_rules(read_asset("app/styles/main.scss"))
}, error = function(e) {
  warning("Theme/SCSS failed, using minimal dark theme: ", conditionMessage(e))
  bs_theme(version = 5, bg = "#050505", fg = "#fafafa", primary = "#ff5c1a")
})

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
    nav_panel("Scenarios", scenarios$ui(ns("scenarios"))),
    nav_panel("Market", eda$ui(ns("eda"))),
    nav_panel("Results", forecast$ui(ns("forecast"))),
    nav_panel("Your risk", exposure$ui(ns("exposure"))),
    nav_panel("COE bot", coe_bot$ui(ns("coe_bot"))),
    nav_panel("About", about$ui(ns("about"))),
    nav_spacer(),
    nav_item(tags$span(class = "badge-live", "Supply · exposure"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    tryCatch(ensure_nvidia_env(project_root()), error = function(e) {
      warning("NVIDIA env load skipped: ", conditionMessage(e))
    })
    store <- tryCatch(load_store(), error = function(e) {
      warning("Data load failed: ", conditionMessage(e))
      stop(e)
    })
    dashboard$server("dashboard", store)
    scenarios$server("scenarios", store)
    eda$server("eda", store)
    forecast$server("forecast", store)
    exposure$server("exposure", store)
    coe_bot$server("coe_bot", store)
    about$server("about", store)
  })
}
