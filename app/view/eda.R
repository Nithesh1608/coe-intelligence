box::use(
  shiny[NS, moduleServer, div, tags, addResourcePath],
  app/logic/viewdata/store[project_root],
)

eda_fig <- function(file, alt = "") {
  tags$figure(
    class = "eda-fig",
    tags$img(src = file.path("eda-assets", file), alt = alt, class = "eda-fig__img")
  )
}

eda_section <- function(n, kicker, title, body, files, aside = NULL) {
  figs <- lapply(files, function(f) eda_fig(f, alt = title))
  div(
    class = "eda-beat eda-beat--full",
    div(
      class = "eda-beat__copy eda-beat__copy--wide",
      tags$span(class = "eda-beat__num", sprintf("%02d", n)),
      tags$span(class = "nx-kicker", kicker),
      tags$h3(title),
      tags$p(body),
      if (!is.null(aside)) tags$p(class = "eda-beat__aside", aside)
    ),
    div(class = "eda-fig-stack", figs)
  )
}

#' Full EDA story — original-theme charts from EDA 1 + EDA 2
#' @export
ui <- function(id) {
  ns <- NS(id)
  addResourcePath("eda-assets", file.path(project_root(), "app", "static", "eda"))
  div(
    class = "page-stack desk eda-page",
    tags$p(class = "eyebrow", "EDA"),
    tags$h2("Exploratory story of COE supply"),
    tags$p(
      class = "lede lede--wide",
      "All charts from our EDA notebooks, in reading order. Light figures keep the original ggplot look; ",
      "the narrative walks from raw supply patterns to cohorts, renewals, formula, and price."
    ),
    div(
      class = "eda-spine",
      tags$span("Supply shape"),
      tags$span(class = "eda-spine__dot", "→"),
      tags$span("Season & cycles"),
      tags$span(class = "eda-spine__dot", "→"),
      tags$span("Exits → quota"),
      tags$span(class = "eda-spine__dot", "→"),
      tags$span("Age & lock-in"),
      tags$span(class = "eda-spine__dot", "→"),
      tags$span("Renewals"),
      tags$span(class = "eda-spine__dot", "→"),
      tags$span("Formula & price")
    ),

    eda_section(
      1, "How much supply is there?",
      "Certificate quotas swing for years — they are not a smooth line",
      paste(
        "Monthly auction quotas for Categories A, B and E show long waves and deep troughs.",
        "A dealer pricing packages against “average supply” is pricing against a fiction."
      ),
      c("01_COE_supply_over_time.png", "02_COE_supply_volatility.png"),
      "EDA 2 · RQ1 — level and rolling volatility."
    ),

    eda_section(
      2, "Within the year and across years",
      "Seasonality is mild; the multi-year cycle is the real story",
      paste(
        "Month-of-year patterns exist but are secondary. Calendar-year averages show the longer boom–bust",
        "that matters for fleet replacement windows."
      ),
      c("04_Monthly_patterns.png", "05_Long_term_movements.png"),
      "EDA 2 · RQ3 — monthly patterns and annual averages."
    ),

    eda_section(
      3, "The mechanical link",
      "Cars leaving the road set up the next certificates",
      paste(
        "LTA’s replacement rule looks back at recent deregistrations. When exits rise, the next quota quarter",
        "usually follows — that is the lever our elastic-net models learn."
      ),
      "03_Deregistrations_and_supply.png",
      "EDA 2 · RQ2 — prior 12-month exits vs subsequent quarterly quota."
    ),

    eda_section(
      4, "What is already on the road",
      "A large age cliff is due by ~2029; a thin cohort follows",
      paste(
        "Age structure is the slow clock. Cars aged 7–9 hit the year-10 decision soon; ages 2–4 were bought",
        "in the tight-quota years and leave a thinner pipeline behind."
      ),
      c("e1_02_age_profile.png", "08_Vehicle_age_distribution.png"),
      "EDA 1 chart 2 and EDA 2 · RQ5 — two views of the same age snapshot."
    ),

    eda_section(
      5, "Some exits are already booked",
      "Five-year renewals lock scrap dates five years ahead",
      paste(
        "Those cars cannot renew again. The locked-in exit cushion was thicker in 2021–24 than in 2026–28 —",
        "knowable today, before any model."
      ),
      "e1_04_locked_exits.png",
      "EDA 1 chart 4 — exits implied by completed 5-year revalidations."
    ),

    eda_section(
      6, "Renewal is a price decision",
      "When renewing gets expensive, more owners scrap instead",
      paste(
        "Renewal activity over time, and the yearly renewal–PQP scatter, both show behaviour on top of the cohort.",
        "Expensive PQP lines up with lower renewal shares — freeing certificates rather than holding them back."
      ),
      c("06_Renewal_activity.png", "e1_05_renewal_pqp.png"),
      "EDA 2 · RQ4 and EDA 1 chart 5."
    ),

    eda_section(
      7, "Quota is formula plus LTA choice",
      "Since 2023 a large share of A/B certificates sat off the pure 25% rule",
      paste(
        "Grey is what the replacement rule gives; blue is extra LTA added; red is held back.",
        "Supply is partly mechanical — and partly discretionary."
      ),
      "e1_03_quota_rule.png",
      "EDA 1 chart 3 — quarterly stacked bars from Feb 2023."
    ),

    eda_section(
      8, "Premiums in context — not our forecast target",
      "Supply timing still shows up in clearing prices",
      paste(
        "Long premium history with regime shading, monthly supply vs premium, and YoY quota–premium scatters",
        "explain why a locked order book cares about supply. We still do not forecast the next premium."
      ),
      c("e1_01_premiums.png", "07_Supply_and_premiums.png", "e1_06_supply_price.png"),
      "EDA 1 charts 1 & 6 and EDA 2 · RQ5."
    ),

    tags$p(
      class = "footnote eda-close",
      "Next: Results shows held-out elastic-net forecasts. Your risk prices a locked book under supply paths."
    )
  )
}

#' @export
server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    addResourcePath("eda-assets", file.path(project_root(), "app", "static", "eda"))
    invisible(store)
  })
}
