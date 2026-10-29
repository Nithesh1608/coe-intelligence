setwd("C:/COE Intelligence")
options(shiny.fullstacktrace = TRUE, warn = 1)

box::use(
  shiny[testServer],
  app/logic/viewdata/store[load_store],
  app/view/exposure,
)

store <- load_store()
fail <- function(...) stop(sprintf(...), call. = FALSE)
ok <- function(msg) cat("OK:", msg, "\n")

shiny::testServer(exposure$server, args = list(store = store), {
  session$setInputs(
    category = "A",
    model = "core_forecast",
    n_cars = 50,
    clear_month = "2025-05-01",
    quoted = 130000,
    stress = 10000
  )
  session$flushReact()

  if (!identical(input$clear_month, "2025-05-01")) fail("month")
  ok("inputs set")

  session$setInputs(clear_month = "2025-08-01", n_cars = 80)
  session$flushReact()
  if (!identical(input$clear_month, "2025-08-01")) fail("month wiped")
  ok("month survives")

  session$setInputs(load_demo = 1L)
  session$flushReact()
  session$setInputs(
    n_cars = 50, category = "A", model = "core_forecast",
    clear_month = "2025-05-01", quoted = 130000
  )
  session$flushReact()

  if (is.null(output$lead_kpis)) fail("no lead kpis")
  ok("lead kpis")
  if (is.null(output$supply_strip)) fail("no supply")
  ok("supply strip")
  if (is.null(output$shift_table)) fail("no shift")
  ok("shift table")
  ok(paste("save:", output$save_title))
  ok(paste("suggest:", output$suggest_note))
  ok(paste("hist:", output$hist_note))
})

cat("\nALL DEALER EXPOSURE TESTS PASSED\n")
