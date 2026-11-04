# Rhino / shinyApp entrypoint.
# Force-load Bedrock keys before any module runs (cwd-independent).
local({
  candidates <- c(
    file.path(getwd(), ".Renviron"),
    "C:/COE Intelligence/.Renviron"
  )
  here <- tryCatch(
    normalizePath(getwd(), winslash = "/", mustWork = FALSE),
    error = function(e) getwd()
  )
  for (i in seq_len(12)) {
    candidates <- c(candidates, file.path(here, ".Renviron"))
    if (file.exists(file.path(here, "rhino.yml"))) break
    parent <- dirname(here)
    if (identical(parent, here)) break
    here <- parent
  }
  candidates <- unique(normalizePath(candidates, winslash = "/", mustWork = FALSE))
  for (path in candidates) {
    if (!file.exists(path)) next
    lines <- tryCatch(
      readLines(path, warn = FALSE, encoding = "UTF-8"),
      error = function(e) character()
    )
    for (raw in lines) {
      line <- trimws(raw)
      if (!nzchar(line) || startsWith(line, "#")) next
      eq <- regexpr("=", line, fixed = TRUE)
      if (eq < 1L) next
      key <- trimws(substr(line, 1L, eq - 1L))
      val <- trimws(substr(line, eq + 1L, nchar(line)))
      if ((startsWith(val, "\"") && endsWith(val, "\"")) ||
            (startsWith(val, "'") && endsWith(val, "'"))) {
        val <- substr(val, 2L, nchar(val) - 1L)
      }
      if (nzchar(key)) do.call(Sys.setenv, stats::setNames(list(val), key))
    }
    tryCatch(readRenviron(path), error = function(e) NULL)
  }
})
rhino::app()
