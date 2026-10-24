# Knit data/data_cleaning.Rmd. Working directory is data/.

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
data_dir <- file.path(root, "data")
dir.create(file.path(root, "reports"), showWarnings = FALSE)
setwd(data_dir)

knitr::opts_knit$set(root.dir = data_dir)

has_pandoc <- tryCatch(
  rmarkdown::pandoc_available("2.8"),
  error = function(e) FALSE
)

if (isTRUE(has_pandoc)) {
  rmarkdown::render(
    file.path(data_dir, "data_cleaning.Rmd"),
    output_file = "cleaning_report.html",
    output_dir = file.path(root, "reports"),
    quiet = FALSE
  )
} else {
  knitr::knit(
    file.path(data_dir, "data_cleaning.Rmd"),
    output = file.path(root, "reports", "cleaning_report.md")
  )
  message("Pandoc missing: wrote reports/cleaning_report.md")
}
