# Knit data/Project.Rmd without pandoc. Working directory is data/.

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
data_dir <- file.path(root, "data")
setwd(data_dir)
dir.create(file.path(root, "reports"), showWarnings = FALSE)

knitr::opts_knit$set(root.dir = data_dir)
knitr::knit(
  file.path(data_dir, "Project.Rmd"),
  output = file.path(root, "reports", "cleaning_report.md")
)
