# Shared .Renviron loader for chat backends (Bedrock / NIM).

`%||%` <- function(a, b) if (is.null(a) || !length(a) || !nzchar(as.character(a)[[1]])) b else a

#' Walk upward from starts looking for rhino.yml / .Renviron.
#' @export
renviron_candidate_dirs <- function(root = NULL) {
  starts <- character()
  if (!is.null(root) && nzchar(as.character(root)[[1]])) {
    starts <- c(starts, as.character(root)[[1]])
  }
  bp <- getOption("box.path", "")
  if (is.character(bp) && length(bp) && nzchar(bp[[1]])) {
    starts <- c(starts, bp[[1]])
  }
  starts <- c(starts, getwd())
  tryCatch({
    box::use(app/logic/viewdata/store[project_root])
    starts <- c(starts, project_root())
  }, error = function(e) NULL)

  dirs <- character()
  for (start in unique(starts[nzchar(starts)])) {
    here <- normalizePath(start, winslash = "/", mustWork = FALSE)
    for (i in seq_len(16)) {
      dirs <- c(dirs, here)
      parent <- dirname(here)
      if (identical(parent, here)) break
      here <- parent
    }
  }
  unique(dirs)
}

#' Read the first usable project .Renviron into this process.
#' @export
ensure_project_renviron <- function(root = NULL) {
  loaded_any <- FALSE
  for (dir in renviron_candidate_dirs(root)) {
    path <- file.path(dir, ".Renviron")
    if (!file.exists(path)) next
    # Prefer the directory that also has rhino.yml (project root).
    if (!file.exists(file.path(dir, "rhino.yml")) && loaded_any) next
    tryCatch(readRenviron(path), error = function(e) NULL)
    loaded_any <- TRUE
    if (file.exists(file.path(dir, "rhino.yml"))) {
      return(invisible(normalizePath(path, winslash = "/", mustWork = FALSE)))
    }
  }
  invisible(if (loaded_any) TRUE else FALSE)
}

#' @export
coeintel_aws_ready <- function() {
  nzchar(Sys.getenv("COEINTEL_AWS_ACCESS_KEY_ID", "")) &&
    nzchar(Sys.getenv("COEINTEL_AWS_SECRET_ACCESS_KEY", ""))
}
