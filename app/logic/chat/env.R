# Bulletproof project .Renviron loader for Bedrock / NIM.
# Does not depend on getwd() being the repo root.

CHAT_ENV_KEYS <- c(
  "COEINTEL_AWS_ACCESS_KEY_ID",
  "COEINTEL_AWS_SECRET_ACCESS_KEY",
  "COEINTEL_AWS_REGION",
  "BEDROCK_MODEL_ID",
  "COE_CHAT_PROVIDER",
  "NVIDIA_API_KEY",
  "NVIDIA_NIM_MODEL"
)

`%||%` <- function(a, b) if (is.null(a) || !length(a) || !nzchar(as.character(a)[[1]])) b else a

strip_quotes <- function(x) {
  x <- as.character(x)[[1]]
  if (grepl('^".*"$', x) || grepl("^'.*'$", x)) {
    return(substr(x, 2L, nchar(x) - 1L))
  }
  x
}

#' Find repo root by walking up for rhino.yml (or data/processed).
#' @export
find_project_root <- function(root = NULL) {
  starts <- character()
  if (!is.null(root) && nzchar(as.character(root)[[1]])) {
    starts <- c(starts, as.character(root)[[1]])
  }
  bp <- getOption("box.path", "")
  if (is.character(bp) && length(bp) && nzchar(bp[[1]])) {
    starts <- c(starts, bp[[1]])
  }
  starts <- c(starts, getwd())
  # Common absolute anchors when Shiny/rsconnect cwd is elsewhere
  starts <- c(
    starts,
    Sys.getenv("COE_PROJECT_ROOT", ""),
    "C:/COE Intelligence",
    "C:/COE Intelligence/"
  )
  starts <- unique(starts[nzchar(starts)])

  for (start in starts) {
    here <- tryCatch(
      normalizePath(start, winslash = "/", mustWork = FALSE),
      error = function(e) start
    )
    for (i in seq_len(20)) {
      if (file.exists(file.path(here, "rhino.yml")) ||
            file.exists(file.path(here, "data", "processed", "vehicle_flows.rds"))) {
        return(here)
      }
      parent <- dirname(here)
      if (identical(parent, here)) break
      here <- parent
    }
  }
  NA_character_
}

#' Parse KEY=VALUE lines ourselves (more reliable than readRenviron on Windows).
apply_renviron_file <- function(path) {
  if (!file.exists(path)) return(FALSE)
  lines <- tryCatch(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character()
  )
  if (!length(lines)) return(FALSE)
  n_set <- 0L
  for (raw in lines) {
    line <- trimws(raw)
    if (!nzchar(line) || startsWith(line, "#")) next
    # Allow export KEY=VAL
    line <- sub("^export\\s+", "", line)
    eq <- regexpr("=", line, fixed = TRUE)
    if (eq < 1L) next
    key <- trimws(substr(line, 1L, eq - 1L))
    val <- trimws(substr(line, eq + 1L, nchar(line)))
    val <- strip_quotes(val)
    if (!nzchar(key)) next
    do.call(Sys.setenv, stats::setNames(list(val), key))
    n_set <- n_set + 1L
  }
  n_set > 0L
}

#' @export
renviron_candidate_paths <- function(root = NULL) {
  paths <- character()
  proj <- find_project_root(root)
  if (!is.na(proj) && nzchar(proj)) {
    paths <- c(paths, file.path(proj, ".Renviron"))
  }
  if (!is.null(root) && nzchar(as.character(root)[[1]])) {
    paths <- c(paths, file.path(as.character(root)[[1]], ".Renviron"))
  }
  bp <- getOption("box.path", "")
  if (is.character(bp) && length(bp) && nzchar(bp[[1]])) {
    paths <- c(paths, file.path(bp[[1]], ".Renviron"))
  }
  paths <- c(paths, file.path(getwd(), ".Renviron"), "C:/COE Intelligence/.Renviron")
  # unique existing
  paths <- unique(normalizePath(paths, winslash = "/", mustWork = FALSE))
  paths[file.exists(paths)]
}

#' Load Bedrock/NIM secrets into this process. Always re-applies project file.
#' @export
ensure_project_renviron <- function(root = NULL) {
  paths <- renviron_candidate_paths(root)
  if (!length(paths)) return(invisible(FALSE))
  # Prefer path next to rhino.yml
  ranked <- paths
  has_rhino <- vapply(paths, function(p) {
    file.exists(file.path(dirname(p), "rhino.yml"))
  }, logical(1))
  ranked <- c(paths[has_rhino], paths[!has_rhino])
  ok <- FALSE
  for (path in unique(ranked)) {
    # Manual parse first
    if (isTRUE(apply_renviron_file(path))) ok <- TRUE
    # Also native reader as backup
    tryCatch(readRenviron(path), error = function(e) NULL)
  }
  invisible(ok)
}

#' @export
coeintel_aws_ready <- function() {
  nzchar(Sys.getenv("COEINTEL_AWS_ACCESS_KEY_ID", "")) &&
    nzchar(Sys.getenv("COEINTEL_AWS_SECRET_ACCESS_KEY", ""))
}

#' Diagnostic string for offline status.
#' @export
chat_env_diag <- function(root = NULL) {
  ensure_project_renviron(root)
  paths <- renviron_candidate_paths(root)
  paste0(
    "key=", nzchar(Sys.getenv("COEINTEL_AWS_ACCESS_KEY_ID", "")),
    " secret=", nzchar(Sys.getenv("COEINTEL_AWS_SECRET_ACCESS_KEY", "")),
    " provider=", Sys.getenv("COE_CHAT_PROVIDER", ""),
    " renviron_files=", if (length(paths)) paste(paths, collapse = " | ") else "NONE",
    " cwd=", getwd(),
    " project=", find_project_root(root) %||% "NA"
  )
}
