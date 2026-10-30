# Date helpers shared by the views. Model logic lives in app/logic/model/.

box::use(
  app/logic/model/features[add_months, quarter_start, month_seq],
)

#' @export
add_months <- add_months

#' @export
quarter_start <- quarter_start

#' @export
month_seq <- month_seq

#' Quarter label such as "Q1 2027".
#' @export
quarter_label <- function(month) {
  m <- as.Date(month)
  sprintf("Q%d %s", (as.integer(format(m, "%m")) - 1L) %/% 3L + 1L, format(m, "%Y"))
}
