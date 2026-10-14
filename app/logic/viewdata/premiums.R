#' @export
monthly_premiums <- function(bidding, cats = c("A", "B", "E")) {
  b <- bidding[bidding$category %in% cats, c("month", "category", "premium")]
  b <- b[!is.na(b$premium), ]
  stats::aggregate(premium ~ month + category, b, mean)
}

#' @export
premium_wide <- function(bidding, cats = c("A", "B", "E"), from = as.Date("2016-01-01")) {
  long <- monthly_premiums(bidding, cats)
  long <- long[long$month >= from, ]
  wide <- stats::reshape(
    long,
    idvar = "month",
    timevar = "category",
    direction = "wide"
  )
  names(wide) <- gsub("premium.", "", names(wide), fixed = TRUE)
  wide[order(wide$month), ]
}
