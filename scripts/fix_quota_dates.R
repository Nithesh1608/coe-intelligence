# Repair level2_quota_test.csv month column (R Date serials written as ints)
path <- "data/processed/model_experiment/level2_quota_test.csv"
df <- utils::read.csv(path, stringsAsFactors = FALSE)
if (is.numeric(df$month) || (is.character(df$month) && grepl("^[0-9]+$", df$month[[1]]))) {
  df$month <- as.character(as.Date(as.numeric(df$month), origin = "1970-01-01"))
}
# Also ensure primary_test_forecast months are ISO
p <- "data/processed/model_experiment/primary_test_forecast.csv"
pf <- utils::read.csv(p, stringsAsFactors = FALSE)
if (is.numeric(pf$month) || (is.character(pf$month) && grepl("^[0-9]+$", pf$month[[1]]))) {
  pf$month <- as.character(as.Date(as.numeric(pf$month), origin = "1970-01-01"))
  utils::write.csv(pf, p, row.names = FALSE)
}
utils::write.csv(df, path, row.names = FALSE)
cat("Fixed months. Sample:\n")
print(utils::head(df[, c("month", "category", "model")], 4))
