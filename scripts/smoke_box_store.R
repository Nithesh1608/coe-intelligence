root <- normalizePath(".", winslash = "/", mustWork = TRUE)
setwd(root)
options(box.path = root)
box::use(app/logic/viewdata/store[load_store])
s <- load_store()
cat(
  "store OK primary=", s$meta$primary_model,
  " panel=", nrow(s$panel),
  " bidding=", nrow(s$bidding),
  " quota=", nrow(s$quota_test),
  "\n",
  sep = ""
)
