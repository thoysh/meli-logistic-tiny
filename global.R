
credentials <- data.frame(
  user = c("user", "admin"), # mandatory
  password = c("user", "admin"), # mandatory
  start = c("2019-04-15"), # optinal (all others)
  expire = c(NA, NA),
  admin = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)

timezoneTy <- "America/Sao_Paulo"
