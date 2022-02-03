#' Define template de Shinyalert
#'
shinyalertTy <- function(...) {
  shinyalert(..., closeOnEsc = F, showConfirmButton = F, closeOnClickOutside = T)
}

